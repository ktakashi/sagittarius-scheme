;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/transport/io.scm - SSH2 protocol transport I/O
;;;  
;;;   Copyright (c) 2010-2025  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

#!nounbound
(library (rfc ssh transport io)
    (export ssh-read-packet
	    ssh-write-packet
	    ssh-write-ssh-message
	    ssh-write-packet-port
	    ssh-data-ready?
	    *ssh:debug-package-handler*
	    *ssh:ignore-package-handler*
	    *ssh:ext-info-handler*)
    (import (rnrs)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (binary io)
	    (srfi :39 parameters)
	    (sagittarius)
	    (sagittarius socket)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto mac)
	    (sagittarius crypto random)
	    (util bytevector))

;; as far as i know, all block cipher has 2^n size (8 or 16) thus 8192 is
;; multiple of them.
;; NB: for some reason Windows RCVBUF is set to 8KB by default.
;;     reading more than that would cause performance issue.
(define-constant +packet-buffer-size+ (* 1024 8))

(define (default-handler payload))
(define (default-debug-handler payload)
  (let ((msg (read-message <ssh-msg-debug>
			   (open-bytevector-input-port payload))))
    (when (~ msg 'always-display)
      (display (~ msg 'message)) (newline))))
(define (default-ext-info-handler extensions)
  ;; does nothing :)
  #t)

(define *ssh:ignore-package-handler* (make-parameter default-handler))
(define *ssh:debug-package-handler* (make-parameter default-debug-handler))
(define *ssh:ext-info-handler* (make-parameter default-ext-info-handler))

(define (ssh-read-packet context)
  (define (read-as-chunk bvs)
    (lambda (size)
      (list->vector (align-bytevectors bvs size))))

  ;; FIXME this is not a good implementation...
  (define (read&decrypt mac-length)
    (define c (~ context 'server-cipher))
    (define block-size (block-cipher-block-length c))
    (define in (~ context 'socket))

    (define hmac
      (let ((m (~ context 'server-mac)))
	(mac-init! m)
	(mac-process! m (integer->bytevector (~ context 'server-sequence) 4))
	m))
    (define (verify-mac transport mac)
      (let ((bv (make-bytevector (mac-mac-size hmac))))
	(mac-done! hmac bv)
	(bytevector=? mac bv)))
    (define (read-block in size)
      (define (read-block1 size)
	(define buf-size (min size +packet-buffer-size+))
	(define buf (make-bytevector buf-size))
	(define (finish buf)
	  (let ((pt (block-cipher-decrypt-last-block c buf)))
	    (mac-process! hmac pt)
	    pt))
	(let loop ((rest buf-size) (read-size 0))
	  (let ((n (socket-recv! in buf (+ 0 read-size) rest)))
	    (if (= n rest)
		(finish buf)
		(loop (- rest n) (+ read-size n))))))
      (let ((count (ceiling (/ size +packet-buffer-size+))))
	(let loop ((size size) (r '()))
	  (if (zero? size)
	      (reverse! r)
	      (let ((bv (read-block1 size)))
		(loop (- size (bytevector-length bv)) (cons bv r)))))))
    ;; TODO there are still multiple of possibly huge allocation
    ;; i would like to have it only once. (or even zero if we change
    ;; read-packet to return port.)
    (let* ((firsts (read-block in block-size))
	   (first (car firsts))
	   ;; i've never seen block cipher which has block size less
	   ;; than 8
	   (total-size (bytevector-u32-ref first 0 (endianness big)))
	   (padding-size (bytevector-u8-ref first 4))
	   ;; hope the rest have multiple of block size...
	   (rest-size (- (+ total-size 4) block-size))
	   (rest (read-block in rest-size))
	   (mac  (socket-recv in mac-length))
	   (pt   (->chunked-binary-input-port 
		  (read-as-chunk (cons first rest)))))
      (verify-mac context mac)
      ;; we know chunked-binary port has set-port-position!
      (set-port-position! pt 5)
      ;; FIXME we even don't want to do this
      ;; make read-packet return port...
      (get-bytevector-n pt (- total-size padding-size 1))))
  (define (recv-n in n)
    (define buf (make-bytevector n))
    (let loop ((n n) (start 0))
      (let ((r (socket-recv! in buf start n)))
	(if (= r n)
	    buf
	    (loop (- n r) (+ start r))))))
  (define (read-data in)
    (let* ((sizes (recv-n in 5))
	   (total (bytevector-u32-ref sizes 0 (endianness big)))
	   (pad   (bytevector-u8-ref sizes 4)))
      (rlet1 payload (recv-n in (- total pad 1))
	     ;; discards padding
	     (recv-n in pad))))

  (let* ((mac-length (or (and-let* ((k (~ context 'client-cipher))
				    (h (~ context 'server-mac))
				    ( (mac? h) ))
			   (mac-mac-size h))
			 0))
	 (payload (if (zero? mac-length)
		      (read-data (~ context 'socket))
		      (read&decrypt mac-length)))
	 (type (bytevector-u8-ref payload 0)))
    (set! (~ context 'server-sequence) (+ (~ context 'server-sequence) 1))
    (cond ((= type +ssh-msg-ignore+)
	   ((*ssh:ignore-package-handler*) payload)
	   (ssh-read-packet context))
	  ((= type +ssh-msg-debug+)
	   ((*ssh:debug-package-handler*) payload)
	   (ssh-read-packet context))
	  ((= type +ssh-msg-unimplemented+)
	   (error 'ssh-read-packet "The previous sequence is not implemented"
		  ;; should we deserialize?
		  payload))
	  ((= type +ssh-msg-ext-info+)
	   ((*ssh:ext-info-handler*) (parse-ext-info payload))
	   (ssh-read-packet context))
	  (else payload))))

(define (ssh-write-packet context msg)
  (ssh-write-packet-port context (open-bytevector-input-port msg)
			 (bytevector-length msg)))
(define (ssh-write-ssh-message context msg)
  (let-values (((in size) (ssh-message->binary-port msg)))
    (ssh-write-packet-port context in size)))

(define (ssh-write-packet-port context in size)
  (define c (~ context 'client-cipher))
  (define block-size (if c (block-cipher-block-length c) 8))
  (define out (~ context 'socket))
  
  (define total-len 
    (let ()
      (define (total-size data-len)
	(define (round-up l)
	  (let ((size (if c (- (block-cipher-block-length c) 1) 7)))
	    (bitwise-and (+ l size) (bitwise-not size))))
	(round-up (+ data-len 4 1 4)))
      (total-size size)))
  (define padding (random-generator-read-random-bytes (~ context 'prng)
						      (- total-len 5 size)))
  (define hmac
    (and-let* ((m (~ context 'client-mac))
	       ( (mac? m) ))
      (mac-init! m)
      (mac-process! m (integer->bytevector (~ context 'client-sequence) 4))
      m))
  (define (encrypt&send c hmac out)
    ;; make buffer a bit bigger so that it won't call get-bytevector-n!
    ;; millions times
    (define buffer-size (* block-size 512))
    (define buffer (make-bytevector buffer-size 0))
    
    (define (do-send out c hmac packet)
      (when hmac (mac-process! hmac packet))
      (let ((m (if c (block-cipher-encrypt-last-block c packet) packet)))
	(socket-send out m)))

    ;; fist time will use block-size
    (define (do-first c hmac buffer)
      (bytevector-u32-set! buffer 0 (- total-len 4) (endianness big))
      (bytevector-u8-set! buffer 4 (bytevector-length padding))
      (let ((n (get-bytevector-n! in buffer 5 (- block-size 5))))
	(if (or (eof-object? n) (< n (- block-size 5)))
	    (let* ((i (+ 5 n))
		   (len (- block-size i)))
	      (bytevector-copy! padding 0 buffer i len)
	      (do-send out c hmac buffer)
	      (do-send out c hmac (bytevector-copy padding len))
	      #f)
	    (do-send out c hmac buffer))))
    (when (do-first c hmac (make-bytevector block-size 0))
      (let loop ((n (get-bytevector-n! in buffer 0 buffer-size)))
	(cond ((eof-object? n) (do-send out c hmac padding))
	      ((< n buffer-size)
	       (let ((rest (- buffer-size n))
		     (padlen (bytevector-length padding)))
		 (cond ((< rest padlen)
			;; not sure if this is correct and ever happen
			(bytevector-copy! padding 0 buffer n rest)
			(do-send out c hmac buffer)
			;; in this case padding must be longer than block
			;; size and copying from rest must make the length
			;; to block-size
			(let ((p (bytevector-copy padding rest)))
			  (unless (= (bytevector-length p) block-size)
			    (error 'ssh-write-packet-port
				   "[Internal] invalid padding size"))
			  (do-send out c hmac p)))
		       (else
			(bytevector-copy! padding 0 buffer n padlen)
			(do-send out c hmac 
				 (bytevector-copy buffer 0 (+ n padlen)))))))
	      (else 
	       (do-send out c hmac buffer)
	       (loop (get-bytevector-n! in buffer 0 buffer-size)))))))
  (encrypt&send c hmac out)
  (set! (~ context 'client-sequence) (+ (~ context 'client-sequence) 1))
  (when hmac
    (let ((mac (make-bytevector (mac-mac-size hmac))))
      (mac-done! hmac mac)
      (socket-send out mac))))

(define (ssh-data-ready? transport :optional (timeout 1000))
    (let1 reads (socket-read-select timeout (~ transport 'socket))
      (not (null? reads))))

(define (parse-ext-info payload)
  (define (read-extension bin)
    (let* ((e (ssh-read-message <ssh-msg-ext-info-extension> bin))
	   (n (~ e 'name))
	   (v (~ e 'value)))
      (cons n
	    (cond ((string=? n +extention-server-sig-algs+)
		   (let ((in (open-bytevector-input-port v)))
		     (~ (ssh-read-message <name-list> in) 'names)))
		  ((string=? n +extention-delay-compression+)
		   (let* ((in (open-bytevector-input-port v))
			  (c->s (ssh-read-message <name-list> in))
			  (s->c (ssh-read-message <name-list> in)))
		     (cons (~ c->s 'names) (~ s->c 'names))))
		  ((string=? n +extention-no-flow-control+)
		   (let ((in (open-bytevector-input-port v)))
		     (ssh-read-message :utf8-string in #f)))
		  ((string=? n +extention-elevation+)
		   (let ((in (open-bytevector-input-port v)))
		     (ssh-read-message :utf8-string in #f)))
		  ;; simply return the payload, we can't handle it
		  (else v)))))
      
  (define bin (open-bytevector-input-port payload))
  (define msg (ssh-read-message <ssh-msg-ext-info> bin))
  (do ((i 0 (+ i 1)) (r '() (cons (read-extension bin) r)))
      ((= i (~ msg 'count)) (reverse! r))))
)
