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
	    ssh-data-ready?
	    *ssh:debug-package-handler*
	    *ssh:ignore-package-handler*
	    *ssh:ext-info-handler*)
    (import (rnrs)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (binary io)
	    (srfi :18 multithreading)
	    (srfi :39 parameters)
	    (sagittarius)
	    (sagittarius socket)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto mac)
	    (sagittarius crypto random)
	    (sagittarius crypto secure)
	    (util bytevector))

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
  (define (recv-n in n)
    (define buf (make-bytevector n))
    (let loop ((n n) (start 0))
      (let ((r (socket-recv! in buf start n)))
	(if (= r n)
	    buf
	    (loop (- n r) (+ start r))))))
  (define (recv-n! in buf n)
    (let loop ((rest n) (read-size 0))
      (let ((n (socket-recv! in buf (+ 0 read-size) rest)))
	(if (= n rest)
	    buf
	    (loop (- rest n) (+ read-size n))))))
  (define hmac
    (and-let* ((m (~ context 'peer-mac))
	       ( (mac? m) )
	       ;; reuse buffer, we will overwrite anyway
	       (buf (~ context 'read-buffer)))
      (bytevector-u32-set! buf 0 (~ context 'peer-sequence) (endianness big))
      (mac-init! m)
      (mac-process! m buf 0 4)
      m))
  ;; FIXME this is not a good implementation...
  (define (read&decrypt mac-length)
    (define c (~ context 'peer-cipher))
    (define block-size (block-cipher-block-length c))
    (define in (~ context 'socket))

    (define (verify-mac transport mac)
      ;; reuse buffer, payload is already copied
      (let ((bv (~ context 'read-buffer))
	    (size (mac-mac-size hmac)))
	(mac-done! hmac bv 0 size)
	(safe-bytevector=? mac bv 0 0 size)))

    (define (read-block in size)
      (define buf (~ context 'read-buffer))
      (define context-buffer-size (bytevector-length buf))

      (define (read-block1 size)
	(define (finish buf)
	  (define pt (make-bytevector size))
	  (let ((n (block-cipher-decrypt! c buf 0 pt 0)))
	    (mac-process! hmac pt)
	    pt))
	(finish (recv-n! in buf size)))

      (let ((count (ceiling (/ size context-buffer-size))))
	(let loop ((size size) (r '()))
	  (if (zero? size)
	      (bytevector-concatenate (reverse! r))
	      (let ((bv (read-block1 (min size context-buffer-size))))
		(loop (- size (bytevector-length bv)) (cons bv r)))))))
    ;; TODO there are still multiple of possibly huge allocation
    ;; i would like to have it only once. (or even zero if we change
    ;; read-packet to return port.)
    (let* ((first (read-block in block-size))
	   ;; i've never seen block cipher which has block size less
	   ;; than 8
	   (total-size (bytevector-u32-ref first 0 (endianness big)))
	   (padding-size (bytevector-u8-ref first 4))
	   ;; hope the rest have multiple of block size...
	   (rest-size (- (+ total-size 4) block-size))
	   (rest (read-block in rest-size))
	   (mac  (recv-n in mac-length))
	   (size (- total-size padding-size 1))
	   (offset (- (bytevector-length first) 5))
	   (payload (make-bytevector size)))
      (verify-mac context mac)
      (let ((first-size (min offset size)))
	(bytevector-copy! first 5 payload 0 first-size)
	(unless (= first-size size)
	  (bytevector-copy! rest 0  payload offset (- size offset))))
      payload))
  
  (define (read-data context)
    (define buffer (~ context 'read-buffer))
    (define in (~ context 'socket))
    (recv-n! in buffer 5)
    (let* ((total (bytevector-u32-ref buffer 0 (endianness big)))
	   (pad   (bytevector-u8-ref buffer 4)))
      (rlet1 payload (recv-n in (- total pad 1))
	     ;; discards padding
	     (recv-n! in buffer pad))))

  (define (rec context)
    (let* ((mac-length (or (and (mac? hmac) (mac-mac-size hmac)) 0))
	   (payload (if (zero? mac-length)
			(read-data context)
			(read&decrypt mac-length)))
	   (type (bytevector-u8-ref payload 0)))
      (set! (~ context 'peer-sequence) (+ (~ context 'peer-sequence) 1))
      ;; FIXME the way handling global message is sloppy
      (cond ((= type +ssh-msg-ignore+)
	     ((*ssh:ignore-package-handler*) payload)
	     (rec context))
	    ((= type +ssh-msg-debug+)
	     ((*ssh:debug-package-handler*) payload)
	     (rec context))
	    ((= type +ssh-msg-unimplemented+)
	     (error 'ssh-read-packet "The previous sequence is not implemented"
		    ;; should we deserialize?
		    payload))
	    ((= type +ssh-msg-ext-info+)
	     (handle-ext-info context payload)
	     (rec context))
	    ((= type +ssh-msg-disconnect+)
	     (let* ((msg (bytevector->ssh-message <ssh-msg-disconnect> payload))
		    (desc (~ msg 'description)))
	       (error 'ssh-read-packet
		      (if (zero? (string-length desc))
			  "Received disconnect message"
			  desc))))
	    (else payload))))
  (mutex-lock! (~ context 'read-lock))
  (guard (e (else (mutex-unlock! (~ context 'read-lock)) (raise e)))
    (let ((payload (rec context)))
      (condition-variable-broadcast! (~ context 'read-cv))
      (mutex-unlock! (~ context 'read-lock))
      payload)))

(define (ssh-write-packet context msg)
  (define c (~ context 'host-cipher))
  ;; minimum packet size is 16
  (define block-size (if c (block-cipher-block-length c) 16))
  (define out (~ context 'socket))
  (define msg-len (bytevector-length msg))
  (define buffer (~ context 'write-buffer))
  (define buffer-len (bytevector-length buffer))
  ;; packet must be minimum of 16, some magic needed
  (define packet-len
    (let ((size (- block-size 1))) ;; 15 or 7, for supporting ciphers
      ;; at least 4 bytes of padding, so add extra 4 here
      (- (bitwise-and (+ msg-len 4 1 4 size) (bitwise-not size)) 4)))
  (define pad-len (- packet-len 1 msg-len))
  
  (define padding
    (random-generator-read-random-bytes (~ context 'prng) pad-len))

  (define hmac
    (and-let* ((m (~ context 'host-mac))
	       ( (mac? m) ))
      ;; the same trick as read :)
      (bytevector-u32-set! buffer 0 (~ context 'host-sequence) (endianness big))
      (mac-init! m)
      (mac-process! m buffer 0 4)
      m))
  (define (do-send m size) (socket-send/range out m 0 size))
  (define (encrypt&send c hmac out)
    (define (encrypt m offset buffer)
      (if c
	  (block-cipher-encrypt! c m offset buffer 0)
	  (bytevector-copy! m offset buffer 0
			    (min (bytevector-length m)
				 (bytevector-length buffer))))
      buffer)

    ;; fist time will use block-size
    (define (do-first)
      (define rest (- block-size 5))
      (define mlen (min rest msg-len))
      (define enc-buffer (make-bytevector block-size))

      (bytevector-u32-set! buffer 0 packet-len (endianness big))
      (bytevector-u8-set! buffer 4 pad-len)
      (when hmac (mac-process! hmac buffer 0 5)) ;; need first 5 bytes
      (bytevector-copy! msg 0 buffer 5 mlen)
      (if (< msg-len rest)
	  (let ((plen (- rest msg-len)))
	    (bytevector-copy! padding 0 buffer (+ 5 msg-len) plen)
	    (do-send (encrypt buffer 0 enc-buffer) block-size)
	    ;; rest of the padding
	    (unless (= plen pad-len)
	      (do-send (encrypt padding plen enc-buffer) block-size))
	    #f)
	  (begin
	    (do-send (encrypt buffer 0 enc-buffer) block-size)
	    mlen)))

    (define (do-last sent)
      (let* ((rest (- msg-len sent))
	     (buf (make-bytevector (+ rest pad-len))))
	(bytevector-copy! msg sent buf 0 rest)
	(bytevector-copy! padding 0 buf rest pad-len)
	(do-send (encrypt buf 0 buffer) (+ rest pad-len))))
    
    (let ((sent (do-first)))
      (when sent
	(do ((i 0 (+ i 1)) (n (div (- msg-len sent) buffer-len)))
	    ((= i n) (do-last (+ (* i buffer-len) sent)))
	  (let* ((offset (+ sent (* i buffer-len)))
		 (e (encrypt msg offset buffer)))
	    (do-send e buffer-len))))))
  (mutex-lock! (~ context 'write-lock))
  (guard (e (else (mutex-unlock! (~ context 'write-lock)) (raise e)))
    (encrypt&send c hmac out)
    (set! (~ context 'host-sequence) (+ (~ context 'host-sequence) 1))
    (when hmac
      (mac-process! hmac msg)
      (mac-process! hmac padding)
      (mac-done! hmac buffer 0 (mac-mac-size hmac))
      (do-send buffer (mac-mac-size hmac)))
    (condition-variable-broadcast! (~ context 'write-cv))
    (mutex-unlock! (~ context 'write-lock))))

(define (ssh-write-ssh-message context msg)
  (ssh-write-packet context (ssh-message->bytevector msg)))

(define (ssh-data-ready? transport :optional (timeout 1000))
    (let1 reads (socket-read-select timeout (~ transport 'socket))
      (not (null? reads))))

(define (handle-ext-info transport payload)
  (let ((ext-info (parse-ext-info payload)))
    (cond ((assq (string->symbol +extension-server-sig-algs+) ext-info) =>
	   (lambda (slot)
	     (set! (~ transport 'server-signature-algorithms) (cdr slot)))))
    ((*ssh:ext-info-handler*) ext-info)))

(define (parse-ext-info payload)
  (define (read-extension bin)
    (let* ((e (ssh-read-message <ssh-msg-ext-info-extension> bin))
	   (n (~ e 'name))
	   (v (~ e 'value)))
      (cons (string->symbol n)
	    (cond ((string=? n +extension-server-sig-algs+)
		   (let ((in (open-bytevector-input-port v)))
		     (~ (ssh-read-message <name-list> in) 'names)))
		  ((string=? n +extension-delay-compression+)
		   (let* ((in (open-bytevector-input-port v))
			  (c->s (ssh-read-message <name-list> in))
			  (s->c (ssh-read-message <name-list> in)))
		     (cons (~ c->s 'names) (~ s->c 'names))))
		  ((string=? n +extension-no-flow-control+)
		   (let ((in (open-bytevector-input-port v)))
		     (ssh-read-message :utf8-string in #f)))
		  ((string=? n +extension-elevation+)
		   (let ((in (open-bytevector-input-port v)))
		     (ssh-read-message :utf8-string in #f)))
		  ;; simply return the payload, we can't handle it
		  (else v)))))
      
  (define bin (open-bytevector-input-port payload))
  (define msg (ssh-read-message <ssh-msg-ext-info> bin))
  (do ((i 0 (+ i 1)) (r '() (cons (read-extension bin) r)))
      ((= i (~ msg 'count)) (reverse! r))))
)
