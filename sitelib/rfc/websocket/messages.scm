;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/websocket/messages.scm - RFC 6455 Websocket frame and message
;;;  
;;;   Copyright (c) 2010-2016  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/bv-string
(library (rfc websocket messages)
  (export websocket-send-text
	  websocket-send-binary
	  websocket-send-close
	  websocket-send-ping
	  websocket-send-pong

	  websocket-receive
	  websocket-receive-fragments
	  websocket-compose-close-status
	  websocket-parse-close-status

	  websocket-closed-error?
	  websocket-error-status
	  websocket-error-message
	  
	  ;; parameters
	  *websocket-mask-data?*
	  
	  ;; low level
	  websocket-recv-frame
	  websocket-send-frame!

	  +websocket-continuation-frame+
	  +websocket-text-frame+
	  +websocket-binary-frame+
	  +websocket-close-frame+
	  +websocket-ping-frame+
	  +websocket-pong-frame+
	  )
  (import (rnrs)
	  (rfc websocket connection)
	  (rfc websocket conditions)
	  (rfc tls)
	  (sagittarius)
	  (sagittarius socket)
	  (prefix (binary io) b:)
	  (math random)
	  (srfi :39 parameters)
	  (util concurrent shared-queue))

(define *websocket-mask-data?* (make-parameter #t))
  
(define-constant +masking-key-length+ 4)

(define-condition-type &websocket-closed &websocket
  make-websocket-closed-error websocket-closed-error?
  (status websocket-error-status)
  (message websocket-error-message))

(define (websocket-closed-error who msg data)
  (let-values (((status msg) (if (zero? (bytevector-length data))
				 (values #f #f)
				 (websocket-parse-close-status data))))
    (raise (condition (make-websocket-closed-error status msg)
		      (make-who-condition who)
		      (make-message-condition msg)))))

(define-constant +websocket-continuation-frame+ #x0)
(define-constant +websocket-text-frame+         #x1)
(define-constant +websocket-binary-frame+       #x2)
(define-constant +websocket-close-frame+        #x8)
(define-constant +websocket-ping-frame+         #x9)
(define-constant +websocket-pong-frame+         #xA)

(define-syntax define-control-frame
  (lambda (x)
    (syntax-case x ()
      ((k name)
       (let ((sname (symbol->string (syntax->datum #'name))))
	 (with-syntax ((opcode (datum->syntax #'k
				(string->symbol
				 (string-append "+websocket-"
						sname
						"-frame+"))))
		       (name (datum->syntax #'k
			      (string->symbol
			       (string-append "websocket-send-" sname)))))
	   #'(define (name conn :optional (data #vu8()))
	       (when (> (bytevector-length data) 125)
		 (assertion-violation 'name
		    "Control frame must not have more than 125 octet data"
		    data))
	       ;; receiving the other should be done by application
	       ;; since we don't know if it's sent immediately or not.
	       (websocket-send-frame! (websocket-connection-port conn) opcode
				      (*websocket-mask-data?*) data #t))))))))
(define-control-frame ping)
(define-control-frame pong)

;; returns raw status
(define (websocket-send-close conn :optional (data #vu8()) (wait? #t))
  (define in/out (websocket-connection-port conn))
  (define state (websocket-connection-state conn))
  (define (restore) (websocket-connection-state-set! conn state) #f)
  ;; receiving the other should be done by application
  ;; since we don't know if it's sent immediately or not.
  (unless (<= (bytevector-length data) 125)
    (assertion-violation 'websocket-send-close "data is too big" data))
  ;; if sending frame failed for some reason, then we
  ;; restore the state.
  ;; TODO should we?
  (guard (e ((restore) #f))
      ;; first set status
    ;; if the server is fast enough, then it would send response
    ;; before the procedure ends. And if user level APIs' dispatcher
    ;; received the close frame in the situation, it would raise
    ;; an error.
    (websocket-connection-state-set! conn 'closing)
    (websocket-send-frame! in/out +websocket-close-frame+ #t data #t)
    (socket-shutdown (websocket-connection-socket conn) SHUT_WR))
  
  ;; waits until server returns close
  (when wait?
    (let loop ()
      (let-values (((fin? op data) (websocket-recv-frame in/out)))
	(if (eqv? op +websocket-close-frame+)
	    (begin (websocket-connection-close! conn) data)
	    (loop))))))

(define websocket-compose-close-status
  (case-lambda
   ((status) (integer->bytevector status))
   ((status msg) (bytevector-append (integer->bytevector status)
				    (string->utf8 msg)))))
(define (websocket-parse-close-status data)
  (values (bytevector-u16-ref data 0 (endianness big))
	  (utf8->string data 2)))

;; split allow users to control how to split the given data
;; and where to stop.
(define (%websocket-send-binary conn data start split opcode)
  (define (rec conn data start split opcode)
    (define mask? (*websocket-mask-data?*))
    (define out (websocket-connection-port conn))
    (let loop ((start start) (opcode opcode))
      (let-values (((len end?) (split data start)))
	(websocket-send-frame! out opcode mask? data end? start
			       (+ start len))
	(unless end? (loop (+ start len) +websocket-continuation-frame+)))))

  (define (split-by-size size)
    (lambda (data start)
      (define len (bytevector-length data))
      (if (>= (+ start size) len)
	  (values (- len start) #t)
	  (values size #f))))
  (define (no-split data start) (values (- (bytevector-length data) start) #t))

  (cond ((procedure? split) (rec conn data start split opcode))
	((integer? split)   (rec conn data start (split-by-size split) opcode))
	((not split)        (rec conn data start no-split opcode))
	(else (assertion-violation
	       'websocket-binary "unknown split strategy" split))))
	
(define websocket-send-binary
  (case-lambda
   ((conn data) (websocket-send-binary conn data 0))
   ((conn data start) (websocket-send-binary conn data 0 #f))
   ((conn data start split)
    (%websocket-send-binary conn data start split +websocket-binary-frame+))))

(define websocket-send-text
  (case-lambda
   ((conn data) (websocket-send-text conn data 0))
   ((conn data start) (websocket-send-text conn data 0 #f))
   ((conn data start split)
    (%websocket-send-binary conn (string->utf8 data) start split
			    +websocket-text-frame+))))

;; Receive message
(define (websocket-receive conn :key (push-pong? #f))
  (define (convert opcode data)
    (if (eqv? opcode +websocket-text-frame+)
	(values opcode (utf8->string data))
	(values opcode data)))
  (let-values (((out extract) (open-bytevector-output-port)))
    (websocket-receive-fragments conn
     (lambda (fin? opcode data)
       (put-bytevector out data)
       (if fin? (convert opcode (extract)) (values opcode #f)))
     :push-pong? push-pong?)))

(define (websocket-receive-fragments conn proc :key (push-pong? #f))
  (define (control-opcode? op) (>= op #x8)) ;; >= %x8 are opcode

  ;; Controle frames are basically ignored unless it's close.
  ;; (may raise an error). if push-pong? is true value, then
  ;; it'd push the pong data to the queue so that caller can
  ;; check the response value.
  ;; TODO control frame may increase in future so make this extensible.
  (define (handle-control-frame conn op data)
    (cond ((eqv? op +websocket-ping-frame+)
	   ;; 5.4.  Fragmentation
	   ;; * Control frames (see Section 5.5) MAY be injected in the
	   ;;   middle of a fragmented message.  Control frames themselves
	   ;;   MUST NOT be fragmented.
	   (values (websocket-send-pong conn data) data))
	  ((eqv? op +websocket-pong-frame+)
	   ;; 5.4.  Fragmentation
	   ;; * An endpoint MUST be capable of handling control frames in the
	   ;;   middle of a fragmented message.
	   (when push-pong?
	     (shared-queue-put! (websocket-connection-pong-queue conn) data))
	   (values #t data))
	  ((eqv? op +websocket-close-frame+)
	   ;; if closing? is #t, then we already sent close message.
	   (let ((closing? (websocket-connection-closing? conn)))
	     (unless closing? (websocket-send-close conn #vu8() #f))
	     (websocket-connection-close! conn)
	     ;; TODO should we assume?
	     (if (or closing? (not (websocket-reconnectable-connection? conn)))
		 (values #f data)
		 (websocket-closed-error 'websocket-receive
					 "Server sent close frame" data))))))

    (define in (websocket-connection-port conn))

  (let loop ((opcode #f))
    (let-values (((fin? op data) (websocket-recv-frame in)))
      (cond ((control-opcode? op)
	     (let-values (((cont? data) (handle-control-frame conn op data)))
	       (if cont?
		   (loop opcode)
		   (proc #t op data))))
	    (fin? (proc #t (or opcode op) data))
	    (else
	     (let ((next-op (or opcode op)))
	       (proc #f next-op data)
	       (loop next-op)))))))


;;; Low level APIs
(define (mask key data start len)
  (if (and (bytevector? key) (= +masking-key-length+ (bytevector-length key)))
      (do ((i start (+ i 1)) (j 0 (+ j 1)))
	  ((= j len) data)
	(bytevector-u8-set! data i
	  (bitwise-xor (bytevector-u8-ref data i) 
		       (bytevector-u8-ref key (mod j +masking-key-length+)))))
      data))

(define (websocket-recv-frame in)
  (define (get-payload-length len in)
    (cond ((< len 126) len)
	  ((= len 126) (b:get-u16 in (endianness big)))
	  (else        (b:get-u64 in (endianness big)))))
  ;; if the input port is a socket port, then it may return incomplete
  ;; length of the data. so we need to make sure reading the given number
  ;; of octets.
  (define (read-n-octets in n)
    (define bv (make-bytevector n))
    (let loop ((c 0))
      (if (= c n)
	  bv
	  (let ((r (get-bytevector-n! in bv c (- n c))))
	    (loop (+ c r))))))
  
  (let* ((b1 (get-u8 in))
	 (b2 (get-u8 in))
	 (payload-length (get-payload-length (bitwise-and b2 #x7F) in))
	 ;; TODO remove magic number here
	 (masking-key (and (bitwise-bit-set? b2 7)
			   (read-n-octets in +masking-key-length+)))
	 (payload (if (zero? payload-length)
		      #vu8()
		      (read-n-octets in payload-length))))
    (values (fxbit-set? b1 7)
	    (fxand b1 #x0F)
	    (mask masking-key payload 0 payload-length))))

(define (websocket-send-frame! out opcode mask? data last?
			       :optional (start 0) (end -1))
  (define bvlen (bytevector-length data))
  (define len (if (< end 0) (- bvlen start) (- end start)))
  
  (when (or (< start 0) (> start bvlen)
	    (and (> end 0) (> start end)) (> len bvlen))
    (assertion-violation 'websocket-send-frame! "invalid range" start end))
  (unless (< opcode #xF)
    (assertion-violation 'websocket-send-frame! "invalid opcode" opcode))
  ;; should never happen since we can't make such a huge bytevector
  (when (> len #xFFFFFFFFFFFFFFFF)
    (assertion-violation 'websocket-send-frame! "payload is too big" data))

  (let* ((b1 (fxior (if last? #x80 0) opcode))
	 (l  (cond ((< len 126) len)
		   ((< len #xFFFF) 126)
		   (else 127)))
	 (b2 (fxior (if mask? #x80 0) l))
	 (masking-key
	  (or (and mask? (read-sys-random (* +masking-key-length+ 8))) #vu8()))
	 (klen (bytevector-length masking-key))
	 (plen (cond ((= l 126) 2) ((= l 127) 8) (else 0)))
	 (bv (make-bytevector (+ 2 plen klen len))))

    (bytevector-u8-set! bv 0 b1)
    (bytevector-u8-set! bv 1 b2)
    (cond ((= l 126) (bytevector-u16-set! bv 2 len (endianness big)))
	  ((= l 127) (bytevector-u64-set! bv 2 len (endianness big))))
    (bytevector-copy! masking-key 0 bv (+ 2 plen) klen)
    (bytevector-copy! data start  bv (+ 2 plen klen) len)
    (mask masking-key bv (+ 2 plen klen) len)
    (put-bytevector out bv)
    
    ;; blow should not be used due to the race condition.
    ;; if we want to use it, then we need to either; lock the port or
    ;; call this procedure in atomic environment. the first one was
    ;; ugly, the latter one was not good for performance. so
    ;; make temporary buffer and put it in one go.
    #|
    (put-u8 out b1)
    (put-u8 out b2)
    (cond ((= l 126) (put-u16 len (endianness big)))
	  ((= l 127) (put-u64 len (endianness big))))
    (if mask?
	(let ((data (bytevector-copy data start end)))
	  (put-bytevector out masking-key)
	  (put-bytevector out (mask masking-key data 0 len)))
	(put-bytevector out data start len))
    |#
    (flush-output-port out)))
)
