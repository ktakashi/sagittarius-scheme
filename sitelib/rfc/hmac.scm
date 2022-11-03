;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; hmac.scm - Keyed-Hashing for Message Authentication code library.
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

;; I need a deprecation annotation for procedures...
#!nounbound
(library (rfc hmac)
    (export make-hmac mac? generate-mac generate-mac!
	    mac-init! mac-process! mac-done!
	    ;; for backward compatibility
	    (rename (*mac:hmac* HMAC))
	    <hmac>
	    verify-mac)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius crypto mac)
	    (sagittarius crypto digests)
	    ;; damn...
	    (math hash)
	    (clos user)
	    (crypto mac))

(define (make-hmac key . opts) (apply make-mac *mac:hmac* key opts))

;; Below are deprecated
(define-class <hmac> (<mac>) ()) ;; dummy
(define-method initialize ((o <hmac>) initargs)
  (call-next-method)
  (let-keywords* initargs 
      ((key  (make-bytevector 0))
       (hash *digest:sha-1*))
    (let* ((digest (if (digest-descriptor? hash)
		       hash
		       (hash-algorithm->digest-descriptor hash)))
	   (mac (make-hmac key :digest digest)))
      (slot-set! o 'hash digest)
      (slot-set! o 'init (lambda (me) (mac-init! mac) me))
      (slot-set! o 'process (lambda (me in start end)
			      (mac-process! mac in start (- end start))))
      (slot-set! o 'done (lambda (me out start end)
			   (mac-done! mac out start (- end start))))
      (slot-set! o 'block-size (digest-descriptor-block-size digest))
      (slot-set! o 'hash-size (mac-mac-size mac))
      (slot-set! o 'oid (mac-oid mac)))))
(define-method write-object ((o <hmac>) out)
  (format out "#<hmac ~a>" (slot-ref o 'hash)))

(register-hash *mac:hmac* <hmac>)
)
