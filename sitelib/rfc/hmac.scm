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

(library (rfc hmac)
    (export HMAC <hmac> verify-mac)
    (import (rnrs) (sagittarius)
	    (math)
	    (sagittarius control)
	    (clos user)
	    (crypto mac))

  ;; hash functions
  (define (hmac-init algo)
    (define (pad-0 key blocksize hashsize)
      (do ((i hashsize (+ i 1)))
	  ((= i (- blocksize hashsize)))
	(bytevector-u8-set! key i 0)))
    (let* ((i-key     (slot-ref algo 'i-key))
	   (key       (slot-ref algo 'key))
	   (key-len   (bytevector-length key))
	   (hash-algo (slot-ref algo 'hash))
	   (hashsize  (hash-size hash-algo))
	   (blocksize (hash-block-size hash-algo))
	   (buffer    (make-bytevector blocksize)))
      ;; (1) make sure we have a large enough key
      (cond ((> (bytevector-length i-key) blocksize)
	     (hash! hash-algo key i-key)
	     (when (< hashsize blocksize)
	       (pad-0 key blocksize hashsize))
	     (set! key-len hashsize))
	    (else
	     (bytevector-copy! i-key 0 key 0 (bytevector-length i-key))
	     (when (< key-len blocksize)
	       (pad-0 key blocksize key-len))))
      ;; create the initial vector for step (3)
      (dotimes (i blocksize)
	(bytevector-u8-set! buffer i
			    (bitwise-xor (bytevector-u8-ref key i) #x36)))
      (hash-init! hash-algo)
      (hash-process! hash-algo buffer)))

  (define (hmac-process algo in)
    (hash-process! (slot-ref algo 'hash) in))

  (define (hmac-done algo out)
    (let* ((hash-algo (slot-ref algo 'hash))
	   (hashsize  (hash-size hash-algo))
	   (blocksize (hash-block-size hash-algo))
	   (buffer    (make-bytevector blocksize))
	   (isha      (make-bytevector hashsize))
	   (key       (slot-ref algo 'key)))
      (hash-done! hash-algo isha)
      (dotimes (i blocksize)
	(bytevector-u8-set! buffer i
			    (bitwise-xor (bytevector-u8-ref key i) #x5c)))
      (hash-init! hash-algo)
      (hash-process! hash-algo buffer)
      (hash-process! hash-algo isha)
      (hash-done! hash-algo buffer)
      (bytevector-copy! buffer 0 out 0 (min hashsize (bytevector-length out)))
      out))

  (define-class <hmac> (<mac>)
    ((hash :init-keyword :hash)
     (i-key :init-keyword :i-key)
     (key   :init-keyword :key)))
  (define-method initialize ((o <hmac>) initargs)
    (call-next-method)
    (let-keywords* initargs 
	((key  (make-bytevector 0))
	 (hash (hash-algorithm SHA-1)))
      (let ((hash (if (hash-algorithm? hash) hash (hash-algorithm hash))))
	(slot-set! o 'hash hash)
	(slot-set! o 'init hmac-init)
	(slot-set! o 'process hmac-process)
	(slot-set! o 'done hmac-done)
	(slot-set! o 'block-size (hash-block-size hash))
	(slot-set! o 'hash-size (hash-size hash))
	;; TODO if hasher is not SHA-1, this must be #f
	(slot-set! o 'oid "1.2.840.113549.2.7")
	(slot-set! o 'state 'init)
	(slot-set! o 'i-key key)
	(slot-set! o 'key (make-bytevector (hash-block-size hash))))))
  (define-method write-object ((o <hmac>) out)
    (format out "#<hmac ~a>" (slot-ref o 'hash)))

  (define-class <hmac-marker> () ())
  (define HMAC (make <hmac-marker>))
  (register-hash HMAC <hmac>)
)