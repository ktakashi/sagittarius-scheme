;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/ciphers/asymmetric.scm - Asymmetric ciphers
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius crypto ciphers asymmetric)
    (export cipher? <cipher>
	    asymmetric-cipher? make-asymmetric-cipher
	    asymmetric-cipher-init! asymmetric-cipher-init

	    asymmetric-cipher-encrypt-bytevector
	    asymmetric-cipher-decrypt-bytevector
	    asymmetric-cipher-done!

	    *scheme:rsa*
	    oaep-encoding pkcs1-v1.5-encoding mgf-1)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto keys)
	    (sagittarius crypto ciphers types)
	    (sagittarius crypto descriptors)
	    (sagittarius crypto ciphers asymmetric rsa)
	    (sagittarius crypto ciphers asymmetric encodings)
	    (sagittarius crypto ciphers asymmetric state))

(define (make-asymmetric-cipher (scheme asymmetric-cipher-descriptor?)
				;; default OAEP 
				:key (encoding oaep-encoding)
				:allow-other-keys opts)
  (let-values (((encoder decoder) (apply encoding scheme opts)))
    (make <asymmetric-cipher> :scheme scheme
	  :encoder encoder :decoder decoder)))

(define (asymmetric-cipher-init! (cipher asymmetric-cipher?)
				 (key asymmetric-key?))
  (asymmetric-cipher-done! cipher)
  (let ((scheme (cipher-scheme cipher)))
    (asymmetric-cipher-key-set! cipher
     ((asymmetric-cipher-descriptor-setup scheme) key))
    cipher))
(define (asymmetric-cipher-init (cipher asymmetric-cipher?)
				(key asymmetric-key?))
  (asymmetric-cipher-init!
   (make <asymmetric-cipher> :scheme (cipher-scheme cipher)
	 :encoder (asymmetric-cipher-encoder cipher)
	 :decoder (asymmetric-cipher-decoder cipher))
   key))

(define (asymmetric-cipher-done! (cipher asymmetric-cipher?))
  (cond ((asymmetric-cipher-key cipher) =>
	 (lambda (key)
	   (let ((scheme (cipher-scheme cipher)))
	     ((asymmetric-cipher-descriptor-done scheme) key)))))
  (asymmetric-cipher-key-set! cipher #f)
  cipher)

(define (asymmetric-cipher-encrypt-bytevector (cipher asymmetric-cipher?)
					      (bv bytevector?)
					      :optional (start 0))
  (let ((encoder (asymmetric-cipher-encoder cipher))
	(key (asymmetric-cipher-key cipher))
	(scheme (cipher-scheme cipher)))
    ((asymmetric-cipher-descriptor-encrypt scheme)
     key (or (and encoder (encoder bv key)) bv) start)))

(define (asymmetric-cipher-decrypt-bytevector (cipher asymmetric-cipher?)
					      (bv bytevector?)
					      :optional (start 0))
  (define (decode decoder key pt)
    (or (and decoder (decoder pt key)) pt))
  (let ((decoder (asymmetric-cipher-decoder cipher))
	(key (asymmetric-cipher-key cipher))
	(scheme (cipher-scheme cipher)))
    (decode decoder key
	    ((asymmetric-cipher-descriptor-decrypt scheme) key bv start))))
)
