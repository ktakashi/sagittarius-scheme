;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; pkcs 12 keystore.scm - PKCS#12 library.
;;;  
;;;   Copyright (c) 2010-2022  Takashi Kato  <ktakashi@ymail.com>
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

;; backward compatibility library, though I don't deprecate this
#!nounbound
(library (rsa pkcs :12 keystore)
    (export <pkcs12-keystore> pkcs12-keystore? make-pkcs12-keystore
	    load-pkcs12-keystore-file load-pkcs12-keystore
	    ;; entry accessors
	    pkcs12-keystore-private-key-ref
	    pkcs12-keystore-certificate-ref
	    pkcs12-keystore-certificate-chain-ref
	    ;; for backward compatibility
	    (rename (pkcs12-keystore-private-key-ref pkcs12-keystore-get-key)
		    (pkcs12-keystore-certificate-ref
		     pkcs12-keystore-get-certificate)
		    (pkcs12-keystore-certificate-chain-ref
		     pkcs12-keystore-get-certificate-chain))

	    pkcs12-keystore-contains-alias?

	    ;; store
	    store-pkcs12-keystore
	    store-pkcs12-keystore-to-file

	    pkcs12-keystore-private-key-set!
	    pkcs12-keystore-certificate-set!
	    (rename (pkcs12-keystore-private-key-set! pkcs12-keystore-set-key!)
		    (pkcs12-keystore-certificate-set!
		     pkcs12-keystore-set-certificate!))

	    pkcs12-keystore-delete-entry!

	    pkcs12-keystore-aliases

	    unwrap-encrypted-private-key-info

	    *pkcs12-privacy-descriptor:pbe/sha1-des3-cbc*
	    *pkcs12-privacy-descriptor:pbe/sha1-des2-cbc*
	    *pkcs12-privacy-descriptor:pbe/sha1-rc2-128-cbc*
	    *pkcs12-privacy-descriptor:pbe/sha1-rc2-40-cbc*
	    *pkcs12-privacy-descriptor:pbes2/aes-128-cbc-hmac-sha256*
	    *pkcs12-privacy-descriptor:pbes2/aes-192-cbc-hmac-sha256*
	    *pkcs12-privacy-descriptor:pbes2/aes-256-cbc-hmac-sha256*
	    
	    (rename (*pkcs12-privacy-descriptor:pbe/sha1-des3-cbc*
		     pkcs12-pbe/sha1-and-des3-cbc)
		    (*pkcs12-privacy-descriptor:pbe/sha1-des2-cbc*
		     pkcs12-pbe/sha1-and-des2-cbc)
		    (*pkcs12-privacy-descriptor:pbe/sha1-rc2-128-cbc*
		     pkcs12-pbe/sha1-and-rc2-128-cbc)
		    (*pkcs12-privacy-descriptor:pbe/sha1-rc2-40-cbc*
		     pkcs12-pbe/sha1-and-rc2-40-cbc))

	    *pkcs12-key-encryption-descriptor:pbes2/aes-128-cbc-hmac-sha256*
	    *pkcs12-key-encryption-descriptor:pbes2/aes-192-cbc-hmac-sha256*
	    *pkcs12-key-encryption-descriptor:pbes2/aes-256-cbc-hmac-sha256*
	    (rename
	     (*pkcs12-key-encryption-descriptor:pbes2/aes-128-cbc-hmac-sha256*
	      pbes2-aes128-cbc-pad/hmac-sha256)
	     (*pkcs12-key-encryption-descriptor:pbes2/aes-192-cbc-hmac-sha256*
	      pbes2-aes192-cbc-pad/hmac-sha256)
	     (*pkcs12-key-encryption-descriptor:pbes2/aes-128-cbc-hmac-sha256*
	      pbes2-aes256-cbc-pad/hmac-sha256))
	    ;; read only accessors 
	    ;; (for debugging use won't be documented)
	    (rename (pkcs12-keystore-private-keys pkcs12-keystore-keys)
		    (pkcs12-keystore-certificates
		     pkcs12-keystore-key-certificates)
		    (pkcs12-keystore-certificates pkcs12-keystore-certificates))
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto digests)
	    (sagittarius crypto pkcs keys)
	    (only (sagittarius crypto pkcs keystore)
		  digest-descriptor->pkcs12-password-integrity-descriptor)
	    (rename (sagittarius crypto keystores pkcs12)
		    (make-pkcs12-keystore pkcs12:make-pkcs12-keystore)))

(define (make-pkcs12-keystore :key (mac-algorithm *digest:sha-256*)
			           mac-iteration
				   key-algorithm cert-algorithm
			      :allow-other-keys opts)
  ;; key-algorithm will be ignored as this will be an optional argument
  ;; of the pkcs12-keystore-private-key-set!
  (let ((desc (digest-descriptor->pkcs12-password-integrity-descriptor
	       mac-algorithm
	       :iteration mac-iteration)))
    (apply pkcs12:make-pkcs12-keystore
	   :integrity-descriptor desc
	   :privacy-descriptor cert-algorithm
	   opts)))

;; We can remove this...
(define (unwrap-encrypted-private-key-info epki password)
  (pkcs-encrypted-private-key-info->pkcs-one-asymmetric-key epki password))
  
(define (pkcs12-keystore-contains-alias? keystore name)
  (not (null? (pkcs12-keystore-entry-types keystore name))))

(define (load-pkcs12-keystore-file in-file password . opt)
  (call-with-input-file in-file
    (lambda (in) (read-pkcs12-keystore password in))
    :transcoder #f))

(define (load-pkcs12-keystore in password . ignroe)
  (read-pkcs12-keystore password in))


(define (store-pkcs12-keystore keystore out password)
  (write-pkcs12-keystore keystore password out))
	     
(define (store-pkcs12-keystore-to-file keystore file password)
  (call-with-output-file file
    (lambda (out) (write-pkcs12-keystore keystore password out))
    :transcoder #f))

(define (pkcs12-keystore-aliases keystore)
  (vector->list (hashtable-keys (pkcs12-keystore-friendly-names keystore))))
)
