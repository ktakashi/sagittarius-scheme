;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/jwe.scm - JSON Web Encryption (JWE)
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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

;; ref: https://tools.ietf.org/html/rfc7516

#!nounbound
(library (rfc jwe)
    (export jwe-header? jwe-header-builder
	    (rename (jwe-header <jwe-header>)
		    (jose-header-typ jwe-header-typ)
		    (jose-header-cty jwe-header-cty)
		    (jose-header-custom-parameters jwe-header-custom-parameters)
		    (jose-crypto-header-alg jwe-header-alg)
		    (jose-crypto-header-jku jwe-header-jku)
		    (jose-crypto-header-jwk jwe-header-jwk)
		    (jose-crypto-header-kid jwe-header-kid)
		    (jose-crypto-header-x5u jwe-header-x5u)
		    (jose-crypto-header-x5c jwe-header-x5c)
		    (jose-crypto-header-x5t jwe-header-x5t)
		    (jose-crypto-header-x5t-s256 jwe-header-x5t-s256)
		    (jose-crypto-header-crit jwe-header-crit))
	    jwe-header-enc jwe-header-zip
	    jwe-header->json jwe-header->json-string write-jwe-header
	    json->jwe-header json-string->jwe-header read-jwe-header

	    jwe-object? make-jwe-object (rename (jwe-object <jwe-object>))
	    jwe-object-header jwe-object-encrypted-key jwe-object-iv
	    jwe-object-cipher-text jwe-object-authentication-tag
	    (rename (jose-object-parts jwe-object-parts))
	    
	    jwe:parse

	    jwe:decrypt
	    make-direct-decryptor
	    
	    jwe:encrypt
	    make-direct-encryptor
	    
	    )
    (import (rnrs)
	    (rfc jose)
	    (rfc jwk)
	    (rfc base64)
	    (rfc hmac)
	    (crypto)
	    (math)
	    (record accessor)
	    (record builder)
	    (sagittarius)
	    (text json)
	    (text json object-builder))

(define-record-type jwe-header
  (parent <jose-crypto-header>)
  (fields enc zip))

(define-syntax jwe-header-builder
  (make-record-builder jwe-header
   ((custom-parameters '() ->jose-header-custom-parameter))))


(define-record-type jwe-object
  (parent <jose-object>)
  (fields header
	  encrypted-key	;; encrypted CEK, session key
	  iv		;; initial vector
	  cipher-text
	  authentication-tag)
  (protocol (lambda (n)
	      (case-lambda
	       ((header encrypted-key iv cipher-text tag)
		((n '()) header encrypted-key iv cipher-text tag))
	       ((parts header encrypted-key iv cipher-text tag)
		((n parts) header encrypted-key iv cipher-text tag))))))

;; internal use
(define-record-type parsed-jwe-header
  (parent jwe-header)
  (fields parsed-base64-url)
  (protocol (lambda (n)
	      (lambda (header parsed-base64-url)
		((apply n (record->list header)) parsed-base64-url)))))

;; it'd be a bit weird order, but shouldn't matter
(define jwe-header-object-builder
  (json-object-builder
   (make-jwe-header
    jose-crypto-header-object-builder
    ("enc" string->symbol)
    (? "zip" #f string->symbol))))

(define jwe-header-serializer
  (json-object-serializer
   (jose-crypto-header-serializer
    ("enc" jwe-header-enc symbol->string)
    (? "zip" #f jwe-header-zip symbol->string))))

(define json->jwe-header
  (make-json->header
   jwe-header-object-builder
   (lambda (obj custom-parameters)
     (if (jwe-header? obj)
	 (jwe-header-builder (from obj) (custom-parameters custom-parameters))
	 (assertion-violation 'json->jwe-header "Something went wrong" obj)))))
(define (read-jwe-header port) (json->jwe-header (json-read port)))
(define (json-string->jwe-header json-string)
  (read-jwe-header (open-string-input-port json-string)))

(define (jwe-header->json jwe-header)
  (object->json jwe-header jwe-header-serializer))
(define write-jwe-header
  (case-lambda
   ((jwe-header) (write-jwe-header jwe-header (current-output-port)))
   ((jwe-header port)
    (json-write/normalized (jwe-header->json jwe-header) port))))
(define (jwe-header->json-string jwe-header)
  (let-values (((out e) (open-string-output-port)))
    (write-jwe-header jwe-header out)
    (e)))

(define (jwe-header->base64url jwe-header)
  (if (parsed-jwe-header? jwe-header)
      (parsed-jwe-header-parsed-base64-url jwe-header)
      (let ((json (jwe-header->json-string jwe-header)))
	(base64url-encode-string json))))

(define (jwe:parse s)
  (let ((part* (jose-split s)))
    (unless (= 5 (length part*))
      (assertion-violation 'jwe:parse "Invalid JWE format, parts must be 5"
			   part*))
    (let ((header (base64url-decode-string (car part*))))
      (apply make-jwe-object part*
	     (make-parsed-jwe-header (json-string->jwe-header header)
				     (car part*))
	     (map (lambda (s) (base64url-decode (string->utf8 s)))
		  (cdr part*))))))

;; (decryptor, jwe-object) -> bytevector
(define (jwe:decrypt decryptor jwe-object)
  (decryptor (jwe-object-header jwe-object)
	     (jwe-object-encrypted-key jwe-object)
	     (jwe-object-iv jwe-object)
	     (jwe-object-cipher-text jwe-object)
	     (jwe-object-authentication-tag jwe-object)))

;; (encryptor, jwe-header, payload) -> jwe-object
(define (jwe:encrypt encryptor jwe-header payload)
  (encryptor jwe-header payload))

(define (make-random-iv-generator prng)
  (lambda (size)
    (read-random-bytes prng size)))
(define default-iv-generator
  (make-random-iv-generator (secure-random ChaCha20)))

;; decryptors
(define (make-direct-decryptor key :key (strict? #t))
  (cond ((symmetric-key? key)
	 (lambda (jwe-header encrypted-key iv cipher-text auth-tag)
	   (when (and strict?
		      (not (eq? (jose-crypto-header-alg jwe-header) 'dir)))
	     (assertion-violation 'direct-decryptor "Alg must be 'dir'"
				  (jose-crypto-header-alg jwe-header)))
	   (core-decrypt jwe-header key iv cipher-text auth-tag)))
	((jwk? key)
	 (make-direct-decryptor (generate-secret-key AES (jwk->octet-key key))
				:strict? strict?))
	(else
	 (assertion-violation 'make-direct-decryptor
			      "Symmetric key or JWK is required" key))))
	
(define (core-decrypt jwe-header key iv cipher-text auth-tag)
  (define enc (jwe-header-enc jwe-header))
  (define aad (jwe-header->aad jwe-header))
  (case enc
    ((A128CBC-HS256)
     (decrypt-aes-hmac key 128 SHA-256 aad iv cipher-text auth-tag))
    ((A192CBC-HS384)
     (decrypt-aes-hmac key 192 SHA-384 aad iv cipher-text auth-tag))
    ((A256CBC-HS512)
     (decrypt-aes-hmac key 256 SHA-512 aad iv cipher-text auth-tag))
    ((A128GCM) (decrypt-aes-gcm key 128 aad iv cipher-text auth-tag))
    ((A192GCM) (decrypt-aes-gcm key 192 aad iv cipher-text auth-tag))
    ((A256GCM) (decrypt-aes-gcm key 256 aad iv cipher-text auth-tag))
    (else (assertion-violation 'get-cipher "Unsupported enc type" enc))))

(define (decrypt-aes-hmac key key-size digest aad iv cipher-text auth-tag)
  (let-values (((mac-key dec-key) (aes-hmac-derive-keys key key-size)))
    (define mode-parameter (make-composite-parameter
			    (make-iv-parameter iv)
			    (make-mode-name-parameter MODE_CBC)
			    (make-padding-parameter pkcs5-padder)))
    (define dec-cipher (make-cipher AES dec-key :mode-parameter mode-parameter))
    (let ((tag (aes-hmac-compute-tag digest mac-key aad iv cipher-text)))
      
      (unless (bytevector=? auth-tag tag)
	;; TODO maybe we should make an specific condition for this
	(assertion-violation 'decrypt-aes-hmac "Incorrect MAC"))
      (cipher-decrypt dec-cipher cipher-text))))

(define (decrypt-aes-gcm key key-size aad iv cipher-text auth-tag)
  (check-key-size 'decrypt-aes-gcm (div key-size 8) (symmetric-key-raw-key key))
  (let ()
    (define mode-parameter (make-composite-parameter
			    (make-iv-parameter iv)
			    (make-mode-name-parameter MODE_GCM)))
    (define dec-cipher (make-cipher AES key :mode-parameter mode-parameter))
    (cipher-update-aad! dec-cipher aad)
    (let-values (((pt tag) (cipher-decrypt/tag dec-cipher cipher-text)))
      (unless (bytevector=? auth-tag tag)
	;; TODO maybe we should make an specific condition for this
	(assertion-violation 'decrypt-aes-gcm "Incorrect MAC"))
      pt)))

;; encryptors
(define (make-direct-encryptor key :key (iv-generator default-iv-generator))
  (cond ((symmetric-key? key)
	 (lambda (jwe-header payload)
	   (core-encryptor key #vu8() jwe-header payload iv-generator)))
	((jwk? key)
	 (make-direct-encryptor (generate-secret-key AES (jwk->octet-key key))
				:iv-generator iv-generator))
	(else (assertion-violation 'make-direct-encryptor
				   "Unsupported type" key))))

(define (core-encryptor key encrypted-key jwe-header payload iv-generator)
  (define enc (jwe-header-enc jwe-header))
  (define encryption-cipher (get-encryptor enc key iv-generator))
  (define aad (jwe-header->aad jwe-header))
  (let-values (((iv cipher-text auth-tag) (encryption-cipher aad payload)))
    (make-jwe-object jwe-header encrypted-key iv cipher-text auth-tag)))

(define (get-encryptor enc key iv-generator)
  (case enc
    ((A128CBC-HS256) (aes-hmac-encryptor key 128 SHA-256 iv-generator))
    ((A192CBC-HS384) (aes-hmac-encryptor key 192 SHA-384 iv-generator))
    ((A256CBC-HS512) (aes-hmac-encryptor key 256 SHA-512 iv-generator))
    ((A128GCM) (aes-gcm-encryptor key 128 iv-generator))
    ((A192GCM) (aes-gcm-encryptor key 192 iv-generator))
    ((A256GCM) (aes-gcm-encryptor key 256 iv-generator))
    (else (assertion-violation 'get-cipher "Unsupported enc type" enc))))

(define (aes-hmac-encryptor key key-size digest iv-generator)
  (let-values (((mac-key enc-key) (aes-hmac-derive-keys key key-size)))
    (define iv (iv-generator 16))
    (define mode-parameter (make-composite-parameter
			    (make-iv-parameter iv)
			    (make-mode-name-parameter MODE_CBC)
			    (make-padding-parameter pkcs5-padder)))
    (define enc-cipher (make-cipher AES enc-key :mode-parameter mode-parameter))
    ;; aad, payload -> (iv, cipher-text, auth-tag)
    (lambda (aad payload)
      (let ((cipher-text (cipher-encrypt enc-cipher payload))
	    (md (hash-algorithm HMAC :key mac-key :hash digest)))
	(values iv cipher-text
		(aes-hmac-compute-tag digest mac-key aad iv cipher-text))))))

(define (aes-gcm-encryptor key key-size iv-generator)
  (define size (div key-size 8))
  (define raw-key (symmetric-key-raw-key key))
  ;; TODO should we check
  (check-key-size 'aes-gcm-encryptor size raw-key)
  (let ()
    (define iv (iv-generator 12))
    (define mode-parameter (make-composite-parameter
			    (make-iv-parameter iv)
			    (make-mode-name-parameter MODE_GCM)))
    (define enc-cipher (make-cipher AES key :mode-parameter mode-parameter))
    (lambda (aad payload)
      (cipher-update-aad! enc-cipher aad)
      (let-values (((cipher-text auth-tag)
		    (cipher-encrypt/tag enc-cipher payload)))
	(values iv cipher-text auth-tag)))))

;; utilities
(define (aes-hmac-compute-tag digest mac-key aad iv cipher-text)
  ;; tag length = lengt(mac-key)
  (let ((tag (make-bytevector (bytevector-length mac-key)))
	(md (hash-algorithm HMAC :key mac-key :hash digest)))
    (hash-init! md)
    (hash-process! md aad)
    (hash-process! md iv)
    (hash-process! md cipher-text)
    (hash-process! md (aad-compute-length aad))
    (hash-done! md tag)
    tag))
    

(define (aes-hmac-derive-keys key key-size)
  (define size/2 (div key-size 8))
  (define size (* size/2 2))
  (define raw-key (symmetric-key-raw-key key))
  (check-key-size 'aes-hmac-derive-keys size raw-key)
  (values (bytevector-copy raw-key 0 size/2)
	  (generate-secret-key AES (bytevector-copy raw-key size/2))))

(define (check-key-size who size raw-key)
  (unless (= size (bytevector-length raw-key))
    (assertion-violation who
			 "Wrong size of key size"
			 `((expected ,size)
			   (got ,(bytevector-length raw-key))))))

(define (jwe-header->aad jwe-header)
  (let ((b64 (jwe-header->base64url jwe-header)))
    (string->utf8 b64)))

;; 64 bits integer represents bits of aad length.
;; convert it to bytevector
(define (aad-compute-length aad)
  (define len (bytevector-length aad))
  (integer->bytevector (* len 8) 8))

)
