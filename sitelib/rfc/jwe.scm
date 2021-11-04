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
	    make-pbes2-decryptor
	    make-aeskw-decryptor
	    
	    make-random-generator
	    jwe:encrypt
	    make-direct-encryptor
	    make-pbes2-encryptor make-salt-generator jwe-header->salt-generator
	    make-aeskw-encryptor
	    
	    )
    (import (rnrs)
	    (rfc jose)
	    (rfc jwk)
	    (rfc base64)
	    (rfc hmac)
	    (rsa pkcs :5)
	    (crypto)
	    (math)
	    (record accessor)
	    (record builder)
	    (sagittarius)
	    (text json)
	    (text json object-builder))

(define-record-type jwe-header
  (parent <jose-crypto-header>)
  (fields enc zip p2s p2c))

(define (maybe-base64url-string->bytevector bv)
  (and bv (base64url-string->bytevector bv)))
(define-syntax jwe-header-builder
  (make-record-builder jwe-header
   ((custom-parameters '() ->jose-header-custom-parameter)
    (p2s #f maybe-base64url-string->bytevector))))

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
    (? "zip" #f string->symbol)
    (? "p2s" #f base64url-string->bytevector)
    (? "p2c" #f))))

(define jwe-header-serializer
  (json-object-serializer
   (jose-crypto-header-serializer
    ("enc" jwe-header-enc symbol->string)
    (? "zip" #f jwe-header-zip symbol->string)
    (? "p2s" #f jwe-header-p2s bytevector->base64url-string)
    (? "p2c" #f jwe-header-p2c))))

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
(define jwe:decrypt
  (case-lambda
   ((decryptor jwe-object)
    (jwe:decrypt decryptor jwe-object '()))
   ((decryptor jwe-object critical-headers)
    (define jwe-header (jwe-object-header jwe-object))
    (jose-crypto-header-check-critical-headers jwe-header critical-headers)
    (decryptor (jwe-object-header jwe-object)
	       (jwe-object-encrypted-key jwe-object)
	       (jwe-object-iv jwe-object)
	       (jwe-object-cipher-text jwe-object)
	       (jwe-object-authentication-tag jwe-object)))))

;; (encryptor, jwe-header, payload) -> jwe-object
(define (jwe:encrypt encryptor jwe-header payload)
  (encryptor jwe-header payload))

;; decryptors
(define (make-aeskw-decryptor kek)
  (cond ((symmetric-key? kek)
	 (lambda (jwe-header encrypted-key iv cipher-text auth-tag)
	   (define alg (jose-crypto-header-alg jwe-header))
	   (define (aeskw-decrypt size)
	     (define enc (jwe-header-enc jwe-header))
	     (define raw-key (symmetric-key-raw-key kek))
	     (check-key-size 'aeskw-encryptor size raw-key)
	     (let ((raw-cek (aes-key-unwrap kek encrypted-key)))
	       (core-decrypt jwe-header (generate-secret-key AES raw-cek)
			     iv cipher-text auth-tag)))
	   (case alg
	     ((A128KW) (aeskw-decrypt 16))
	     ((A192KW) (aeskw-decrypt 24))
	     ((A256KW) (aeskw-decrypt 32))
	     (else (assertion-violation
		    'aeskw-decryptor "Unknown algorithm" alg)))))
	((jwk? kek)
	 (make-aeskw-decryptor (generate-secret-key AES (jwk->octet-key kek))))
	(else
	 (assertion-violation 'make-aeskw-decryptor "Unknown key" kek))))

(define (make-pbes2-decryptor password)
  (lambda (jwe-header encrypted-key iv cipher-text auth-tag)
    (define alg (jose-crypto-header-alg jwe-header))
    (define p2s (jwe-header-p2s jwe-header))
    (define p2c (jwe-header-p2c jwe-header))
    (unless (memq alg
		  '(PBES2-HS256+A128KW PBES2-HS384+A192KW PBES2-HS512+A256KW))
      (assertion-violation 'pbes2-decryptor "Alg must be one of PBES2-*" alg))
    (unless (and p2s p2c)
      (assertion-violation 'pbes2-decryptor
			   "Parameter 'p2s' and 'p2c' must be presented"))
    (let* ((ps-key (pbes2-derive-kek alg password  p2s p2c))
	   (raw-cek (aes-key-unwrap ps-key encrypted-key)))
      (core-decrypt jwe-header (generate-secret-key AES raw-cek) iv
		    cipher-text auth-tag))))

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

;; encryptors utils
(define (make-random-generator prng)
  (lambda (size)
    (read-random-bytes prng size)))
(define default-iv-generator
  (make-random-generator (secure-random ChaCha20)))

(define +default-salt-size+ 16)
(define +default-iteration-counnt+ 4096)

(define (make-salt-generator size iteration
			     :key (prng (secure-random ChaCha20)))
  (when (or (< size 8) (< iteration 1000))
    (assertion-violation 'make-salt-generator
			 "Salt size or iterationn is too small" size iteration))
  (lambda ()
    (values (read-random-bytes prng size) iteration)))

(define default-salt-generator
  (make-salt-generator +default-salt-size+ +default-iteration-counnt+))
;;; Utility API
(define (jwe-header->salt-generator jwe-header
				    :key (prng (secure-random ChaCha20)))
  (define p2s (jwe-header-p2s jwe-header))
  (define p2c (jwe-header-p2c jwe-header))
  (lambda ()
    (values (or p2s (read-random-bytes prng +default-salt-size+))
	    (or p2c +default-iteration-counnt+))))

(define default-cek-generator (make-random-generator (secure-random ChaCha20)))

;; encryptors
(define (make-aeskw-encryptor kek :key (cek-generator default-cek-generator)
			               (iv-generator default-iv-generator))
  (define (aeskw-encrypt kek size jwe-header payload)
    (define raw-key (symmetric-key-raw-key kek))
    (define enc (jwe-header-enc jwe-header))
    (check-key-size 'aeskw-encryptor size raw-key)
    (let ((raw-cek (cek-generator (get-cek-byte-size enc))))
      (core-encryptor (generate-secret-key AES raw-cek)
		      (aes-key-wrap kek raw-cek)
		      jwe-header payload iv-generator)))
  (cond ((symmetric-key? kek)
	 (lambda (jwe-header payload)
	   (define alg (jose-crypto-header-alg jwe-header))
	   (case alg
	     ((A128KW) (aeskw-encrypt kek 16 jwe-header payload))
	     ((A192KW) (aeskw-encrypt kek 24 jwe-header payload))
	     ((A256KW) (aeskw-encrypt kek 32 jwe-header payload))
	     (else (assertion-violation
		    'aeskw-encryptor "Unknown algorithm" alg)))))
	((jwk? kek)
	 (make-aeskw-encryptor (generate-secret-key AES (jwk->octet-key kek))
			       :iv-generator iv-generator
			       :cek-generator cek-generator))
	(else (assertion-violation 'make-aeskw-encryptor
				   "Unsupported KEK type" kek))))

(define (make-pbes2-encryptor password 
			      :key (salt-generator default-salt-generator)
			           (cek-generator default-cek-generator)
				   (iv-generator default-iv-generator))
  (lambda (jwe-header payload)
    (define alg (jose-crypto-header-alg jwe-header))
    (define enc (jwe-header-enc jwe-header))
    (let-values (((raw-salt iteration) (salt-generator)))
      (let ((ps-key (pbes2-derive-kek alg password raw-salt iteration))
	    (new-header (jwe-header-builder (from jwe-header)
			 (p2s raw-salt)
			 (p2c iteration)))
	    (raw-cek (cek-generator (get-cek-byte-size enc))))
	(core-encryptor (generate-secret-key AES raw-cek)
			(aes-key-wrap ps-key raw-cek)
			new-header payload iv-generator)))))

(define (make-direct-encryptor key :key (iv-generator default-iv-generator))
  (cond ((symmetric-key? key)
	 (lambda (jwe-header payload)
	   (core-encryptor key #vu8() jwe-header payload iv-generator)))
	((jwk? key)
	 (make-direct-encryptor (generate-secret-key AES (jwk->octet-key key))
				:iv-generator iv-generator))
	(else (assertion-violation 'make-direct-encryptor
				   "Unsupported key" key))))

(define (aes-key-wrap key pt)
  (define wrapper (make-aes-key-wrap key))
  (wrapper pt))

(define (aes-key-unwrap key ct)
  (define unwrapper (make-aes-key-unwrap key))
  (unwrapper ct))

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

(define (get-cek-byte-size enc)
  (case enc
    ((A128CBC-HS256 A128GCM) 16)
    ((A192CBC-HS384 A192GCM) 24)
    ((A256CBC-HS512 A256GCM) 32)
    (else (assertion-violation 'get-cek-byte-size "Unsupported enc type" enc))))

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

(define (pbes2-prf-param alg)
    (case alg
      ((PBES2-HS256+A128KW) (values 16 SHA-256))
      ((PBES2-HS384+A192KW) (values 24 SHA-384))
      ((PBES2-HS512+A256KW) (values 32 SHA-256))
      (else (assertion-violation 'pbes2-prf-param "Unknown algorithm" alg))))

(define (pbes2-derive-kek alg password raw-salt iteration)
  (define bv-password
    (cond ((string? password) (string->utf8 password))
	  ((bytevector? password) password)
	  (else (assertion-violation 'pbes2-derive-kek
		 "password must be a string or bytevector"))))
  (define (format-salt alg salt)
    (bytevector-append (string->utf8 (symbol->string alg)) #vu8(0) salt))
  (let-values (((dk-len digest) (pbes2-prf-param alg)))
    (let* ((salt (format-salt alg raw-salt))
	   (key (pbkdf-2 bv-password salt iteration dk-len :hash digest)))
      (generate-secret-key AES key))))

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
