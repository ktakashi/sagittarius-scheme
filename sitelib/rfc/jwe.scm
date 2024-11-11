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

;; ref:
;; - https://tools.ietf.org/html/rfc7516 (original)
;; - https://tools.ietf.org/html/draft-amringer-jose-chacha-02 (xc20p)

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
	    jwe-header-p2s jwe-header-p2c ;; PBEES2
	    jwe-header-iv jwe-header-tag  ;; AESGCMKW
	    jwe-header-apu jwe-header-apv jwe-header-epk
	    jwe-header->json jwe-header->json-string write-jwe-header
	    json->jwe-header json-string->jwe-header read-jwe-header

	    jwe-object? make-jwe-object (rename (jwe-object <jwe-object>))
	    jwe-object-header jwe-object-encrypted-key jwe-object-iv
	    jwe-object-cipher-text jwe-object-authentication-tag
	    (rename (jose-object-parts jwe-object-parts))

	    jwe:parse jwe:serialize

	    jwe:decrypt
	    make-direct-jwe-decryptor
	    make-pbes2-jwe-decryptor
	    make-aeskw-jwe-decryptor
	    make-rsa-jwe-decryptor
	    make-ecdh-es-jwe-decryptor
	    (rename (make-ecdh-es-jwe-decryptor make-ecdh-jwe-decryptor))
	    make-c20pkw-jwe-decryptor
	    

	    jwe:encrypt
	    make-direct-jwe-encryptor
	    make-pbes2-jwe-encryptor
	    make-aeskw-jwe-encryptor
	    make-rsa-jwe-encryptor
	    make-ecdh-es-jwe-encryptor
	    (rename (make-ecdh-es-jwe-encryptor make-ecdh-jwe-encryptor))
	    make-c20pkw-jwe-encryptor

	    ;; generators
	    make-random-generator
	    make-salt-generator jwe-header->salt-generator
	    ;; For testing...
	    (rename (ecdh-derive-shared-key jwe:ecdh-derive-shared-key))

	    )
    (import (rnrs)
	    (rfc jose)
	    (rfc jwk)
	    (rfc base64)
	    (rfc zlib)
	    (sagittarius)
	    (sagittarius crypto keys)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto digests)
	    (sagittarius crypto random)
	    (sagittarius crypto mac)
	    (sagittarius crypto kdfs)
	    (record accessor)
	    (record builder)
	    (text json)
	    (text json object-builder))

(define-record-type jwe-header
  (parent <jose-crypto-header>)
  (fields enc zip p2s p2c iv tag apu apv epk))

(define (maybe-base64url-string->bytevector bv)
  (and bv
       (cond ((bytevector? bv) bv)
	     ((string? bv) (base64url-string->bytevector bv))
	     (else (assertion-violation 'jwe-header-builder
		    "bytevector or base64url string required" bv)))))
(define (maybe-json->jwk json)
  (and json
       (or (and (jwk? json) json)
	   (json->jwk json))))
(define-syntax jwe-header-builder
  (make-record-builder jwe-header
   ((custom-parameters '() ->jose-header-custom-parameter)
    (p2s #f maybe-base64url-string->bytevector)
    (iv #f maybe-base64url-string->bytevector)
    (tag #f maybe-base64url-string->bytevector)
    (apu #f maybe-base64url-string->bytevector)
    (apv #f maybe-base64url-string->bytevector))))

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
    (? "p2c" #f)
    (? "iv" #f base64url-string->bytevector)
    (? "tag" #f base64url-string->bytevector)
    (? "apu" #f base64url-string->bytevector)
    (? "apv" #f base64url-string->bytevector)
    (? "epk" #f maybe-json->jwk))))

(define jwe-header-serializer
  (json-object-serializer
   (jose-crypto-header-serializer
    ("enc" jwe-header-enc symbol->string)
    (? "zip" #f jwe-header-zip symbol->string)
    (? "p2s" #f jwe-header-p2s bytevector->base64url-string)
    (? "p2c" #f jwe-header-p2c)
    (? "iv" #f jwe-header-iv bytevector->base64url-string)
    (? "tag" #f jwe-header-tag bytevector->base64url-string)
    (? "apu" #f jwe-header-apu bytevector->base64url-string)
    (? "apv" #f jwe-header-apv bytevector->base64url-string)
    (? "epk" #f jwe-header-epk jwk->json)
    )))

(define json->jwe-header
  (make-json->header
   jwe-header-object-builder
   (lambda (obj custom-parameters)
     (if (jwe-header? obj)
	 (jwe-header-builder (from obj) (custom-parameters custom-parameters))
	 (assertion-violation 'json->jwe-header "Something went wrong" obj)))))
(define read-jwe-header
  (case-lambda
   (() (read-jwe-header (current-input-port)))
   ((port) (json->jwe-header (json-read port)))))
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

(define (jwe:serialize jwe-object)
  (define ->base64url bytevector->base64url-string)
  (let ((header (jwe-header->base64url (jwe-object-header jwe-object)))
	(encrypted-key (->base64url (jwe-object-encrypted-key jwe-object)))
	(iv (->base64url (jwe-object-iv jwe-object)))
	(cipher-text (->base64url (jwe-object-cipher-text jwe-object)))
	(tag (->base64url (jwe-object-authentication-tag jwe-object))))
    (string-append header "."
		   encrypted-key "."
		   iv "."
		   cipher-text "."
		   tag)))

;;;; Interface APIs
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


;;;; Cryptographic implementations
;;; Decryptors
;; ECDH-ES key unwrap
(define (make-ecdh-es-jwe-decryptor key)
  (cond ((or (ecdsa-private-key? key) (rfc7748-private-key? key))
	 (lambda (jwe-header encrypted-key iv cipher-text auth-tag)
	   (define alg (jose-crypto-header-alg jwe-header))
	   (define enc (jwe-header-enc jwe-header))

	   (define (ecdh-direct share-key dummy) (make-symmetric-key share-key))
	   (define (ecdh-aes-unwrap share-key encrypted-key)
	     (let ((kek (make-symmetric-key share-key)))
	       (make-symmetric-key (aes-key-unwrap kek encrypted-key))))
	   (define (ecdh-c20pkw-unwrap scheme)
	     (define iv (jwe-header-iv jwe-header))
	     (define tag (jwe-header-tag jwe-header))
	     (unless (and iv tag)
	       (assertion-violation 'ecdh-es-jwe-decryptor
				    "iv and tag is required for (X)C20KW"))
	     (lambda (share-key encrypted-key)
	       (let ((kek (make-symmetric-key share-key)))
		 (make-symmetric-key
		  (decrypt-chacha20-poly1305 scheme kek #vu8() iv
					     encrypted-key tag)))))

	   (define (ecdh-decryptor mode)
	     (define epk (jwe-header-epk jwe-header))
	     (define pub (and epk (jwk->public-key epk)))

	     (unless (or (ecdsa-public-key? pub) (rfc7748-public-key? pub))
	       (assertion-violation 'ecdh-es-jwe-decryptor
		 "epk is not an ECDSA, X25519 or X448 public key"))
	     (let* ((z (calculate-key-agreement *key:ecdh* key pub))
		    (cek (mode (ecdh-derive-shared-key jwe-header z)
			   encrypted-key)))
	       (core-decrypt jwe-header cek iv cipher-text auth-tag)))
	   (case alg
	     ((ECDH-ES) (ecdh-decryptor ecdh-direct))
	     ((ECDH-ES+A128KW ECDH-ES+A198KW ECDH-ES+A256KW)
	      (ecdh-decryptor ecdh-aes-unwrap))
	     ((ECDH-ES+C20PKW)
	      (ecdh-decryptor (ecdh-c20pkw-unwrap *scheme:chacha20-poly1305*)))
	     ((ECDH-ES+XC20PKW)
	      (ecdh-decryptor (ecdh-c20pkw-unwrap *scheme:xchacha20-poly1305*)))
	     (else
	      (assertion-violation 'ecdh-es-jwe-decryptor "Unknown alg" alg)))))
	((jwk? key)
	 (make-ecdh-es-jwe-decryptor (jwk->private-key key)))
	(else (assertion-violation 'make-ecdh-es-jwe-decryptor
				   "ECDSA private key is required" key))))

;; RSA key unwrap
(define (make-rsa-jwe-decryptor key)
  (cond ((rsa-private-key? key)
	 (lambda (jwe-header encrypted-key iv cipher-text auth-tag)
	   (define alg (jose-crypto-header-alg jwe-header))
	   (define (rsa-decryptor encoding . opts)
	     (define rsa-cipher
	       (asymmetric-cipher-init!
		(apply make-asymmetric-cipher *scheme:rsa*
		       :encoding encoding opts)
		key))
	     (define enc (jwe-header-enc jwe-header))
	     (let ((raw-cek (asymmetric-cipher-decrypt-bytevector
			     rsa-cipher encrypted-key)))
	       (core-decrypt jwe-header
			     (make-symmetric-key raw-cek)
			     iv cipher-text auth-tag)))
	   (case alg
	     ((RSA1_5) (rsa-decryptor pkcs1-v1.5-encoding))
	     ((RSA-OAEP)
	      (rsa-decryptor oaep-encoding :digest *digest:sha-1*))
	     ((RSA-OAEP-256)
	      (rsa-decryptor oaep-encoding :digest *digest:sha-256*))
	     (else (assertion-violation 'make-rsa-encryptor
					"Unknown alg" alg)))))
	((jwk? key)
	 (make-rsa-jwe-decryptor (jwk->private-key key)))
	(else (assertion-violation 'make-rsa-jwe-decryptor
				   "RSA private key is required" key))))
;; AES key unwrap
(define (make-aeskw-jwe-decryptor kek)
  (cond ((symmetric-key? kek)
	 (lambda (jwe-header encrypted-key iv cipher-text auth-tag)
	   (define alg (jose-crypto-header-alg jwe-header))
	   (define (aeskw-decrypt size)
	     (define enc (jwe-header-enc jwe-header))
	     (define raw-key (symmetric-key-value kek))
	     (check-key-size 'aeskw-encryptor size raw-key)
	     (let ((raw-cek (aes-key-unwrap kek encrypted-key)))
	       (core-decrypt jwe-header
			     (make-symmetric-key raw-cek)
			     iv cipher-text auth-tag)))

	   (define (aesgcmkw-decrypt size)
	     (define enc (jwe-header-enc jwe-header))
	     (define cek-iv (jwe-header-iv jwe-header))
	     (define tag (jwe-header-tag jwe-header))
	     (let ((raw-cek (decrypt-aes-gcm kek size
					     #vu8() cek-iv encrypted-key tag)))
	       (core-decrypt jwe-header
			     (make-symmetric-key raw-cek)
			     iv cipher-text auth-tag)))
	   (case alg
	     ((A128KW A192KW A256KW)
	      (aeskw-decrypt (get-aes-key-byte-size alg)))
	     ((A128GCMKW A192GCMKW A256GCMKW)
	      (aesgcmkw-decrypt (get-aes-key-byte-size alg)))
	     (else (assertion-violation
		    'aeskw-jwe-decryptor "Unknown algorithm" alg)))))
	((jwk? kek)
	 (make-aeskw-jwe-decryptor
	  (make-symmetric-key (jwk->octet-key kek))))
	(else
	 (assertion-violation 'make-aeskw-jwe-decryptor "Unknown key" kek))))

;; Chacha20 / XChaCha20 key unwrap
(define (make-c20pkw-jwe-decryptor kek)
  (cond ((symmetric-key? kek)
	 (lambda (jwe-header encrypted-key iv cipher-text auth-tag)
	   (define alg (jose-crypto-header-alg jwe-header))
	   (define (c20pkw-decrypt scheme)
	     (define enc (jwe-header-enc jwe-header))
	     (define cek-iv (jwe-header-iv jwe-header))
	     (define tag (jwe-header-tag jwe-header))
	     (let ((raw-cek (decrypt-chacha20-poly1305 scheme kek #vu8() cek-iv
						       encrypted-key tag)))
	       (core-decrypt jwe-header (make-symmetric-key raw-cek)
			     iv cipher-text auth-tag)))
	   (case alg
	     ((C20PKW) (c20pkw-decrypt *scheme:chacha20-poly1305*))
	     ((XC20PKW) (c20pkw-decrypt *scheme:xchacha20-poly1305*))
	     (else (assertion-violation
		    'aeskw-jwe-decryptor "Unknown algorithm" alg)))))
	((jwk? kek)
	 (make-c20pkw-jwe-decryptor (make-symmetric-key (jwk->octet-key kek))))
	(else
	 (assertion-violation 'make-c20pkw-jwe-decryptor "Unknown key" kek))))

;; PBE key unwrap
(define (make-pbes2-jwe-decryptor password)
  (lambda (jwe-header encrypted-key iv cipher-text auth-tag)
    (define alg (jose-crypto-header-alg jwe-header))
    (define p2s (jwe-header-p2s jwe-header))
    (define p2c (jwe-header-p2c jwe-header))
    (unless (memq alg
		  '(PBES2-HS256+A128KW PBES2-HS384+A192KW PBES2-HS512+A256KW))
      (assertion-violation 'pbes2-jwe-decryptor
			   "Alg must be one of PBES2-*" alg))
    (unless (and p2s p2c)
      (assertion-violation 'pbes2-jwe-decryptor
			   "Parameter 'p2s' and 'p2c' must be presented"))
    (let* ((ps-key (pbes2-derive-kek alg password p2s p2c))
	   (raw-cek (aes-key-unwrap ps-key encrypted-key)))
      (core-decrypt jwe-header (make-symmetric-key raw-cek) iv
		    cipher-text auth-tag))))

;; Direct decryption
(define (make-direct-jwe-decryptor key :key (strict? #t))
  (cond ((symmetric-key? key)
	 (lambda (jwe-header encrypted-key iv cipher-text auth-tag)
	   (when (and strict?
		      (not (eq? (jose-crypto-header-alg jwe-header) 'dir)))
	     (assertion-violation 'direct-jwe-decryptor "Alg must be 'dir'"
				  (jose-crypto-header-alg jwe-header)))
	   (core-decrypt jwe-header key iv cipher-text auth-tag)))
	((jwk? key)
	 (make-direct-jwe-decryptor
	  (make-symmetric-key (jwk->octet-key key))
	  :strict? strict?))
	(else
	 (assertion-violation 'make-direct-jwe-decryptor
			      "Symmetric key or JWK is required" key))))

;; Decryption utilities
(define (core-decrypt jwe-header key iv cipher-text auth-tag)
  (define enc (jwe-header-enc jwe-header))
  (define aad (jwe-header->aad jwe-header))
  (define (decrypt enc key aad iv cipher-text auth-tag)
    (case enc
      ((A128CBC-HS256 A192CBC-HS384 A256CBC-HS512)
       (decrypt-aes-hmac key (get-aes-key-byte-size enc)
			 (get-hmac-digest enc) aad iv cipher-text auth-tag))
      ((A128GCM A192GCM A256GCM)
       (decrypt-aes-gcm key (get-aes-key-byte-size enc)
			aad iv cipher-text auth-tag))
      ((C20P)
       (decrypt-chacha20-poly1305
	*scheme:chacha20-poly1305* key aad iv cipher-text auth-tag))
      ((XC20P)
       (decrypt-chacha20-poly1305
	*scheme:xchacha20-poly1305* key aad iv cipher-text auth-tag))
      (else (assertion-violation 'get-cipher "Unsupported enc type" enc))))
  (define (decompress jwe-header payload)
    (case (jwe-header-zip jwe-header)
      ((DEF) (inflate-bytevector payload))
      ((#f)  payload)
      (else (assertion-violation 'core-decrypt
				 "Unknown 'zip' algorithm"
				 (jwe-header-zip jwe-header)0))))
  (decompress jwe-header (decrypt enc key aad iv cipher-text auth-tag)))

;; AES-CBC-HMAC
(define (decrypt-aes-hmac key key-size digest aad iv cipher-text auth-tag)
  (let-values (((mac-key dec-key) (aes-hmac-derive-keys key key-size)))
    (define dec-cipher
      (block-cipher-init! (make-block-cipher *scheme:aes* *mode:cbc*)
			  (cipher-direction decrypt)
			  dec-key
			  (make-iv-parameter iv)))
    (let ((tag (aes-hmac-compute-tag digest mac-key aad iv cipher-text)))
      (unless (bytevector=? auth-tag tag)
	;; TODO maybe we should make an specific condition for this
	(assertion-violation 'decrypt-aes-hmac "Incorrect MAC"))
      (let ((r (block-cipher-decrypt-last-block dec-cipher cipher-text)))
	(block-cipher-done! dec-cipher)
	r))))

;; AES GCM
(define (decrypt-aes-gcm key key-size aad iv cipher-text auth-tag)
  (check-key-size 'decrypt-aes-gcm key-size (symmetric-key-value key))
  (let ()
    (define dec-cipher
      (block-cipher-init! (make-block-cipher *scheme:aes* *mode:gcm* no-padding)
			  (cipher-direction decrypt)
			  key
			  (make-iv-parameter iv)))
    (block-cipher-update-aad! dec-cipher aad)
    (let ((pt (block-cipher-decrypt-last-block dec-cipher cipher-text)))
      (block-cipher-done/tag! dec-cipher auth-tag)
      ;; https://datatracker.ietf.org/doc/html/rfc7518 5.3
      ;; Tag must be 128 bits
      (unless (= (bytevector-length auth-tag) 16)
	(error 'AES-GCM "Invalid tag length"))
      pt)))

;; ChaCha20 or XChaCha20
(define (decrypt-chacha20-poly1305 scheme key aad iv cipher-text auth-tag)
  (define cipher (stream-cipher-init! (make-stream-cipher scheme)
				      (cipher-direction decrypt)
				      key
				      (make-iv-parameter iv)))
  (stream-cipher-update-aad! cipher aad)
  (let ((v (stream-cipher-decrypt cipher cipher-text)))
    (stream-cipher-done/tag! cipher auth-tag)
    v))

;;; Encryptors
;; Random generators
(define (make-random-generator prng)
  (lambda (size)
    (random-generator-read-random-bytes prng size)))
(define default-iv-generator
  (make-random-generator (secure-random-generator *prng:chacha20*)))

(define +default-salt-size+ 16)
(define +default-iteration-counnt+ 4096)

(define (make-salt-generator size iteration
	 :key (prng (secure-random-generator *prng:chacha20*)))
  (when (or (< size 8) (< iteration 1000))
    (assertion-violation 'make-salt-generator
			 "Salt size or iterationn is too small" size iteration))
  (lambda ()
    (values (random-generator-read-random-bytes prng size) iteration)))

(define default-salt-generator
  (make-salt-generator +default-salt-size+ +default-iteration-counnt+))

(define (jwe-header->salt-generator jwe-header
	 :key (prng (secure-random-generator *prng:chacha20*)))
  (define p2s (jwe-header-p2s jwe-header))
  (define p2c (jwe-header-p2c jwe-header))
  (lambda ()
    (values (or p2s
		(random-generator-read-random-bytes prng +default-salt-size+))
	    (or p2c +default-iteration-counnt+))))

(define default-cek-generator
  (make-random-generator (secure-random-generator *prng:chacha20*)))

(define (default-ec-keypair-generator ec-parameter)
  (let ((kp (cond ((eq? ec-parameter *key:x25519*)
		   (generate-key-pair *key:x25519*))
		  ((eq? ec-parameter *key:x448*)
		   (generate-key-pair *key:x448*))
		  (else (generate-key-pair *key:ecdsa*
					   :ec-parameter ec-parameter)))))
    (values (key-pair-private kp) (key-pair-public kp))))

;; ECDH-ES key wrap
(define (make-ecdh-es-jwe-encryptor
	 key :key (ec-keypair-generator default-ec-keypair-generator)
		  (cek-generator default-cek-generator)
		  (iv-generator default-iv-generator))
  (cond ((or (ecdsa-public-key? key) (rfc7748-public-key? key))
	 (let ((key-parameter
		(cond ((ecdsa-public-key? key) (ecdsa-key-parameter key))
		      ((x25519-public-key? key) *key:x25519*)
		      ((x448-public-key? key) *key:x448*)
		      (else (assertion-violation 'make-ecdh-es-jwe-encryptor
						 "Unknown key" key)))))
	   (lambda (jwe-header payload)
	     (define alg (jose-crypto-header-alg jwe-header))
	     (define enc (jwe-header-enc jwe-header))

	     (define (ecdh-direct shared-key)
	       (values (make-symmetric-key shared-key) #vu8()
		       #f #f))
	     (define (ecdh-aes-wrap shared-key)
	       (let ((kek (make-symmetric-key shared-key))
		     (cek (generate-cek cek-generator enc)))
		 (values (make-symmetric-key cek)
			 (aes-key-wrap kek cek)
			 #f #f)))
	     (define ((ecdh-c20pkw-wrap scheme iv-size) shared-key)
	       (define kek (make-symmetric-key shared-key))
	       (define wrap (c20p-encryptor scheme iv-size kek iv-generator))
	       (let ((cek (generate-cek cek-generator enc)))
		 (let-values (((iv ecek tag) (wrap #vu8() cek)))
		   (values (make-symmetric-key cek) ecek iv tag))))
	     (define (calc-z priv key)
	       (calculate-key-agreement *key:ecdh* priv key))
	     (define (ecdh-encryptor mode)
	       (let*-values (((priv pub) (ec-keypair-generator key-parameter))
			     ((cek encrypted-key iv tag)
			      (mode (ecdh-derive-shared-key jwe-header
				     (calc-z priv key)))))
		 (let ((new-header (jwe-header-builder (from jwe-header)
				    (epk (public-key->jwk pub))
				    (iv iv)
				    (tag tag))))
		   (core-encryptor cek encrypted-key new-header
				   payload iv-generator))))
	     (case alg
	       ((ECDH-ES) (ecdh-encryptor ecdh-direct))
	       ((ECDH-ES+A128KW ECDH-ES+A198KW ECDH-ES+A256KW)
		(ecdh-encryptor ecdh-aes-wrap))
	       ((ECDH-ES+C20PKW)
		(ecdh-encryptor
		 (ecdh-c20pkw-wrap *scheme:chacha20-poly1305* 12)))
	       ((ECDH-ES+XC20PKW)
		(ecdh-encryptor
		 (ecdh-c20pkw-wrap *scheme:xchacha20-poly1305* 24)))
	       (else
		(assertion-violation 'ecdh-es-jwe-encryptor "Unknown alg" alg))))))
	((jwk? key)
	 (make-ecdh-es-jwe-encryptor (jwk->public-key key)
				  :cek-generator cek-generator
				  :iv-generator iv-generator
				  :ec-keypair-generator ec-keypair-generator))
	(else (assertion-violation 'make-ecdh-es-jwe-encryptor
				   "Key must be an ECDSA public key" key))))

;; RSA key wrap
(define (make-rsa-jwe-encryptor key :key (cek-generator default-cek-generator)
					 (iv-generator default-iv-generator))
  (define (rsa-encryptor key jwe-header payload encoding . opts)
    (define rsa-cipher
      (asymmetric-cipher-init! (apply make-asymmetric-cipher *scheme:rsa*
				      :encoding encoding opts)
			       key))
    (define enc (jwe-header-enc jwe-header))
    (let ((raw-cek (generate-cek cek-generator enc)))
      (core-encryptor (make-symmetric-key raw-cek)
		      (asymmetric-cipher-encrypt-bytevector rsa-cipher raw-cek)
		      jwe-header payload iv-generator)))

  (cond ((rsa-public-key? key)
	 (lambda (jwe-header payload)
	   (define alg (jose-crypto-header-alg jwe-header))
	   (case alg
	     ((RSA1_5)
	      (rsa-encryptor key jwe-header payload pkcs1-v1.5-encoding))
	     ((RSA-OAEP)
	      (rsa-encryptor key jwe-header payload oaep-encoding
			     :digest *digest:sha-1*))
	     ((RSA-OAEP-256)
	      (rsa-encryptor key jwe-header payload oaep-encoding
			     :digest *digest:sha-256*))
	     (else (assertion-violation 'make-rsa-jwe-encryptor
					"Unknown alg" alg)))))
	((jwk? key)
	 (make-rsa-jwe-encryptor (jwk->public-key key)
				 :cek-generator cek-generator
				 :iv-generator iv-generator))
	(else (assertion-violation 'make-rsa-jwe-encryptor
				   "Key must be an RSA public key" key))))

;; AES key wrap
(define (make-aeskw-jwe-encryptor kek :key (cek-generator default-cek-generator)
					   (iv-generator default-iv-generator)
					   (kek-iv-generator default-iv-generator))
  (define (aeskw-encrypt kek size jwe-header payload)
    (define raw-key (symmetric-key-value kek))
    (define enc (jwe-header-enc jwe-header))
    (check-key-size 'aeskw-jwe-encryptor size raw-key)
    (let ((raw-cek (generate-cek cek-generator enc)))
      (core-encryptor (make-symmetric-key raw-cek)
		      (aes-key-wrap kek raw-cek)
		      jwe-header payload iv-generator)))

  (define (aesgcmkw-encrypt kek size jwe-header payload)
    ;; if the header already has iv, use it
    ;; NOTE: we can't use tag as it'd be generated
    (define kek-iv-g
      (cond ((jwe-header-iv jwe-header) => (lambda (iv) (lambda (size) iv)))
	    (else kek-iv-generator)))
    (define cek-key-encryptor (aes-gcm-encryptor kek size kek-iv-g))
    (define enc (jwe-header-enc jwe-header))
    (let ((raw-cek (generate-cek cek-generator enc)))
      (let-values (((iv encrypted-cek tag) (cek-key-encryptor #vu8() raw-cek)))
	(let ((new-header (jwe-header-builder (from jwe-header)
			   (iv iv)
			   (tag tag))))
	  (core-encryptor (make-symmetric-key raw-cek)
			  encrypted-cek
			  new-header payload iv-generator)))))

  (cond ((symmetric-key? kek)
	 (lambda (jwe-header payload)
	   (define alg (jose-crypto-header-alg jwe-header))
	   (case alg
	     ((A128KW A192KW A256KW)
	      (aeskw-encrypt kek (get-aes-key-byte-size alg)
			     jwe-header payload))
	     ((A128GCMKW A192GCMKW A256GCMKW)
	      (aesgcmkw-encrypt kek (get-aes-key-byte-size alg)
				jwe-header payload))
	     (else (assertion-violation
		    'aeskw-jwe-encryptor "Unknown algorithm" alg)))))
	((jwk? kek)
	 (make-aeskw-jwe-encryptor
	  (make-symmetric-key (jwk->octet-key kek))
	  :iv-generator iv-generator
	  :cek-generator cek-generator
	  :kek-iv-generator kek-iv-generator))
	(else (assertion-violation 'make-aeskw-jwe-encryptor
				   "Unsupported KEK type" kek))))

;; Chacha20 / XChaCha20 key wrap
(define (make-c20pkw-jwe-encryptor kek :key (cek-generator default-cek-generator)
					    (iv-generator default-iv-generator)
					    (kek-iv-generator default-iv-generator))
  (define (c20pkw-encrypt scheme iv-size kek jwe-header payload)
    ;; if the header already has iv, use it
    ;; NOTE: we can't use tag as it'd be generated
    (define kek-iv-g
      (cond ((jwe-header-iv jwe-header) => (lambda (iv) (lambda (size) iv)))
	    (else kek-iv-generator)))
    (define cek-key-encryptor (c20p-encryptor scheme iv-size kek kek-iv-g))
    (define enc (jwe-header-enc jwe-header))
    (let ((raw-cek (generate-cek cek-generator enc)))
      (let-values (((iv encrypted-cek tag) (cek-key-encryptor #vu8() raw-cek)))
	(let ((new-header (jwe-header-builder (from jwe-header)
			   (iv iv)
			   (tag tag))))
	  (core-encryptor (make-symmetric-key raw-cek)
			  encrypted-cek
			  new-header payload iv-generator)))))

  (cond ((symmetric-key? kek)
	 (lambda (jwe-header payload)
	   (define alg (jose-crypto-header-alg jwe-header))
	   (case alg
	     ((C20PKW)
	      (c20pkw-encrypt *scheme:chacha20-poly1305*
			      12 kek jwe-header payload))
	     ((XC20PKW)
	      (c20pkw-encrypt *scheme:xchacha20-poly1305*
			      24 kek jwe-header payload))
	     (else (assertion-violation
		    'aeskw-jwe-encryptor "Unknown algorithm" alg)))))
	((jwk? kek)
	 (make-c20pkw-jwe-encryptor (make-symmetric-key (jwk->octet-key kek))
	  :iv-generator iv-generator
	  :cek-generator cek-generator
	  :kek-iv-generator kek-iv-generator))
	(else (assertion-violation 'make-c20pkw-jwe-encryptor
				   "Unsupported KEK type" kek))))

;; PBE key wrap
(define (make-pbes2-jwe-encryptor password
				  :key (salt-generator default-salt-generator)
				       (cek-generator default-cek-generator)
				       (iv-generator default-iv-generator))
  (lambda (jwe-header payload)
    (define alg (jose-crypto-header-alg jwe-header))
    (define enc (jwe-header-enc jwe-header))
    (unless (memq alg
		  '(PBES2-HS256+A128KW PBES2-HS384+A192KW PBES2-HS512+A256KW))
      (assertion-violation 'pbes2-jwe-encryptor
			   "Alg must be one of PBES2-*" alg))
    (let-values (((raw-salt iteration) (salt-generator)))
      (let ((ps-key (pbes2-derive-kek alg password raw-salt iteration))
	    (new-header (jwe-header-builder (from jwe-header)
			 (p2s raw-salt)
			 (p2c iteration)))
	    (raw-cek (generate-cek cek-generator enc)))
	(core-encryptor (make-symmetric-key raw-cek)
			(aes-key-wrap ps-key raw-cek)
			new-header payload iv-generator)))))

;; Direct encryption
(define (make-direct-jwe-encryptor key
				   :key (iv-generator default-iv-generator)
					(strict? #t))
  (cond ((symmetric-key? key)
	 (lambda (jwe-header payload)
	   (define alg (jose-crypto-header-alg jwe-header))
	   (when (and strict? (not (eq? alg 'dir)))
	     (assertion-violation 'direct-encryptor
				  "Alg must be 'dir'" jwe-header))
	   (core-encryptor key #vu8() jwe-header payload iv-generator)))
	((jwk? key)
	 (make-direct-jwe-encryptor
	  (make-symmetric-key (jwk->octet-key key))
	  :iv-generator iv-generator
	  :strict? strict?))
	(else (assertion-violation 'make-direct-jwe-encryptor
				   "Unsupported key" key))))

;; Encryption utilities
(define (ecdh-derive-shared-key jwe-header z)
  (define alg (jose-crypto-header-alg jwe-header))
  (define enc (jwe-header-enc jwe-header))
  (define alg-id
    (case alg
      ((ECDH-ES) enc)
      ((ECDH-ES+A128KW ECDH-ES+A198KW ECDH-ES+A256KW) alg)
      ((ECDH-ES+C20PKW ECDH-ES+XC20PKW) alg)
      (else (assertion-violation 'ecdh-derive-shared-key
				 "Unknown alg, bug?" alg))))
  (let ((key-length (* (get-aes-key-byte-size alg-id #t) 8)))
    (concat-kdf *digest:sha-256*
		z
		key-length
		(encode-data/length (string->utf8 (symbol->string alg-id)))
		(encode-data/length (jwe-header-apu jwe-header))
		(encode-data/length (jwe-header-apv jwe-header))
		(integer->bytevector key-length 4))))

(define (encode-data/length bv)
  (if bv
      (bytevector-append (integer->bytevector (bytevector-length bv) 4) bv)
      #vu8(0 0 0 0)))
;; This is defined in NIST 800-56A, so maybe better to make a separate library
;; ref:
;; https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-56Cr1.pdf
(define (concat-kdf digest z key-size . rest)
  (define (derive-key algo z key-size otherinfo)
    (define hsize (digest-descriptor-digest-size algo))
    (define hash-bits (* hsize 8))
    (define reps (div (- (+ key-size hash-bits) 1) hash-bits))
    (define md (make-message-digest algo))
    (define i32 (make-bytevector 4 0))
    (when (> reps (- (expt 2 32) 1))
      (assertion-violation 'concat-kdf "Too big key size"))
    (message-digest-init! md)
    (let ((buf (make-bytevector hsize)))
      (let-values (((out e) (open-bytevector-output-port)))
	(do ((i 1 (+ i 1)))
	    ((> i reps))
	  (bytevector-u32-set! i32 0 i (endianness big))
	  (message-digest-process! md i32)
	  (message-digest-process! md z)
	  (for-each (lambda (bv) (message-digest-process! md bv)) otherinfo)
	  (message-digest-done! md buf)
	  (put-bytevector out buf))
	(let ((derived-key-material (e)))
	  (if (= (bytevector-length derived-key-material) (div key-size 8))
	      derived-key-material
	      (bytevector-copy derived-key-material 0 (div key-size 8)))))))
  (derive-key digest z key-size rest))

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
  (define (compress jwe-header payload)
    (case (jwe-header-zip jwe-header)
      ((DEF) (deflate-bytevector payload))
      ((#f)  payload)
      (else (assertion-violation 'core-encryptor
				 "Unknown 'zip' algorithm"
				 (jwe-header-zip jwe-header)))))
  (let-values (((iv cipher-text auth-tag)
		(encryption-cipher aad (compress jwe-header payload))))
    (make-jwe-object jwe-header encrypted-key iv cipher-text auth-tag)))

(define (get-encryptor enc key iv-generator)
  (case enc
    ((A128CBC-HS256 A192CBC-HS384 A256CBC-HS512)
     (aes-hmac-encryptor key (get-aes-key-byte-size enc)
			 (get-hmac-digest enc) iv-generator))
    ((A128GCM A192GCM A256GCM)
     (aes-gcm-encryptor key (get-aes-key-byte-size enc) iv-generator))
    ((C20P)
     (c20p-encryptor *scheme:chacha20-poly1305* 12 key iv-generator))
    ((XC20P)
     (c20p-encryptor *scheme:xchacha20-poly1305* 24 key iv-generator))
    (else (assertion-violation 'get-cipher "Unsupported enc type" enc))))

(define (get-aes-key-byte-size enc :optional (for-cek? #f))
  (case enc
    ((A128GCM A128KW A128GCMKW ECDH-ES+A128KW)   16)
    ((A192GCM A192KW A192GCMKW ECDH-ES+A198KW)   24)
    ((A256GCM A256KW A256GCMKW ECDH-ES+A256KW)   32)
    ((A128CBC-HS256)                             (* 16 (if for-cek? 2 1)))
    ((A192CBC-HS384)                             (* 24 (if for-cek? 2 1)))
    ((A256CBC-HS512)                             (* 32 (if for-cek? 2 1)))
    ((C20P XC20P ECDH-ES+C20PKW ECDH-ES+XC20PKW) 32)
    (else (assertion-violation
	   'get-aes-key-byte-size "Unsupported alg/enc type" enc))))
(define (get-hmac-digest enc)
  (case enc
    ((A128CBC-HS256) *digest:sha-256*)
    ((A192CBC-HS384) *digest:sha-384*)
    ((A256CBC-HS512) *digest:sha-512*)
    (else (assertion-violation 'get-hmac-digest "Unsupported enc type" enc))))

(define (aes-hmac-encryptor key key-size digest iv-generator)
  (let-values (((mac-key enc-key) (aes-hmac-derive-keys key key-size)))
    (define iv (iv-generator 16))
    (define enc-cipher
      (block-cipher-init! (make-block-cipher *scheme:aes* *mode:cbc*)
			  (cipher-direction encrypt)
			  enc-key
			  (make-iv-parameter iv)))
    ;; aad, payload -> (iv, cipher-text, auth-tag)
    (lambda (aad payload)
      (let ((cipher-text (block-cipher-encrypt-last-block enc-cipher payload)))
	(block-cipher-done! enc-cipher)
	(values iv cipher-text
		(aes-hmac-compute-tag digest mac-key aad iv cipher-text))))))

(define (aes-gcm-encryptor key key-size iv-generator)
  (define raw-key (symmetric-key-value key))
  (check-key-size 'aes-gcm-encryptor key-size raw-key)
  (let ()
    (define iv (iv-generator 12))
    (define enc-cipher
      (block-cipher-init! (make-block-cipher *scheme:aes* *mode:gcm* no-padding)
			  (cipher-direction encrypt)
			  key
			  (make-iv-parameter iv)))
    (lambda (aad payload)
      (block-cipher-update-aad! enc-cipher aad)
      (let* ((ct (block-cipher-encrypt-last-block enc-cipher payload))
	     (tag (block-cipher-done/tag enc-cipher
		   (block-cipher-max-tag-length enc-cipher))))
	(values iv ct tag)))))

(define (c20p-encryptor scheme iv-size key iv-generator)
  (define iv (iv-generator iv-size))
  (define enc-cipher
    (stream-cipher-init! (make-stream-cipher scheme)
			 (cipher-direction encrypt)
			 key
			 (make-iv-parameter iv)))
  ;; aad, payload -> (iv, cipher-text, auth-tag)
  (lambda (aad payload)
    (stream-cipher-update-aad! enc-cipher aad)
    (let* ((cipher-text (stream-cipher-encrypt enc-cipher payload))
	   (tag (stream-cipher-done/tag enc-cipher 16)))
      (values iv cipher-text tag))))

;; utilities
(define (generate-cek generator enc) (generator (get-aes-key-byte-size enc #t)))

(define (aes-hmac-compute-tag digest mac-key aad iv cipher-text)
  ;; tag length = lengt(mac-key)
  (let ((tag (make-bytevector (bytevector-length mac-key)))
	(md (make-mac *mac:hmac* mac-key :digest digest)))
    (mac-init! md)
    (mac-process! md aad)
    (mac-process! md iv)
    (mac-process! md cipher-text)
    (mac-process! md (aad-compute-length aad))
    (mac-done! md tag)
    tag))

(define (aes-hmac-derive-keys key key-size)
  (define size (* key-size 2))
  (define raw-key (symmetric-key-value key))
  (check-key-size 'aes-hmac-derive-keys size raw-key)
  (values (bytevector-copy raw-key 0 key-size)
	  (make-symmetric-key (bytevector-copy raw-key key-size))))

(define (pbes2-prf-param alg)
    (case alg
      ((PBES2-HS256+A128KW) (values 16 *digest:sha-256*))
      ((PBES2-HS384+A192KW) (values 24 *digest:sha-384*))
      ((PBES2-HS512+A256KW) (values 32 *digest:sha-256*))
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
	   (key (pbkdf-2 bv-password salt iteration dk-len
			 :prf (mac->prf-provider *mac:hmac* :digest digest))))
      (make-symmetric-key key))))

(define (check-key-size who size raw-key)
  (unless (= size (bytevector-length raw-key))
    (assertion-violation who "Wrong size of key size"
     `((expected ,size) (got ,(bytevector-length raw-key))))))

(define (jwe-header->aad jwe-header)
  (let ((b64 (jwe-header->base64url jwe-header)))
    (string->utf8 b64)))

;; 64 bits integer represents bits of aad length.
;; convert it to bytevector
(define (aad-compute-length aad)
  (define len (bytevector-length aad))
  (integer->bytevector (* len 8) 8))

)
