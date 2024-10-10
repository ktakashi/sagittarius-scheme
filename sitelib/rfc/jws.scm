;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/jws.scm - JSON Web Signature (JWS)
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

;; refs
;;  - https://tools.ietf.org/html/rfc7515
;;  - https://datatracker.ietf.org/doc/html/rfc7797 (for b64)
#!nounbound
#!read-macro=sagittarius/bv-string
(library (rfc jws)
    (export jws-header? jws-header-builder
	    (rename (jws-header <jws-header>)
		    (jose-header-typ jws-header-typ)
		    (jose-header-cty jws-header-cty)
		    (jose-header-custom-parameters
		     jws-header-custom-parameters)
		    (jose-crypto-header-alg jws-header-alg)
		    (jose-crypto-header-jku jws-header-jku)
		    (jose-crypto-header-jwk jws-header-jwk)
		    (jose-crypto-header-kid jws-header-kid)
		    (jose-crypto-header-x5u jws-header-x5u)
		    (jose-crypto-header-x5c jws-header-x5c)
		    (jose-crypto-header-x5t jws-header-x5t)
		    (jose-crypto-header-x5t-s256 jws-header-x5t-s256)
		    (jose-crypto-header-crit jws-header-crit))
	    jws-header-b64
	    jws-header->json write-jws-header jws-header->json-string
	    json->jws-header read-jws-header json-string->jws-header
	    
	    make-jws-object jws-object? (rename (jws-object <jws-object>))
	    jws-object-header jws-object-payload
	    (rename (jose-object-parts jws-object-parts))

	    jws-signed-object? (rename (jws-signed-object <jws-signed-object>))
	    jws-signed-object-signature
	    jws-object->string

	    public-key->jws-verifier
	    private-key->jws-signer
	    make-mac-jws-verifier make-mac-jws-signer
	    make-rsa-jws-verifier make-rsa-jws-signer
	    make-ecdsa-jws-verifier make-ecdsa-jws-signer
	    make-eddsa-jws-verifier make-eddsa-jws-signer
	    
	    jws:parse jws:serialize jws:sign jws:verify
	    ;; for convenience
	    jws:algorithm
	    )
    (import (rnrs)
	    (rfc jose)
	    (rfc jwk)
	    (rfc base64)
	    (record accessor)
	    (record builder)
	    (security signature)
	    (sagittarius crypto keys)
	    (sagittarius crypto mac)
	    (sagittarius crypto digests)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (text json)
	    (text json object-builder)
	    (util bytevector))

(define-record-type jws-header
  (parent <jose-crypto-header>)
  (fields b64))

(define-syntax jws-header-builder
  (make-record-builder jws-header
   ((custom-parameters '() ->jose-header-custom-parameter)
    (b64 #t))))

;; internal use ;)
(define-record-type parsed-jws-header
  (parent jws-header)
  (fields parsed-base64-url)
  (protocol (lambda (n)
	      (lambda (header parsed-base64-url)
		((apply n (record->list header)) parsed-base64-url)))))

(define-enumeration jws:algorithm
  (HS256 HS384 HS512
   RS256 RS384 RS512
   ES256 ES384 ES512
   PS256 PS384 PS512
   EdDSA)
  jws-algorithm)

(define jws-header-object-builder
  (json-object-builder
   (make-jws-header
    jose-crypto-header-object-builder
    (? "b64" #t)))) ;; default #t
(define jws-header-serializer
  (json-object-serializer
   (jose-crypto-header-serializer
    (? "b64" #t jws-header-b64))))

(define json->jws-header
  (make-json->header
   jws-header-object-builder
   (lambda (obj custom-parameters)
     (if (jws-header? obj)
	 (jws-header-builder (from obj)(custom-parameters custom-parameters))
	 (assertion-violation 'json->jws-header "Something was wrong" obj)))))

(define read-jws-header
  (case-lambda
   (() (read-jws-header (current-input-port)))
   ((port) (json->jws-header (json-read port)))))
(define (json-string->jws-header json-string)
  (read-jws-header (open-string-input-port json-string)))

(define (jws-header->json jws-header)
  (object->json jws-header jws-header-serializer))
(define write-jws-header
  (case-lambda
   ((jws-header) (write-jws-header jws-header (current-output-port)))
   ((jws-header port)
    (json-write/normalized (jws-header->json jws-header) port))))
(define (jws-header->json-string jws-header)
  (let-values (((out e) (open-string-output-port)))
    (write-jws-header jws-header out)
    (e)))
(define (jws-header->base64url jws-header)
  (if (parsed-jws-header? jws-header)
      (parsed-jws-header-parsed-base64-url jws-header)
      (let ((json (jws-header->json-string jws-header)))
	(base64url-encode-string json))))

(define ->base64url bytevector->base64url-string)
(define (make-signing-input header payload)
  (bytevector-append
   (string->utf8 (jws-header->base64url header))
   #*"."
   (if (jws-header-b64 header)
       (base64url-encode payload)
       payload)))
  
(define-record-type jws-object
  (parent <jose-object>)
  (fields header
	  payload
	  signing-input)
  (protocol (lambda (n)
	      (case-lambda
	       ((header payload)
		((n '()) header payload (make-signing-input header payload)))
	       ((parts header payload)
		((n parts)
		 header payload (make-signing-input header payload)))))))
  
(define-record-type jws-signed-object
  (parent jws-object)
  (fields signature)
  (protocol (lambda (n)
	      (lambda (parts header payload signature)
		((n parts header payload) signature)))))

(define (jws-object->string jws-object)
  (let ((header (jws-header->base64url (jws-object-header jws-object)))
	(payload (->base64url (jws-object-payload jws-object))))
    (if (jws-signed-object? jws-object)
	(string-append header "." payload "."
		       (->base64url (jws-signed-object-signature jws-object)))
	(string-append header "." payload "."))))

(define jws:serialize
  (case-lambda
   ((jws-object) (jws:serialize jws-object #f))
   ((jws-object detach-payload?)
    ;; RFC 7797 4.2
    (define (check-b64 jws-object)
      ;; #x2e = #\.
      (define (dot? i) (eqv? (bytevector-u8-ref payload i) #x2e))
      (define header (jws-object-header jws-object))
      (define payload (jws-object-payload jws-object))
      (and (not (jws-header-b64 header))
	   (do ((i 0 (+ i 1)) (len (bytevector-length payload)))
	       ((or (= i len) (dot? i)) (and (not (= i len)) (dot? i))))))
    (unless (jws-signed-object? jws-object)
      (assertion-violation 'jws:serialize "Unsigned object can't be serialized"
			   jws-object))
    (if (or detach-payload? (check-b64 jws-object))
	(string-append (jws-header->base64url (jws-object-header jws-object))
		       ".."
		       (->base64url (jws-signed-object-signature jws-object)))
	(jws-object->string jws-object)))))

;; allow user to provide payload in case of no payload form
(define jws:parse
  (case-lambda
   ((s) (jws:parse s #f))
   ((s maybe-payload)
    (define (get-payload input maybe-payload)
      (cond (maybe-payload
	     (if (and (string-null? input) (bytevector? maybe-payload))
		 maybe-payload
		 (assertion-violation 'jws:parse "Input contains payload"
				      input)))
	    ((string-null? input) #vu8()) ;; shortcut
	    (else (base64url-decode (string->utf8 input)))))
    (let ((part* (jose-split s)))
      (unless (= 3 (length part*))
	(assertion-violation 'jws:parse "Invalid JWS format, parts must be 3"
			     part*))
      (let ((header (base64url-decode-string (car part*)))
	    ;; we hold the below as bytevectors (for convenience)
	    (payload (get-payload (cadr part*) maybe-payload))
	    (signature (base64url-decode (string->utf8 (caddr part*)))))
	;; we make 'signed-object' even no signature is provided
	;; in that case, it has to be alg = 'none'.
	;; NOTE: 'signed-object' != verified. So the caller must verify
	;;       the object.
	(make-jws-signed-object part*
	 (make-parsed-jws-header (json-string->jws-header header) (car part*))
	 payload signature))))))

(define (jws:sign jws-object signer)
  (let* ((signing-input (jws-object-signing-input jws-object))
	 (signature (signer (jws-object-header jws-object)
			    signing-input)))
    (unless (bytevector? signature)
      (assertion-violation 'jws:sign "Signer returned non signature" signature))
    ;; (jws:parse (string-append signing-input "." (->base64url signature)))
    (let* ((h&p (jose-split (utf8->string signing-input) #f))
	   (h (car h&p))
	   (p (cadr h&p)))
      (make-jws-signed-object (list h p (->base64url signature))
       (make-parsed-jws-header 
	(json-string->jws-header (base64url-decode-string h)) h)
       (jws-object-payload jws-object) signature))))

(define jws:verify
  (case-lambda
   ((jws-object verifier) (jws:verify jws-object verifier '()))
   ((jws-object verifier critical-headers)
    (unless (jws-signed-object? jws-object)
      (assertion-violation 'jws:verify "JWS object is not signed" jws-object))
    (let ((signing-input (jws-object-signing-input jws-object))
	  (signature (jws-signed-object-signature jws-object))
	  (jws-header (jws-object-header jws-object)))
      (and (check-critical-headers critical-headers jws-header)
	   (verifier (jws-object-header jws-object)
		     signing-input
		     signature))))))

;;; verifiers (maybe separate to different library?)
(define (check-critical-headers critical-headers jws-header)
  (jose-crypto-header-check-critical-headers jws-header critical-headers))

(define (make-mac-jws-verifier key)
  (define (get-algorithm alg)
    (case alg
      ((HS256) *digest:sha-256*)
      ((HS384) *digest:sha-384*)
      ((HS512) *digest:sha-512*)
      (else (assertion-violation 'mac-jws-verifier "Unknown algorithm" alg))))
  (cond ((jwk? key) (make-mac-jws-verifier (jwk->octet-key key)))
	((bytevector? key)
	 (lambda (header signed-content signature)
	   (let* ((algo (get-algorithm (jose-crypto-header-alg header)))
		  (mac (make-mac *mac:hmac* key :digest algo)))
	     (verify-mac mac signed-content signature))))
	(else (assertion-violation 'make-mac-jws-verifier
				   "JWK:oct or bytevector is required" key))))

(define (make-rsa-jws-verifier key)
  (define (get-verifier alg)
    (case alg
      ((RS256) (*rsa/sha256-verifier-provider* key))
      ((RS384) (*rsa/sha384-verifier-provider* key))
      ((RS512) (*rsa/sha512-verifier-provider* key))
      ((PS256)
       (*rsassa-pss-verifier-provider* key
				       :digest *digest:sha-256*
				       :salt-length 32))
      ((PS384)
       (*rsassa-pss-verifier-provider* key
				       :digest *digest:sha-384*
				       :salt-length 48))
      ((PS512)
       (*rsassa-pss-verifier-provider* key
				       :digest *digest:sha-512*
				       :salt-length 64))
      (else (assertion-violation 'rsa-jws-verifier "Unknown algorithm" alg))))
  (cond ((jwk? key) (make-rsa-jws-verifier (jwk->public-key key)))
	((rsa-public-key? key)
	 (lambda (header signed-content signature)
	   (define rsa-verifier (get-verifier (jose-crypto-header-alg header)))
	   (rsa-verifier signed-content signature)))
	(else (assertion-violation 'make-rsa-jws-verifier
				   "Public key required" key))))

(define (make-ecdsa-jws-verifier key)
  (define (get-verifier alg)
    (case alg
      ((ES256 ES256K) (*ecdsa/sha256-verifier-provider* key :der-encode #f))
      ((ES384) (*ecdsa/sha384-verifier-provider* key :der-encode #f))
      ((ES512) (*ecdsa/sha512-verifier-provider* key :der-encode #f))
      (else (assertion-violation 'ecdsa-jws-verifier "Unknown algorithm" alg))))
  (cond ((jwk? key) (make-ecdsa-jws-verifier (jwk->public-key key)))
	((ecdsa-public-key? key)
	 (lambda (header signed-content signature)
	   (define ecdsa-verifier
	     (get-verifier (jose-crypto-header-alg header)))
	   (ecdsa-verifier signed-content signature)))
	(else (assertion-violation 'make-ecdsa-jws-verifier
				   "Public key required" key))))

(define (make-eddsa-jws-verifier key)
  (define (get-verifier alg)
    (case alg
      ((EdDSA) (*eddsa-verifier-provider* key))
      (else (assertion-violation 'ecdsa-jws-verifier "Unknown algorithm" alg))))
  (cond ((jwk? key) (make-eddsa-jws-verifier (jwk->public-key key)))
	((public-key? key)
	 (lambda (header signed-content signature)
	   (define eddsa-verifier
	     (get-verifier (jose-crypto-header-alg header)))
	   (eddsa-verifier signed-content signature)))
	(else (assertion-violation 'make-eddsa-jws-verifier
				   "Public key required" key))))

;;; signers (ditto)
(define (make-mac-jws-signer key)
  (define (get-algorithm alg)
    (case alg
      ((HS256) *digest:sha-256*)
      ((HS384) *digest:sha-384*)
      ((HS512) *digest:sha-512*)
      (else (assertion-violation 'mac-jws-signer "Unknown algorithm" alg))))
  (cond ((jwk? key) (make-mac-jws-signer (jwk->octet-key key)))
	((bytevector? key)
	 (lambda (header signing-content)
	   (let* ((algo (get-algorithm (jose-crypto-header-alg header)))
		  (mac (make-mac *mac:hmac* key :digest algo)))
	     (generate-mac mac signing-content))))
	(else (assertion-violation 'make-mac-jws-signer
				   "JWK:oct or bytevector required" key))))

(define (make-rsa-jws-signer key)
  (define (get-signer alg)
    (case alg
      ((RS256) (*rsa/sha256-signer-provider* key))
      ((RS384) (*rsa/sha384-signer-provider* key))
      ((RS512) (*rsa/sha512-signer-provider* key))
      ((PS256)
       (*rsassa-pss-signer-provider* key
				     :digest *digest:sha-256* :salt-length 32))
      ((PS384)
       (*rsassa-pss-signer-provider* key
				     :digest *digest:sha-384* :salt-length 48))
      ((PS512)
       (*rsassa-pss-signer-provider* key
				     :digest *digest:sha-512* :salt-length 64))
      (else (assertion-violation 'rsa-jws-signer "Unknown algorithm" alg))))
  (cond ((jwk? key) (make-rsa-jws-signer (jwk->private-key key)))
	((private-key? key)
	 (lambda (header signing-content)
	   (define rsa-signer (get-signer (jose-crypto-header-alg header)))
	   (rsa-signer signing-content)))
	(else (assertion-violation 'make-rsa-jws-signer
				   "Private key required" key))))

(define (make-ecdsa-jws-signer key)
  (define (get-signer alg)
    (case alg
      ((ES256 ES256K) (*ecdsa/sha256-signer-provider* key :der-encode #f))
      ((ES384) (*ecdsa/sha384-signer-provider* key :der-encode #f))
      ((ES512) (*ecdsa/sha512-signer-provider* key :der-encode #f))
      (else (assertion-violation 'ecdsa-jws-verifier "Unknown algorithm" alg))))
  (cond ((jwk? key) (make-ecdsa-jws-signer (jwk->private-key key)))
	((private-key? key)
	 (lambda (header signing-content)
	   (define ecdsa-signer (get-signer (jose-crypto-header-alg header)))
	   (ecdsa-signer signing-content)))
	(else (assertion-violation 'make-ecdsa-jws-signer
				   "Private key required" key))))

(define (make-eddsa-jws-signer key)
  (define (get-signer alg)
    (case alg
      ((EdDSA) (*eddsa-signer-provider* key))
      (else (assertion-violation 'eddsa-jws-signer "Unknown algorithm" alg))))
  (cond ((jwk? key) (make-eddsa-jws-signer (jwk->private-key key)))
	((private-key? key)
	 (lambda (header signing-content)
	   (define eddsa-signer (get-signer (jose-crypto-header-alg header)))
	   (eddsa-signer signing-content)))
	(else (assertion-violation 'make-ecdsa-jws-signer
				   "Private key required" key))))

(define (public-key->jws-verifier key)
  (cond ((jwk:oct? key) (public-key->jws-verifier (jwk->octet-key key)))
	((jwk? key) (public-key->jws-verifier (jwk->public-key key)))
	((rsa-public-key? key) (make-rsa-jws-verifier key))
	((bytevector? key) (make-mac-jws-verifier key))
	((ecdsa-public-key? key) (make-ecdsa-jws-verifier key))
	((eddsa-public-key? key) (make-eddsa-jws-verifier key))
	(else
	 (assertion-violation
	  'public-key->jws-verifier "Unknown public key" key))))

(define (private-key->jws-signer key)
  (cond ((jwk:oct? key) (private-key->jws-signer (jwk->octet-key key)))
	((jwk? key) (private-key->jws-signer (jwk->private-key key)))
	((rsa-private-key? key) (make-rsa-jws-signer key))
	((bytevector? key) (make-mac-jws-signer key))
	((ecdsa-private-key? key) (make-ecdsa-jws-signer key))
	((eddsa-private-key? key) (make-eddsa-jws-signer key))
	(else
	 (assertion-violation 'private-key->jws-signer
			      "Unknown private key" key))))
)
