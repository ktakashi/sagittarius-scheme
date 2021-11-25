;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/jwk.scm - JSON Web Key (JWK)
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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
;; - https://tools.ietf.org/html/rfc7517
;; - https://datatracker.ietf.org/doc/html/rfc8037

#!nounbound
(library (rfc jwk)
    (export json-string->jwk-set read-jwk-set json->jwk-set
	    jwk-set->json-string write-jwk-set jwk-set->json
	    ;; key set
	    jwk-set-keys jwk-set? make-jwk-set
	    jwk-set:find-key jwk-set->public-jwk-set
	    jwk-matcher:kty jwk-matcher:use jwk-matcher:key-ops jwk-matcher:alg
	    jwk-matcher:kid jwk-matcher:x5t jwk-matcher:x5t-s256
	    jwk-matcher:crv
	    jwk-matcher:rsa jwk-matcher:ec jwk-matcher:oct jwk-matcher:okp
	    
	    ;; keys
	    json-string->jwk read-jwk json->jwk
	    jwk->json-string write-jwk jwk->json
	    jwk? jwk-kty jwk-use jwk-key-ops jwk-alg jwk-kid
	    jwk-x5u jwk-x5c jwk-x5t jwk-x5t-s256
	    ;; EC
	    jwk:ec? make-jwk:ec jwk:ec-crv jwk:ec-x jwk:ec-y
	    jwk:ec-private? make-jwk:ec-private jwk:ec-private-d
	    ;; RSA
	    jwk:rsa? jwk:rsa-n jwk:rsa-e
	    jwk:rsa-private? make-jwk:rsa-private jwk:rsa-private-d
	    jwk:rsa-crt-private? make-jwk:rsa-crt-private
	    jwk:rsa-crt-private-p  jwk:rsa-crt-private-q jwk:rsa-crt-private-dp
	    jwk:rsa-crt-private-dq jwk:rsa-crt-private-qi
	    ;; oct
	    jwk:oct? make-jwk:oct jwk:oct-k
	    ;; OKP
	    jwk:okp? make-jwk:okp jwk:okp-crv jwk:okp-x
	    jwk:okp-private? make-jwk:okp-private jwk:okp-private-d

	    jwk->public-key
	    jwk->private-key
	    jwk->octet-key
	    jwk->public-jwk

	    jwk-config-builder jwk:config?

	    jwk->jwk-config
	    public-key->jwk
	    private-key->jwk
	    secret-key->jwk
	    key->jwk
	    )
    (import (rnrs)
	    (text json parse)
	    (text json object-builder)
	    (sagittarius)
	    (rfc jwa)
	    (rfc base64)
	    (rfc x.509)
	    (srfi :39)
	    (record builder)
	    (crypto)
	    (math)
	    (math ec))

  (define-record-type jwk
    ;; sort of similar to JOSE header but different record type
    (fields kty use key-ops alg kid x5u x5c x5t x5t-s256))

  (define-record-type jwk-set
    (fields keys))

  (define (jwk-set:find-key jwks matcher)
    (find matcher (jwk-set-keys jwks)))

  (define (jwk-set->public-jwk-set jwks)
    (make-jwk-set (map jwk->public-jwk (jwk-set-keys jwks))))

  (define-syntax define-jwk-matcher
    (lambda (x)
      (define (generate-name* k f)
	(define n (symbol->string (syntax->datum f)))
	(datum->syntax
	 k
	 (list (string->symbol (string-append "jwk-matcher:" n))
	       (string->symbol (string-append "jwk-" n)))))
      (syntax-case x ()
	((k field) #'(k field equal?))
	((k field comp=)
	 (with-syntax (((name acc) (generate-name* #'k #'field)))
	   #'(define (name field)
	       (lambda (jwk)
		 ;; to make composable
		 (and (jwk? jwk)
		      (comp= field (acc jwk))
		      jwk))))))))
  (define-jwk-matcher kty)
  (define-jwk-matcher use)
  (define-jwk-matcher key-ops member)
  (define-jwk-matcher alg)
  (define-jwk-matcher kid)
  ;; we don't compare x5u and x5c, these are actual certificates
  (define-jwk-matcher x5t)
  (define-jwk-matcher x5t-s256)

  ;; others
  (define (jwk-matcher:crv crv)
    (lambda (jwk)
      (cond ((jwk:ec? jwk)
	     (and (equal? (jwk:ec-crv jwk) crv) jwk))
	    ((jwk:okp? jwk)
	     (and (equal? (jwk:okp-crv jwk) crv) jwk))
	    (else #f))))
  (define jwk-matcher:rsa (jwk-matcher:kty 'RSA))
  (define jwk-matcher:ec (jwk-matcher:kty 'EC))
  (define jwk-matcher:oct (jwk-matcher:kty 'oct))
  (define jwk-matcher:okp (jwk-matcher:kty 'OKP))

  
  ;;; implementation specific record
  ;; ref: https://tools.ietf.org/html/rfc7518#section-6.1
  ;; EC
  (define-record-type jwk:ec
    (parent jwk)
    (fields crv x y))

  (define-record-type jwk:ec-private
    (parent jwk:ec)
    (fields d))

  ;; RSA
  (define-record-type jwk:rsa
    (parent jwk)
    (fields n e))

  (define-record-type jwk:rsa-private
    (parent jwk:rsa)
    (fields d))

  (define-record-type jwk:rsa-crt-private
    (parent jwk:rsa-private)
    ;; TODO support other prime factor
    ;; I couldn't find example or clear specification
    ;; e.g. the RFC says JSON array but seems JSON object so
    ;;      not sure what it is
    (fields p q dp dq qi))

  ;; Symmetric key
  (define-record-type jwk:oct
    (parent jwk)
    (fields k))

  ;; https://datatracker.ietf.org/doc/html/rfc8037
  ;; OKP
  (define-record-type jwk:okp
    (parent jwk)
    (fields crv x))
  (define-record-type jwk:okp-private
    (parent jwk:okp)
    (fields d))
  
  (define (base64-url-string->bytevector s) (base64url-decode (string->utf8 s)))
  (define (base64-string->bytevector s) (base64-decode-string s :transcoder #f))
  (define (b64-string->x509-certificate s)
    (make-x509-certificate (base64-string->bytevector s)))
  (define jwk-builder
    (json-object-builder
     (make-jwk
      ("kty" string->symbol)
      (? "use" #f string->symbol)
      (? "key-ops" '() (@ list string->symbol))
      (? "alg" #f string->symbol)
      (? "kid" #f)
      (? "x5u" #f)
      (? "x5c" '() (@ list b64-string->x509-certificate))
      (? "x5t" #f base64-url-string->bytevector)
      (? "x5t#S256" #f base64-url-string->bytevector))))
  
  (define jwks-builder
    (json-object-builder
     (make-jwk-set
      ("keys"
       (@ list jwk-builder)))))

  ;; the RFC should've make separate key material object so that
  ;; we could make our life much easier but no choice. fxxk!!!
  (define (make-jwk-by-type jwk key-param)
    (define (ctr&params jwk key-param)
      (define decode-b64 base64-url-string->bytevector)
      (define (pref k) (hashtable-ref key-param k #f))
      (define (maybe->bytevector v) (and v (decode-b64 v)))
      (define (maybe->integer v)
	(and v (bytevector->integer (decode-b64 v))))
      (case (jwk-kty jwk)
	((EC)
	 (let ((crv (string->symbol (hashtable-ref key-param 'crv)))
	       (x (maybe->integer (pref 'x)))
	       (y (maybe->integer (pref 'y))))
	   (cond ((pref 'd) =>
		  (lambda (d)
		    (values make-jwk:ec-private crv x y (maybe->integer d))))
		 (else (values make-jwk:ec crv x y)))))
	((RSA)
	 (let ((n (maybe->integer (pref 'n)))
	       (e (maybe->integer (pref 'e))))
	   (cond ((maybe->integer (pref 'd)) =>
		  (lambda (d)
		    (cond ((maybe->integer (pref 'p)) =>
			   (lambda (p)
			     ;; TODO should we fallback if there's
			     ;; missing values?
			     (values make-jwk:rsa-crt-private
				     n
				     e
				     d
				     p
				     (maybe->integer (pref 'q))
				     (maybe->integer (pref 'dp))
				     (maybe->integer (pref 'dq))
				     (maybe->integer (pref 'qi))
				     ;; TODO oth
				     )))
			  (else (values make-jwk:rsa-private n e d)))))
		 (else (values make-jwk:rsa n e)))))
	((oct) (values make-jwk:oct (decode-b64 (pref 'k))))
	((OKP)
	 (let ((crv (string->symbol (hashtable-ref key-param 'crv)))
	       (x (maybe->bytevector (pref 'x))))
	   (cond ((pref 'd) =>
		  (lambda (d)
		    (values make-jwk:okp-private crv x (maybe->bytevector d))))
		 (else (values make-jwk:okp crv x)))))
	(else => (lambda (t) (error 'read-jwk-set "unsupported key type" t)))))
    (let-values (((ctr . param) (ctr&params jwk key-param)))
      (let ((use (jwk-use jwk))
	    (key-ops (jwk-key-ops jwk)))
	(when (and use (not (null? key-ops)))
	  (error 'read-jwk-set "use and key_ops must not be together"))
	(apply ctr (jwk-kty jwk) use key-ops
	       (jwk-alg jwk) (jwk-kid jwk) (jwk-x5u jwk) (jwk-x5c jwk)
	       (jwk-x5t jwk) (jwk-x5t-s256 jwk) param))))

  (define (json->jwk-set json)
    (define key-parameters (make-eq-hashtable))
    (define (parameter-handler k v)
      (hashtable-set! key-parameters (string->symbol k) v))
    
    (define (post-object-build obj)
      (if (jwk? obj)
	  (let ((r (make-jwk-by-type obj key-parameters)))
	    (hashtable-clear! key-parameters)
	    r)
	  obj))
    (parameterize ((*post-json-object-build* post-object-build))
      (json->object json jwks-builder parameter-handler)))

  (define read-jwk-set
    (case-lambda
     (() (read-jwk-set (current-input-port)))
     ((port) (json->jwk-set (json-read port)))))
    
  (define (json-string->jwk-set json-string)
    (read-jwk-set (open-string-input-port json-string)))

  (define (json->jwk json)
    (define key-parameters (make-eq-hashtable))
    (define (parameter-handler k v)
      (hashtable-set! key-parameters (string->symbol k) v))
    
    (define (post-object-build obj)
      (if (jwk? obj)
	  (let ((r (make-jwk-by-type obj key-parameters)))
	    (hashtable-clear! key-parameters)
	    r)
	  obj))
    (parameterize ((*post-json-object-build* post-object-build))
      (json->object json jwk-builder parameter-handler)))

  (define read-jwk
    (case-lambda
     (() (read-jwk (current-input-port)))
     ((port) (json->jwk (json-read port)))))
    
  (define (json-string->jwk json-string)
    (read-jwk (open-string-input-port json-string)))
  
  (define (bytevector->b64-string bv)
    (utf8->string (base64-encode bv :line-width #f)))
  (define (bytevector->b64url-string bv) (utf8->string (base64url-encode bv)))
  (define (x509-certificate->b64-string cert)
    (bytevector->b64-string (x509-certificate->bytevector cert)))
  (define-syntax jwk-serializer
    (syntax-rules ()
      ((_ (key acc ...) ...)
       (json-object-serializer
	(("kty" jwk-kty symbol->string)
	 (key acc ...) ...
	 (? "use" #f jwk-use symbol->string)
	 (? "key_ops" '() jwk-key-ops (-> symbol->string))
	 (? "alg" #f jwk-alg symbol->string)
	 (? "kid" #f jwk-kid)
	 (? "x5u" #f jwk-x5u)
	 (? "x5c" '() jwk-x5c (-> x509-certificate->b64-string))
	 (? "x5t" #f jwk-x5t bytevector->b64url-string)
	 (? "x5t#S256" #f jwk-x5t-s256 bytevector->b64url-string))))))

  (define (bytevector->b64u-string bv)
    (utf8->string (base64url-encode bv)))
  (define (integer->b64u-string i)
    (utf8->string (base64url-encode (integer->bytevector i))))
  
  (define jwk:ec-serializer
    (jwk-serializer ("crv" jwk:ec-crv symbol->string)
		    ("x" jwk:ec-x integer->b64u-string)
		    ("y" jwk:ec-y integer->b64u-string)))
  (define jwk:ec-private-serializer
    (jwk-serializer ("crv" jwk:ec-crv symbol->string)
		    ("x" jwk:ec-x integer->b64u-string)
		    ("y" jwk:ec-y integer->b64u-string)
		    ("d" jwk:ec-private-d integer->b64u-string)))
  ;; RSA
  (define jwk:rsa-serializer
    (jwk-serializer ("n" jwk:rsa-n integer->b64u-string)
		    ("e" jwk:rsa-e integer->b64u-string)))
  (define jwk:rsa-private-serializer
    (jwk-serializer ("n" jwk:rsa-n integer->b64u-string)
		    ("e" jwk:rsa-e integer->b64u-string)
		    ("d" jwk:rsa-private-d integer->b64u-string)))
  (define jwk:rsa-crt-private-serializer
    (jwk-serializer ("n" jwk:rsa-n integer->b64u-string)
		    ("e" jwk:rsa-e integer->b64u-string)
		    ("d" jwk:rsa-private-d integer->b64u-string)
		    ("p" jwk:rsa-crt-private-p integer->b64u-string)
		    ("q" jwk:rsa-crt-private-q integer->b64u-string)
		    ("dp" jwk:rsa-crt-private-dp integer->b64u-string)
		    ("dq" jwk:rsa-crt-private-dq integer->b64u-string)
		    ("qi" jwk:rsa-crt-private-qi integer->b64u-string)))
  ;; oct
  (define jwk:oct-serializer
    (jwk-serializer ("k" jwk:oct-k bytevector->b64u-string)))
  
  ;; OKP
  (define jwk:okp-serializer
    (jwk-serializer ("crv" jwk:okp-crv symbol->string)
		    ("x" jwk:okp-x bytevector->b64u-string)))
  (define jwk:okp-private-serializer
    (jwk-serializer ("crv" jwk:okp-crv symbol->string)
		    ("x" jwk:okp-x bytevector->b64u-string)
		    ("d" jwk:okp-private-d bytevector->b64u-string)))
  
  (define (dispatch-jwk o)
    (cond ((jwk:ec-private? o) (object->json o jwk:ec-private-serializer))
	  ((jwk:ec? o) (object->json o jwk:ec-serializer))
	  ((jwk:rsa-crt-private? o)
	   (object->json o jwk:rsa-crt-private-serializer))
	  ((jwk:rsa-private? o) (object->json o jwk:rsa-private-serializer))
	  ((jwk:rsa? o) (object->json o jwk:rsa-serializer))
	  ((jwk:oct? o) (object->json o jwk:oct-serializer))
	  ((jwk:okp-private? o) (object->json o jwk:okp-private-serializer))
	  ((jwk:okp? o) (object->json o jwk:okp-serializer))
	  (else (error 'jwk-set-serializer "unknown object" o))))
  (define jwk-set-serializer
    (json-object-serializer
     (("keys" jwk-set-keys (-> dispatch-jwk)))))

  (define (jwk-set->json jwk-set) (object->json jwk-set jwk-set-serializer))
  (define write-jwk-set
    (case-lambda
     ((jwk-set) (write-jwk-set jwk-set (current-output-port)))
     ((jwk-set out) (json-write/normalized (jwk-set->json jwk-set) out))))
  (define (jwk-set->json-string jwk-set)
    (let-values (((out extract) (open-string-output-port)))
      (write-jwk-set jwk-set out)
      (extract)))

  (define (jwk->json jwk)
    (object->json jwk (json-object-serializer dispatch-jwk)))
  (define write-jwk
    (case-lambda
     ((jwk) (write-jwk jwk (current-output-port)))
     ((jwk out) (json-write/normalized (jwk->json jwk) out))))
  (define (jwk->json-string jwk)
    (let-values (((out extract) (open-string-output-port)))
      (write-jwk jwk out)
      (extract)))
  
  (define (jwk->public-key jwk)
    ;; jwk:ec-private or jwk:rsa-private can also be a public key
    ;; TODO should it?
    (cond ((jwk:ec? jwk)
	   (jwa:make-ec-public-key
	    (jwk:ec-crv jwk) (jwk:ec-x jwk) (jwk:ec-y jwk)))
	  ((jwk:rsa? jwk)
	   (jwa:make-rsa-public-key
	    (jwk:rsa-n jwk) (jwk:rsa-e jwk)))
	  ((jwk:okp? jwk)
	   (case (jwk:okp-crv jwk)
	     ((Ed25519) (jwa:make-ed25519-public-key (jwk:okp-x jwk)))
	     ((Ed448) (jwa:make-ed448-public-key (jwk:okp-x jwk)))
	     ((X25519) (jwa:make-x25519-public-key (jwk:okp-x jwk)))
	     ((X448) (jwa:make-x448-public-key (jwk:okp-x jwk)))
	     (else
	      (assertion-violation 'jwk->public-key
				   "given OKP crv is not supported"
				   (jwk:okp-crv jwk)))))
	  (else
	   (assertion-violation 'jwk->public-key
				"given JWK object is not a public key" jwk))))
  (define (jwk->private-key jwk)
    (cond ((jwk:ec-private? jwk)
	   (jwa:make-ec-private-key
	    (jwk:ec-crv jwk) (jwk:ec-private-d jwk)
	    (jwk:ec-x jwk) (jwk:ec-y jwk)))
	  ((jwk:rsa-private? jwk)
	   (let ((n (jwk:rsa-n jwk))
		 (e (jwk:rsa-e jwk))
		 (d (jwk:rsa-private-d jwk)))
	     (if (jwk:rsa-crt-private? jwk)
		 (jwa:make-rsa-crt-private-key
		  n
		  e
		  d
		  (jwk:rsa-crt-private-p jwk)
		  (jwk:rsa-crt-private-q jwk)
		  (jwk:rsa-crt-private-dp jwk)
		  (jwk:rsa-crt-private-dq jwk)
		  (jwk:rsa-crt-private-qi jwk))
		 (jwa:make-rsa-private-key n e d))))
	  ((jwk:okp-private? jwk)
	   (case (jwk:okp-crv jwk)
	     ((Ed25519) (jwa:make-ed25519-private-key (jwk:okp-private-d jwk)))
	     ((Ed448) (jwa:make-ed448-private-key (jwk:okp-private-d jwk)))
	     ((X25519) (jwa:make-x25519-private-key (jwk:okp-private-d jwk)))
	     ((X448) (jwa:make-x448-private-key (jwk:okp-private-d jwk)))
	     (else
	      (assertion-violation 'jwk->prviate-key
				   "given OKP crv is not supported"
				   (jwk:okp-crv jwk)))))
	  (else
	   (assertion-violation 'jwk->private-key
				"given JWK object is not a private key" jwk))))
  (define (jwk->octet-key jwk)
    (if (jwk:oct? jwk)
	(jwk:oct-k jwk)
	(assertion-violation 'jwk->octet-key
			     "given JWK object is not a oct type object" jwk)))

  ;; it's a bit inefficient
  (define (jwk->public-jwk jwk) (public-key->jwk (jwk->public-key jwk)))

  (define (oid->curv&alg oid)
    (cond ((string=? oid "1.2.840.10045.3.1.7") (values 'P-256 'ES256))
	  ((string=? oid "1.3.132.0.34") (values 'P-384 'ES384))
	  ((string=? oid "1.3.132.0.35") (values 'P-521 'ES521))
	  (else (assertion-violation 'oid->curv "Unknown JWK curv OID" oid))))

  (define-record-type jwk:config
    (fields use kid key-ops alg kid x5u x5c e))
  (define-syntax jwk-config-builder
    (make-record-builder jwk:config
			 ((key-ops '())
			  (x5c '())
			  ;; very ugry way of handling
			  ;; non CRT RSA private key which
			  ;; I don't think exists...
			  (e 65537))))

  (define (jwk->jwk-config jwk)
    (jwk-config-builder
     (use (jwk-use jwk))
     (kid (jwk-kid jwk))
     (key-ops (jwk-key-ops jwk))
     (alg (jwk-alg jwk))
     (x5u (jwk-x5u jwk))
     (x5c (jwk-x5c jwk))
     (e (if (jwk:rsa? jwk) (jwk:rsa-e jwk) 65537))))
  
  (define (x5c->fingerprint x5c algo)
    (and (not (null? x5c))
	 (utf8->string
	  (base64-encode
	   (hash algo (x509-certificate->bytevector (car x5c)))))))
  (define (make-key ctr type config . rest)
    (apply ctr type
	   (jwk:config-use config)
	   (jwk:config-key-ops config)
	   (jwk:config-alg config)
	   (jwk:config-kid config)
	   (jwk:config-x5u config)
	   (map x509-certificate->bytevector (jwk:config-x5c config))
	   (x5c->fingerprint (jwk:config-x5c config) SHA-1)
	   (x5c->fingerprint (jwk:config-x5c config) SHA-256)
	   rest))
  (define public-key->jwk
    (case-lambda
     ((public-key) (public-key->jwk public-key (jwk-config-builder)))
     ((public-key config)
      (define (rsa-public-key->jwk public-key)
	(make-key make-jwk:rsa 'RSA config
		  (rsa-public-key-modulus public-key)
		  (rsa-public-key-exponent public-key)))
      (define (ecdsa-public-key->jwk public-key)
	(define ec-param (ecdsa-public-key-parameter public-key))
	(define oid (ec-parameter-oid ec-param))
	(define Q (ecdsa-public-key-Q public-key))
	(let-values (((curv alg) (oid->curv&alg oid)))
	  (make-key make-jwk:ec 'EC config
		    curv (ec-point-x Q) (ec-point-y Q))))
      (define (eddsa-public-key->jwk public-key)
	(make-key make-jwk:okp 'OKP config
		  (if (ed25519-key? public-key) 'Ed25519 'Ed448)
		  (eddsa-public-key-data public-key)))
      (define (rfc7748-public-key->jwk public-key)
	(make-key make-jwk:okp 'OKP config
		  (if (x25519-public-key? public-key) 'X25519 'X448)
		  (rfc7748-public-key-data public-key)))
      (cond ((rsa-public-key? public-key) (rsa-public-key->jwk public-key))
	    ((ecdsa-public-key? public-key) (ecdsa-public-key->jwk public-key))
	    ((eddsa-public-key? public-key) (eddsa-public-key->jwk public-key))
	    ((rfc7748-public-key? public-key)
	     (rfc7748-public-key->jwk public-key))
	    (else (assertion-violation 'public-key->jwk "Unsupported key"
				       public-key))))))

  (define private-key->jwk
    (case-lambda
     ((private-key) (private-key->jwk private-key (jwk-config-builder)))
     ((private-key config)
      (define (rsa-private-key->jwk private-key)
	(if (rsa-private-crt-key? private-key)
	    (make-key make-jwk:rsa-crt-private 'RSA config
		      (rsa-private-key-modulus private-key)
		      (rsa-private-crt-key-public-exponent private-key)
		      (rsa-private-key-private-exponent private-key)
		      (rsa-private-crt-key-p private-key)
		      (rsa-private-crt-key-q private-key)
		      (rsa-private-crt-key-dP private-key)
		      (rsa-private-crt-key-dQ private-key)
		      (rsa-private-crt-key-qP private-key))
	    (make-key make-jwk:rsa-private 'RSA config
		      (rsa-private-key-modulus private-key)
		      (jwk:config-e config)
		      (rsa-private-key-private-exponent private-key))))
      (define (ecdsa-private-key->jwk private-key)
	(define ec-param (ecdsa-private-key-parameter private-key))
	(define oid (ec-parameter-oid ec-param))
	(define public-key (ecdsa-private-key-public-key private-key))
	(define Q (ecdsa-public-key-Q public-key))
	(let-values (((curv alg) (oid->curv&alg oid)))
	  (make-key make-jwk:ec-private 'EC config
		    curv (ec-point-x Q) (ec-point-y Q)
		    (ecdsa-private-key-d private-key))))
      (define (eddsa-private-key->jwk private-key)
	(define public-key (eddsa-private-key-public-key private-key))
	(make-key make-jwk:okp-private 'OKP config
		  (if (ed25519-key? private-key) 'Ed25519 'Ed448)
		  (eddsa-public-key-data public-key)
		  (eddsa-private-key-random private-key)))
      (define (rfc7748-private-key->jwk private-key)
	(define public-key (rfc7748-private-key-public-key private-key))
	(make-key make-jwk:okp-private 'OKP config
		  (if (x25519-private-key? private-key) 'X25519 'X448)
		  (rfc7748-public-key-data public-key)
		  (rfc7748-private-key-random private-key)))
      (cond ((rsa-private-key? private-key) (rsa-private-key->jwk private-key))
	    ((ecdsa-private-key? private-key)
	     (ecdsa-private-key->jwk private-key))
	    ((eddsa-private-key? private-key)
	     (eddsa-private-key->jwk private-key))
	    ((rfc7748-private-key? private-key)
	     (rfc7748-private-key->jwk private-key))
	    (else (assertion-violation 'private-key->jwk "Unsupported key"
				       private-key))))))

  (define secret-key->jwk
    (case-lambda
     ((secret-key) (secret-key->jwk secret-key (jwk-config-builder)))
     ((secret-key config)
      (cond ((bytevector? secret-key)
	     (make-key make-jwk:oct 'oct config secret-key))
	    ((symmetric-key? secret-key)
	     (make-key make-jwk:oct 'oct config
		       (symmetric-key-raw-key secret-key)))
	    (else (assertion-violation 'secret-key->jwk "Unsupported key"
				       secret-key))))))

  (define key->jwk
    (case-lambda
     ((key) (key->jwk key (jwk-config-builder)))
     ((key config)
      (cond ((public-key? key) (public-key->jwk key config))
	    ((private-key? key) (private-key->jwk key config))
	    ((or (bytevector? key) (symmetric-key? key))
	     (secret-key->jwk key config))
	    (else (assertion-violation 'key->jwk "Unsupported key" key))))))
    
  )
