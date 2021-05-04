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

;; ref: https://tools.ietf.org/html/rfc7515
#!nounbound
(library (rfc jws)
    (export jws-header? jws-header-builder
	    (rename (jws-header <jws-header>)
		    (jose-header-typ jws-header-typ)
		    (jose-header-cty jws-header-cty)
		    (jose-crypto-header-alg jws-header-alg)
		    (jose-crypto-header-jku jws-header-jku)
		    (jose-crypto-header-jwk jws-header-jwk)
		    (jose-crypto-header-kid jws-header-kid)
		    (jose-crypto-header-x5u jws-header-x5u)
		    (jose-crypto-header-x5c jws-header-x5c)
		    (jose-crypto-header-x5t jws-header-x5t)
		    (jose-crypto-header-x5t-s256 jws-header-x5t-s256)
		    (jose-crypto-header-crit jws-header-crit)
		    (jose-crypto-header-custom-parameters
		     jws-header-custom-parameters))
	    jws-header->json write-jws-header jws-header->json-string
	    json->jws-header read-jws-header json-string->jws-header
	    

	    make-jws-object jws-object? (rename (jws-object <jws-object>))
	    jws-object-header jws-object-payload

	    jws-signed-object? (rename (jws-signed-object <jws-signed-object>))
	    jws-signed-object-signature
	    jws-object->string

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
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (text json)
	    (text json object-builder))

(define-record-type jws-header
  (parent <jose-crypto-header>))
(define (->custom-parameter v)
  (cond ((hashtable? v) (hashtable-copy v)) ;; make it immutable
	((pair? v)
	 (let ((ht (make-hashtable string-hash string=?)))
	   (for-each (lambda (slot)
		       (hashtable-set! ht (car slot) (cadr slot))) v)
	   (->custom-parameter ht)))
	(else (assertion-violation 'jws-header-builder
				   "Custom parameter must be alist or hashtable"
				   v))))
(define-syntax jws-header-builder
  (make-record-builder jws-header
   ((custom-parameters '() ->custom-parameter))))

;; internal use ;)
(define-record-type parsed-jws-header
  (parent jws-header)
  (fields parsed-base64-url)
  (protocol (lambda (n)
	      (lambda (header parsed-base64-url)
		((apply n (record->list header)) parsed-base64-url)))))

(define-enumeration jws:algorithm
  (HS256 HS384 HS512 RS256 RS384 RS512 ES256 ES384 ES512 PS256 PS384 PS512)
  jws-algorithm)

(define (string->jws-algorithm s) (string->symbol s)) ;; for now
(define jws-header-object-builder
  (json-object-builder
   (make-jws-header
    (? "typ" #f string->symbol)
    (? "cty" #f)
    ("alg" string->jws-algorithm)
    (? "jku" #f)
    (? "jwk" #f json-string->jwk)
    (? "kid" #f)
    (? "x5u" #f) ;; TODO convert it to certificate
    (? "x5c" #f) ;; TODO convert it to certificate chain
    (? "x5t" #f) ;; TODO convert it fingerprint (bytevector)
    (? "x5t#S256" #f) ;; ditto
    (? "crit" #f)
    ;; dummy custom-parameters
    (? "___" #f)
    )))
(define (jws-algorithm->string s) (symbol->string s))
(define custom-serializer
  (make-hashtable-serializer jose-crypto-header-custom-parameters))
(define jws-header-serializer
  (json-object-serializer
   ((? "typ" #f jose-header-typ symbol->string)
    (? "cty" #f jose-header-cty)
    ("alg" jose-crypto-header-alg jws-algorithm->string)
    (? "jku" #f jose-crypto-header-jku)
    (? "jwk" #f jose-crypto-header-jwk)
    (? "kid" #f jose-crypto-header-kid)
    (? "x5u" #f jose-crypto-header-x5u)
    (? "x5c" #f jose-crypto-header-x5c)
    (? "x5t" #f jose-crypto-header-x5t)
    (? "x5t#S256" #f jose-crypto-header-x5t-s256)
    (? "crit" #f jose-crypto-header-crit)
    custom-serializer
    )))

(define (json->jws-header json)
  (define custom-parameters (make-hashtable string-hash string=?))
  (define (parameter-handler k v) (hashtable-set! custom-parameters k v))
  (define (post-object-build obj)
    (if (jws-header? obj)
	(jws-header-builder (from obj) (custom-parameters custom-parameters))))
  (parameterize ((*post-json-object-build* post-object-build))
    (json->object json jws-header-object-builder parameter-handler)))

(define (read-jws-header port) (json->jws-header (json-read port)))
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

(define (->base64url bv) (utf8->string (base64url-encode bv)))
(define-record-type jws-object
  (fields header
	  payload
	  signing-input)
  (protocol (lambda (p)
	      (lambda (header payload)
		(p header payload
		   (string-append (jws-header->base64url header) "."
				  (->base64url payload)))))))

(define-record-type jws-signed-object
  (parent jws-object)
  (fields signature)
  (protocol (lambda (n)
	      (lambda (header payload signature)
		((n header payload) signature)))))

(define *base64url-charset*
  (list->char-set (map integer->char (vector->list *base64-encode-url-table*))))

(define (jws-object->string jws-object)
  (let ((header (jws-header->base64url (jws-object-header jws-object)))
	(payload (->base64url (jws-object-payload jws-object))))
    (if (jws-signed-object? jws-object)
	(string-append header "." payload "."
		       (->base64url (jws-signed-object-signature jws-object)))
	(string-append header "." payload))))

(define jws:serialize
  (case-lambda
   ((jws-object) (jws:serialize jws-object #f))
   ((jws-object detach-payload?)
    (unless (jws-signed-object? jws-object)
      (assertion-violation 'jws:serialize "Unsigned object can't be serialized"
			   jws-object))
    (if detach-payload?
	(string-append (jws-header->base64url (jws-object-header jws-object))
		       ".."
		       (->base64url (jws-signed-object-signature jws-object)))
	(jws-object->string jws-object)))))

;; allow user to provide payload in case of no payload form
(define jws:parse
  (case-lambda
   ((s) (jws:parse s #f))
   ((s maybe-payload)
    (define (string->json s) (json-read (open-string-input-port s)))
    (define (get-payload input maybe-payload)
      (if maybe-payload
	  (if (and (zero? (string-length input)) (bytevector? maybe-payload))
	      maybe-payload
	      (assertion-violation 'jws:parse "Input contains payload" input))
	  (base64url-decode (string->utf8 input))))
    (let ((part* (string-tokenize s *base64url-charset*)))
      (unless (= (length part*) 3)
	(assertion-violation 'jws:parse "Invalid JWS format" s))
      (let ((header (base64url-decode-string (car part*)))
	    ;; we hold the below as bytevectors (for convenience)
	    (payload (get-payload (cadr part*) maybe-payload))
	    (signature (base64url-decode (string->utf8 (caddr part*)))))
	(make-jws-signed-object
	 (make-parsed-jws-header (json-string->jws-header header) (car part*))
	 payload signature))))))

(define (jws:sign jws-object siner)
  (error 'jws:sign "not yet"))

(define (jws:verify jws-object verifier)
  (unless (jws-signed-object? jws-object)
    (assertion-violation 'jws:verify "JWS object is not signed" jws-object))
  (let ((signing-input (jws-object-signing-input jws-object))
	(signature (jws-signed-object-signature jws-object)))
    (verifier (jws-object-header jws-object)
	      (string->utf8 signing-input)
	      signature)))

)
