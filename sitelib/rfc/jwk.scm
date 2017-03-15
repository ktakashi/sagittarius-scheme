;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/jwk.scm - JSON Web Key (JWK)
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

;; ref: https://tools.ietf.org/html/rfc7517

(library (rfc jwk)
    (export json-string->jwk-set read-jwk-set
	    jwk-set->json-string
	    ;; key set
	    jwk-set-keys jwk-set? make-jwk-set
	    
	    ;; keys
	    ;; EC
	    jwk:ec? make-jwk:ec jwk:ec-crv jwk:ec-x jwk:ec-y
	    jwk:ec-private? make-jwk:ec-private jwk:ec-private-d
	    ;; RSA
	    jwk:rsa?
	    jwk:rsa-public? make-jwk:rsa-public
	    jwk:rsa-public-n jwk:rsa-public-e
	    jwk:rsa-private? make-jwk:rsa-private jwk:rsa-private-d
	    jwk:rsa-crt-private? make-jwk:rsa-crt-private
	    jwk:rsa-private-crt-p jwk:rsa-private-crt-q jwk:rsa-private-crt-dp
	    jwk:rsa-private-crt-dq jwk:rsa-private-crt-qi
	    ;; oct
	    jwk:oct? make-jwk:oct jwk-oct-k)
    (import (rnrs)
	    (text json object-builder)
	    (sagittarius)
	    (rfc jose)
	    (srfi :39))

  (define-record-type jwk
    ;; sort of similar to JOSE header but different record type
    (fields kty use key-ops alg kid x5u x5c x5t x5t-s256))

  (define-record-type jwk-set
    (fields keys))

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
    (parent jwk))
  (define-record-type jwk:rsa-public
    (parent jwk:rsa)
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
  
  (define jwk-builder
    (json-object-builder
     (make-jwk-set
      ("keys"
       (@ list
	  (make-jwk
	   "kty"
	   (? "use" #f)
	   (? "key-ops" #f (@ list))
	   (? "alg" #f)
	   (? "kid" #f)
	   (? "x5u" #f)
	   (? "x5c" #f (@ list))
	   (? "x5t" #f)
	   (? "x5t#S256" #f)))))))

  ;; the RFC should've make separate key material object so that
  ;; we could make our life much easier but no choice. fxxk!!!
  (define (make-jwk-by-type jwk key-param)
    (define (ctr&params jwk key-param)
      (define (decode-b64 s) (base64-url-decode (string->utf8 s)))
      (define (pref k) (hashtable-ref key-param k #f))
      (define (maybe->integer v)
	(and v (bytevector->integer (decode-b64 v))))
      (case (string->symbol (jwk-kty jwk))
	((EC)
	 (let ((crv (hashtable-ref key-param 'crv))
	       (x (decode-b64 (pref 'x)))
	       (y (decode-b64 (pref 'y))))
	   (cond ((pref 'd) =>
		  (lambda (d)
		    (values make-jwk:ec-private crv x y (decode-b64 d))))
		 (else (values make-jwk:ec crv x y)))))
	((RSA)
	 (cond ((pref 'n) =>
		(lambda (n)
		  (values make-jwk:rsa-public
			  (bytevector->integer (decode-b64 n))
			  (bytevector->integer (decode-b64 (pref 'e))))))
	       (else
		(let ((d (bytevector->integer (decode-b64 (pref 'd)))))
		  (cond ((pref 'p) =>
			 (lambda (p)
			   (values make-jwk:rsa-crt-private
				   d
				   (maybe->integer p)
				   (maybe->integer (pref 'q))
				   (maybe->integer (pref 'dp))
				   (maybe->integer (pref 'dq))
				   (maybe->integer (pref 'di))
				   ;; TODO oth
				   )))
			(else (values make-jwk:rsa-private d)))))))
	((oct) (values make-jwk:oct (decode-b64 (pref 'k))))
	(else => (lambda (t) (error 'read-jwk-set "unsupported key type" t)))))
    (let-values (((ctr . param) (ctr&params jwk key-param)))
      (let ((use (jwk-use jwk))
	    (key-ops (jwk-key-ops jwk)))
	(when (and use key-ops)
	  (error 'read-jwk-set "use and key_ops must not be together"))
	(apply ctr (jwk-kty jwk) use key-ops
	       (jwk-alg jwk) (jwk-kid jwk) (jwk-x5u jwk) (jwk-x5c jwk)
	       (jwk-x5t jwk) (jwk-x5t-s256 jwk) param))))
	     
  (define (read-jwk-set port)
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
      (read-object-from-json jwk-builder port parameter-handler)))
    
  (define (json-string->jwk-set json-string)
    (read-jwk-set (open-string-input-port json-string)))

  (define-syntax jwk-serializer
    (syntax-rules ()
      ((_ (key acc ...) ...)
       (json-object-serializer
	(("kty" jwk-kty)
	 (key acc ...) ...
	 (? "use" #f jwk-use)
	 (? "key_ops" #f jwk-key-ops (->))
	 (? "alg" #f jwk-alg)
	 (? "kid" #f jwk-kid)
	 (? "x5u" #f jwk-x5u)
	 (? "x5c" #f jwk-x5c (->))
	 (? "x5t" #f jwk-x5t)
	 (? "x5t#S256" #f jwk-x5t-s256))))))

  (define (bytevector->b64-string bv)
    (utf8->string (base64-url-encode bv)))
  (define jwk:ec-serializer
    (jwk-serializer ("crv" jwk:ec-crv)
		    ("x" jwk:ec-x bytevector->b64-string)
		    ("y" jwk:ec-y bytevector->b64-string)))
  (define jwk:ec-private-serializer
    (jwk-serializer ("crv" jwk:ec-crv)
		    ("x" jwk:ec-x bytevector->b64-string)
		    ("y" jwk:ec-y bytevector->b64-string)
		    ("d" jwk:ec-private-d bytevector->b64-string)))
  ;; RSA
  (define (integer->b64-string i)
    (utf8->string (base64-url-encode (integer->bytevector i))))
  (define jwk:rsa-public-serializer
    (jwk-serializer ("n" jwk:rsa-public-n integer->b64-string)
		    ("e" jwk:rsa-public-e integer->b64-string)))
  (define jwk:rsa-private-serializer
    (jwk-serializer ("d" jwk:rsa-private-d integer->b64-string)))
  (define jwk:rsa-crt-private-serializer
    (jwk-serializer ("d" jwk:rsa-private-d integer->b64-string)
		    ("p" jwk:rsa-crt-private-p integer->b64-string)
		    ("q" jwk:rsa-crt-private-q integer->b64-string)
		    ("dp" jwk:rsa-crt-private-dp integer->b64-string)
		    ("dq" jwk:rsa-crt-private-dq integer->b64-string)
		    ("qi" jwk:rsa-crt-private-qi integer->b64-string)))
  ;; oct
  (define jwk:oct-serializer
    (jwk-serializer ("k" jwk:oct-k bytevector->b64-string)))

  (define (dispatch-jwk o)
    (cond ((jwk:ec-private? o) (object->json o jwk:ec-private-serializer))
	  ((jwk:ec? o) (object->json o jwk:ec-serializer))
	  ((jwk:rsa-public? o) (object->json o jwk:rsa-public-serializer))
	  ((jwk:rsa-private? o) (object->json o jwk:rsa-private-serializer))
	  ((jwk:oct? o) (object->json o jwk:oct-serializer))
	  (else (error 'jwk-set-serializer "unknown object" o))))
  (define jwk-set-serializer
    (json-object-serializer
     (("keys" jwk-set-keys (-> dispatch-jwk)))))

  (define (jwk-set->json-string jwk-set)
    (object->json-string jwk-set jwk-set-serializer))
    
  )
