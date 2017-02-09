;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; crypto/key/ecdsa.scm - ECDSA cipher and so
;;;
;;;  Copyright (c) 2010-2017 Takashi Kato. All rights reserved.
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; references:
;;  - https://tools.ietf.org/html/rfc5915
;;  - https://tools.ietf.org/html/rfc5480
;;  - https://tools.ietf.org/html/rfc6090
(library (crypto key ecdsa)
    (export ECDSA
	    <ecdsa-private-key>
	    <ecdsa-public-key>

	    ;; NIST parameters
	    NIST-P-192
	    NIST-P-224
	    NIST-P-256
	    NIST-P-384
	    NIST-P-521
	              
	    NIST-K-163
	    NIST-K-233
	    NIST-K-283
	    NIST-K-409
	    NIST-K-571
	              
	    NIST-B-163
	    NIST-B-233
	    NIST-B-283
	    NIST-B-409
	    NIST-B-571

	    ;; SEC 2 parameters
	    sect113r1
	    
	    secp192r1 ;; the same as NIST-P-*
	    secp224r1
	    secp256r1
	    secp384r1
	    secp521r1
	             
	    sect163k1 ;; the same as NIST-K-*
	    sect233k1
	    sect283k1
	    sect409k1
	    sect571k1
	             
	    sect163r2 ;; the same as NIST-B-*
	    sect233r1
	    sect283r1
	    sect409r1
	    sect571r1
	    )
    (import (rnrs)
	    (sagittarius)
	    (math)
	    (math ec)
	    (clos user)
	    (sagittarius crypto)
	    (crypto key pair)
	    (asn.1)
	    (util bytevector))
  (define ECDSA :ecdsa)

  (define-class <ecdsa-private-key> (<private-key>)
    ((d :init-keyword :d)		   ;; private key
     (parameter :init-keyword :parameter :init-keyword #f) ;; domain parameter
     (public-key :init-keyword :public-key :init-value #f))) ;; public key
  (define-class <ecdsa-public-key> (<public-key>)
    ((Q :init-keyword :Q)
     (parameter :init-keyword :parameter :init-keyword #f))) ;; domain parameter

  (define (read-random-bits prng nbits)
    (bytevector->uinteger (read-random-bytes prng (div nbits 8))))
  (define-method generate-key-pair ((m (eql ECDSA))
				    :key (ec-parameter secp256r1)
					 (prng (secure-random RC4)))
    (let* ((n (ec-parameter-n ec-parameter))
	   (nbits (bitwise-length n))
	   (G (ec-parameter-g ec-parameter))
	   (curve (ec-parameter-curve ec-parameter)))
      (do ((d (read-random-bits prng nbits) (read-random-bits prng nbits)))
	  ((and (> d 2) (< d n))
	   (let ((pub (make <ecdsa-public-key> :Q (ec-point-mul curve G d))))
	     (make-keypair (make <ecdsa-private-key> :d d
				 :parameter ec-parameter :public-key pub)
			   pub))))))

  (define-method generate-private-key ((m (eql ECDSA)) d
				       :optional (parameter secp256r1)
						 (publick-key #f))
    (make <ecdsa-private-key> :d d :parameter parameter
	  :public-key publick-key))

  (define-method generate-public-key ((m (eql ECDSA)) x y
				      :optional (parameter secp256r1))
    (make <ecdsa-public-key> :Q (make-ec-point x y) :parameter parameter))
  
  (define (random-k-generator prng)
    (lambda (n d)
      (let ((bits (bitwise-length n)))
	(do ((r (read-random-bits prng bits) (read-random-bits prng bits)))
	    ((and (not (zero? r)) (< r n)) r)))))

  ;; RFC 6979
  (define (determistic-k-generator digest message)
    (error 'determistic-k-generator "not supported yet"))

  (define (compute-e digest n bv)
    (define M (hash digest bv))
    (let ((len (bitwise-length n))
	  (M-bits (* (bytevector-length M) 8)))
      (let ((e (bytevector->uinteger M)))
	(if (< len M-bits)
	    (bitwise-arithmetic-shift-right e (- M-bits len))
	    e))))
  
  (define (ecdsa-sign bv key
		      :key (k-generator
			    (random-k-generator (secure-random RC4)))
			   (der-encode #t)
			   (digest :hash SHA-1))
    (define (compute-r ec n d)
      (define G (ec-parameter-g ec))
      (define curve (ec-parameter-curve ec))
      (let loop ()
	(let* ((k (k-generator n d))
	       (p (ec-point-mul curve G k))
	       (r (mod (ec-point-x p) n)))
	  (if (zero? r)
	      (loop)
	      (values r k)))))
    (define (compute-s r k e d n) (mod (* (mod-inverse k n) (+ e (* d r))) n))
    (let* ((ec-param (slot-ref key 'parameter))
	   (n (ec-parameter-n ec-param))
	   (e (compute-e digest n bv))
	   (d (slot-ref key 'd)))
      (let loop ()
	(let-values (((r k) (compute-r ec-param n d)))
	  (let ((s (compute-s r k e d n)))
	    (cond ((zero? s) (loop))
		  (der-encode (encode (make-der-sequence
				       (make-der-integer r)
				       (make-der-integer s))))
		  (else
		   (let ((size (ceiling (/ (bitwise-length n) 8))))
		     (bytevector-append (integer->bytevector r size)
					(integer->bytevector s size))))))))))
  
  (define (ecdsa-verify M S key
			:key (der-encode #t)
			     (digest :hash SHA-1))
    ;; FIXME this is almost copy&paste...
    (define (parse-r&s S n)
      (if der-encode
	  (let ((r&s (read-asn.1-object (open-bytevector-input-port S))))
	    (unless (and (is-a? r&s <asn.1-sequence>)
			 (= 2 (length (slot-ref r&s 'sequence))))
	      (error 'ecdsa-verify "invalid signature"))
	    (values (der-integer->integer (car (slot-ref r&s 'sequence)))
		    (der-integer->integer (cadr (slot-ref r&s 'sequence)))))
	  (let ((size  (ceiling (/ (bitwise-length n) 8))))
	    (let*-values (((r s) (bytevector-split-at* S size)))
	      (values (bytevector->integer r) (bytevector->integer s))))))
    (unless (is-a? key <ecdsa-public-key>) (error 'ecdsa-verify "invalid key"))
    (let* ((ec (slot-ref key 'parameter))
	   (n (ec-parameter-n ec))
	   (e (compute-e digest n M)))
      (let-values (((r s) (parse-r&s S n)))
	(when (or (< r 1) (< n r)  ;; r in range [1, n-1]
		  (< s 1) (< n s)) ;; s in range [1, n-1]
	  (error 'ecdsa-verify "inconsistent"))
	(let* ((w  (mod-inverse s n))
	       (u1 (mod (* e w) n))
	       (u2 (mod (* r w) n))
	       (G (ec-parameter-g ec))
	       (Q (slot-ref key 'Q))
	       (curve (ec-parameter-curve ec)))
	  (let ((point (ec-point-add curve
				     (ec-point-mul curve G u1)
				     (ec-point-mul curve Q u2))))
	    (or (= (mod (ec-point-x point) n) (mod r n))
		(error 'ecdsa-verify "inconsistent")))))))
  
  (define-class <ecdsa-cipher-spi> (<cipher-spi>) ())
  (define-method initialize ((o <ecdsa-cipher-spi>) initargs)
    (let ((key (car initargs)))
      (slot-set! o 'name 'ECDSA)
      (slot-set! o 'key key)
      (slot-set! o 'encrypt 
		 (lambda ignore (error 'encrypt "not supported in DSA")))
      (slot-set! o 'decrypt
		 (lambda ignore (error 'decrypt "not supported in DSA")))
      (slot-set! o 'padder #f)
      (slot-set! o 'signer ecdsa-sign)
      (slot-set! o 'verifier ecdsa-verify)
      (slot-set! o 'keysize #f)))
  (register-spi ECDSA <ecdsa-cipher-spi>)

  ;; key export/import
  (define (lookup-named-curve-parameter oid)
    (lookup-ec-parameter (slot-ref oid 'identifier)))
  #|
     SubjectPublicKeyInfo  ::=  SEQUENCE  {
       algorithm         AlgorithmIdentifier,
       subjectPublicKey  BIT STRING
     }

      AlgorithmIdentifier  ::=  SEQUENCE  {
        algorithm   OBJECT IDENTIFIER,
        parameters  ANY DEFINED BY algorithm OPTIONAL
      }
      parameters must be ECParameters
  |#
  (define id-ec-public-key (make-der-object-identifier "1.2.840.10045.2.1"))
  (define-method export-public-key ((m (eql ECDSA)) (key <ecdsa-public-key>))
    (define param (slot-ref key 'parameter))
    (define curve (ec-parameter-curve param))
    (unless curve
      (assertion-violation 'export-public-key
			   "No EC parameter is set for the key"))
    (let ((encoded (encode-ec-point curve (slot-ref key 'Q))))
      (asn.1-encode
       (make-der-sequence
	(make-der-sequence
	 id-ec-public-key
	 (if (ec-parameter-oid param)
	     (make-der-object-identifier (ec-parameter-oid param))
	     (ec-parameter->asn.1-object param)))
	(make-der-bit-string encoded)))))
  
  (define-method export-public-key ((key <ecdsa-public-key>))
    (export-public-key ECDSA key))

  (define-method import-public-key ((marker (eql ECDSA)) (in <bytevector>))
    (import-public-key ECDSA (open-bytevector-input-port in)))
  (define-method import-public-key ((marker (eql ECDSA)) (in <port>))
    (import-public-key ECDSA (read-asn.1-object in)))

  (define id-prime-field (make-der-object-identifier "1.2.840.10045.1.1"))
  (define id-f2m-field (make-der-object-identifier "1.2.840.10045.1.2"))
  #|
      ECParameters ::= SEQUENCE {
        version INTEGER { ecpVer1(1) } (ecpVer1),
        fieldID FieldID {{FieldTypes}},
        curve   Curve,
        base    ECPoint,
        order   INTEGER,
        cofactor INTEGER OPTIONAL,
        ...
      }
      Curve ::= SEQUENCE {
        a FieldElement,
        b FieldElement,
        seed BIT STRING OPTIONAL
      }
  |#
  (define (->ec-parameter p)
    (define objs (slot-ref p 'sequence))
    (define (parse-field-id field-id)
      (let ((objs (slot-ref field-id 'sequence)))
	(values (car objs) (cadr objs))))
    (define (parse-curve curve)
      (let ((obj (slot-ref curve 'sequence)))
	(values (bytevector->uinteger (slot-ref (car obj) 'string))
		(bytevector->uinteger (slot-ref (cadr obj) 'string))
		(if (= (length obj) 3)
		    (slot-ref (caddr obj) 'data)
		    #vu8()))))
    (define (make-curve field-type field-param a b)
      (define (parse-f2m-parameter f2m)
	(define param (caddr f2m))
	;; lazy way. 
	(cond ((is-a? param <der-null>) (values 0 0 0))
	      ((is-a? param <der-integer>)
	       (values (der-integer->integer param) 0 0))
	      ((is-a? param <der-sequence>)
	       (apply values
		      (map der-integer->integer
			   (slot-ref param 'sequence))))))
      (cond ((equal? id-prime-field field-type)
	     (make-elliptic-curve (make-ec-field-fp
				   (der-integer->integer field-param)) a b))
	    ((equal? id-f2m-field field-type)
	     (let* ((f2m (slot-ref field-param 'sequence))
		    (m (der-integer->integer (car f2m))))
	       (let-values (((k1 k2 k3) (parse-f2m-parameter f2m)))
		 (make-elliptic-curve (make-ec-field-f2m m k1 k2 k3) a b))))
	    (else
	     (error 'import-public-key "unknown field type" field-type))))

    (let-values (((field-type field-param) (parse-field-id (cadr objs)))
		 ((a b S) (parse-curve (caddr objs))))
      (let* ((Gxy (cadddr objs))
	     (n (der-integer->integer (car (cddddr objs))))
	     (h (der-integer->integer (cadr (cddddr objs))))
	     (curve (make-curve field-type field-param a b))
	     (base (decode-ec-point curve (slot-ref Gxy 'string))))
	(make-ec-parameter curve base n h S))))

  (define (ec-parameter->asn.1-object ep)
    (define curve (ec-parameter-curve ep))
    (define field (elliptic-curve-field curve))
    (define (make-asn.1-curve curve)
      (define (uinteger->der-octet-string a)
	(make-der-octet-string (uinteger->bytevector a)))
      (let ((base (make-der-sequence
		   (uinteger->der-octet-string (elliptic-curve-a curve))
		   (uinteger->der-octet-string (elliptic-curve-b curve)))))
	(and-let* ((S (ec-parameter-seed ep)))
	  (asn.1-sequence-add base (make-der-bit-string S)))
	base))
    (define (make-asn.1-field field)
      (if (ec-field-fp? field)
	  (let ((p (ec-field-fp-p field)))
	    (make-der-sequence id-prime-field (make-der-integer p)))
	  (let ((m (ec-field-f2m-m field))
		(k1 (ec-field-f2m-k1 field))
		(k2 (ec-field-f2m-k2 field))
		(k3 (ec-field-f2m-k3 field))
		(param (make-der-sequence (make-der-integer m)))
		(base (make-der-sequence id-f2m-field param)))
	    (cond ((and (zero? k1) (zero? k2) (zero? k3))
		   (asn.1-sequence-add param
		     (make-der-object-identifier "1.2.840.10045.1.2.3.1"))
		   (asn.1-sequence-add param (make-der-null)))
		  ((and (zero? k2) (zero? k3))
		   (asn.1-sequence-add param
		     (make-der-object-identifier "1.2.840.10045.1.2.3.2"))
		   (asn.1-sequence-add param (make-der-integer k1)))
		  (else
		   (asn.1-sequence-add param
		     (make-der-object-identifier "1.2.840.10045.1.2.3.3"))
		   (asn.1-sequence-add param 
		    (make-der-sequence
		     (make-der-integer k1)
		     (make-der-integer k2)
		     (make-der-integer k3)))))
	    base)))
    (make-der-sequence
     (make-der-integer 1)
     (make-asn.1-field field)
     (make-asn.1-curve curve)
     (make-der-octet-string (encode-ec-point curve (ec-parameter-g ep)))
     (make-der-integer (ec-parameter-n ep))
     (make-der-integer (ec-parameter-h ep))))
  
  (define-method import-public-key ((marker (eql ECDSA)) (in <asn.1-sequence>))
    (let ((objs (slot-ref in 'sequence)))
      (unless (= (length objs) 2)
	(assertion-violation 'import-private-key "invalid sequence size" in))
      (unless (and (is-a? (car objs) <asn.1-sequence>)
		   (is-a? (cadr objs) <der-bit-string>))
	(assertion-violation who "bad component" in))
      (let* ((aid (car objs))
	     (spk (cadr objs))
	     (ec-param (cadr (slot-ref aid 'sequence)))
	     (p (if (is-a? ec-param <der-object-identifier>)
		    (lookup-named-curve-parameter ec-param)
		    (->ec-parameter ec-param)))
	     (Q (decode-ec-point (ec-parameter-curve p) (slot-ref spk 'data))))
	(make <ecdsa-public-key> :Q Q :parameter p))))
  
  #|
     ECParameters ::= CHOICE {
       namedCurve         OBJECT IDENTIFIER
       -- implicitCurve   NULL
       -- specifiedCurve  SpecifiedECDomain
     }
  |#
  #|
   ECPrivateKey ::= SEQUENCE {
     version        INTEGER { ecPrivkeyVer1(1) } (ecPrivkeyVer1),
     privateKey     OCTET STRING,
     parameters [0] ECParameters {{ NamedCurve }} OPTIONAL,
     publicKey  [1] BIT STRING OPTIONAL
   }
  |#
  (define-method export-private-key ((m (eql ECDSA)) (key <ecdsa-private-key>))
    (define param (slot-ref key 'parameter))
    (define curve (ec-parameter-curve param))
    (define oid (and param (ec-parameter-oid param)))
    (define pub (slot-ref key 'public-key))
    (asn.1-encode
     (apply make-der-sequence
      (cons* (make-der-integer 1)
	     (make-der-octet-string (integer->bytevector (slot-ref key 'd)))
	     (make-der-tagged-object #t 0
	      (if oid
		  (make-der-object-identifier oid)
		  (ec-parameter->asn.1-object param)))
	     (if (and pub (slot-ref pub 'parameter))
		 (list
		  (make-der-tagged-object #t 1
		   (make-der-bit-string
		    (encode-ec-point curve (slot-ref pub 'Q)))))
		 '())))))
      
  (define-method export-private-key ((key <ecdsa-private-key>))
    (export-private-key ECDSA key))

  (define-method import-private-key ((marker (eql ECDSA)) (in <bytevector>))
    (import-private-key ECDSA (open-bytevector-input-port in)))
  (define-method import-private-key ((marker (eql ECDSA)) (in <port>))
    (import-private-key ECDSA (read-asn.1-object in)))
  (define-method import-private-key ((marker (eql ECDSA)) (in <asn.1-sequence>))
    (define (find-tag objs n)
      (exists (lambda (obj)
		(and (is-a? obj <asn.1-tagged-object>)
		     (= (slot-ref obj 'tag-no) n)
		     (slot-ref obj 'obj))) objs))
    (let ((objs (slot-ref in 'sequence)))
      (when (< (length objs) 3) 
	(assertion-violation 'import-private-key "invalid sequence size" in))
      (unless (= 1 (der-integer->integer (car objs)))
	(assertion-violation 'import-private-key "invalid version"))
      (let* ((tag0 (find-tag (cddr objs) 0))
	     (param (and tag0
			 (if (is-a? tag0 <der-object-identifier>)
			     (lookup-named-curve-parameter tag0)
			     (->ec-parameter tag0)))))
	(make <ecdsa-private-key>
	  :d (bytevector->uinteger (der-octet-string-octets (cadr objs)))
	  :parameter param
	  :public-key (and-let* (( param )
				 (p (find-tag (cddr objs) 1)))
			(make <ecdsa-public-key>
			  :Q (decode-ec-point (ec-parameter-curve param)
					      (slot-ref p 'data))
			  :parameter param))))))
  
  )
