;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/keys/operations/asymmetric/ecdsa.scm - ECDSA key op
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
(library (sagittarius crypto keys operations asymmetric ecdsa)
    (export generate-key-pair
	    generate-public-key
	    generate-private-key
	    import-public-key
	    import-private-key
	    export-public-key
	    export-private-key

	    *key:ecdsa*
	    ecdsa-key-parameter
	    ecdsa-public-key? <ecdsa-public-key>
	    ecdsa-public-key-Q
	    ecdsa-private-key? <ecdsa-private-key>
	    ecdsa-private-key-d ecdsa-private-key-public-key

	    ec-parameter?
	    (rename (NIST-P-192 *ec-parameter:p192*)
		    (NIST-P-224 *ec-parameter:p224*)
		    (NIST-P-256 *ec-parameter:p256*)
		    (NIST-P-384 *ec-parameter:p384*)
		    (NIST-P-521 *ec-parameter:p521*)
		    (NIST-K-163 *ec-parameter:k163*)
		    (NIST-K-233 *ec-parameter:k233*)
		    (NIST-K-283 *ec-parameter:k283*)
		    (NIST-K-409 *ec-parameter:k409*)
		    (NIST-K-571 *ec-parameter:k571*)
		    (NIST-B-163 *ec-parameter:b163*)
		    (NIST-B-233 *ec-parameter:b233*)
		    (NIST-B-283 *ec-parameter:b283*)
		    (NIST-B-409 *ec-parameter:b409*)
		    (NIST-B-571 *ec-parameter:b571*)
		    (secp192r1 *ec-parameter:secp192r1*)
		    (secp224r1 *ec-parameter:secp224r1*)
		    (secp256r1 *ec-parameter:secp256r1*)
		    (secp384r1 *ec-parameter:secp384r1*)
		    (secp521r1 *ec-parameter:secp521r1*)
		    (sect163k1 *ec-parameter:sect163k1*)
		    (sect233k1 *ec-parameter:sect233k1*)
		    (sect283k1 *ec-parameter:sect283k1*)
		    (sect409k1 *ec-parameter:sect409k1*)
		    (sect571k1 *ec-parameter:sect571k1*)
		    (sect163r2 *ec-parameter:sect163r2*)
		    (sect233r1 *ec-parameter:sect233r1*)
		    (sect283r1 *ec-parameter:sect283r1*)
		    (sect409r1 *ec-parameter:sect409r1*)		    
		    (sect571r1 *ec-parameter:sect571r1*)
		    (secp192k1 *ec-parameter:secp192k1*)
		    (secp224k1 *ec-parameter:secp224k1*)
		    (secp256k1 *ec-parameter:secp256k1*)
		    (sect163r1 *ec-parameter:sect163r1*)
		    (sect239k1 *ec-parameter:sect239k1*)
		    (sect113r1 *ec-parameter:sect113r1*)
		    (brainpool-p160r1 *ec-parameter:brainpool-p160r1*)
		    (brainpool-p160r1 *ec-parameter:brainpool-p160t1*)
		    (brainpool-p192r1 *ec-parameter:brainpool-p192r1*)
		    (brainpool-p192r1 *ec-parameter:brainpool-p192t1*)
		    (brainpool-p224r1 *ec-parameter:brainpool-p224r1*)
		    (brainpool-p224r1 *ec-parameter:brainpool-p224t1*)
		    (brainpool-p256r1 *ec-parameter:brainpool-p256r1*)
		    (brainpool-p256r1 *ec-parameter:brainpool-p256t1*)
		    (brainpool-p320r1 *ec-parameter:brainpool-p320r1*)
		    (brainpool-p320t1 *ec-parameter:brainpool-p320t1*)
		    (brainpool-p384r1 *ec-parameter:brainpool-p384r1*)
		    (brainpool-p384t1 *ec-parameter:brainpool-p384t1*)
		    (brainpool-p512r1 *ec-parameter:brainpool-p512r1*)
		    (brainpool-p512t1 *ec-parameter:brainpool-p512t1*)))
    (import (rnrs)
	    (clos user)
	    (srfi :117 list-queues)
	    (sagittarius)
	    (sagittarius mop immutable)
	    (sagittarius crypto asn1)
	    (sagittarius crypto keys types)
	    (sagittarius crypto keys operations asymmetric apis)
	    (sagittarius crypto digests)
	    (sagittarius crypto random)
	    (sagittarius crypto math prime)
	    (sagittarius crypto math ec)
	    (sagittarius crypto math modular))

(define *key:ecdsa* :ecdsa)

(define-class <ecdsa-key-parameter-holder> (<immutable>)
  ((parameter :init-keyword :parameter :reader ecdsa-key-parameter)))
(define-class <ecdsa-private-key> (<private-key> <ecdsa-key-parameter-holder>)
  ((d :init-keyword :d :reader ecdsa-private-key-d)
   (public-key :init-keyword :public-key :reader ecdsa-private-key-public-key)))
(define (ecdsa-private-key? o) (is-a? o <ecdsa-private-key>))

(define-class <ecdsa-public-key> (<public-key> <ecdsa-key-parameter-holder>)
  ((Q :init-keyword :Q :reader ecdsa-public-key-Q)))
(define (ecdsa-public-key? o) (is-a? o <ecdsa-public-key>))

(define (put-indent out indent)
  (do ((i 0 (+ i 1)))
      ((= i indent))
    (display #\space out)))
(define (describe-ec-point point out indent)
  (put-indent out indent) (format out "x: ~x~%" (ec-point-x point))
  (put-indent out indent) (format out "y: ~x~%" (ec-point-y point)))
(define (describe-ec-field field out indent)
  (cond
   ((ec-field-fp? field)
    (put-indent out indent) (format out "type: fp~%")
    (put-indent out indent) (format out "   p: ~x~%" (ec-field-fp-p field)))
   ((ec-field-f2m? field)
    (put-indent out indent) (format out "type: f2m~%")
    (put-indent out indent) (format out "   m: ~x~%" (ec-field-f2m-m field))
    (put-indent out indent) (format out "  k1: ~x~%" (ec-field-f2m-k1 field))
    (put-indent out indent) (format out "  k2: ~x~%" (ec-field-f2m-k2 field))
    (put-indent out indent) (format out "  k3: ~x~%" (ec-field-f2m-k3 field)))
   (else
    (put-indent out indent) (format out " type: unknown~%")
    (put-indent out indent) (format out "value: ~x~%" field))))
(define (describe-ec-curve curve out indent)
  (let ((field (elliptic-curve-field curve))
	(a (elliptic-curve-a curve))
	(b (elliptic-curve-b curve)))
    (put-indent out indent) (format out "field:~%")
    (describe-ec-field field out (+ 3 indent))
    (put-indent out indent) (format out "    a: ~x~%" a)
    (put-indent out indent) (format out "    b: ~x~%" b)))
(define (describe-ec-parameter p out indent)
  (put-indent out indent) (format out "  oid: ~a~%" (ec-parameter-oid p))
  (put-indent out indent) (format out "curve:~%")
  (describe-ec-curve (ec-parameter-curve p) out (+ indent 1))
  (put-indent out indent) (format out "    g:~%")
  (describe-ec-point (ec-parameter-g p) out (+ indent 5))
  (put-indent out indent) (format out "    n: ~a~%" (ec-parameter-n p))
  (put-indent out indent) (format out "    h: ~a~%" (ec-parameter-h p))
  (put-indent out indent) (format out " seed: ~a~%" (ec-parameter-seed p)))

(define-method write-object ((o <ecdsa-public-key>) (p <port>))
  (let-values (((out e) (open-string-output-port)))
    (format out "#<ecdsa-public-key~%")
    (format out "           Q:~%") (describe-ec-point (slot-ref o 'Q) out 12)
    (format out "   parameter:~%")
    (describe-ec-parameter (slot-ref o 'parameter) out 8)
    (display #\> out)
    (display (e) p)))


(define (read-random-bits prng nbits)
  (bytevector->uinteger
   (random-generator-read-random-bytes prng (div nbits 8))))

(define-method generate-key-pair ((m (eql *key:ecdsa*))
				  :key (ec-parameter secp256r1)
				       (prng (secure-random-generator *prng:chacha20*))
				  :allow-other-keys)
  (let* ((n (ec-parameter-n ec-parameter))
	 (nbits (bitwise-length n))
	 (G (ec-parameter-g ec-parameter))
	 (curve (ec-parameter-curve ec-parameter)))
    (do ((d (read-random-bits prng nbits) (read-random-bits prng nbits)))
	((and (> d 2) (< d n))
	 (let ((pub (make <ecdsa-public-key> :Q (ec-point-mul curve G d)
			  :parameter ec-parameter)))
	   (make-key-pair (make <ecdsa-private-key> :d d
				:parameter ec-parameter :public-key pub)
			  pub))))))

(define (generate-ecdsa-private-key (d integer?)
				    (parameter ec-parameter?)
				    (public-key (or #f ecdsa-public-key?)))
  (make <ecdsa-private-key> :d d :parameter parameter
	:public-key public-key))
(define-method generate-private-key ((m (eql *key:ecdsa*)) d
				     :optional (parameter secp256r1)
					       (public-key #f))
  (generate-ecdsa-private-key d parameter public-key))

(define (generate-ecdsa-public-key (x integer?) (y integer?)
				   (parameter ec-parameter?))
  (make <ecdsa-public-key> :Q (make-ec-point x y) :parameter parameter))
(define-method generate-public-key ((m (eql *key:ecdsa*)) x y
				    :optional (parameter secp256r1))
  (generate-ecdsa-public-key x y parameter))


(define *ecdsa-key-oid* "1.2.840.10045.2.1")
(define-method oid->key-operation ((oid (equal *ecdsa-key-oid*))) *key:ecdsa*)

;; TODO should be move to (sagittarius crypto asn1)
(define (parse-seq (seq der-sequence?))
  (apply values (list-queue-list (asn1-collection-elements seq))))

;; NOTE ECDSA public key == ECPoint
;; ECPoint ::= OCTET STRING
(define (ecdsa-import-raw-public-key (key bytevector?)
				     (ec-parameter ec-parameter?))
  (let ((Q (decode-ec-point (ec-parameter-curve ec-parameter) key)))
    (make <ecdsa-public-key> :Q Q :parameter ec-parameter)))

(define (ecdsa-import-spki-public-key (public-key der-sequence?))
  (let*-values (((aid key) (parse-seq public-key))
		((oid param) (parse-seq aid)))
    (unless (der-bit-string? key)
      (assertion-violation 'ecdsa-import-spki-public-key
			   "Invalid SubjectPublicKeyInfo format" public-key))
    (let ((p (if (der-object-identifier? param)
		 (lookup-named-curve-parameter param)
		 (->ec-parameter param))))
      (ecdsa-import-raw-public-key (der-bit-string->bytevector key) p))))

(define-method import-public-key ((m (eql *key:ecdsa*)) (in <port>) . opts)
  (apply import-public-key m (get-bytevector-all in) opts))
(define-method import-public-key ((m (eql *key:ecdsa*)) (in <bytevector>)
				  :optional (format (public-key-format subject-public-key-info))
					    (ec-parameter #f))
  (case format
    ((raw) (ecdsa-import-raw-public-key in ec-parameter))
    ((subject-public-key-info)
     (ecdsa-import-spki-public-key (bytevector->asn1-object in)))
    (else (assertion-violation 'import-public-key
			       "Unknown public key format" format))))
(define-method import-public-key ((m (eql *key:ecdsa*)) (in <der-sequence>)
				  . ignore)
  (ecdsa-import-spki-public-key in))

(define-method export-public-key ((key <ecdsa-public-key>) . opts)
  (apply export-public-key *key:ecdsa* key opts))

(define (export-raw-ecdsa-public-key (public-key ecdsa-public-key?))
  (encode-ec-point (ec-parameter-curve (ecdsa-key-parameter public-key))
		   (ecdsa-public-key-Q public-key)))

(define (export-spki-ecdsa-public-key (public-key ecdsa-public-key?))
  (define param (ecdsa-key-parameter public-key))
  (let ((raw-public-key (export-raw-ecdsa-public-key public-key)))
    (asn1-encodable->bytevector
     (der-sequence
      (der-sequence (oid-string->der-object-identifier *ecdsa-key-oid*)
		    (cond ((ec-parameter-oid param) =>
			   oid-string->der-object-identifier)
			  (else (ec-parameter->asn1-object param))))
      (bytevector->der-bit-string raw-public-key)))))
(define-method export-public-key ((m (eql *key:ecdsa*)) (key <ecdsa-public-key>)
				  :optional (format (public-key-format subject-public-key-info)))
  (case format
    ((raw) (export-raw-ecdsa-public-key key))
    ((subject-public-key-info) (export-spki-ecdsa-public-key key))
    (else (assertion-violation 'export-public-key
			       "Unknown public key format" format))))

(define-method import-private-key ((m (eql *key:ecdsa*)) (in <bytevector>)
				   . opts)
  (apply import-private-key m (open-bytevector-input-port in) opts))
(define-method import-private-key ((m (eql *key:ecdsa*)) (in <port>)
				   . opts)
  (apply import-private-key m (read-asn1-object in) opts))

;; ECPrivateKey ::= SEQUENCE {
;;   version        INTEGER { ecPrivkeyVer1(1) } (ecPrivkeyVer1),
;;   privateKey     OCTET STRING,
;;   parameters [0] ECParameters {{ NamedCurve }} OPTIONAL,
;;   publicKey  [1] BIT STRING OPTIONAL
;; }
(define-method import-private-key ((m (eql *key:ecdsa*)) (in <der-sequence>)
				   ;; For PKCS#8
				   :optional (ec-parameter #f))
  (let-values (((version private-key . rest)
		(deconstruct-asn1-collection in)))
    (unless (= 1 (der-integer->integer version))
      (assertion-violation 'import-private-key
			   "Invalid ECPrivateKey version" version))
    (let ((param (cond ((asn1-collection-find-tag in 0) =>
			der-tagged-object-obj)
		       (else ec-parameter)))
	  (tag1 (asn1-collection-find-tag in 1)))
      (unless param
	(assertion-violation 'import-private-key "ECParameters not found"))
      (let* ((pub-key (and tag1 (der-tagged-object-obj tag1)))
	     (parameter (cond ((ec-parameter? param) param)
			      ((der-object-identifier? param)
			       (lookup-named-curve-parameter param))
			      (else (->ec-parameter param)))))
	(make <ecdsa-private-key>
	  :d (bytevector->uinteger (der-octet-string->bytevector private-key))
	  :parameter parameter
	  :public-key (and pub-key
			   (import-public-key *key:ecdsa*
			      (der-bit-string->bytevector pub-key)
			      (public-key-format raw)
			      parameter)))))))

(define-method export-private-key ((key <ecdsa-private-key>) . opts)
  (apply export-private-key *key:ecdsa* key opts))

(define-method export-private-key ((m (eql *key:ecdsa*))
				   (key <ecdsa-private-key>) . opts)
  (let* ((param (ecdsa-key-parameter key))
	 (curve (and param (ec-parameter-curve param)))
	 (oid (and param (ec-parameter-oid param)))
	 (pub (ecdsa-private-key-public-key key)))
    (asn1-encodable->bytevector
     (apply der-sequence
	    (integer->der-integer 1)
	    (bytevector->der-octet-string
	     (integer->bytevector (ecdsa-private-key-d key)))
	    (filter values
	     (list (and param
			(make <der-tagged-object>
			  :explicit? #t
			  :tag-no 0
			  :obj (if oid
				   (oid-string->der-object-identifier oid)
				   (ec-parameter->asn1-object param))))
		   (and pub
			(ecdsa-key-parameter pub)
			(make <der-tagged-object>
			  :explicit? #t
			  :tag-no 1
			  :obj (bytevector->der-bit-string
				(export-public-key *key:ecdsa* pub
						   (public-key-format raw)))))))))))

;; EC parameter related, it's not in the scope of PKI
(define (lookup-named-curve-parameter oid)
  (let ((s (der-object-identifier->oid-string oid)))
    (cond ((lookup-ec-parameter s))
	  (else (raise
		 (condition (make-implementation-restriction-violation)
			    (make-who-condition 'lookup-named-curve-parameter)
			    (make-message-condition
			     "Curve with the given OID is not supported")
			    (make-irritants-condition s)))))))

(define id-prime-field "1.2.840.10045.1.1")
(define id-f2m-field   "1.2.840.10045.1.2")
;;; Excerpted from
;;   https://www.itu.int/ITU-T/formal-language/itu-t/x/x894/2018-cor1/ANSI-X9-62.html
;; FIELD-ID ::= TYPE-IDENTIFIER
;; 
;; FieldTypes FIELD-ID ::= {
;;   { Prime-p  IDENTIFIED BY  prime-field } |
;;   { Characteristic-two  IDENTIFIED BY  characteristic-two-field },
;;   ... -- Additional field types may be added
;; }
;; FieldID { FIELD-ID:IOSet } ::= SEQUENCE {-- Finite field
;;   fieldType	FIELD-ID.&id({IOSet}),
;;   parameters	FIELD-ID.&Type({IOSet}{@fieldType}) 
;; }
;; -- Identifying an elliptic curve by its coefficients (and optional seed)
;; Curve ::= SEQUENCE {
;;   a	FieldElement, -- Elliptic curve coefficient a
;;   b	FieldElement, -- Elliptic curve coefficient b
;;   seed	BIT STRING OPTIONAL 
;;   -- Shall be present if used in SpecifiedECDomain with version of
;;   -- ecdpVer2 or ecdpVer3
;; }
;; FieldElement ::= OCTET STRING
;; -- Type used to control version of EC domain parameters
;; SpecifiedECDomainVersion ::= INTEGER {
;;   ecdpVer1(1) , ecdpVer2(2) , ecdpVer3(3) } 
;; -- Identifying elliptic curve domain parameters explicitly with this type
;; SpecifiedECDomain ::= SEQUENCE { 
;;   version	SpecifiedECDomainVersion ( ecdpVer1 | ecdpVer2 | ecdpVer3 ),
;;   fieldID	FieldID {{FieldTypes}},  
;;   curve	Curve,
;;   base	ECPoint, -- Base point G 
;;   order	INTEGER, -- Order n of the base point
;;   cofactor	INTEGER OPTIONAL, -- The integer h = #E(Fq)/n
;;   hash	HashAlgorithm OPTIONAL,
;;   ... -- Additional parameters may be added
;; }

;; Handling SpecifiedECDomain, very lazily
(define (->ec-parameter param)
  (define (parse-curve curve)
    (let-values (((a b . rest) (parse-seq curve)))
      (values (bytevector->uinteger (der-octet-string->bytevector a))
	      (bytevector->uinteger (der-octet-string->bytevector b))
	      (if (null? rest)
		  #vu8()
		  (der-bit-string->bytevector (car rest))))))
  ;; -- Integer k for reduction polynomial x^m + x^k + 1 
  ;; Trinomial ::= INTEGER 
  ;; Pentanomial ::= SEQUENCE {  
  ;;   k1	INTEGER,
  ;;   k2	INTEGER,
  ;;   k3	INTEGER
  ;; }
  ;; -- The object class for binary field basis types
  ;; CHARACTERISTIC-TWO ::= TYPE-IDENTIFIER
  ;; -- Allowable basis types are given the following info object set
  ;; BasisTypes CHARACTERISTIC-TWO ::= {
  ;; { NULL  		IDENTIFIED BY  gnBasis } |
  ;; { Trinomial  	IDENTIFIED BY  tpBasis } |
  ;; { Pentanomial	IDENTIFIED BY  ppBasis },
  ;; ... -- Additional basis types may be added
  ;; }
  ;; -- Parameters for a binary field
  ;; Characteristic-two ::= SEQUENCE {
  ;;   m		INTEGER, -- Field size is 2^m
  ;;   basis		CHARACTERISTIC-TWO.&id({BasisTypes}),
  ;;   parameters	CHARACTERISTIC-TWO.&Type({BasisTypes}{@basis}) 
  ;; }
  (define (parse-f2m-parameters f2m)
    (let-values (((m basis parameters) (parse-seq f2m)))
      (let ((m (der-integer->integer m)))
	;; We ignore basis, it has to be one of the following OID
	;; - 1.2.840.10045.1.2.3.1
	;; - 1.2.840.10045.1.2.3.2
	;; - 1.2.840.10045.1.2.3.3
	;; But I'm lazy to check
	(cond ((der-null? parameters) (values m 0 0 0))
	      ((der-integer? parameters)
	       (values m (der-integer->integer parameters) 0 0))
	      ((der-sequence? parameters)
	       (apply values m
		      (map der-integer->integer
			   (list-queue-list
			    (asn1-collection-elements parameters)))))
	      (else
	       (assertion-violation '->ec-parameter
				    "Invalid SpecifiedECDomain format"))))))
  (define (make-curve (field-type der-object-identifier?) field-param a b)
    (define oid (der-object-identifier->oid-string field-type))
    (cond ((equal? oid id-prime-field)
	   (make-elliptic-curve (make-ec-field-fp
				 (der-integer->integer field-param)) a b))
	  ((equal? oid id-f2m-field)
	   (let-values (((m k1 k2 k3) (parse-f2m-parameters field-param)))
	     (make-elliptic-curve (make-ec-field-f2m m k1 k2 k3) a b)))
	  (else
	   (assertion-violation '->ec-parameter
				"Unknown field type" field-type))))
  (let*-values (((version field-id curve base order cofactor . rest)
		 (parse-seq param))
		((field-type field-param) (parse-seq field-id))
		((a b S) (parse-curve curve)))
    (let ((n (der-integer->integer order))
	  (h (der-integer->integer cofactor))
	  (curve (make-curve field-type field-param a b)))
      (make-ec-parameter curve
			 (decode-ec-point curve
					  (der-octet-string->bytevector base))
			 n h S))))

(define (ec-parameter->asn1-object (ep ec-parameter?))
  (define (make-asn1-field field)
    (if (ec-field-fp? field)
	(let ((p (ec-field-fp-p field)))
	  (der-sequence (oid-string->der-object-identifier id-prime-field)
			(integer->der-integer p)))
	(let* ((m (ec-field-f2m-m field))
	       (k1 (ec-field-f2m-k1 field))
	       (k2 (ec-field-f2m-k2 field))
	       (k3 (ec-field-f2m-k3 field)))
	  (der-sequence
	   (oid-string->der-object-identifier id-f2m-field)
	   (cond ((and (zero? k1) (zero? k2) (zero? k3))
		  (der-sequence
		   (integer->der-integer m)
		   (oid-string->der-object-identifier "1.2.840.10045.1.2.3.1")
		   (make-der-null)))
		 ((and (zero? k2) (zero? k3))
		  (der-sequence
		   (integer->der-integer m)
		   (oid-string->der-object-identifier "1.2.840.10045.1.2.3.2")
		   (integer->der-integer k1)))
		 (else
		  (der-sequence
		   (integer->der-integer m)
		   (oid-string->der-object-identifier "1.2.840.10045.1.2.3.3")
		   (der-sequence
		    (integer->der-integer k1)
		    (integer->der-integer k2)
		    (integer->der-integer k3)))))))))
  (define (make-asn1-curve curve ep)
    (define (uinteger->der-octet-string ui)
      (bytevector->der-octet-string (uinteger->bytevector ui)))
    (apply der-sequence
	   (uinteger->der-octet-string (elliptic-curve-a curve))
	   (uinteger->der-octet-string (elliptic-curve-b curve))
	   (let ((S (ec-parameter-seed ep)))
	     (if (or (not S) (zero? (bytevector-length S)))
		 '()
		 (list (bytevector->der-bit-string S))))))
  (let* ((curve (ec-parameter-curve ep))
	 (g (encode-ec-point curve (ec-parameter-g ep))))
    (der-sequence
     (integer->der-integer 1)
     (make-asn1-field (elliptic-curve-field curve))
     (make-asn1-curve curve ep)
     (bytevector->der-octet-string g)
     (integer->der-integer (ec-parameter-n ep))
     (integer->der-integer (ec-parameter-h ep)))))
)
