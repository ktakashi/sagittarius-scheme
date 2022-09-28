;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/keys/operations/asymmetric/dsa.scm - DSA key op
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
(library (sagittarius crypto keys operations asymmetric dsa)
    (export generate-key-pair
	    generate-public-key
	    generate-private-key
	    import-public-key
	    import-private-key
	    export-public-key
	    export-private-key

	    *key:dsa*
	    dsa-key-parameter? <dsa-key-parameter>
	    dsa-key-parameter-p
	    dsa-key-parameter-q
	    dsa-key-parameter-g
	    
	    dsa-public-key? <dsa-public-key>
	    dsa-public-key-Y

	    dsa-private-key? <dsa-private-key>
	    dsa-private-key-Y dsa-private-key-X

	    generate-dsa-parameter)
    (import (rnrs)
	    (clos user)
	    (math modular)
	    (srfi :117 list-queues)
	    (sagittarius)
	    (sagittarius mop immutable)
	    (sagittarius crypto asn1)
	    (sagittarius crypto keys types)
	    (sagittarius crypto keys operations asymmetric apis)
	    (sagittarius crypto digests)
	    (sagittarius crypto random)
	    (sagittarius crypto math prime))

(define *key:dsa* :dsa)

(define-class <dsa-key-parameter> (<immutable> <asn1-encodable>)
  ((p :init-keyword :p :reader dsa-key-parameter-p)
   (q :init-keyword :p :reader dsa-key-parameter-q)
   (g :init-keyword :p :reader dsa-key-parameter-g)))
(define (dsa-key-parameter? o) (is-a? o <dsa-key-parameter>))
(define-method asn1-encodable->asn1-object ((o <dsa-key-parameter>) type)
  (der-sequence (integer->der-integer (dsa-key-parameter-p o))
		(integer->der-integer (dsa-key-parameter-q o))
		(integer->der-integer (dsa-key-parameter-g o))))

(define-class <dsa-key-parameter-holder> (<immutable>)
  ((parameter :init-keyword :parameter :reader dsa-key-parameter)))

(define-class <dsa-public-key> (<public-key> <dsa-key-parameter-holder>)
  ((Y :init-keyword :Y :reader dsa-public-key-Y)))
(define (dsa-public-key? o) (is-a? o <dsa-public-key>))

(define-class <dsa-private-key> (<private-key> <dsa-key-parameter-holder>)
  ((Y :init-keyword :Y :reader dsa-private-key-Y)
   (X :init-keyword :X :reader dsa-private-key-X)))
(define (dsa-private-key? o) (is-a? o <dsa-private-key>))

(define +key-length+ `((1024 . ,*digest:sha-1*)
		       (2048 . ,*digest:sha-256*)
		       (3072 . ,*digest:sha-256*)))

(define (generate-dsa-parameter L :key (prng (secure-random-generator *prng:chacha20*)))
  (define (search-p L* q)
    (let ((test (bitwise-arithmetic-shift-left 1 (- L 1)))
	  (r0 (bitwise-arithmetic-shift-left q 1))
	  (buf (make-bytevector L*)))
      (let loop ()
	(random-generator-read-random-bytes! prng buf)
	(let* ((X (bytevector->integer buf))
	       (c (mod X r0))
	       (p (- X (- c 1))))
	  (if (and (>= p test) (probable-prime? p))
	      p
	      (loop))))))
  (define (search-g h p q) (mod-expt h (/ (- p 1) q) p))
  (let* ((md (cond ((assv L +key-length+) => cdr)
		   (else (assertion-violation 'generate-dsa-parameter
					      "Key size is not supported" L))))
	 (N* (digest-descriptor-digest-size md))
	 (q (generate-random-prime N* prng))
	 (p (search-p (div L 8) q))
	 (g (search-g 2 p q)))
    (make <dsa-key-parameter> :p p :q q :g g)))

(define (generate-dsa-keypair (parameter dsa-key-parameter?)
			      :key (prng (secure-random-generator *prng:chacha20*)))
  (define (search-x q)
    (let* ((len (div (bitwise-length q) 8))
	   (buf (make-bytevector len)))
      (let loop ()
	(random-generator-read-random-bytes! prng buf)
	(let ((x (bytevector->integer buf)))
	  (if (= x q)
	      (loop)
	      x)))))
  (let ((p (dsa-key-parameter-p parameter))
	(q (dsa-key-parameter-q parameter))
	(g (dsa-key-parameter-g parameter)))
    (let ((x (search-x q)))
      (values x (mod-expt g x p)))))
  
(define-method generate-key-pair ((m (eql *key:dsa*))
				  :key (size 1024)
				       (prng (secure-random-generator *prng:chacha20*))
				       (dsa-parameter #f))
  (define pr (or dsa-parameter (generate-dsa-parameter size :prng prng)))
  (let-values (((x y) (generate-dsa-keypair pr :prng prng)))
    (make-key-pair (make <dsa-private-key> :parameter pr :Y y :X x)
		   (make <dsa-public-key> :parameter pr :Y y))))

(define-method generate-public-key ((m (eql *key:dsa*))
				    (parameter <dsa-key-parameter>)
				    y)
  (make <dsa-public-key> :parameter parameter :Y y))

(define-method generate-private-key ((m (eql *key:dsa*))
				     (parameter <dsa-key-parameter>)
				     y x)
  (make <dsa-private-key> :parameter parameter :Y y :X x))

(define *dsa-key-oids* '("1.2.840.10040.4.1" "1.2.840.10040.4.3"))
(define-method oid->key-operation ((oid (member *dsa-key-oids*))) *key:dsa*)

;; ASN.1 module of DSA keys are from
;; https://www.rfc-editor.org/rfc/rfc5912.html
(define (parse-seq seq)
  (apply values (list-queue-list (asn1-collection-elements seq))))
(define (dsa-import-raw-public-key (key der-integer?)
				   (parameter dsa-key-parameter?))
  (make <dsa-public-key> :parameter parameter :Y (der-integer->integer key)))
(define (dsa-import-spki-public-key (key der-sequence?))
  (define (check-der-integer v)
    (unless (der-integer? v)
      (assertion-violation 'dsa-import-private-key "Invalid Dss-Params" v))
    (der-integer->integer v))
  (define (dsa-aid->dsa-parameter (aid der-sequence?))
    (let-values (((oid param) (parse-seq aid)))
      (oid->key-operation (der-object-identifier->oid-string oid)) ;; check
      (let-values (((p q g) (parse-seq param)))
	(make <dsa-key-parameter> :p (check-der-integer p)
	      :q (check-der-integer q) :g (check-der-integer g)))))
  (let-values (((aid key) (parse-seq key)))
    (let ((parameter (dsa-aid->dsa-parameter aid)))
      (dsa-import-raw-public-key
       (bytevector->asn1-object (der-bit-string->bytevector key))
       parameter))))

(define-method import-public-key ((m (eql *key:dsa*)) (in <bytevector>) . opts)
  (apply import-public-key m (open-bytevector-input-port in) opts))
(define-method import-public-key ((m (eql *key:dsa*)) (in <port>) . opts)
  (apply import-public-key m (read-asn1-object in) opts))
;; RAW format of DSA public key requires DSA parameter
(define-method import-public-key ((m (eql *key:dsa*))
				  (in <asn1-object>)
				  :optional (format (public-key-format subject-public-key-info))
					     (parameter #f))
  (case format
    ((raw) (dsa-import-raw-public-key in parameter))
    ((subject-public-key-info) (dsa-import-spki-public-key in))
    (else (assertion-violation 'import-public-key "Unknown public key format"
			       format))))

(define (dsa-export-raw-public-key (key dsa-public-key?))
  (asn1-encodable->bytevector (integer->der-integer (dsa-public-key-Y key))))
(define (dsa-export-spki-public-key (key dsa-public-key?))
  (asn1-encodable->bytevector
   (der-sequence
    (der-sequence (oid-string->der-object-identifier "1.2.840.10040.4.1")
		  (dsa-key-parameter key))
    (bytevector->der-bit-string (dsa-export-raw-public-key key)))))

(define-method export-public-key ((k <dsa-public-key>) . opts)
  (apply export-public-key *key:dsa* k opts))
(define-method export-public-key ((m (eql *key:dsa*))
				  (k <dsa-public-key>)
				  :optional (format (public-key-format subject-public-key-info)))
  (case format
    ((raw) (dsa-export-raw-public-key k))
    ((subject-public-key-info) (dsa-export-spki-public-key k))
    (else (assertion-violation 'export-public-key "Unknown public key format"
			       format))))

;; // From OpenSSL
;; DSAPrivateKey ::= OCTETSTRING {
;;   version Version,
;;   p INTEGER,
;;   q INTEGER,
;;   g INTEGER,
;;   publicExponent  INTEGER,
;;   privateExponent INTEGER
;; }
;; OCTETSTRING is used in PrivateKeyInfo, here we treat as sequence
(define-method import-private-key ((m (eql *key:dsa*)) (in <bytevector>) . opts)
  (apply import-private-key m (open-bytevector-input-port in) opts))
(define-method import-private-key ((m (eql *key:dsa*)) (in <port>) . opts)
  (apply import-private-key m (read-asn1-object in) opts))
(define-method import-private-key ((m (eql *key:dsa*)) (in <der-sequence>)
				   . ignore)
  (let-values (((v p q g Y X) (parse-seq in)))
    (make <dsa-private-key>
      :parameter (make <dsa-key-parameter> :p (der-integer->integer p)
		       :q (der-integer->integer q)
		       :g (der-integer->integer g))
      :Y (der-integer->integer Y)
      :X (der-integer->integer X))))

(define-method export-private-key ((key <dsa-private-key>) . opts)
  (apply export-private-key *key:dsa* key opts))
(define-method export-private-key ((m (eql *key:dsa*))
				   (key <dsa-private-key>) . opts)
  (let ((param (dsa-key-parameter key)))
    (asn1-encodable->bytevector
     (der-sequence
      (integer->der-integer 0)
      (integer->der-integer (dsa-key-parameter-p param))
      (integer->der-integer (dsa-key-parameter-q param))
      (integer->der-integer (dsa-key-parameter-g param))
      (integer->der-integer (dsa-private-key-Y key))
      (integer->der-integer (dsa-private-key-X key))))))

)
