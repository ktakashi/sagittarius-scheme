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

	    ;; parameter holder
	    dsa-key-parameter
	    
	    dsa-public-key? <dsa-public-key>
	    dsa-public-key-Y

	    dsa-private-key? <dsa-private-key>
	    dsa-private-key-Y dsa-private-key-X

	    generate-dsa-parameter
	    get-dsa-digest)
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
   (q :init-keyword :q :reader dsa-key-parameter-q)
   (g :init-keyword :g :reader dsa-key-parameter-g)))
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
(define-method write-object ((pr <dsa-public-key>) out)
  (let ((param (dsa-key-parameter pr)))
    (format out "#<dsa-public-key~%")
    (format out "   p: ~x~%" (dsa-key-parameter-p param))
    (format out "   q: ~x~%" (dsa-key-parameter-q param))
    (format out "   g: ~x~%" (dsa-key-parameter-g param))
    (format out "   Y: ~x~%>" (dsa-public-key-Y pr))))

(define-class <dsa-private-key> (<private-key> <dsa-key-parameter-holder>)
  ((Y :init-keyword :Y :reader dsa-private-key-Y)
   (X :init-keyword :X :reader dsa-private-key-X)))
(define (dsa-private-key? o) (is-a? o <dsa-private-key>))
(define-method write-object ((pr <dsa-private-key>) out)
  (let ((param (dsa-key-parameter pr)))
    (format out "#<dsa-private-key~%")
    (format out "   p: ~x~%" (dsa-key-parameter-p param))
    (format out "   q: ~x~%" (dsa-key-parameter-q param))
    (format out "   g: ~x~%" (dsa-key-parameter-g param))
    (format out "   Y: ~x~%" (dsa-private-key-Y pr))
    (format out "   X: ~x~%>" (dsa-private-key-X pr))))

(define +key-length+ `((1024 . ,*digest:sha-1*)
		       (2048 . ,*digest:sha-256*)
		       (3072 . ,*digest:sha-256*)))

(define (get-dsa-digest p)
  (cond ((assv (bitwise-length p) +key-length+) => cdr)
	(else #f)))

;; FIPS-186-4 A 1.1.2 Generation of the Probable Primes p and q Using an Approved Hash Function
;; Input:
;; 1. L The desired length of the prime p (in bits).
;; 2. N The desired length of the prime q (in bits).
;; 3. seedlen The desired length of the domain parameter seed; seedlen shall be
;;            equal to or greater than N.
(define (generate-pq L N digest prng)
  (define seedlen (div N 8))
  (define seed (make-bytevector seedlen))
  (define outlen (* (digest-descriptor-digest-size digest) 8))
  (define n (div (- L 1) outlen))
  (define b (mod (- L 1) outlen))
  (define w (make-bytevector (div L 8)))
  (define output (make-bytevector (digest-descriptor-digest-size digest)))
  (define md (make-message-digest digest))
  (define (inc! buf)
    (define len (bytevector-length buf))
    (let loop ((i (- len 1)))
      (unless (= i 0)
	(let ((b (bitwise-and (+ (bytevector-u8-ref buf i) 1) #xFF)))
	  (bytevector-u8-set! buf i b)
	  (when (zero? b) (loop (- i 1)))))))
  (define (compute-X offset)
    (do ((j 1 (+ j 1)) (olen (bytevector-length output)))
	((= j (- n 1))
	 (let ((remaining (- (bytevector-length w) (* n olen))))
	   (inc! offset)
	   (digest-message! md offset output)
	   (bytevector-copy! output (- olen remaining)
			     w 0 remaining)
	   (let ((b (bytevector-u8-ref w 0)))
	     (bytevector-u8-set! w 0 (bitwise-ior b #x80)))
	   (bytevector->uinteger w)))
      (inc! offset)
      (let ((p (- (bytevector-length w) (* j olen))))
	(digest-message! md offset w p))))
  (let loop () ;; step 5
    (random-generator-read-random-bytes! prng seed)
    (digest-message! md seed output)
    (let* ((U (mod (bytevector->uinteger output) (expt 2 (- N 1))))
	   (q (bitwise-ior (bitwise-ior U 1)
			   (bitwise-arithmetic-shift-left 1 (- N 1)))))
      (if (probable-prime? q)
	  (let ((offset (bytevector-copy seed))
		(limit (* 4 L)))
	    (let loop2 ((counter 0))
	      (if (= counter limit)
		  (loop) ;; goto step 5
		  (let* ((X (compute-X offset))
			 (c (mod X (* 2 q)))
			 (p (- X (- c 1))))
		    (if (and (= (bitwise-length p) L) (probable-prime? p))
			(values p q)
			(loop2 (+ counter 1)))))))
	  (loop)))))

(define (generate-dsa-parameter L :key (prng (secure-random-generator *prng:chacha20*)))
  (define (search-g p q)
    (define (random-range min max)
      (define size (div (bitwise-length max) 8))
      (let loop ()
	(let ((v (random-generator-random-integer prng size)))
	  (if (and (< v min) (> v max))
	      (loop)
	      v))))
    (let ((e (div (- p 1) q)))
      (let loop ()
	(let* ((h (random-range 2 (- p 2)))
	       (g (mod-expt h e p)))
	  (if (= g 1)
	      (loop)
	      g)))))
		   
  (let* ((digest (cond ((assv L +key-length+) => cdr)
		       (else
			(assertion-violation 'generate-dsa-parameter
					     "Key size is not supported" L))))
	 (N (digest-descriptor-digest-size digest)))
    (let-values (((p q) (generate-pq L (* N 8) digest prng)))
      (make <dsa-key-parameter> :p p :q q :g (search-g p q)))))

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
				  :key (size 2048)
				       (prng (secure-random-generator *prng:chacha20*))
				       (dsa-parameter #f)
				  :allow-other-keys)
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
(define-method key->oid ((key <dsa-key-parameter-holder>)) *dsa-key-oids*)

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
