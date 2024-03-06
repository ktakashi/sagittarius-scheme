;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/keys/operations/asymmetric/ecdh.scm - ECDH
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

;; reference:
;;  - SEC1 v2
;;    http://www.secg.org/sec1-v2.pdf
;;  - 800-56A
;;    http://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-56Ar2.pdf
#!nounbound
(library (sagittarius crypto keys operations asymmetric ecdh)
    (export generate-key-pair
	    generate-public-key
	    generate-private-key
	    import-public-key
	    import-private-key
	    export-public-key
	    export-private-key

	    *key:ecdh*	;; RFC 7748 key generation if possible
	    *key:ecdhc*	;; ECDHC
	    *key:x25519*
	    *key:x448*
	    
	    rfc7748-key? <rfc7748-key>
	    rfc7748-key-parameter

	    x25519-key? x448-key? ;; we may use them in the future?
	    
	    rfc7748-private-key? <rfc7748-private-key>
	    rfc7748-private-key-random
	    rfc7748-private-key-public-key

	    x25519-private-key? <x25519-private-key>
	    x448-private-key? <x448-private-key>

	    rfc7748-public-key? <rfc7748-public-key>
	    rfc7748-public-key-data

	    x25519-public-key? <x25519-public-key>
	    x448-public-key? <x448-public-key>

	    x25519-calculate-agreement
	    x448-calculate-agreement
	    )
    (import (rnrs)
	    (core misc)
	    (clos user)
	    (srfi :117 list-queues)
	    (sagittarius)
	    (sagittarius mop immutable)
	    (sagittarius crypto asn1)
	    (sagittarius crypto keys types)
	    (sagittarius crypto keys operations asymmetric apis)
	    (sagittarius crypto keys operations asymmetric ecdsa)
	    (sagittarius crypto digests)
	    (sagittarius crypto random)
	    (sagittarius crypto math prime)
	    (sagittarius crypto math ec)
	    (sagittarius crypto math modular))

(define *key:ecdh* :ecdh)
(define *key:ecdhc* :ecdhc)

;; SEC1 v2, section 3.3.1
;; ec-param - a curve parameter
;; du       - private key d from ec-priavate-key
;; Qv       - public key Q from ec-public-key (ec-point)
(define (ecdh-calculate-agreement ec-param du Qv)
  (define curve (ec-parameter-curve ec-param))
  (define h (ec-parameter-h ec-param))
  (let ((P (if (= h 1)
	       (ec-point-mul curve Qv du)
	       (let* ((n (ec-parameter-n ec-param))
		      (d (mod (* (mod-inverse h n) du) n))
		      (q (ec-point-mul curve Qv h)))
		 (ec-point-mul curve q d)))))
    (when (ec-point-infinity? P) (error 'ecdh-calculate-agreement "invalid"))
    (ec-point-x P)))

;; SEC1 v2, section 3.3.2
;; ec-param - a curve parameter
;; du       - private key d from ec-priavate-key
;; Qv       - public key Q from ec-public-key (ec-point)
;; NB: prefix ECDEC is taken from cofactor agreement of bouncy castle
(define (ecdhc-calculate-agreement ec-param du Qv)
  (define curve (ec-parameter-curve ec-param))
  (define h (ec-parameter-h ec-param))
  (define n (ec-parameter-n ec-param))
  (let ((P (ec-point-mul curve Qv (mod (* du h) n))))
    (when (ec-point-infinity? P) (error 'ecdh-calculate-agreement "invalid"))
    (ec-point-x P)))

(define (calculate-key-agreement-impl agreement-calculator priv pub)
  (define param (ecdsa-key-parameter priv))
  (define curve (ec-parameter-curve param))
  (unless (elliptic-curve=? curve (ec-parameter-curve (ecdsa-key-parameter pub)))
    (assertion-violation 'calculate-key-agreement
			 "Key type are not the same"))
  (let ((size (div (+ (ec-field-size (elliptic-curve-field curve)) 7) 8)))
    ;; it's rather weird to return integer as secret key
    ;; so convert it to bytevector.
    ;; NOTE: actual implementation return integer for whatever the reason
    (integer->bytevector
     (agreement-calculator param (ecdsa-private-key-d priv)
			   (ecdsa-public-key-Q pub))
     size)))

(define-method calculate-key-agreement ((m (eql *key:ecdh*))
					(priv <ecdsa-private-key>)
					(pub <ecdsa-public-key>))
  (calculate-key-agreement-impl ecdh-calculate-agreement priv pub))

(define-method calculate-key-agreement ((m (eql *key:ecdhc*))
					(priv <ecdsa-private-key>)
					(pub <ecdsa-public-key>))
  (calculate-key-agreement-impl ecdhc-calculate-agreement priv pub))


(define *key:x25519* :x25519)
(define *key:x448* :x448)
(define-class <rfc7748-key> (<immutable>)
  ((parameter :init-keyword :parameter :reader rfc7748-key-parameter)))
(define (rfc7748-key? o) (is-a? o <rfc7748-key>))
(define-class <rfc7748-private-key> (<private-key> <rfc7748-key>)
  ((random :init-keyword :random :reader rfc7748-private-key-random)
   (public-key :init-keyword :public-key
	       :reader rfc7748-private-key-public-key)))
(define (rfc7748-private-key? o) (is-a? o <rfc7748-private-key>))

(define-class <x25519-key> () ())
(define (x25519-key? o) (is-a? o <x25519-key>))
(define-class <x448-key> () ())
(define (x448-key? o) (is-a? o <x448-key>))

(define-class <x25519-private-key> (<rfc7748-private-key> <x25519-key>) ())
(define (x25519-private-key? o) (is-a? o <x25519-private-key>))

(define-class <x448-private-key> (<rfc7748-private-key> <x448-key>) ())
(define (x448-private-key? o) (is-a? o <x448-private-key>))

;;; Public key
(define-class <rfc7748-public-key> (<public-key> <rfc7748-key>)
  ((data :init-keyword :data :reader rfc7748-public-key-data)))
(define (rfc7748-public-key? o) (is-a? o <rfc7748-public-key>))
(define-class <x25519-public-key> (<rfc7748-public-key> <x25519-key>) ())
(define (x25519-public-key? o) (is-a? o <x25519-public-key>))

(define-class <x448-public-key> (<rfc7748-public-key> <x448-key>) ())
(define (x448-public-key? o) (is-a? o <x448-public-key>))

;; key generation x25519
(define-method generate-public-key ((m (eql *key:x25519*))
				    (in <bytevector>) . ignore)
  (make <x25519-public-key> :data in :parameter x25519-curve-parameter))

(define-method generate-private-key ((m (eql *key:x25519*))
				     (in <bytevector>) . ignore)
  (let ((pub (compute-public-key in x25519-curve-parameter)))
    (make <x25519-private-key> :random in :parameter x25519-curve-parameter
	  :public-key (generate-public-key m pub))))
(define-method generate-key-pair ((m (eql *key:x25519*))
				  :key (prng (secure-random-generator *prng:chacha20*))
				  :allow-other-keys)
  (let* ((random (random-generator-read-random-bytes prng 32))
	 (private-key (generate-private-key *key:x25519* random)))
    (make-key-pair private-key (rfc7748-private-key-public-key private-key))))

;; key generation x448
(define-method generate-public-key ((m (eql *key:x448*))
				    (in <bytevector>) . ignore)
  (make <x448-public-key> :data in :parameter x448-curve-parameter))

(define-method generate-private-key ((m (eql *key:x448*))
				     (in <bytevector>) . ignore)
  (let ((pub (compute-public-key in x448-curve-parameter)))
    (make <x448-private-key> :random in :parameter x448-curve-parameter
	  :public-key (generate-public-key m pub))))
(define-method generate-key-pair ((m (eql *key:x448*))
				  :key (prng (secure-random-generator *prng:chacha20*))
				  :allow-other-keys)
  (let* ((random (random-generator-read-random-bytes prng 56))
	 (private-key (generate-private-key *key:x448* random)))
    (make-key-pair private-key (rfc7748-private-key-public-key private-key))))

(define-method calculate-key-agreement ((m (eql *key:x25519*)) . params)
  (apply calculate-key-agreement *key:ecdh* params))
(define-method calculate-key-agreement ((m (eql *key:x448*)) . params)
  (apply calculate-key-agreement *key:ecdh* params))
(define-method calculate-key-agreement ((m (eql *key:ecdh*))
					(priv <rfc7748-private-key>)
					(pub <rfc7748-public-key>))
  (define parameter (rfc7748-key-parameter priv))
  (define agreement (curve-parameter-calculate-agreement parameter))
  (unless (equal? parameter (rfc7748-key-parameter pub))
    (assertion-violation 'calculate-key-agreement
			 "Key types are not the same" priv pub))
  (agreement (rfc7748-private-key-random priv)
	     (rfc7748-public-key-data pub)))

;; import / export
(define *x25519-key-oid* "1.3.101.110")
(define *x448-key-oid* "1.3.101.111")
(define-method oid->key-operation ((oid (equal *x25519-key-oid*))) *key:x25519*)
(define-method oid->key-operation ((oid (equal *x448-key-oid*))) *key:x448*)
(define-method key->oid ((key <rfc7748-key>))
  (if (x25519-key? key)
      *x25519-key-oid*
      *x448-key-oid*))

(define-method export-public-key ((key <rfc7748-public-key>) . opts)
  (apply export-public-key *key:ecdh* key opts))
(define-method export-public-key ((m (eql *key:x25519*))
				  (key <rfc7748-public-key>) . opts)
  (apply export-public-key *key:ecdh* key opts))
(define-method export-public-key ((m (eql *key:x448*))
				  (key <rfc7748-public-key>) . opts)
  (apply export-public-key *key:ecdh* key opts))
(define-method export-public-key ((m (eql *key:ecdh*))
				  (key <rfc7748-public-key>)
				  :optional (format (public-key-format raw)))
  (define (eddh-key->oid key)
    (cond ((x25519-public-key? key) *x25519-key-oid*)
	  ((x448-public-key? key) *x448-key-oid*)
	  (else (assertion-violation 'export-public-key
				     "Unknown key" key))))
  (case format
    ((raw) (rfc7748-public-key-data key))
    ((subject-public-key-info)
     (asn1-encodable->bytevector
      (der-sequence
       (der-sequence (oid-string->der-object-identifier (eddh-key->oid key)))
       (bytevector->der-bit-string (rfc7748-public-key-data key)))))
    (else (assertion-violation 'export-public-key
			       "Unknown public key format" format))))

(define-method import-public-key ((m (eql *key:x25519*)) (in <port>) . opts)
  (apply import-public-key m (get-bytevector-all in) opts))
(define-method import-public-key ((m (eql *key:x25519*)) (in <bytevector>)
				  :optional (format (public-key-format raw)))
  (case format
    ((raw) (generate-public-key m in))
    ((subject-public-key-info) (import-public-key *key:ecdh* in format))
    (else (assertion-violation 'export-public-key
			       "Unknown public key format" format))))
(define-method import-public-key ((m (eql *key:x448*)) (in <port>) . opts)
  (apply import-public-key m (get-bytevector-all in) opts))
(define-method import-public-key ((m (eql *key:x448*)) (in <bytevector>)
				  :optional (format (public-key-format raw)))
  (case format
    ((raw) (generate-public-key m in))
    ((subject-public-key-info) (import-public-key *key:ecdh* in format))
    (else (assertion-violation 'export-public-key
			       "Unknown public key format" format))))

(define-method import-public-key ((m (eql *key:ecdh*)) (in <bytevector>) . opts)
  (apply import-public-key m (open-bytevector-input-port in) opts))
(define-method import-public-key ((m (eql *key:ecdh*)) (in <port>) . opts)
  (apply import-public-key m (read-asn1-object in) opts))
(define-method import-public-key ((m (eql *key:ecdh*)) (in <der-sequence>)
				  :optional (format (public-key-format subject-public-key-info)))
  (let*-values (((aid key) (deconstruct-asn1-collection in))
		((oid . ignore) (deconstruct-asn1-collection aid)))
    (let ((op (oid->key-operation (der-object-identifier->oid-string oid))))
      (import-public-key op (der-bit-string->bytevector key)))))


(define-method export-private-key ((key <rfc7748-private-key>) . opts)
  (apply export-private-key *key:ecdh* key opts))
(define-method export-private-key ((m (eql *key:x25519*))
				   (key <rfc7748-private-key>) . opts)
  (apply export-private-key *key:ecdh* key opts))
(define-method export-private-key ((m (eql *key:x448*))
				   (key <rfc7748-private-key>) . opts)
  (apply export-private-key *key:ecdh* key opts))
(define-method export-private-key ((m (eql *key:ecdh*))
				   (key <rfc7748-private-key>) . ignore)
  (rfc7748-private-key-random key))

(define-method import-private-key ((m (eql *key:x25519*)) (in <port>) . opts)
  (apply import-private-key m (get-bytevector-all in) opts))
(define-method import-private-key ((m (eql *key:x25519*)) (in <bytevector>)
				  . opts)
  (generate-private-key m in))
(define-method import-private-key ((m (eql *key:x448*)) (in <port>) . opts)
  (apply import-private-key m (get-bytevector-all in) opts))
(define-method import-private-key ((m (eql *key:x448*)) (in <bytevector>)
				  . opts)
  (generate-private-key m in))

;; Commons
(define (compute-public-key k parameter)
  (define u (curve-parameter-u parameter))
  (define agreement (curve-parameter-calculate-agreement parameter))
  (define size (div (+ (curve-parameter-bits parameter) 7) 8))
  (agreement k (integer->bytevector/endian u (endianness little) size)))

;;; Parameters
(define-vector-type curve-parameter
  (make-curve-parameter name p A order cofactor u v bits decoder agreement)
  curve-parameter?
  (name curve-parameter-name)
  (p curve-parameter-p)
  (A curve-parameter-A)
  (order curve-parameter-order)
  (cofactor curve-parameter-cofactor)
  (u curve-parameter-u)
  (v curve-parameter-v)
  (bits curve-parameter-bits)
  ;; scalar decoder
  (decoder curve-parameter-decoder)
  ;; it's a bit of cross reference though
  (agreement curve-parameter-calculate-agreement))

;; scalar decoder
(define (x25519-scalar-decode k)
  (unless (and (bytevector? k) (= (bytevector-length k) 32))
    (assertion-violation 'x25519-scalar-decode "Invalid scalar length"))
  (bytevector-u8-set! k 0 (bitwise-and (bytevector-u8-ref k 0) 248))
  (let ((last (bytevector-u8-ref k 31)))
    (bytevector-u8-set! k 31 (bitwise-ior (bitwise-and last 127) 64)))
  (bytevector->integer/endian k (endianness little)))

(define (x448-scalar-decode k)
  (unless (and (bytevector? k) (= (bytevector-length k) 56))
    (assertion-violation 'x56-scalar-decode "Invalid scalar length"))
  (bytevector-u8-set! k 0 (bitwise-and (bytevector-u8-ref k 0) 252))
  (bytevector-u8-set! k 55 (bitwise-ior (bytevector-u8-ref k 55) 128))
  (bytevector->integer/endian k (endianness little)))

(define (x25519-calculate-agreement k u)
  (define decoder (curve-parameter-decoder x25519-curve-parameter))
  (define (mask u)
    ;; When receiving such an array, implementations of X25519
    ;; (but not X448) MUST mask the most significant bit in the final byte.
    ;; This is done to preserve compatibility with point formats that
    ;; reserve the sign bit for use in other protocols and to increase
    ;; resistance to implementation fingerprinting
    (bytevector-u8-set! u 31 (bitwise-and #x7F (bytevector-u8-ref u 31)))
    u)
  (encode-u-cordinate
   (calculate-u-cordinate
    (decoder (bytevector-copy k))
    (bytevector->integer/endian (mask u) (endianness little))
    x25519-curve-parameter)
   32))

(define (x448-calculate-agreement k u)
  (define decoder (curve-parameter-decoder x448-curve-parameter))
  (encode-u-cordinate
   (calculate-u-cordinate (decoder (bytevector-copy k))
			  (bytevector->integer/endian u (endianness little))
			  x448-curve-parameter)
   56))

(define x25519-curve-parameter
  (make-curve-parameter
   'X25519
   (- (expt 2 255) 19) ;; P
   486662	       ;; A
   (+ (expt 2 252) #x14def9dea2f79cd65812631a5cf5d3ed) ;; order
   8						       ;; cofactor
   9						       ;; u
   #x20ae19a1b8a086b4e01edd2c7748d14c923d4d7e6d7c61b229e9c5a27eced3d9 ;; v
   255
   x25519-scalar-decode
   x25519-calculate-agreement))

(define x448-curve-parameter
  (make-curve-parameter
   'X448
   (- (expt 2 448) (expt 2 224) 1)	;; P
   156326				;; A
   (- (expt 2 446) #x8335dc163bb124b65129c96fde933d8d723a70aadc873d6d54a7bb0d)
   4
   5
   #x7d235d1295f5b1f66c98ab6e58326fcecbae5d34f55545d060f75dc28df3f6edb8027e2346430d211312c4b150677af76fd7223d457b5b1a
   448
   x448-scalar-decode
   x448-calculate-agreement))

(define (encode-u-cordinate u size)
  (integer->bytevector/endian u (endianness little) size))

(define (calculate-u-cordinate k u parameter)
  (define a24 (/ (- (curve-parameter-A parameter) 2) 4))
  (define p (curve-parameter-p parameter))
  (define bits (curve-parameter-bits parameter))
  (define x_1 u) ;; seems constant...

  (define (cswap swap x_2 x_3)
    (let ((dummy (bitwise-and (- swap) (bitwise-xor x_2 x_3))))
      (values (bitwise-xor x_2 dummy)
	      (bitwise-xor x_3 dummy))))
  
  (define (finish x_2 x_3 z_2 z_3 swap)
    (let-values (((x_2 x_3) (cswap swap x_2 x_3))
		 ((z_2 z_3) (cswap swap z_2 z_3)))
      (mod-mul x_2 (mod-expt z_2 (- p 2) p) p)))

  (let loop ((x_2 1) (z_2 0) (x_3 u) (z_3 1) (swap 0) (t (- bits 1)))
    (if (< t 0)
	(finish x_2 x_3 z_2 z_3 swap)
	(let* ((k_t (if (bitwise-bit-set? k t) 1 0))
	       (swap0 (bitwise-xor swap k_t)))
	  (let-values (((x_2 x_3) (cswap swap0 x_2 x_3))
		       ((z_2 z_3) (cswap swap0 z_2 z_3)))
	    (let* ((A (mod-add x_2 z_2 p))
		   (AA (mod-square A p))
		   (B (mod-sub x_2 z_2 p))
		   (BB (mod-square B p))
		   (E (mod-sub AA BB p))
		   (C (mod-add x_3 z_3 p))
		   (D (mod-sub x_3 z_3 p))
		   (DA (mod-mul D A p))
		   (CB (mod-mul C B p)))
	      (loop (mod-mul AA BB p) ;; x_2
		    (mod-mul E (mod-add AA (mod-mul a24 E p) p) p) ;; z_2
		    (mod-square (mod-add DA CB p) p) ;; x_3
		    (mod-mul x_1 (mod-square (mod-sub DA CB p) p) p) ;; z_3
		    k_t
		    (- t 1))))))))
)
