;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/math/ec.scm - Elliptic curve
;;;  
;;;   Copyright (c) 2017-2022  Takashi Kato  <ktakashi@ymail.com>
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

;; moved from (math ec)

;; this library provides 3 things
;;  - curve parameters
;;  - constructors (ec point and curve)
;;  - and arithmetic procedure for ec points
;;
;; there are 2 curves used, one is Fp and other one is F2m.
;; Fp is 
;;   y^2 = x^3 + ax + b (mod p)
;; F2m is
;;   y^2 + xy = x^3 + ax^2 + b (mod p)
#!nounbound
(library (sagittarius crypto math ec)
    (export make-ec-point ec-point-infinity?
	    ec-point?
	    ec-point-x
	    ec-point-y
	    ec-point-add
	    ec-point-twice
	    ec-point-negate
	    ec-point-sub
	    ec-point-mul

	    encode-ec-point
	    decode-ec-point
	    
	    ;; NIST parameters
	    NIST-P-192 (rename (NIST-P-192 secp192r1))
	    NIST-P-224 (rename (NIST-P-224 secp224r1))
	    NIST-P-256 (rename (NIST-P-256 secp256r1))
	    NIST-P-384 (rename (NIST-P-384 secp384r1))
	    NIST-P-521 (rename (NIST-P-521 secp521r1))

	    NIST-K-163 (rename (NIST-K-163 sect163k1))
	    NIST-K-233 (rename (NIST-K-233 sect233k1))
	    NIST-K-283 (rename (NIST-K-283 sect283k1))
	    NIST-K-409 (rename (NIST-K-409 sect409k1))
	    NIST-K-571 (rename (NIST-K-571 sect571k1))

	    NIST-B-163 (rename (NIST-B-163 sect163r2))
	    NIST-B-233 (rename (NIST-B-233 sect233r1))
	    NIST-B-283 (rename (NIST-B-283 sect283r1))
	    NIST-B-409 (rename (NIST-B-409 sect409r1))
	    NIST-B-571 (rename (NIST-B-571 sect571r1))

	    ;; SEC 2 parameters
	    ;; below 3 are v1
	    secp160r1
	    secp160k1
	    secp160r2

	    ;; v2
	    secp192k1
	    secp224k1
	    secp256k1

	    sect163r1
	    sect239k1
	    sect113r1

	    ;; curve25519
	    curve25519

	    ;; brainpool
	    brainpool-p160r1
	    brainpool-p160t1
	    brainpool-p192r1
	    brainpool-p192t1
	    brainpool-p224r1
	    brainpool-p224t1
	    brainpool-p256r1
	    brainpool-p256t1
	    brainpool-p320r1
	    brainpool-p320t1
	    brainpool-p384r1
	    brainpool-p384t1
	    brainpool-p512r1
	    brainpool-p512t1
	    
	    ;; elliptic curve accessors
	    elliptic-curve?
	    elliptic-curve-field
	    elliptic-curve-a
	    elliptic-curve-b

	    elliptic-curve=?
	    
	    ;; field
	    ec-field-fp?
	    ec-field-fp-p
	    ec-field-f2m?
	    ec-field-f2m-m
	    ec-field-f2m-k1
	    ec-field-f2m-k2
	    ec-field-f2m-k3
	    ec-field-size
	    
	    ;; parameter accessors
	    ec-parameter?
	    ec-parameter-curve
	    ec-parameter-g
	    ec-parameter-n
	    ec-parameter-h
	    ec-parameter-seed
	    ec-parameter-oid
	    lookup-ec-parameter
	    make-ec-parameter
	    make-fp-ec-parameter
	    make-f2m-ec-parameter
	    
	    ;; for testing
	    make-elliptic-curve
	    make-ec-field-fp
	    make-ec-field-f2m
	    ec-infinity-point
	    )
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax)
	    (core inline)
	    (core record)
	    (core misc)	   ;; for defaine-vector-type
	    (sagittarius)
	    (sagittarius crypto math modular) ;; these are needed on for Fp
	    (sagittarius crypto math ec fields))

;; to make constant foldable, we use vectors to represent
;; data structure
;;;
;; EC Curve
;; curve is a vector which contains type and parameters
;; for the curve.


(define-vector-type ec-curve (make-elliptic-curve field a b) elliptic-curve?
  (field elliptic-curve-field)
  (a     elliptic-curve-a)
  (b     elliptic-curve-b))

(define (elliptic-curve=? curve1 curve2)
  (and (elliptic-curve? curve1)
       (elliptic-curve? curve2)
       (= (elliptic-curve-a curve1) (elliptic-curve-a curve2))
       (= (elliptic-curve-b curve1) (elliptic-curve-b curve2))
       (ec-field=? (elliptic-curve-field curve1) (elliptic-curve-field curve2))))

(define (ec-field=? field1 field2)
  (cond ((and (ec-field-fp? field1) (ec-field-fp? field2))
	 (= (ec-field-fp-p field1) (ec-field-fp-p field2)))
	((and (ec-field-f2m? field1) (ec-field-f2m? field2))
	 (and (= (ec-field-f2m-m  field1) (ec-field-f2m-m  field1))
	      (= (ec-field-f2m-k1 field1) (ec-field-f2m-k1 field1))
	      (= (ec-field-f2m-k2 field1) (ec-field-f2m-k2 field1))
	      (= (ec-field-f2m-k3 field1) (ec-field-f2m-k3 field1))))
	(else #f)))

(define-syntax define-predicate-generic
  (syntax-rules ()
    ((_ (name field args ...) (pred body ...) ...)
     (define (name field args ...)
       (cond ((pred field) body ...)
	     ...
	     (else (assertion-violation
		    'name "No predicate matched" field)))))))

(define (ec-curve=? a b) (equal? a b))

(define-predicate-generic (field-size field)
  (ec-field-fp? (bitwise-length (ec-field-fp-p field)))
  (ec-field-f2m? (ec-field-f2m-m field)))
(define ec-field-size field-size)

;; EC point
(define-vector-type ec-point (make-ec-point x y) ec-point?
  (x     ec-point-x)
  (y     ec-point-y))

;; keep it immutable...
(define ec-infinity-point '#(ec-point #f #f))

;; we don't check x and y, these can be #f for infinite point
(define (ec-point-infinity? p)
  (or (not (ec-point-x p))
      (not (ec-point-y p))))

(define (ec-point=? a b) (equal? a b))

(define (encode-ec-point curve ep)
  (if (ec-point-infinity? ep)
      #vu8(00)
      (let ((size (div (+ (ec-field-size (elliptic-curve-field curve)) 7) 8)))
	(bytevector-append #vu8(#x04)
			   (integer->bytevector (ec-point-x ep) size)
			   (integer->bytevector (ec-point-y ep) size)))))

(define (decode-ec-point curve bv)
  (define size (div (+ (ec-field-size (elliptic-curve-field curve)) 7) 8))
  (define type (bytevector-u8-ref bv 0))
  
  (case type
    ((#x00) ec-infinity-point)
    ;; TODO support compressed 0x02 and 0x03
    ((#x02 #x03)
     (unless (= (bytevector-length bv) (+ size 1))
       (assertion-violation 'decode-ec-point
			    "Incorrect length for compressed encoding"))
     (let* ((~y (bitwise-and type 1))
	    (x (bytevector->integer bv 1 (+ size 1)))
	    (p (decompress-point curve ~y x)))
       (unless (valid-ec-point? p)
	 (assertion-violation 'decompress-point "Invalid point"))
       p))
    ((#x04)
     (let ((x (bytevector->integer bv 1 (+ 1 size)))
	   (y (bytevector->integer bv (+ 1 size))))
       (make-ec-point x y)))
    (else
     (implementation-restriction-violation 'decode-ec-point
					   "not supported" type))))

;; FIXME should check better...
(define (valid-ec-point? p) #t)

(define (decompress-point curve y x)
  (define field (elliptic-curve-field curve))
  (define (decompress-fp-point curve x ~y)
    (define field (elliptic-curve-field curve))
    (define p (ec-field-fp-p field))
    (define a (elliptic-curve-a curve))
    (define b (elliptic-curve-b curve))
    ;; y^2 = x^3 + ax + b
    ;;     = (x^2 + a)x + b
    (let* ((rhs (mod-add (mod-mul (mod-add (mod-square x p) a p) x p) b p))
	   (y (mod-sqrt rhs p)))
      (unless y
	(assertion-violation 'decompress-point "Invalid point compression"))
      (let ((neg? (not (eqv? (bitwise-bit-set? y 0) (= ~y 1)))))
	(make-ec-point x (if neg? (mod-sub p y p) y)))))
  (define (decompress-f2m-point curve x y)
    (error 'decompress-fp-point "not yet"))
  (cond ((ec-field-fp? field)
	 (decompress-fp-point curve x y))
	((ec-field-f2m? field)
	 (decompress-f2m-point curve x y))
	(else (assertion-violation 'decompress-point "Unknown curve" curve))))

;; Twice
(define-predicate-generic (field-ec-point-twice field curve x)
  (ec-field-fp?
   (if (zero? (ec-point-y x))
       ec-infinity-point
       (let* ((xx (ec-point-x x))
	      (xy (ec-point-y x))
	      (p (ec-field-fp-p field))
	      ;; gamma = ((xx^2)*3 + curve.a)/(xy*2)
	      (gamma (mod-div (mod-add (mod-mul (mod-square xx p) 3 p)
				       (elliptic-curve-a curve)
				       p)
			      (mod-mul xy 2 p)
			      p))
	      ;; x3 = gamma^2 - x*2
	      (x3 (mod-sub (mod-square gamma p) (mod-mul xx 2 p) p))
	      ;; y3 = gamma*(xx - x3) - xy
	      (y3 (mod-sub (mod-mul gamma (mod-sub xx x3 p) p) xy p)))
	 (make-ec-point x3 y3))))
  (ec-field-f2m?
   (if (zero? (ec-point-x x))
       ec-infinity-point
       (let* ((xx (ec-point-x x))
	      (xy (ec-point-y x))
	      (l1 (f2m-add field (f2m-div field xy xx) xx))
	      (x3 (f2m-add field (f2m-add field (f2m-square field l1) l1)
			   (elliptic-curve-a curve)))
	      (y3 (f2m-add field (f2m-add field (f2m-square field xx)
					  (f2m-mul field l1 x3))
			   x3)))
	 (make-ec-point x3 y3)))))

(define (ec-point-twice curve x)
  (if (ec-point-infinity? x)
      x
      (field-ec-point-twice (elliptic-curve-field curve) curve x)))

;; Add
(define-predicate-generic (field-ec-point-add field curve x y)
  (ec-field-fp? 
   (if (equal? (ec-point-x x) (ec-point-x y))
       (if (equal? (ec-point-y x) (ec-point-y y))
	   (ec-point-twice curve x)
	   ec-infinity-point)
       (let* ((xx (ec-point-x x))
	      (xy (ec-point-y x))
	      (yx (ec-point-x y))
	      (yy (ec-point-y y))
	      (p (ec-field-fp-p field))
	      ;; gamma = (yy - xy)/(yx-xx)
	      (gamma (mod-div (mod-sub yy xy p) (mod-sub yx xx p) p))
	      ;; x3 = gamma^2 - xx - yx
	      (x3 (mod-sub (mod-sub (mod-square gamma p) xx p) yx p))
	      ;; y3 = gamma*(xx - x3) - xy
	      (y3 (mod-sub (mod-mul gamma (mod-sub xx x3 p) p) xy p)))
	 (make-ec-point x3 y3))))
  (ec-field-f2m? 
   (let* ((xx (ec-point-x x))
	  (xy (ec-point-y x))
	  (yx (ec-point-x y))
	  (yy (ec-point-y y))
	  (dx (f2m-add field xx yx))
	  (dy (f2m-add field xy yy)))
     (if (zero? dx)
	 (if (zero? dy)
	     (ec-point-twice curve x)
	     ec-infinity-point)
	 (let* ((L (f2m-div field dy dx))
		(x3 (f2m-add field
			     (f2m-add field
				      (f2m-add field
					       (f2m-square field L) L)
				      dx)
			     (elliptic-curve-a curve)))
		(y3 (f2m-add field
			     (f2m-add field
				      (f2m-mul field L 
					       (f2m-add field xx x3))
				      x3)
			     xy)))
	   (make-ec-point x3 y3))))))

(define (ec-point-add curve x y)
  (cond ((ec-point-infinity? x) y)
	((ec-point-infinity? y) x)
	(else
	 (field-ec-point-add (elliptic-curve-field curve) curve x y))))

;; Negate
(define-predicate-generic (field-ec-point-negate field x)
  (ec-field-fp? 
   (make-ec-point (ec-point-x x)
		  (mod-negate (ec-point-y x) (ec-field-fp-p field))))
  (ec-field-f2m?
   (let ((xx (ec-point-x x)))
     (make-ec-point xx (f2m-add field xx (ec-point-y x))))))

(define (ec-point-negate curve x)
  (field-ec-point-negate (elliptic-curve-field curve) x))

(define (ec-point-sub curve x y)
  (if (ec-point-infinity? y)
      x
      ;; add -y
      (ec-point-add curve x (ec-point-negate curve y))))

;; http://en.wikipedia.org/wiki/Non-adjacent_form
;; this is probably super slow but for now...
(define (ec-point-mul curve p k)
  (unless (integer? k) (error 'ec-point-mul "integer required for k" k))
  (let ((h (* k 3))
	(neg (ec-point-negate curve p)))
    (let loop ((R p) (i (- (bitwise-length h) 2)))
      (if (zero? i)
	  R
	  (let ((R (ec-point-twice curve R))
		(hbit? (bitwise-bit-set? h i)))
	    (if (eqv? hbit? (bitwise-bit-set? k i))
		(loop R (- i 1))
		(loop (ec-point-add curve R (if hbit? p neg)) (- i 1))))))))
  
  ;;;;
  ;;; Parameters
;; Parameter contains followings
;;  - curve
;;  - base point x y (as ec-point)
;;  - Order q of the point G (and of the elliptic curve group E)
;;  - h = (l - 1) / 160 where l is bit length of prime p
;;  - seed

(define (ec-parameter? o) 
  (and (vector? o) (= (vector-length o) 7) 
       (eq? (vector-ref o 0) 'ec-parameter)))
(define (ec-parameter-curve o) (vector-ref o 1))
(define (ec-parameter-g o)     (vector-ref o 2)) ;; as EC point
(define (ec-parameter-n o)     (vector-ref o 3))
(define (ec-parameter-h o)     (vector-ref o 4))
(define (ec-parameter-seed o)  (vector-ref o 5))
(define (ec-parameter-oid o)   (vector-ref o 6))

  ;;; Parameters
(define *lookup-table* (make-string-hashtable))
(define (register-ec-parameter oid value)
  (when (string? oid)
    (hashtable-set! *lookup-table* oid value)))
(define (lookup-ec-parameter oid) (hashtable-ref *lookup-table* oid #f))

;; from
;;   https://www.nsa.gov/ia/_files/nist-routines.pdf (gone)
;;   http://csrc.nist.gov/groups/ST/toolkit/documents/dss/NISTReCur.pdf (*)
;;   http://koclab.cs.ucsb.edu/teaching/cren/docs/w02/nist-routines.pdf
;;   http://www.secg.org/sec2-v2.pdf
;; 
;; (*) is not used

(define (make-ec-parameter curve base n h S :optional (oid #f))
  (let ((p `#(ec-parameter ,curve ,base ,n ,h ,S ,oid)))
    (when oid (register-ec-parameter oid p))
    p))

(define (make-fp-ec-parameter p a b Gx Gy n h S :optional (oid #f))
  (make-ec-parameter (make-elliptic-curve (make-ec-field-fp p) a b)
		     (make-ec-point Gx Gy)
		     n h S oid))
  ;;; Fp
(define-syntax define-fp-parameter
  (syntax-rules ()
    ((_ name p a b Gx Gy n h oid)
     (define name
       (make-fp-ec-parameter p a b Gx Gy n h #f oid)))
    ((_ name p a b Gx Gy n h S oid)
     (define name
       (make-fp-ec-parameter p a b Gx Gy n h (uinteger->bytevector S) oid)))))
(define-fp-parameter NIST-P-192
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC
  #x64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1
  #x188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012
  #x07192B95FFC8DA78631011ED6B24CDD573F977A11E794811
  #xFFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831
  1
  #x3045AE6FC8422F64ED579528D38120EAE12196D5
  "1.2.840.10045.3.1.1")

(define-fp-parameter secp160r1
  #xffffffffffffffffffffffffffffffff7fffffff
  #xffffffffffffffffffffffffffffffff7ffffffc
  #x1c97befc54bd7a8b65acf89f81d4d4adc565fa45
  #x4a96b5688ef573284664698968c38bb913cbfc82
  #x23a628553168947d59dcc912042351377ac5fb32
  #x0100000000000000000001f4c8f927aed3ca752257
  #x01
  "1.3.132.0.8")

(define-fp-parameter secp160k1
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73
  0
  7
  #x3b4c382ce37aa192a4019e763036f4f5dd4d7ebb
  #x938cf935318fdced6bc28286531733c3f03c4fee
  #x0100000000000000000001b8fa16dfab9aca16b6b3
  1
  "1.3.132.0.9")

(define-fp-parameter secp160r2
  #xfffffffffffffffffffffffffffffffeffffac73
  #xfffffffffffffffffffffffffffffffeffffac70
  #xb4e134d3fb59eb8bab57274904664d5af50388ba
  #x52dcb034293a117e1f4ff11b30f7199d3144ce6d
  #xfeaffef2e331f296e071fa0df9982cfea7d43f2e
  #x0100000000000000000000351ee786a818f3a1a16b
  #x01
  "1.3.132.0.30")

(define-fp-parameter secp192k1
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFEE37
  0
  3
  #xDB4FF10EC057E9AE26B07D0280B7F4341DA5D1B1EAE06C7D
  #x9B2F2F6D9C5628A7844163D015BE86344082AA88D95E2F9D
  #xFFFFFFFFFFFFFFFFFFFFFFFE26F2FC170F69466A74DEFD8D
  1
  "1.3.132.0.31")

(define-fp-parameter NIST-P-224
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE
  #xB4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4
  #xB70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21
  #xBD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D
  1
  #xBD71344799D5C7FCDC45B59FA3B9AB8F6A948BC5
  "1.3.132.0.33")

(define-fp-parameter secp224k1
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFE56D
  0
  5
  #xA1455B334DF099DF30FC28A169A467E9E47075A90F7E650EB6B7A45C
  #x7E089FED7FBA344282CAFBD6F7E319F7C0B0BD59E2CA4BDB556D61A5
  #x010000000000000000000000000001DCE8D2EC6184CAF0A971769FB1F7
  1
  "1.3.132.0.32")

(define-fp-parameter NIST-P-256
  #xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF
  #xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC
  #x5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B
  #x6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296
  #x4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5
  #xFFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551
  1
  #xC49D360886E704936A6678E1139D26B7819F7E90
  "1.2.840.10045.3.1.7")

(define-fp-parameter secp256k1
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
  0
  7
  #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
  #x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
  1
  "1.3.132.0.10")

(define-fp-parameter NIST-P-384
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFF
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFC
  #xB3312FA7E23EE7E4988E056BE3F82D19181D9C6EFE8141120314088F5013875AC656398D8A2ED19D2A85C8EDD3EC2AEF
  #xAA87CA22BE8B05378EB1C71EF320AD746E1D3B628BA79B9859F741E082542A385502F25DBF55296C3A545E3872760AB7
  #x3617DE4A96262C6F5D9E98BF9292DC29F8F41DBD289A147CE9DA3113B5F0B8C00A60B1CE1D7E819D7A431D7C90EA0E5F
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFc7634d81f4372ddf581a0db248b0a77aecec196accc52973
  1
  #xA335926AA319A27A1D00896A6773A4827ACDAC73
  "1.3.132.0.34")

(define-fp-parameter NIST-P-521
  #x000001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
  #x000001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC
  #x00000051953eb9618e1c9a1f929a21a0b68540eea2da725b99b315f3b8b489918ef109e156193951ec7e937b1652c0bd3bb1bf073573df883d2c34f1ef451fd46b503f00
  #x000000c6858e06b70404e9cd9e3ecb662395b4429c648139053fb521f828af606b4d3dbaa14b5e77efe75928fe1dc127a2ffa8de3348b3c1856a429bf97e7e31c2e5bd66
  #x0000011839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650
  #x000001fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa51868783bf2f966b7fcc0148f709a5d03bb5c9b8899c47aebb6fb71e91386409
  1
  #xD09E8800291CB85396CC6717393284AAA0DA64BA
  "1.3.132.0.35")

;; RFC7748
(define-fp-parameter curve25519
  ;; (- (expt 2 255) 19)
  #x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFED
  #x2AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA984914A144
  #x7B425ED097B425ED097B425ED097B425ED097B425ED097B4260B5E9C7710C864
  #x2AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD245A
  #x20AE19A1B8A086B4E01EDD2C7748D14C923D4D7E6D7C61B229E9C5A27ECED3D9
  #x1000000000000000000000000000000014DEF9DEA2F79CD65812631A5CF5D3ED ;; order
  8 ;; cofactor
  0 ;; S
  "1.3.6.1.4.1.3029.1.5.1")

;; brainpool
;; https://www.rfc-editor.org/rfc/rfc5639
(define-fp-parameter brainpool-p160r1
  #xE95E4A5F737059DC60DFC7AD95B3D8139515620F
  #x340E7BE2A280EB74E2BE61BADA745D97E8F7C300
  #x1E589A8595423412134FAA2DBDEC95C8D8675E58
  #xBED5AF16EA3F6A4F62938C4631EB5AF7BDBCDBC3
  #x1667CB477A1A8EC338F94741669C976316DA6321
  #xE95E4A5F737059DC60DF5991D45029409E60FC09
  #x1
  "1.3.36.3.3.2.8.1.1.1")

(define-fp-parameter brainpool-p160t1
  #xE95E4A5F737059DC60DFC7AD95B3D8139515620F
  #xE95E4A5F737059DC60DFC7AD95B3D8139515620C
  #x7A556B6DAE535B7B51ED2C4D7DAA7A0B5C55F380
  #xB199B13B9B34EFC1397E64BAEB05ACC265FF2378
  #xADD6718B7C7C1961F0991B842443772152C9E0AD
  #xE95E4A5F737059DC60DF5991D45029409E60FC09
  #x1
  "1.3.36.3.3.2.8.1.1.2")

(define-fp-parameter brainpool-p192r1
  #xC302F41D932A36CDA7A3463093D18DB78FCE476DE1A86297
  #x6A91174076B1E0E19C39C031FE8685C1CAE040E5C69A28EF
  #x469A28EF7C28CCA3DC721D044F4496BCCA7EF4146FBF25C9
  #xC0A0647EAAB6A48753B033C56CB0F0900A2F5C4853375FD6
  #x14B690866ABD5BB88B5F4828C1490002E6773FA2FA299B8F
  #xC302F41D932A36CDA7A3462F9E9E916B5BE8F1029AC4ACC1
  #x1
  "1.3.36.3.3.2.8.1.1.3")
(define-fp-parameter brainpool-p192t1
  #xC302F41D932A36CDA7A3463093D18DB78FCE476DE1A86297
  #xC302F41D932A36CDA7A3463093D18DB78FCE476DE1A86294
  #x13D56FFAEC78681E68F9DEB43B35BEC2FB68542E27897B79
  #x3AE9E58C82F63C30282E1FE7BBF43FA72C446AF6F4618129
  #x097E2C5667C2223A902AB5CA449D0084B7E5B3DE7CCC01C9
  #xC302F41D932A36CDA7A3462F9E9E916B5BE8F1029AC4ACC1
  #x1
  "1.3.36.3.3.2.8.1.1.4")

(define-fp-parameter brainpool-p224r1
  #xD7C134AA264366862A18302575D1D787B09F075797DA89F57EC8C0FF
  #x68A5E62CA9CE6C1C299803A6C1530B514E182AD8B0042A59CAD29F43
  #x2580F63CCFE44138870713B1A92369E33E2135D266DBB372386C400B
  #x0D9029AD2C7E5CF4340823B2A87DC68C9E4CE3174C1E6EFDEE12C07D
  #x58AA56F772C0726F24C6B89E4ECDAC24354B9E99CAA3F6D3761402CD
  #xD7C134AA264366862A18302575D0FB98D116BC4B6DDEBCA3A5A7939F
  #x1
  "1.3.36.3.3.2.8.1.1.5")
(define-fp-parameter brainpool-p224t1
  #xD7C134AA264366862A18302575D1D787B09F075797DA89F57EC8C0FF
  #xD7C134AA264366862A18302575D1D787B09F075797DA89F57EC8C0FC
  #x4B337D934104CD7BEF271BF60CED1ED20DA14C08B3BB64F18A60888D
  #x6AB1E344CE25FF3896424E7FFE14762ECB49F8928AC0C76029B4D580
  #x0374E9F5143E568CD23F3F4D7C0D4B1E41C8CC0D1C6ABD5F1A46DB4C
  #xD7C134AA264366862A18302575D0FB98D116BC4B6DDEBCA3A5A7939F
  #x1
  "1.3.36.3.3.2.8.1.1.6")

(define-fp-parameter brainpool-p256r1
  #xA9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5377
  #x7D5A0975FC2C3057EEF67530417AFFE7FB8055C126DC5C6CE94A4B44F330B5D9
  #x26DC5C6CE94A4B44F330B5D9BBD77CBF958416295CF7E1CE6BCCDC18FF8C07B6
  #x8BD2AEB9CB7E57CB2C4B482FFC81B7AFB9DE27E1E3BD23C23A4453BD9ACE3262
  #x547EF835C3DAC4FD97F8461A14611DC9C27745132DED8E545C1D54C72F046997
  #xA9FB57DBA1EEA9BC3E660A909D838D718C397AA3B561A6F7901E0E82974856A7
  #x1
  "1.3.36.3.3.2.8.1.1.7")
(define-fp-parameter brainpool-p256t1
  #xA9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5377
  #xA9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5374
  #x662C61C430D84EA4FE66A7733D0B76B7BF93EBC4AF2F49256AE58101FEE92B04
  #xA3E8EB3CC1CFE7B7732213B23A656149AFA142C47AAFBC2B79A191562E1305F4
  #x2D996C823439C56D7F7B22E14644417E69BCB6DE39D027001DABE8F35B25C9BE
  #xA9FB57DBA1EEA9BC3E660A909D838D718C397AA3B561A6F7901E0E82974856A7
  #x1
  "1.3.36.3.3.2.8.1.1.8")

(define-fp-parameter brainpool-p320r1
  #xD35E472036BC4FB7E13C785ED201E065F98FCFA6F6F40DEF4F92B9EC7893EC28FCD412B1F1B32E27
  #x3EE30B568FBAB0F883CCEBD46D3F3BB8A2A73513F5EB79DA66190EB085FFA9F492F375A97D860EB4
  #x520883949DFDBC42D3AD198640688A6FE13F41349554B49ACC31DCCD884539816F5EB4AC8FB1F1A6
  #x43BD7E9AFB53D8B85289BCC48EE5BFE6F20137D10A087EB6E7871E2A10A599C710AF8D0D39E20611
  #x14FDD05545EC1CC8AB4093247F77275E0743FFED117182EAA9C77877AAAC6AC7D35245D1692E8EE1
  #xD35E472036BC4FB7E13C785ED201E065F98FCFA5B68F12A32D482EC7EE8658E98691555B44C59311
  #x1
  "1.3.36.3.3.2.8.1.1.9")
(define-fp-parameter brainpool-p320t1
  #xD35E472036BC4FB7E13C785ED201E065F98FCFA6F6F40DEF4F92B9EC7893EC28FCD412B1F1B32E27
  #xD35E472036BC4FB7E13C785ED201E065F98FCFA6F6F40DEF4F92B9EC7893EC28FCD412B1F1B32E24
  #xA7F561E038EB1ED560B3D147DB782013064C19F27ED27C6780AAF77FB8A547CEB5B4FEF422340353
  #x925BE9FB01AFC6FB4D3E7D4990010F813408AB106C4F09CB7EE07868CC136FFF3357F624A21BED52
  #x63BA3A7A27483EBF6671DBEF7ABB30EBEE084E58A0B077AD42A5A0989D1EE71B1B9BC0455FB0D2C3
  #xD35E472036BC4FB7E13C785ED201E065F98FCFA5B68F12A32D482EC7EE8658E98691555B44C59311
  #x1
  "1.3.36.3.3.2.8.1.1.10")

(define-fp-parameter brainpool-p384r1
  #x8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B412B1DA197FB71123ACD3A729901D1A71874700133107EC53
  #x7BC382C63D8C150C3C72080ACE05AFA0C2BEA28E4FB22787139165EFBA91F90F8AA5814A503AD4EB04A8C7DD22CE2826
  #x04A8C7DD22CE28268B39B55416F0447C2FB77DE107DCD2A62E880EA53EEB62D57CB4390295DBC9943AB78696FA504C11
  #x1D1C64F068CF45FFA2A63A81B7C13F6B8847A3E77EF14FE3DB7FCAFE0CBD10E8E826E03436D646AAEF87B2E247D4AF1E
  #x8ABE1D7520F9C2A45CB1EB8E95CFD55262B70B29FEEC5864E19C054FF99129280E4646217791811142820341263C5315
  #x8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B31F166E6CAC0425A7CF3AB6AF6B7FC3103B883202E9046565
  #x1
  "1.3.36.3.3.2.8.1.1.11")
(define-fp-parameter brainpool-p384t1
  #x8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B412B1DA197FB71123ACD3A729901D1A71874700133107EC53
  #x8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B412B1DA197FB71123ACD3A729901D1A71874700133107EC50
  #x7F519EADA7BDA81BD826DBA647910F8C4B9346ED8CCDC64E4B1ABD11756DCE1D2074AA263B88805CED70355A33B471EE
  #x18DE98B02DB9A306F2AFCD7235F72A819B80AB12EBD653172476FECD462AABFFC4FF191B946A5F54D8D0AA2F418808CC
  #x25AB056962D30651A114AFD2755AD336747F93475B7A1FCA3B88F2B6A208CCFE469408584DC2B2912675BF5B9E582928
  #x8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B31F166E6CAC0425A7CF3AB6AF6B7FC3103B883202E9046565
  #x1
  "1.3.36.3.3.2.8.1.1.12")

(define-fp-parameter brainpool-p512r1
  #xAADD9DB8DBE9C48B3FD4E6AE33C9FC07CB308DB3B3C9D20ED6639CCA703308717D4D9B009BC66842AECDA12AE6A380E62881FF2F2D82C68528AA6056583A48F3
  #x7830A3318B603B89E2327145AC234CC594CBDD8D3DF91610A83441CAEA9863BC2DED5D5AA8253AA10A2EF1C98B9AC8B57F1117A72BF2C7B9E7C1AC4D77FC94CA
  #x3DF91610A83441CAEA9863BC2DED5D5AA8253AA10A2EF1C98B9AC8B57F1117A72BF2C7B9E7C1AC4D77FC94CADC083E67984050B75EBAE5DD2809BD638016F723
  #x81AEE4BDD82ED9645A21322E9C4C6A9385ED9F70B5D916C1B43B62EEF4D0098EFF3B1F78E2D0D48D50D1687B93B97D5F7C6D5047406A5E688B352209BCB9F822
  #x7DDE385D566332ECC0EABFA9CF7822FDF209F70024A57B1AA000C55B881F8111B2DCDE494A5F485E5BCA4BD88A2763AED1CA2B2FA8F0540678CD1E0F3AD80892
  #xAADD9DB8DBE9C48B3FD4E6AE33C9FC07CB308DB3B3C9D20ED6639CCA70330870553E5C414CA92619418661197FAC10471DB1D381085DDADDB58796829CA90069
  #x1
  "1.3.36.3.3.2.8.1.1.13")
(define-fp-parameter brainpool-p512t1
  #xAADD9DB8DBE9C48B3FD4E6AE33C9FC07CB308DB3B3C9D20ED6639CCA703308717D4D9B009BC66842AECDA12AE6A380E62881FF2F2D82C68528AA6056583A48F3
  #xAADD9DB8DBE9C48B3FD4E6AE33C9FC07CB308DB3B3C9D20ED6639CCA703308717D4D9B009BC66842AECDA12AE6A380E62881FF2F2D82C68528AA6056583A48F0
  #x7CBBBCF9441CFAB76E1890E46884EAE321F70C0BCB4981527897504BEC3E36A62BCDFA2304976540F6450085F2DAE145C22553B465763689180EA2571867423E
  #x640ECE5C12788717B9C1BA06CBC2A6FEBA85842458C56DDE9DB1758D39C0313D82BA51735CDB3EA499AA77A7D6943A64F7A3F25FE26F06B51BAA2696FA9035DA
  #x5B534BD595F5AF0FA2C892376C84ACE1BB4E3019B71634C01131159CAE03CEE9D9932184BEEF216BD71DF2DADF86A627306ECFF96DBB8BACE198B61E00F8B332
  #xAADD9DB8DBE9C48B3FD4E6AE33C9FC07CB308DB3B3C9D20ED6639CCA70330870553E5C414CA92619418661197FAC10471DB1D381085DDADDB58796829CA90069
  #x1
  "1.3.36.3.3.2.8.1.1.14")

  ;;; F2m
(define (make-f2m-ec-parameter m k1 k2 k3 a b Gx Gy n h S :optional (oid #f))
  (make-ec-parameter (make-elliptic-curve (make-ec-field-f2m m k1 k2 k3) a b)
		     (make-ec-point Gx Gy)
		     n h S oid))

(define-syntax define-f2m-parameter
  (syntax-rules ()
    ((_ "body" name (m k1 k2 k3) a b Gx Gy n h S oid)
     (define name
       (make-f2m-ec-parameter m k1 k2 k3 a b Gx Gy n h S oid)))
    ((_ name (m k1 k2 k3) a b Gx Gy n h S oid)
     (define-f2m-parameter "body" name (m k1 k2 k3) a b Gx Gy n h
       (uinteger->bytevector S)
       oid))
    ((_ name (m k1 k2 k3) a b Gx Gy n h oid)
     (define-f2m-parameter "body" name (m k1 k2 k3) a b Gx Gy n h #f oid))))
  
;; f(x) = x^163 + x^7 + x^6 + x^3 + 1
(define-f2m-parameter NIST-K-163
  (163 3 6 7)
  1
  1
  #x02FE13C0537BBC11ACAA07D793DE4E6D5E5C94EEE8
  #x0289070FB05D38FF58321F2E800536D538CCDAA3D9
  #x04000000000000000000020108A2E0CC0D99F8A5EF
  2
  "1.3.132.0.1")

(define-f2m-parameter sect163r1
  (163 3 6 7)
  #x07B6882CAAEFA84F9554FF8428BD88E246D2782AE2
  #x0713612DCDDCB40AAB946BDA29CA91F73AF958AFD9
  #x0369979697AB43897789566789567F787A7876A654
  #x00435EDB42EFAFB2989D51FEFCE3C80988F41FF883
  #x03FFFFFFFFFFFFFFFFFFFF48AAB689C29CA710279B
  2
  "1.3.132.0.2")

(define-f2m-parameter NIST-B-163
  (163 3 6 7)
  1
  #x020a601907b8c953ca1481eb10512f78744a3205fd
  #x03f0eba16286a2d57ea0991168d4994637e8343e36
  #x00d51fbc6c71a0094fa2cdd545b11c5c0c797324f1
  #x040000000000000000000292fe77e70c12a4234c33
  2
  #x85E25BFE5C86226CDB12016F7553F9D0E693A268
  "1.3.132.0.15")

(define-f2m-parameter NIST-K-233
  (233 74 0 0)
  0
  1
  #x017232BA853A7E731AF129F22FF4149563A419C26BF50A4C9D6EEFAD6126
  #x01DB537DECE819B7F70F555A67C427A8CD9BF18AEB9B56E0C11056FAE6A3
  #x8000000000000000000000000000069D5BB915BCD46EFB1AD5F173ABDF
  4
  "1.3.132.0.26")

(define-f2m-parameter NIST-B-233
  (233 74 0 0)
  1
  #x0066647EDE6C332C7F8C0923BB58213B333B20E9CE4281FE115F7D8F90AD
  #x00FAC9DFCBAC8313BB2139F1BB755FEF65BC391F8B36F8F8EB7371FD558B
  #x01006A08A41903350678E58528BEBF8A0BEFF867A7CA36716F7E01F81052
  #x1000000000000000000000000000013E974E72F8A6922031D2603CFE0D7
  2
  #x74D59FF07F6B413D0EA14B344B20A2DB049B50C3
  "1.3.132.0.27")

(define-f2m-parameter sect239k1
  (239 158 0 0)
  0
  1
  #x29A0B6A887A983E9730988A68727A8B2D126C44CC2CC7B2A6555193035DC
  #x76310804F12E549BDB011C103089E73510ACB275FC312A5DC6B76553F0CA
  #x2000000000000000000000000000005A79FEC67CB6E91F1C1DA800E478A5
  4
  "1.3.132.0.3")

(define-f2m-parameter NIST-K-283
  (283 5 7 12)
  0
  1
  #x0503213F78CA44883F1A3B8162F188E553CD265F23C1567A16876913B0C2AC2458492836
  #x01CCDA380F1C9E318D90F95D07E5426FE87E45C0E8184698E45962364E34116177DD2259
  #x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9AE2ED07577265DFF7F94451E061E163C61
  4
  "1.3.132.0.16")

(define-f2m-parameter NIST-B-283
  (283 5 7 12)
  1
  #x027B680AC8B8596DA5A4AF8A19A0303FCA97FD7645309FA2A581485AF6263E313B79A2F5
  #x05F939258DB7DD90E1934F8C70B0DFEC2EED25B8557EAC9C80E2E198F8CDBECD86B12053
  #x03676854FE24141CB98FE6D4B20D02B4516FF702350EDDB0826779C813F0DF45BE8112F4
  #x03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEF90399660FC938A90165B042A7CEFADB307
  2
  #x77E2B07370EB0F832A6DD5B62DFC88CD06BB84BE
  "1.3.132.0.17")

(define-f2m-parameter NIST-K-409
  (409 87 0 0)
  0
  1
  #x0060F05F658F49C1AD3AB1890F7184210EFD0987E307C84C27ACCFB8F9F67CC2C460189EB5AAAA62EE222EB1B35540CFE9023746
  #x01E369050B7C4E42ACBA1DACBF04299C3460782F918EA427E6325165E9EA10E3DA5F6C42E9C55215AA9CA27A5863EC48D8E0286B
  #x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE5F83B2D4EA20400EC4557D5ED3E3E7CA5B4B5C83B8E01E5FCF
  4
  "1.3.132.0.36")

(define-f2m-parameter NIST-B-409
  (409 87 0 0)
  1
  #x0021A5C2C8EE9FEB5C4B9A753B7B476B7FD6422EF1F3DD674761FA99D6AC27C8A9A197B272822F6CD57A55AA4F50AE317B13545F
  #x015D4860D088DDB3496B0C6064756260441CDE4AF1771D4DB01FFE5B34E59703DC255A868A1180515603AEAB60794E54BB7996A7
  #x0061B1CFAB6BE5F32BBFA78324ED106A7636B9C5A7BD198D0158AA4F5488D08F38514F1FDF4B4F40D2181B3681C364BA0273C706
  #x010000000000000000000000000000000000000000000000000001E2AAD6A612F33307BE5FA47C3C9E052F838164CD37D9A21173
  2
  #x4099B5A457F9D69F79213D094C4BCD4D4262210B
  "1.3.132.0.37")

(define-f2m-parameter NIST-K-571
  (571 2 5 10)
  0
  1
  #x026EB7A859923FBC82189631F8103FE4AC9CA2970012D5D46024804801841CA44370958493B205E647DA304DB4CEB08CBBD1BA39494776FB988B47174DCA88C7E2945283A01C8972
  #x0349DC807F4FBF374F4AEADE3BCA95314DD58CEC9F307A54FFC61EFC006D8A2C9D4979C0AC44AEA74FBEBBB9F772AEDCB620B01A7BA7AF1B320430C8591984F601CD4C143EF1C7A3
  #x020000000000000000000000000000000000000000000000000000000000000000000000131850E1F19A63E4B391A8DB917F4138B630D84BE5D639381E91DEB45CFE778F637C1001
  4
  "1.3.132.0.38")

(define-f2m-parameter NIST-B-571
  (571 2 5 10)
  1
  #x02F40E7E2221F295DE297117B7F3D62F5C6A97FFCB8CEFF1CD6BA8CE4A9A18AD84FFABBD8EFA59332BE7AD6756A66E294AFD185A78FF12AA520E4DE739BACA0C7FFEFF7F2955727A
  #x0303001D34B856296C16C0D40D3CD7750A93D1D2955FA80AA5F40FC8DB7B2ABDBDE53950F4C0D293CDD711A35B67FB1499AE60038614F1394ABFA3B4C850D927E1E7769C8EEC2D19
  #x037BF27342DA639B6DCCFFFEB73D69D78C6C27A6009CBBCA1980F8533921E8A684423E43BAB08A576291AF8F461BB2A8B3531D2F0485C19B16E2F1516E23DD3C1A4827AF1B8AC15B
  #x03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE661CE18FF55987308059B186823851EC7DD9CA1161DE93D5174D66E8382E9BB2FE84E47
  2
  #x2AA058F73A0E33AB486B0F610410C53A7F132310
  "1.3.132.0.39")

;; f(x) = x^113 + x^9 + 1
(define-f2m-parameter sect113r1
  (113 9 0 0)
  #x003088250CA6E7C7FE649CE85820F7
  #x00E8BEE4D3E2260744188BE0E9C723
  #x009D73616F35F4AB1407D73562C10F
  #x00A52830277958EE84D1315ED31886
  #x0100000000000000D9CCEC8A39E56F
  2
  #x10E723AB14D696E6768756151756FEBF8FCB49A9
  #f)
)
