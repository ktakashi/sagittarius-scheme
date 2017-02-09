;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; ec.scm - Elliptic curve
;;; 

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
#!core
(library (math ec)
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
	    sect113r1

	    ;; ellitic curve accessors
	    elliptic-curve?
	    elliptic-curve-field
	    elliptic-curve-a
	    elliptic-curve-b

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
	    (sagittarius))

  ;; modular arithmetic
  ;; these are needed on for Fp
  ;; a + b (mod p)
  (define (mod-add a b p) (mod (+ a b) p))
  ;; a - b (mod p)
  (define (mod-sub a b p) (mod (- a b) p))
  ;; a * b (mod p)
  ;;(define (mod-mul a b p) (mod (* a b) p))
  (define (mod-mul a b p) (* (mod a p) (mod b p)))
  ;; a / b (mod p)
  (define (mod-div a b p) (mod (* a (mod-inverse b p)) p))
  ;; a^2 (mod p)
  (define (mod-square a p) (mod-expt a 2 p))
  ;; -a (mod p)
  (define (mod-negate a p) (mod (- a) p))
  ;; mod-inverse is defined in (sagittarius)
  ;; mod-expt is defined in (sagittarius)


  ;; to make constant foldable, we use vectors to represent
  ;; data structure
  ;;;
  ;; EC Curve
  ;; curve is a vector which contains type and parameters
  ;; for the curve.

  ;; make my life easier
  (define-syntax define-vector-type
    (lambda (x)
      (define (order-args args fs)
	(map (lambda (a) 
	       (cond ((memp (lambda (f) (bound-identifier=? a f)) fs) => car)
		     (else
		      (syntax-violation 'define-vector-type "unknown tag" a))))
	     args))
      (define (generate-accessor k acc)
	;; starting from 1 because 0 is type tag
	(let loop ((r '()) (i 1) (acc acc))
	  (syntax-case acc ()
	    ((name rest ...)
	     (with-syntax ((n (datum->syntax k i)))
	       (loop (cons #'(define (name o) (vector-ref o n)) r)
		     (+ i 1)
		     #'(rest ...))))
	    (() r))))
      (syntax-case x ()
	((k type (ctr args ...) pred
	    (field accessor) ...)
	 (and (identifier? #'pred) (identifier? #'type) (identifier? #'ctr))
	 (with-syntax (((ordered-args ...)
			(order-args #'(args ...) #'(field ...)))
		       ((acc ...) (generate-accessor #'k #'(accessor ...))))
	 #'(begin
	     (define (ctr args ...) (vector 'type ordered-args ...))
	     (define (pred o) 
	       (and (vector? o)
		    (= (vector-length o) (+ (length #'(field ...)) 1))
		    (eq? (vector-ref o 0) 'type)))
	     acc ...))))))

  ;;; Finite field
  ;; Fp
  (define-vector-type ec-field-fp (make-ec-field-fp p) ec-field-fp?
    (p ec-field-fp-p))

  ;; F2m
  ;; TODO check valid reduction polynominal
  ;;      It must be either trinominal (X^m + X^k + 1 with m > k >= 1) or
  ;;      pentanominal (X^m X^k3 + X^k2 + X^k1 + 1 with m > k3 > k2 > k1 >= 1)
  (define-vector-type ec-field-f2m (make-ec-field-f2m m k1 k2 k3) ec-field-f2m?
    (m  ec-field-f2m-m)
    (k1 ec-field-f2m-k1)
    (k2 ec-field-f2m-k2)
    (k3 ec-field-f2m-k3))

  ;; F2m field operations
  (define (f2m-ppb? f)
    (not (or (zero? (ec-field-f2m-k2 f)) (zero? (ec-field-f2m-k3 f)))))
  (define (f2m-add field x y)
    (if (zero? y)
	x
	(bitwise-xor x y)))
  
  (define (f2m-mul field x y)
    (define ax x)
    (define bx y)
    (define cz (if (bitwise-bit-set? ax 0) bx 0))
    (define m  (ec-field-f2m-m field))
    (define k1 (ec-field-f2m-k1 field))
    (define k2 (ec-field-f2m-k2 field))
    (define k3 (ec-field-f2m-k3 field))
    (define (mult-z-mod a)
      (define az (bitwise-arithmetic-shift-left a 1))
      (if (bitwise-bit-set? az m)
	  (let* ((bl (bitwise-length az))
		 (bm (- (- (bitwise-arithmetic-shift-left 1 bl) 1)
			(bitwise-arithmetic-shift-left 1 m)))
		 (cm 1)
		 (k1m (bitwise-arithmetic-shift-left 1 k1))
		 (r (bitwise-and az bm)))
	    (if (f2m-ppb? field)
		(let ((k2m (bitwise-arithmetic-shift-left 1 k2))
		      (k3m (bitwise-arithmetic-shift-left 1 k3)))
		  (bitwise-xor r cm k1m k2m k3m))
		(bitwise-xor r cm k1m)))
	  az))
    (do ((i 1 (+ i 1)) (bx (mult-z-mod bx) (mult-z-mod bx))
	 (cz cz (if (bitwise-bit-set? ax i)
		    (bitwise-xor cz bx)
		    cz)))
	((= i m) cz)))
  
  (define (f2m-div field x y) (f2m-mul field x (f2m-inverse field y)))
  (define (f2m-square field x) (f2m-mul field x x))
  (define (f2m-inverse field x)
    (define m  (ec-field-f2m-m field))
    (define k1 (ec-field-f2m-k1 field))
    (define k2 (ec-field-f2m-k2 field))
    (define k3 (ec-field-f2m-k3 field))
    (define uz x)
    (define vz
      (let ((ppb? (f2m-ppb? field)))
	(bitwise-ior (bitwise-arithmetic-shift-left 1 m)
		     1
		     (bitwise-arithmetic-shift-left 1 k1)
		     (if ppb? (bitwise-arithmetic-shift-left 1 k2) 0)
		     (if ppb? (bitwise-arithmetic-shift-left 1 k3) 0))))
    (when (<= uz 0)
      (assertion-violation 'f2m-inverse "x is zero or negative" x))
    (let loop ((uz uz) (vz vz) (g1z 1) (g2z 0))
      (if (= uz 0)
	  g2z
	  (let ((j (- (bitwise-length uz) (bitwise-length vz))))
	    (let-values (((uz vz g1z g2z j)
			  (if (< j 0)
			      (values vz uz g2z g1z (- j))
			      (values uz vz g1z g2z j))))
	      (loop (bitwise-xor uz (bitwise-arithmetic-shift-left vz j))
		    vz
		    (bitwise-xor g1z (bitwise-arithmetic-shift-left g2z j))
		    g2z))))))
    
  (define-vector-type ec-curve (make-elliptic-curve field a b) elliptic-curve?
    (field elliptic-curve-field)
    (a     elliptic-curve-a)
    (b     elliptic-curve-b))
  

  (define-record-type predicate-generic
    (fields name table)
    (protocol (lambda (p)
		(lambda (n)
		  (p n (make-eq-hashtable))))))
  (define-syntax define-predicate-generic
    (syntax-rules ()
      ((_ name)
       (begin
	 (define dummy (make-predicate-generic 'name))
	 (define-syntax name
	   (lambda (x)
	     (syntax-case x ()
	       ((_ self args (... ...))
		#'(let* ((table (predicate-generic-table dummy))
			 (target self)
			 (eof (cons #f #f))
			 (itr (%hashtable-iter table)))
		    (let loop ()
		      (let-values (((k v) (itr eof)))
			(when (eq? k eof)
			  (assertion-violation 'name
			   "predicate for given argument is not registered"
			   self))
			(if (k target)
			    (v target args (... ...))
			    (loop))))))
	       (n (identifier? #'n) #'dummy))))))))
  (define-syntax define-predicate-method
    (syntax-rules ()
      ((_ name pred (self args ...) body ...)
       (define dummy
	 (let ((table (predicate-generic-table name))
	       (name (lambda (self args ...) body ...)))
	   (when (hashtable-contains? table pred)
	     (assertion-violation 'name
				  "specified predicate is already registered"
				  pred))
	   (hashtable-set! table pred name))))))

  (define (ec-curve=? a b) (equal? a b))

  (define-predicate-generic field-size)
  (define-predicate-method field-size ec-field-fp? (field)
    (bitwise-length (ec-field-fp-p field)))
  (define-predicate-method field-size ec-field-f2m? (field)
    (ec-field-f2m-m field))
  (define (ec-field-size field) (field-size field))
  
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
    (case (bytevector-u8-ref bv 0)
      ((#x00) ec-infinity-point)
      ;; TODO support compressed 0x02 and 0x03
      ((#x04)
       (let ((x (bytevector->integer bv 1 (+ 1 size)))
	     (y (bytevector->integer bv (+ 1 size))))
	 (make-ec-point x y)))
      (else
       (implementation-restriction-violation 'decode-ec-point
					     "not supported"))))
  
  ;; Twice
  (define-predicate-generic field-ec-point-twice)
  (define-predicate-method field-ec-point-twice ec-field-fp? (field curve x)
    (if (zero? (ec-point-y x))
	ec-infinity-point
	(let* ((xx (ec-point-x x))
	       (xy (ec-point-y x))
	       (p (ec-field-fp-p field))
	       ;; gamma = ((xx^2)*3 + curve.a)/(xy*2)
	       (gamma (mod-div (mod-add (mod-mul (mod-square xx p) 3 p)
					(elliptic-curve-a curve)
					p)
			       (mod-mul xy 2 p) p))
	       ;; x3 = gamma^2 - x*2
	       (x3 (mod-sub (mod-square gamma p) (mod-mul xx 2 p) p))
	       ;; y3 = gamma*(xx - x3) - xy
	       (y3 (mod-sub (mod-mul gamma (mod-sub xx x3 p) p) xy p)))
	  (make-ec-point x3 y3))))
	
  (define-predicate-method field-ec-point-twice ec-field-f2m? (field curve x)
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
	  (make-ec-point x3 y3))))
  
  (define (ec-point-twice curve x)
    (if (ec-point-infinity? x)
	x
	(field-ec-point-twice (elliptic-curve-field curve) curve x)))

  ;; Add
  (define-predicate-generic field-ec-point-add)
  (define-predicate-method field-ec-point-add ec-field-fp? (field curve x y)
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

  (define-predicate-method field-ec-point-add ec-field-f2m? (field curve x y)
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
	    (make-ec-point x3 y3)))))
  
  (define (ec-point-add curve x y)
    (cond ((ec-point-infinity? x) y)
	  ((ec-point-infinity? y) x)
	  (else
	   (field-ec-point-add (elliptic-curve-field curve) curve x y))))

  ;; Negate
  (define-predicate-generic field-ec-point-negate)
  (define-predicate-method field-ec-point-negate ec-field-fp? (field x)
    (make-ec-point (ec-point-x x)
		   (mod-negate (ec-point-y x) (ec-field-fp-p field))))
  (define-predicate-method field-ec-point-negate ec-field-f2m? (field x)
    (let ((xx (ec-point-x x)))
      (make-ec-point xx (f2m-add field xx (ec-point-y x)))))
  
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
      ((_ name p a b Gx Gy n h S oid)
       (define-constant name
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
  
  ;;; F2m
  (define (make-f2m-ec-parameter m k1 k2 k3 a b Gx Gy n h S :optional (oid #f))
    (make-ec-parameter (make-elliptic-curve (make-ec-field-f2m m k1 k2 k3) a b)
		       (make-ec-point Gx Gy)
		       n h S oid))
  
  (define-syntax define-f2m-parameter
    (syntax-rules ()
      ((_ "body" name (m k1 k2 k3) a b Gx Gy n h S oid)
       (define-constant name
	 (make-f2m-ec-parameter m k1 k2 k3 a b Gx Gy n h S oid)))
      ((_ name (m k1 k2 k3) a b Gx Gy n h S oid)
       (define-f2m-parameter "body" name (m k1 k2 k3) a b Gx Gy n h
	 (uinteger->bytevector S)
	 oid))
      ((_ name (m k1 k2 k3) a b Gx Gy n h oid)
       (define-f2m-parameter "body" name (m k1 k2 k3) a b Gx Gy n h #vu8()
	 oid))))
  
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
    (163 9 0 0)
    #x003088250CA6E7C7FE649CE85820F7
    #x00E8BEE4D3E2260744188BE0E9C723
    #x009D73616F35F4AB1407D73562C10F
    #x00A52830277958EE84D1315ED31886
    #x0100000000000000D9CCEC8A39E56F
    2
    #x10E723AB14D696E6768756151756FEBF8FCB49A9
    #f)
)
