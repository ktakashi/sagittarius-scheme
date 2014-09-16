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
    (export make-ec-point
	    ec-point-add
	    ec-point-twice
	    ec-point-negate
	    ec-point-sub
	    ;; NIST parameters
	    P-192
	    P-224

	    ec-parameter-curve
	    )
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax)
	    (core inline)
	    (sagittarius))

  ;; modular arithmetic
  ;; these are needed on for Fp
  ;; a + b (mod p)
  (define (mod-add a b p) (mod (+ a b) p))
  ;; a - b (mod p)
  (define (mod-sub a b p) (mod (- a b) p))
  ;; a * b (mod p)
  (define (mod-mul a b p) (mod (* a b) p))
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

  ;; Fp curve
  (define (make-fp-curve q a b) (vector 'fp q a b))
  ;; TODO F2m curve
  (define (make-f2m-curve m k1 k2 k3 a b n h) 
    (error 'make-f2m-curve "not supported yet"))

  (define-syntax curve-type
    (syntax-rules ()
      ((_ o) (vector-ref o 0))))
  (define-syntax fp-curve-q
    (syntax-rules ()
      ((_ o) (vector-ref o 1))))
  (define-syntax fp-curve-a
    (syntax-rules ()
      ((_ o) (vector-ref o 2))))
  (define-syntax fp-curve-b
    (syntax-rules ()
      ((_ o) (vector-ref o 3))))

  (define (%curve? o type) (and (vector? o) 
				(not (zero? (vector-length o)))
				(eq? (curve-type o) type)))
  (define (fp-curve? o)  (%curve? o 'fp))
  (define (f2m-curve? o) (%curve? o 'f2m))
  (define (ec-curve=? a b) (equal? a b))

  ;; EC point
  (define (make-ec-point curve x y) (vector curve x y))
  (define-inliner make-ec-point (math ec) ((_ c x y) (vector c x y)))

  (define (infinity-point curve) (make-ec-point curve #f #f))
  (define-syntax ec-point-curve
    (syntax-rules ()
      ((_ o) (vector-ref o 0))))
  (define-syntax ec-point-x
    (syntax-rules ()
      ((_ o) (vector-ref o 1))))
  (define-syntax ec-point-y
    (syntax-rules ()
      ((_ o) (vector-ref o 2))))
  ;; we don't check x and y, these can be #f for infinite point
  (define (ec-point? o)
    (and (vector? o)
	 (= (vector-length o) 3)
	 (or (fp-curve? (ec-point-curve o))
	     (f2m-curve? (ec-point-curve o)))))
  (define (ec-point-infinity? p)
    (or (not (ec-point-x p))
	(not (ec-point-y p))))
  (define (ec-point=? a b) (equal? a b))

  (define (ec-point-twice x)
    (define (fp-ec-point-twice x)
      (let* ((xx (ec-point-x x))
	     (xy (ec-point-y x))
	     (curve (ec-point-curve x))
	     (p (fp-curve-q curve))
	     ;; gamma = ((xx^2)*3 + curve.a)/(xy*2)
	     (gamma (mod-div (mod-add (mod-mul (mod-square xx p) 3 p)
				      (fp-curve-a curve)
				      p)
			     (mod-mul xy 2 p) p))
	     ;; x3 = gamma^2 - x*2
	     (x3 (mod-sub (mod-square gamma p) (mod-mul xx 2 p) p))
	     ;; y3 = gamma*(xx - x3) - xy
	     (y3 (mod-sub (mod-mul gamma (mod-sub xx x3 p) p) xy p)))
	(make-ec-point curve x3 y3)))
    (define (f2m-ec-point-twice x)
      (error 'ec-point-twice "not supported yet" x))
    (cond ((ec-point-infinity? x) x)
	  ((zero? (ec-point-y x)) (infinity-point (ec-point-curve x)))
	  (else 
	   (if (fp-curve? (ec-point-curve x))
	       (fp-ec-point-twice x)
	       (f2m-ec-point-twice x)))))

  (define (ec-point-add x y)
    (define (fp-ec-point-add x y)
      (let* ((xx (ec-point-x x))
	     (xy (ec-point-y x))
	     (yx (ec-point-x y))
	     (yy (ec-point-y y))
	     (curve (ec-point-curve y))
	     ;; if the curve are the same then p are the same
	     (p (fp-curve-q curve))
	     ;; gamma = (yy - xy)/(yx-xx)
	     (gamma (mod-div (mod-sub yy xy p) (mod-sub yx xx p) p))
	     ;; x3 = gamma^2 - xx - yx
	     (x3 (mod-sub (mod-sub (mod-square gamma p) xx p) yx p))
	     ;; y3 = gamma*(xx - x3) - xy
	     (y3 (mod-sub (mod-mul gamma (mod-sub xx x3 p) p) xy p)))
	(make-ec-point curve x3 y3)))
    (define (f2m-ec-point-add x y)
      (error 'ec-point-add "not supported yet" x y))
    (cond ((not (ec-curve=? (ec-point-curve x) (ec-point-curve y)))
	   (error 'ec-point-add "attempt to adding differenct curve point"
		  x y))
	  ((ec-point-infinity? x) y)
	  ((ec-point-infinity? y) x)
	  ((= (ec-point-x x) (ec-point-x y))
	   (if (= (ec-point-y x) (ec-point-y y))
	       (ec-point-twice x)
	       (infinity-point (ec-point-curve x))))
	  (else 
	   (if (fp-curve? (ec-point-curve x))
	       (fp-ec-point-add x y)
	       (f2m-ec-point-add x y)))))

  (define (ec-point-negate x)
    (let ((curve (ec-point-curve x)))
      (if (fp-curve? curve)
	  (make-ec-point curve (ec-point-x x) 
			 (mod-negate (ec-point-y x) (fp-curve-q curve)))
	  (error 'ec-point-negate "not supported yet"))))

  (define (ec-point-sub x y)
    (cond ((not (ec-curve=? (ec-point-curve x) (ec-point-curve y)))
	   (error 'ec-point-add "attempt to adding differenct curve point"
		  x y))
	  ((ec-point-infinity? y) x)
	  (else
	   ;; add -y
	   (ec-point-add x (ec-point-negate y)))))
  
  ;;;;
  ;;; Parameters
  ;; Parameter contains followings
  ;;  - curve
  ;;  - base point x y (as ec-point)
  ;;  - Order q of the point G (and of the elliptic curve group E)
  ;;  - h = (l - 1) / 160 where l is bit length of prime p
  ;;  - seed

  (define-syntax ec-parameter-curve
    (syntax-rules ()
      ((_ o) (vector-ref o 0))))

  ;; from https://www.nsa.gov/ia/_files/nist-routines.pdf
  ;;      http://csrc.nist.gov/groups/ST/toolkit/documents/dss/NISTReCur.pdf
  (define-constant P-192
    (let ((curve (make-fp-curve 
		  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF
		  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC
		  #x64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1)))
      `#(,curve 
	 ,(make-ec-point curve
			 #x188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012
			 #x07192B95FFC8DA78631011ED6B24CDD573F977A11E794811)
	 #xFFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831
	 1 ;; (div (- 192 1) 160)
	 ;;3045AE6FC8422F64ED579528D38120EAE12196D5
	 #vu8(#x30 #x45 #xAE #x6F #xC8 #x42 #x2F #x64 #xED #x57
	      #x95 #x28 #xD3 #x81 #x20 #xEA #xE1 #x21 #x96 #xD5)
	 )))

  (define-constant P-224
    (let ((curve (make-fp-curve 
		  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001
		  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE
		  #xB4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4)))
      `#(,curve 
	 ,(make-ec-point 
	   curve
	   #xB70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21
	   #xBD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34)
	 #xFFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D
	 1 ;; (div (- 224 1) 160)
	 ;; BD71344799D5C7FCDC45B59FA3B9AB8F6A948BC5
	 #vu8(#x30 #x45 #xAE #x6F #xC8 #x42 #x2F #x64 #xED #x57
	      #x95 #x28 #xD3 #x81 #x20 #xEA #xE1 #x21 #x96 #xD5)
	 )))
)
