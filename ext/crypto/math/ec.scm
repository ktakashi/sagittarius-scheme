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
	    ec-point-mul
	    ;; NIST parameters
	    P-192
	    P-224

	    ec-parameter?
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
  (define-vector-type ec-field-f2m (make-ec-field-f2m m rp) ec-field-f2m?
    (m  ec-field-f2m-m)
    (rp ec-field-f2m-rp))

  (define-vector-type ec-curve (make-elliptic-curve field a b) elliptic-curve?
    (field elliptic-curve-field)
    (a     elliptic-curve-a)
    (b     elliptic-curve-b))
  

  (define (ec-curve=? a b) (equal? a b))

  ;; EC point
  (define-vector-type ec-point (make-ec-point x y) ec-point?
    (x     ec-point-x)
    (y     ec-point-y))

  (define ec-infinity-point (make-ec-point #f #f))

  ;; we don't check x and y, these can be #f for infinite point
  (define (ec-point-infinity? p)
    (or (not (ec-point-x p))
	(not (ec-point-y p))))
  
  (define (ec-point=? a b) (equal? a b))

  (define (ec-point-twice curve x)
    (define (fp-ec-point-twice curve field x)
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
	(make-ec-point x3 y3)))
    
    (define (f2m-ec-point-twice curve field x)
      (error 'ec-point-twice "not supported yet" x))
    (cond ((ec-point-infinity? x) x)
	  ((zero? (ec-point-y x)) ec-infinity-point)
	  (else
	   ;; TODO dispatch
	   (let ((field (elliptic-curve-field curve)))
	     (if (ec-field-fp? field)
		 (fp-ec-point-twice curve field x)
		 (f2m-ec-point-twice curve field x))))))

  (define (ec-point-add curve x y)
    (define (fp-ec-point-add field x y)
      (let* ((xx (ec-point-x x))
	     (xy (ec-point-y x))
	     (yx (ec-point-x y))
	     (yy (ec-point-y y))
	     ;; if the curve are the same then p are the same
	     (p (ec-field-fp-p field))
	     ;; gamma = (yy - xy)/(yx-xx)
	     (gamma (mod-div (mod-sub yy xy p) (mod-sub yx xx p) p))
	     ;; x3 = gamma^2 - xx - yx
	     (x3 (mod-sub (mod-sub (mod-square gamma p) xx p) yx p))
	     ;; y3 = gamma*(xx - x3) - xy
	     (y3 (mod-sub (mod-mul gamma (mod-sub xx x3 p) p) xy p)))
	(make-ec-point x3 y3)))
    (define (f2m-ec-point-add field x y)
      (error 'ec-point-add "not supported yet" x y))
    (cond ((ec-point-infinity? x) y)
	  ((ec-point-infinity? y) x)
	  ((= (ec-point-x x) (ec-point-x y))
	   (if (= (ec-point-y x) (ec-point-y y))
	       (ec-point-twice x)
	       ec-infinity-point))
	  (else
	   ;; TODO dispatch
	   (let ((field (elliptic-curve-field curve)))
	     (if (ec-field-fp? field)
		 (fp-ec-point-add field x y)
		 (f2m-ec-point-add field x y))))))

  (define (ec-point-negate curve x)
    ;; TODO dispatch
    (let ((field (elliptic-curve-field curve)))
      (if (ec-field-fp? field)
	  (make-ec-point (ec-point-x x) 
			 (mod-negate (ec-point-y x) (ec-field-fp-p field)))
	  (error 'ec-point-negate "not supported yet"))))

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
    (and (vector? o) (= (vector-length o) 6) 
	 (eq? (vector-ref o 0) 'ec-parameter)))
  (define (ec-parameter-curve o) (vector-ref o 1))

  ;; from https://www.nsa.gov/ia/_files/nist-routines.pdf
  ;;      http://csrc.nist.gov/groups/ST/toolkit/documents/dss/NISTReCur.pdf
  (define-constant P-192
    `#(ec-parameter
       ,(make-elliptic-curve 
	 (make-ec-field-fp #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF)
	 #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC
	 #x64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1) 
       ,(make-ec-point #x188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012
		       #x07192B95FFC8DA78631011ED6B24CDD573F977A11E794811)
       #xFFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831
       1 ;; (div (- 192 1) 160)
       ;;3045AE6FC8422F64ED579528D38120EAE12196D5
       #vu8(#x30 #x45 #xAE #x6F #xC8 #x42 #x2F #x64 #xED #x57
	    #x95 #x28 #xD3 #x81 #x20 #xEA #xE1 #x21 #x96 #xD5)))

  (define-constant P-224
    `#(ec-parameter
       ,(make-elliptic-curve 
	 (make-ec-field-fp
	  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001)
	 #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE
	 #xB4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4)
       ,(make-ec-point
	 #xB70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21
	 #xBD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34)
       #xFFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D
       1 ;; (div (- 224 1) 160)
       ;; BD71344799D5C7FCDC45B59FA3B9AB8F6A948BC5
       #vu8(#xBD #x71 #x34 #x47 #x99 #xD5 #xC7 #xFC #xDC #x45
	    #xB5 #x9F #xA3 #xB9 #xAB #x8F #x6A #x94 #x8B #xC5)))
)
