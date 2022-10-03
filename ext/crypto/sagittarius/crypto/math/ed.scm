;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/math/ed.scm - Edwards curve utilities
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
(library (sagittarius crypto math ed)
    (export eddsa-parameter?
	    eddsa-parameter-name
	    eddsa-parameter-p
	    eddsa-parameter-b
	    eddsa-parameter-c
	    eddsa-parameter-n
	    eddsa-parameter-d
	    eddsa-parameter-a
	    eddsa-parameter-B
	    eddsa-parameter-L

	    ed25519-parameter
	    ed448-parameter

	    ed-point-add
	    ed-point-double
	    ed-point-mul
	    ed-point=?
	    ed-point-encode-base
	    ed-point-decode-base)
    (import (rnrs)
	    (core misc)
	    (math ec) ;; TODO should we move to (sagittarius crypto math ec)?
	    (math modular)
	    (sagittarius)
	    (sagittarius crypto random))

(define-vector-type ed-point
  (make-ed-point x y z t) ed-point?
  (x ed-point-x)
  (y ed-point-y)
  (z ed-point-z)
  (t ed-point-t))
;;; Parameters
(define-vector-type eddsa-parameter
  (make-eddsa-parameter name p b c n d a B L) eddsa-parameter?
  (name eddsa-parameter-name)
  (p eddsa-parameter-p)
  (b eddsa-parameter-b)
  (c eddsa-parameter-c)
  (n eddsa-parameter-n)
  (d eddsa-parameter-d)
  (a eddsa-parameter-a)
  (B eddsa-parameter-B)
  (L eddsa-parameter-L))

;; 5.1 Ed25519ph, Ed25519ctx, and Ed25519
(define ed25519-parameter
  (let ((xb #x216936d3cd6e53fec0a4e231fdd6dc5c692cc7609525a7b2c9562d608f25d51a)
	(yb #x6666666666666666666666666666666666666666666666666666666666666658)
	(p (ec-field-fp-p
	    (elliptic-curve-field (ec-parameter-curve curve25519))))
	(d #x52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3))
    (make-eddsa-parameter
     'ed25519
     p					       ;; p = 2^255 - 19
     256				       ;; b
     3					       ;; c 
     254				       ;; n
     d					       ;; d
     -1					       ;; a
     (make-ed-point xb yb 1 (mod (* xb yb) p)) ;; B (x y z t)
     (ec-parameter-n curve25519) ;; L (ec-parameter-n = order)
     )))

;; once we have short Weierstrass form in (math ec), do like above for p and L
(define ed448-parameter
  (let ((p (- (expt 2 448) (expt 2 224) 1))
	(xb #x4f1970c66bed0ded221d15a622bf36da9e146570470f1767ea6de324a3d3a46412ae1af72ab66511433b80e18b00938e2626a82bc70cc05e)
	(yb #x693f46716eb6bc248876203756c9c7624bea73736ca3984087789c1e05a0c2d73ad3ff1ce67c39c4fdbd132c4ed7c8ad9808795bf230fa14)
	(L (- (expt 2 446) #x8335dc163bb124b65129c96fde933d8d723a70aadc873d6d54a7bb0d)))
    (make-eddsa-parameter
     'ed448
     p			       ;; p
     456		       ;; b
     2			       ;; c
     447		       ;; n
     -39081		       ;; d
     1			       ;; a
     (make-ed-point xb yb 1 0) ;; B (x y z t) note: t isn't used
     L			       ;; L
    )))

;; Based on RFC 8032 Appendix A
;; field calculation (modular arith)
(define (ed-field-add p x y) (mod-add x y p))
(define (ed-field-sub p x y) (mod-sub x y p))
(define (ed-field-mul p x y) (mod-mul x y p))
;;(define (ed-field-inv p x)   (mod (mod-expt x (- p 2) p) p))
(define (ed-field-inv p x)   (mod-inverse x p))
(define (ed-field-div p x y) (mod-div x y p))
  ;; (ed-field-mul p x (ed-field-inv p y)))
(define (ed-field-zero? p x) (zero? x))
(define (ed-field-sign p x)  (mod x 2))
(define (ed-field-negate p x) (mod-negate x p))
(define (ed-field-sqrt p x) (mod-sqrt x p))
	
(define (bytevector->ed-field p bv b)
  (let ((rv (mod (bytevector->integer/endian bv (endianness little))
		 (expt 2 (- b 1)))))
    (and (< rv p) rv)))

;; Ed point calculation
(define ed-point-zero
  (let ((zero (make-ed-point 0 1 1 0)))
    (lambda (parameter)
      zero)))

(define (ed-point-add parameter x y)
  (case (eddsa-parameter-name parameter)
    ((ed25519) (ed25519-point-add parameter x y))
    ((ed448) (ed448-point-add parameter x y))
    (else (assertion-violation 'ed-point-add "Not supported"
			       (eddsa-parameter-name parameter)))))
(define (ed25519-point-add parameter x y)
  (define p (eddsa-parameter-p parameter))
  (define d (eddsa-parameter-d parameter))
  (define xx (ed-point-x x))
  (define xy (ed-point-y x))
  (define xt (ed-point-t x))
  (define xz (ed-point-z x))
  (define yx (ed-point-x y))
  (define yy (ed-point-y y))
  (define yt (ed-point-t y))
  (define yz (ed-point-z y))
  (let* ((zcp (ed-field-mul p xz yz))
	 (A (ed-field-mul p (ed-field-sub p xy xx) (ed-field-sub p yy yx)))
	 (B (ed-field-mul p (ed-field-add p xy xx) (ed-field-add p yy yx)))
	 (C (ed-field-mul p (ed-field-add p d d) (ed-field-mul p xt yt)))
	 (D (ed-field-add p zcp zcp))
	 (E (ed-field-sub p B A))
	 (H (ed-field-add p B A))
	 (F (ed-field-sub p D C))
	 (G (ed-field-add p D C)))
    (make-ed-point (ed-field-mul p E F) (ed-field-mul p G H)
		   (ed-field-mul p F G) (ed-field-mul p E H))))
(define (ed448-point-add parameter x y)
  (define p (eddsa-parameter-p parameter))
  (define d (eddsa-parameter-d parameter))
  (define xx (ed-point-x x))
  (define xy (ed-point-y x))
  (define xz (ed-point-z x))
  (define yx (ed-point-x y))
  (define yy (ed-point-y y))
  (define yz (ed-point-z y))
  (let* ((xcp (ed-field-mul p xx yx))
	 (ycp (ed-field-mul p xy yy))
	 (zcp (ed-field-mul p xz yz))
	 (B (ed-field-mul p zcp zcp))
	 (E (ed-field-mul p d (ed-field-mul p xcp ycp)))
	 (F (ed-field-sub p B E))
	 (G (ed-field-add p B E)))
    (make-ed-point (ed-field-mul p
		    (ed-field-mul p zcp F)
		    (ed-field-sub p
		     (ed-field-mul p 
		      (ed-field-add p xx xy)
		      (ed-field-add p yx yy))
		     (ed-field-add p xcp ycp)))
		   (ed-field-mul p
		    (ed-field-mul p zcp G) (ed-field-sub p ycp xcp))
		   (ed-field-mul p F G)
		   0)))

(define (ed-point-double parameter p)
  (case (eddsa-parameter-name parameter)
    ((ed25519) (ed25519-point-double parameter p))
    ((ed448) (ed448-point-double parameter p))
    (else (assertion-violation 'ed-point-double "Not supported"
			       (eddsa-parameter-name parameter)))))
(define (ed25519-point-double parameter e)
  (define p (eddsa-parameter-p parameter))
  (define x (ed-point-x e))
  (define y (ed-point-y e))
  (define z (ed-point-z e))
  (let* ((A (ed-field-mul p x x))
	 (B (ed-field-mul p y y))
	 (Ch (ed-field-mul p z z))
	 (C (ed-field-add p Ch Ch))
	 (H (ed-field-add p A B))
	 (xys (ed-field-add p x y))
	 (E (ed-field-sub p H (ed-field-mul p xys xys)))
	 (G (ed-field-sub p A B))
	 (F (ed-field-add p C G)))
    (make-ed-point (ed-field-mul p E F) (ed-field-mul p G H)
		   (ed-field-mul p F G) (ed-field-mul p E H))))

(define (ed448-point-double parameter e)
  (define p (eddsa-parameter-p parameter))
  (define x (ed-point-x e))
  (define y (ed-point-y e))
  (define z (ed-point-z e))
  (let* ((x1s (ed-field-mul p x x))
	 (y1s (ed-field-mul p y y))
	 (z1s (ed-field-mul p z z))
	 (xys (ed-field-add p x y))
	 (F (ed-field-add p x1s y1s))
	 (J (ed-field-sub p F (ed-field-add p z1s z1s))))
    (make-ed-point (ed-field-mul p
		    (ed-field-sub p
		     (ed-field-sub p (ed-field-mul p xys xys) x1s) y1s) J)
		   (ed-field-mul p F (ed-field-sub p x1s y1s))
		   (ed-field-mul p F J)
		   0)))

;; scalar multiplication
(define (ed-point-mul parameter p k)
  (unless (integer? k)
    (assertion-violation 'ed-point-mul "integer required for k" k))
  (do ((r (ed-point-zero parameter)
	  (if (odd? k) (ed-point-add parameter r s) r))
       (s p (ed-point-double parameter s))
       (k k (bitwise-arithmetic-shift-right k 1)))
      ((<= k 0) r)))

(define (ed-point=? parameter x y)
  (define p (eddsa-parameter-p parameter))
  (define xx (ed-point-x x))
  (define xy (ed-point-y x))
  (define xz (ed-point-z x))
  (define yx (ed-point-x y))
  (define yy (ed-point-y y))
  (define yz (ed-point-z y))
  (let ((xn1 (ed-field-mul p xx yz))
	(xn2 (ed-field-mul p yx xz))
	(yn1 (ed-field-mul p xy yz))
	(yn2 (ed-field-mul p yy xz)))
    (and (= xn1 xn2) (= yn1 yn2))))

(define (ed-point-solve-x2 parameter x y)
  (case (eddsa-parameter-name parameter)
    ((ed25519) (ed25519-point-solve-x2 parameter x y))
    ((ed448) (ed448-point-solve-x2 parameter x y))
    (else (assertion-violation 'ed-point-solve-x2 "Not supported"
			       (eddsa-parameter-name parameter)))))
(define (ed25519-point-solve-x2 parameter x y)
  (define p (eddsa-parameter-p parameter))
  (define d (eddsa-parameter-d parameter))
  (ed-field-div p
		(ed-field-sub p (ed-field-mul p y y) 1)
		(ed-field-add p (ed-field-mul p (ed-field-mul p d y) y) 1)))
(define (ed448-point-solve-x2 parameter x y)
  (define p (eddsa-parameter-p parameter))
  (define d (eddsa-parameter-d parameter))
  (ed-field-div p
		(ed-field-sub p (ed-field-mul p y y) 1)
		(ed-field-sub p (ed-field-mul p (ed-field-mul p d y) y) 1)))
;; encode
(define (ed-point-encode-base parameter point b)
  (define p (eddsa-parameter-p parameter))
  (define x (ed-point-x point))
  (define y (ed-point-y point))
  (define z (ed-point-z point))
  (let ((xp (ed-field-div p x z))
	(yp (ed-field-div p y z)))
    (let ((s (integer->bytevector/endian yp (endianness little) (div b 8))))
      (when (odd? xp)
	(bytevector-u8-set! s (div (- b 1) 8)
	 (bitwise-ior
	  (bytevector-u8-ref s (div (- b 1) 8))
	  (bitwise-arithmetic-shift-left 1 (mod (- b 1) 8)))))
      s)))

;; decode
(define (ed-point-decode-base parameter s b)
  (define p (eddsa-parameter-p parameter))
  (and-let* (( (= (bytevector-length s) (div b 8)) )
	     (xs (bitwise-arithmetic-shift-right
		  (bytevector-u8-ref s (div (- b 1) 8))
		  (bitwise-and (- b 1) 7)))
	     (y (bytevector->ed-field p s b))
	     (x (ed-field-sqrt p (ed-point-solve-x2 parameter s y)))
	     ( (not (and (ed-field-zero? p x)
			 (not (= xs (ed-field-sign p x))))) )
	     (x (if (= (ed-field-sign p x) xs) x (ed-field-negate p x))))
    ;; NOTE: t is only used on Ed25519, Ed448 doesn't use it
    (make-ed-point x y 1 (ed-field-mul p x y))))

)
