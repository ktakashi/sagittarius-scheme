;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/combinators - Combinators
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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


;; $ suffix is taken from Gauche for partial application
(library (sagittarius combinators)
    (export compose reverse-compose 
	    (rename (compose .$)
		    ;; does this make sense?
		    (reverse-compose $.))
	    kestrel thrush ~> cardinal
	    idiot starling
	    ;; formal names
	    (rename (idiot identity)
		    (kestrel constant)
		    (cardinal flip)
		    (starling substitution))
	    
	    pa$ (rename (pa$ partial-apply$))
	    apply$ map$ for-each$
	    complement
	    any-pred every-pred
	    ;; R6RS uses exists and for-all
	    (rename (any-pred exists-pred)
		    (every-pred for-all-pred))
	    )
    (import (rnrs)
	    (only (srfi :1) reverse! fold))

;; 2 arguments as helper
(define (compose0 f g)
  (lambda args
    (let-values ((r (apply g args)))
      (apply f r))))

;; compose f g -> (a -> f(g(a)))
(define (compose f . g*)
  (cond ((null? g*) f)
	((null? (cdr g*)) (compose0 f (car g*)))
	(else (fold-left compose0 f g*))))

;; reverse-compose f g -> (a -> g(f(a)))
(define (reverse-compose f . g*)
  (cond ((null? g*) f)
	((null? (cdr g*)) (compose0 (car g*) f))
	(else (fold compose0 f g*))))

;; from https://github.com/raganwald-deprecated/homoiconic
;;  and https://github.com/fantasyland/fantasy-birds
;; Kestrel (K-Combinator) Kxy = x
(define (kestrel x . x*)
  (if (null? x*)
      (lambda y x)
      ;; not sure if this is correct...
      (lambda y (apply values x x*))))

;; Thrush Txy = yx
(define (thrush x . x*)
  (if (null? x*)
      (lambda (f . f*)
	((apply reverse-compose f f*) x))
      (lambda (f . f*)
	(apply (apply reverse-compose f f*) x x*))))

;; threading macro (for convenience)
(define-syntax ~>
  (syntax-rules ()
    ((_ x f f* ...)
     ((thrush x) f f* ...))))

;; Cardinal (C-Combinator) Cxyz = xzy
(define (cardinal x0 . x*)
  (define x (apply reverse-compose x0 x*))
  (lambda (y . y*)
    (lambda (z0 . z*)
      (define z (apply reverse-compose z0 z*))
      (let ((xz (x z)))
	(if (null? y*)
	    (xz y)
	    (apply xz y y*))))))

;; Starling (S-Combinator) Sxyz = xz(yz)
(define (starling x0 . x*)
  (define x (apply reverse-compose x0 x*))
  (lambda (y0 . y*)
    (define y (apply reverse-compose y0 y*))
    (lambda (z0 . z*)
      (if (null? z*)
	  (let-values ((r (y z0)))
	    (apply (x z0) r))
	  (let-values ((r (apply y z0 z*)))
	    (apply (apply x z0 z*) r))))))

;; Idiot (I-Combinator, identity) Ix = x
;; Should we define like this?
;; (define (idiot a) a)
(define idiot values)

;; found in Gauche's document
(define pa$
  (case-lambda
   ((proc) (lambda m (apply proc m)))
   ((proc arg) (lambda m (apply proc arg m)))
   ((proc arg . args)
    (lambda m (apply proc arg (append args m))))))

(define (apply$ proc) (pa$ apply proc))
(define (map$ proc) (pa$ map proc))
(define (for-each$ proc) (pa$ for-each proc))

(define (complement pred) (lambda args (not (apply pred args))))
(define (any-pred pred . pred*)
  (lambda args
    (or (apply pred args)
	(and (not (null? pred*))
	     (exists (lambda (p) (apply p args)) pred*)))))
(define (every-pred pred . pred*)
  (lambda args
    (and (apply pred args)
	 (or (null? pred*)
	     (for-all (lambda (p) (apply p args)) pred*)))))

)
