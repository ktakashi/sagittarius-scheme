;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a127/lseqs.scm - Lazy sequence
;;;
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :127 lseqs)
    (export ;; Constructors
	    generator->lseq
	    ;; Predicates
	    lseq? lseq=?
	    ;; Selectors
	    lseq-car     lseq-cdr
	    lseq-first   lseq-rest lseq-ref
	    lseq-take    lseq-drop
	    ;; The whole lazy sequence
	    lseq-realize lseq->generator
	    lseq-length
	    lseq-append  lseq-zip
	    ;; Mapping and filtering
	    lseq-map        lseq-for-each
	    lseq-filter     lseq-remove
	    ;; Searching
	    lseq-find         lseq-find-tail 
	    ;; Do we need this?
	    (rename lseq-find-tail lseq-find-rest)
	    lseq-any          lseq-every
	    lseq-index
	    lseq-take-while   lseq-drop-while
	    lseq-member       lseq-memq     lseq-memv)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (sagittarius generators)
	    (sagittarius control))

(define (generator->lseq gen)
  (let ((obj (gen)))
    (if (eof-object? obj)
	'()
	(cons obj gen))))

(define-inline (lseq-car lseq) (car lseq))
(define lseq-first      lseq-car)
(define-inline (lseq-cdr lseq)
  (let ((g? (cdr lseq)))
    (if (procedure? g?)
	(let* ((o (g?))
	       (d (if (eof-object? o) '() (cons o g?))))
	  (set-cdr! lseq d)
	  d)
	g?)))
(define lseq-rest      lseq-cdr)

(define (lseq? o)
  (or (null? o) ;; seems ok from test
      ;; TODO should we check arity?
      (and (pair? o) 
	   (or (procedure? (cdr o))
	       (lseq? (cdr o))))))

;; NB: it won't stop if both sequences are infinite....
(define (lseq=? = lsq1 lsq2)
  (cond ((and (null? lsq1) (null? lsq2)))
	((or (null? lsq1) (null? lsq2)) #f)
	(else
	 (and (= (lseq-car lsq1) (lseq-car lsq2))
	      (lseq=? = (lseq-cdr lsq1) (lseq-cdr lsq2))))))

(define (lseq-take lseq i)
  (unless (integer? i)
    (assertion-violation 'lseq-take "integer is required" i))
  (when (< i 0)
    (assertion-violation 'lseq-take "zero or positive integer is required" i))
  (generator->lseq
   (lambda ()
     (if (zero? i)
	 (eof-object)
	 (let ((r (lseq-car lseq)))
	   (set! lseq (lseq-cdr lseq))
	   (set! i (- i 1))
	   r)))))

(define (lseq-drop lseq n) 
  (unless (integer? n)
    (assertion-violation 'lseq-take "integer is required" n))
  (when (< n 0)
    (assertion-violation 'lseq-take "zero or positive integer is required" n))
  (let loop ((i 0) (lseq lseq))
    (if (= i n)
	lseq
	(loop (+ i 1) (lseq-cdr lseq)))))

(define (lseq-ref lseq i)
  (lseq-first (lseq-drop lseq i)))

;; lseq-length follows the lazy sequence so this is fine.
;; TODO better way
(define (lseq-realize lseq) (lseq-length lseq) lseq)
(define (lseq->generator lseq)
  (lambda ()
    (if (null? lseq)
	(eof-object)
	(let ((o (lseq-car lseq)))
	  (set! lseq (lseq-cdr lseq))
	  o))))
(define (lseq-length lseq)
  (let loop ((i 0) (lseq lseq))
    (if (null? lseq)
	i
	(loop (+ i 1) (lseq-cdr lseq)))))

;; TODO better implementation...
(define (lseq-append . lseq)
  (define gens (map lseq->generator lseq))
  (generator->lseq 
   (lambda ()
     (let loop ()
       (if (null? gens)
	   (eof-object)
	   (let* ((gen (car gens))
		  (o (gen)))
	     (cond ((eof-object? o)
		    (set! gens (cdr gens))
		    (loop))
		   (else o))))))))
	   

(define (lseq-zip lseq . lseqs) (apply lseq-map list lseq lseqs))

;; TODO better way
(define (lseq-map proc lseq . lseqs)
  (generator->lseq
   (apply gmap proc (lseq->generator lseq) (map lseq->generator lseqs))))
(define (lseq-for-each proc lseq . lseqs)
  (if (null? lseqs)
      (for-each proc (lseq-realize lseq))
      ;; in case we have multiple lseqs, then we make sure the process
      ;; stops when the shortest lseq is realised.
      ;; TODO this is a bit memory inefficient.
      (apply generator-for-each proc
	     (lseq->generator lseq) (map lseq->generator lseqs))))

(define (lseq-filter pred lseq)
  (generator->lseq
   (lambda ()
     (let loop ()
       (if (null? lseq) 
	   (eof-object)
	   (let ((o (lseq-car lseq)))
	     (set! lseq (lseq-cdr lseq))
	     (if (pred o)
		 o
		 (loop))))))))
(define-inline (lseq-remove pred lseq) 
  (lseq-filter (lambda (o) (not (pred o))) lseq))


(define (lseq-find pred lseq)
  (let ((l (lseq-find-tail pred lseq)))
    (and l (lseq-car l))))

(define (lseq-find-tail pred lseq)
  (let loop ((lseq lseq))
    (cond ((null? lseq) #f)
	  ((pred (lseq-car lseq)) lseq)
	  (else (loop (lseq-cdr lseq))))))


(define (lseq-any pred lseq . lseqs)
  (define (lseq-any1 pred lseq)
    (let loop ((lseq lseq))
      (cond ((null? lseq) #f)
	    ((pred (lseq-car lseq)))
	    (else (loop (lseq-cdr lseq))))))
  (if (null? lseqs)
      (lseq-any1 pred lseq)
      (let loop ((lseqs (cons lseq lseqs)))
	(cond ((exists null? lseqs) #f)
	      ((apply pred (map lseq-car lseqs)))
	      (else (loop (map lseq-cdr lseqs)))))))

(define (lseq-every pred lseq . lseqs)
  (define (lseq-every1 pred lseq)
    (let loop ((lseq lseq) (v #f))
      (cond ((null? lseq) v)
	    ((pred (lseq-car lseq)) =>
	     (lambda (v)
	       (loop (lseq-cdr lseq) v)))
	    (else #f))))
  (if (null? lseqs)
      (lseq-every1 pred lseq)
      (let loop ((lseqs (cons lseq lseqs)) (v #f))
	(cond ((exists null? lseqs) v)
	      ((apply pred (map lseq-car lseqs)) =>
	       (lambda (v)
		 (loop (map lseq-cdr lseqs) v)))
	      (else #f)))))

(define (lseq-take-while pred lseq)
  (generator->lseq
   (lambda ()
     (if (null? lseq)
	 (eof-object)
	 (let ((r (lseq-car lseq)))
	   (cond ((pred r) (set! lseq (lseq-cdr lseq)) r)
		 (else (eof-object))))))))

(define (lseq-drop-while pred lseq)
  (lseq-find-tail (lambda (o) (not (pred o))) lseq))

(define (lseq-index pred lseq . lseqs)
  (define (lseq-index1 pred lseq)
    (let loop ((lseq lseq) (i 0))
      (cond ((null? lseq) #f)
	    ((pred (lseq-car lseq)) i)
	    (else (loop (lseq-cdr lseq) (+ i 1))))))
  (if (null? lseqs)
      (lseq-index1 pred lseq)
      (let loop ((lseqs (cons lseq lseqs)) (i 0))
	(cond ((exists null? lseqs) #f)
	      ((apply pred (map lseq-car lseqs)) i)
	      (else 
	       (loop (map lseq-cdr lseqs) (+ i 1)))))))

(define-inline (lseq-memq x lseq) (lseq-find-tail (lambda (o) (eq? o x)) lseq))
(define-inline (lseq-memv x lseq) (lseq-find-tail (lambda (o) (eqv? o x)) lseq))
(define-inline (lseq-member x lseq :optional (= equal?) )
  (lseq-find-tail (lambda (o) (= o x)) lseq))
)
