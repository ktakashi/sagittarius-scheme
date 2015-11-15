;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/generators - Generators
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

;; base library for generators
;; SRFI-121 will be implemented with this

(library (sagittarius generators)
    (export make-generator make-iota-generator

	    ;; predefined generator
	    null-generator

	    ;; operations
	    gcons* gappend gconcatenate gmerge gunion gintersection gmap gfold
	    gfilter gremove gfilter-map gstate-filter gbuffer-filter
	    gtake gdrop gtake-while gdrop-while gpairs gtuple glists
	    gvectors gstrings
	    gdelete gdelete-neighbor-dups
	    generator-fold generator-for-each
	    generator-collect generator-last generator-find generator-length
	    generator-count generator-any generator-every 
	    generator=? generator-unfold

	    ;; converter
	    generator->list generator->reverse-list)
    (import (rnrs)
	    (util list)
	    (srfi :26 cut)
	    (srfi :31 rec)
	    (sagittarius)
	    (sagittarius control))

  ;; constructors
  (define null-generator
    (let ((r (eof-object)))
      (lambda () r)))

  ;; make-generator :: [a ...] -> Generator a
  (define (make-generator . args)
    (let ((next args)
	  (e (null-generator)))
      (lambda ()
	(if (null? next)
	    e
	    (rlet1 r (car next)
	      (set! next (cdr next)))))))

  ;; make-iota-generator :: count (start (step))
  (define (make-iota-generator count :optional (start 0) (step 1))
    (let ((next start)
	  (i 0)
	  (e (null-generator)))
      (lambda ()
	(if (= i count)
	    e
	    (rlet1 r (+ next step)
	      (set! next r)
	      (set! i (+ count 1)))))))

  ;; converter
  (define (generator->reverse-list g . maybe-k) 
    (let ((k (and-let* ((not (null? maybe-k)))
	       (rlet1 k (car maybe-k)
		 (unless (and (integer? k) (positive? k))
		   (assertion-violation 'generator->list
					"non negative integer required" k))))))
      (let loop ((i 0) (r '()))
	(if (eqv? i k)
	    r
	    (let1 e (g)
	      (if (eof-object? e)
		  r
		  (loop (+ i 1) (cons e r))))))))
  (define (generator->list g . maybe-k)
    (let1 r (apply generator->reverse-list g maybe-k)
      (reverse! r)))

  ;; gcons* :: [Generator a] -> Generator a
  (define (gcons* . args)
    (cond ((null? args) null-generator)
	  ((null? (cdr args)) (car args))
	  (else 
	   (let ((v (car args)))
	     (set! args (cdr args))
	     v))))

  ;; gappend :: [Generator a] -> Generator a
  (define (gappend . args)
    (let ((gs args) (g #f))
      (if (null? args)
	  null-generator
	  (rec (f)
	    (unless g (set! g (car gs)) (set! gs (cdr gs)))
	    (let ((v (g)))
	      (cond ((not (eof-object? v)) v)
		    ((null? gs) v)
		    (else (set! g #f) (f))))))))

  ;; gconcatenate :: Generator Generator a -> Generator a
  (define (gconcatenate gen)
    (let ((g (gen)))
      (lambda ()
	(let loop ()
	  (if (eof-object? g)
	      g
	      (let ((v (g)))
		(if (eof-object? v)
		    (begin (set! g (gen)) (loop))
		    v)))))))

  ;; gmerge :: ((a, a) -> Bool, Generator a, Generator a, ...) -> Generator a
  ;; SRFI-121 requires comparator but procedures is more convenient i guess.
  (define gmerge
    (case-lambda
     ((proc gen) gen)
     ((proc gen1 gen2)
      (let ((e1 (gen1))
	    (e2 (gen2)))
	(lambda ()
	  (if (eof-object? e1)
	      (if (eof-object? e2)
		  e2
		  (begin0 e2 (set! e2 (gen2))))
	      (if (eof-object? e2)
		  (begin0 e1 (set! e1 (gen1))))
		  (if (proc e1 e2)
		      (begin0 e1 (set! e1 (gen1)))
		      (begin0 e2 (set! e2 (gen2))))))))
     ((proc . gens)
      (apply gmerge proc (map (cut gmerge proc <>) (slices gens 2))))))

  ;; gmap :: (a -> b, Generator a) -> Generator b
  (define gmap
    (case-lambda
     ((fn gen)
      (lambda () (let ((v (g))) (if (eof-object? v) v (fn v)))))
     ;; TODO how to merge generators?
     #;
     ((fn gen . more)
      (let ((gs ()))))))
)

	    