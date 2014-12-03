;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; comparators.scm: Comparators
;;;  
;;;   Copyright (c) 2014  Takashi Kato  <ktakashi@ymail.com>
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

(library (sagittarius comparators)
  (export <comparator> comparator? make-comparator
	  comparator-comparison-procedure?
	  comparator-hash-function?
	  comparator-type-test-procedure
	  comparator-equality-predicate
	  comparator-comparison-procedure
	  comparator-hash-function
	  comparator-test-type
	  comparator-check-type
	  comparator-equal?
	  comparator-compare
	  comparator-hash
	  ;; built-in comparatorss
	  eq-comparator
	  eqv-comparator
	  equal-comparator
	  string-comparator

	  ;; others
	  make-comparison<
	  make-comparison>
	  make-comparison<=
	  make-comparison>=
	  make-comparison=/<
	  make-comparison=/>
	  )
  (import (rnrs)
	  (sagittarius)
	  (core inline)
	  (clos user))

  (define (make-comparator type-test equality comparison hash
			   :optional (name #f))
    ;; TODO parameter check
    (let ((eq (if (eq? equality #t)
		  (lambda (x y) (eqv? (comparison x y) 0))
		  equality)))
      (%make-comparator type-test eq comparison hash name)))

  (define eq-comparator (%eq-comparator))
  (define eqv-comparator (%eqv-comparator))
  (define equal-comparator (%equal-comparator))
  (define string-comparator (%string-comparator))
  
  (define-syntax define-comparison
    (syntax-rules ()
      ((_ (name . formals) expr)
       (begin
	 (define (name . formals)
	   expr)
	 (define-inliner name (sagittarius comparators)
	   ((_ . formals) expr))))))

  (define-comparison (make-comparison< <)
    (lambda (a b)
      (cond ((< a b) -1)
	    ((< b a) 1)
	    (else 0))))
  (define-comparison (make-comparison> >)
    (lambda (a b)
      (cond ((> a b) -1)
	    ((> b a) 1)
	    (else 0))))
  (define-comparison (make-comparison<= <=)
    (lambda (a b)
      (if (<= a b) (if (<= b a) 0 -1) 1)))

  (define-comparison (make-comparison>= >=)
    (lambda (a b)
      (if (>= a b) (if (>= b a) 0 1) -1)))

  (define-comparison (make-comparison=/< = <)
    (lambda (a b)
      (cond ((= a b) 0)
	    ((< a b) -1)
	    (else 1))))

  (define-comparison (make-comparison=/> = >)
    (lambda (a b)
      (cond ((= a b) 0)
	    ((> b a) 1)
	    (else -1))))

  )
