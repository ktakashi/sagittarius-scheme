;;; -*- Scheme -*-
;;;
;;; eql.scm - eql specializer library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

;; The code is based on the following site:
;;  http://d.hatena.ne.jp/leque/20110105/p1
#!compatible
(library (sagittarius mop eql)
    (export <eql-specializable-generic> eql)
    (import (rnrs)
	    (sagittarius)
	    (clos user)
	    (clos core))

  (define-class <eql-specializer> (<class>)
    ((object :accessor eql-specializer-object
	     :init-keyword :eql-specializer-object)))

  (define-method write-object ((obj <eql-specializer>) port)
    (format port "#<eql-specializer (eql ~s)>" (eql-specializer-object obj)))

  (define (eql obj)
    (make <eql-specializer> :eql-specializer-object obj))

  (define-class <eql-specializable-generic> (<generic>) ())

  (define-method compute-applicable-methods ((gf <eql-specializable-generic>)
					     args)
    (define (specializer-match? sp obj)
      (or (and (is-a? sp <eql-specializer>)
	       (eqv? obj (eql-specializer-object sp)))
	  (is-a? obj sp)))
    (define (method-applicable? method)
      (let loop ((sps (method-specializers method))
		 (args args))
	(if (null? sps)
	    (or (null? args)
		(method-optional method))
	    (and (not (null? args))
		 (and (specializer-match? (car sps) (car args))
		      (loop (cdr sps) (cdr args)))))))
    (define (specializer-more-specific? a b arg)
      (or
       ;; eqv-ness check was done in compute-applicable-methods
       (is-a? a <eql-specializer>)
       (find (lambda (a) (eq? a b))
	     (cdr (member a (class-cpl (class-of arg)))))))
    (define (more-specific? a b)
      (let loop ((sp-a (method-specializers a))
		 (sp-b (method-specializers b))
		 (args args))
	(cond
	 ((and (null? sp-a) (null? sp-b))
	  (let ((opt-a (method-optional a))
		(opt-b (method-optional b)))
	    (if (eq? opt-a opt-b)
		(assertion-violation 'compute-applicable-methods
				     "two methods are equally specific"
				     a b)
		(and opt-a (not opt-b)))))
	 ((null? sp-a) #f)
	 ((null? sp-b) #t)
	 ((eq? (car sp-a) (car sp-b))
	  (loop (cdr sp-a) (cdr sp-b) (cdr args)))
	 (else
	  (specializer-more-specific? (car sp-a) (car sp-b) (car args))))))
    (list-sort more-specific?
	       (filter method-applicable? (generic-methods gf))))
)