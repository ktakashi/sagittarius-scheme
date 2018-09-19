;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/jmespath/compiler.scm - JMESPath compiler
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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

;; Reference
;; http://jmespath.org/specification.html
#!nounbound
(library (text json jmespath compiler)
    (export jmespath:compile)
    (import (rnrs)
	    (srfi :133 vectors))

(define-record-type jmespath-eval-context
  (fields source parent))
(define (make-root-context source)
  (make-jmespath-eval-context source #f))
(define (make-child-context json parent)
  (make-jmespath-eval-context json parent))

;; from OrExpressions section
(define (false-value? v)
  (or (null? v)
      (and (vector? v) (zero? (vector-length v)))
      (and (string? v) (zero? (string-length v)))
      (not v)
      (eqv? v 'null)))

;; receives AST parsed by (text json jmespath compiler)
;; and returns a procedure takes one argument, sexp JSON
(define (jmespath:compile jmespath-ast)
  (define expression (compile-expression jmespath-ast))
  (lambda (json)
    (define root-context (make-root-context json))
    (expression json root-context)))

(define (compile-expression e)
  (cond ((string? e) (jmespath:compile-identifier e))
	((pair? e)
	 (case (car e)
	   ((->) (jmespath:compile-sub-expression e))
	   ((not) (jmespath:compile-not-expression e))
	   ((index) (jmespath:compile-index-expression e))
	   ((slice) (jmespath:compile-slice-expression e))
	   ((or) (jmespath:compile-or-expression e))
	   ((and) (jmespath:compile-and-expression e))
	   ((function) (jmespath:compile-function e))
	   (else (error 'compile-jmespath "Unsupported command" e))))))
  
(define (jmespath:compile-identifier s)
  (define key=? (lambda (k&v) (and (string=? s (car k&v)) k&v)))
  (lambda (json context)
    (if (vector? json)
	(cond ((vector-any key=? json) => cdr)
	      (else 'null))
	'null)))

(define (jmespath:compile-sub-expression e)
  (let ((e* (map compile-expression (cdr e))))
    (lambda (json context)
      (let loop ((json json) (context context) (e* e*))
	(if (null? e*)
	    json
	    (let ((v ((car e*) json context)))
	      (loop v (make-child-context v context) (cdr e*))))))))

(define (jmespath:compile-index-expression e)
  (let ((n (cadr e)))
    (cond ((eq? n '*) (lambda (json _) (if (list? json) json 'null)))
	  ((and (exact? n) (integer? n))
	   (lambda (json _)
	     (if (list? json)
		 (let ((l (length json)))
		   (cond ((and (<= 0 n) (< n l))
			  (list-ref json n))
			 ((and (negative? n) (< (- (abs n) 1) l))
			  (list-ref json (+ l n)))
			 (else 'null)))
		 'null)))
	  (else (assertion-violation 'jmespath:compile
		  "Index must have either exact integer or *" e)))))

(define (jmespath:compile-slice-expression e)
  (define (get n l step positive-default negative-default)
    (cond ((not n) (if (negative? step) negative-default positive-default))
	  ((negative? n) (+ l n))
	  (else n)))
  (let ((start (cadr e))
	(end (caddr e))
	(step (cadddr e)))
    (unless (number? step)
      (assertion-violation 'jmespath:compile "step must be a number" e))
    (when (zero? step)
      (assertion-violation 'jmespath:compile "step can't be 0" e))
    (let ((cmp (if (negative? step) < >=)))
      (lambda (json _)
	(if (list? json)
	    (let* ((l (length json))
		   (s (get start l step 0 (- l 1)))
		   (e (get end l step l 0)))
	      ;; TODO slow?
	      (let loop ((i s) (r '()))
		(if (cmp i e)
		    (reverse r)
		    (loop (+ i step) (cons (list-ref json i) r)))))
	    'null)))))

(define (jmespath:compile-or-expression e)
  (let ((e* (map compile-expression (cdr e))))
    (lambda (json context)
      (let loop ((e* e*))
	(if (null? e*)
	    'null
	    (let ((v ((car e*) json context)))
	      (if (false-value? v)
		  (loop (cdr e*))
		  v)))))))

(define (jmespath:compile-and-expression e)
  (when (null? e)
    (assertion-violation 'jmespath:compile "And must have at least one expression"))
  (let ((e* (map compile-expression (cdr e))))
    (lambda (json context)
      (let ((v ((car e*) json context)))
	(if (false-value? v)
	    v
	    (let loop ((e* (cdr e*)) (v v))
	      (if (null? e*)
		  v
		  (let ((v2 ((car e*) json context)))
		    (if (false-value? v2)
			v2
			(loop (cdr e*) v2))))))))))

(define (jmespath:compile-not-expression e)
  (let ((e (compile-expression (cadr e))))
    (lambda (json context)
      (let ((v (e json context)))
	;; false-like values?
	(false-value? v)))))

(define (jmespath:compile-function e)
  (define (lookup-function name)
    (cond ((assq name +jmespath:buildin-functions+) => cdr)
	  ;; TODO user defined function
	  (else #f)))
  (let ((func (lookup-function (string->symbol (cadr e))))
	(e* (map compile-expression (cddr e))))
    (unless func
      (assertion-violation 'jmespath:compile "No such function" (cadr e)))
    (lambda (json context)
      (let ((args (map (lambda (e) (e json context)) e*)))
	(apply func context args)))))
(define (jmespath:parent-function context)
  (let ((parent (jmespath-eval-context-parent context)))
    (if parent
	(jmespath-eval-context-source parent)
	'null)))
(define +jmespath:buildin-functions+
  `(
    ;; This is not standard but we want it
    (parent . ,jmespath:parent-function)
    ))
)
