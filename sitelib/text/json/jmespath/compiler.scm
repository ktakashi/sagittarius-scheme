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
	    (srfi :1 lists)
	    (srfi :133 vectors))

(define-record-type jmespath-eval-context
  (fields source parent wildcard?))
(define (make-root-context source)
  (make-jmespath-eval-context source #f #f))
(define (make-child-context json parent . wildcard?)
  (make-jmespath-eval-context json parent
			      (and (not (null? wildcard?))
				   (car wildcard?))))

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

(define (wildcard-expression? e)
  (or (eq? e '*) (equal? e '(index *))))
(define (compile-expression e)
  (cond ((string? e) (jmespath:compile-identifier e))
	((eq? e '*) (jmespath:compile-wilecard-expression e))
	((pair? e)
	 (case (car e)
	   ((ref) (jmespath:compile-sub-expression e))
	   ((not) (jmespath:compile-not-expression e))
	   ((index) (jmespath:compile-index-expression e))
	   ((slice) (jmespath:compile-slice-expression e))
	   ((filter) (jmespath:compile-filter-expression e))
	   ((or) (jmespath:compile-or-expression e))
	   ((and) (jmespath:compile-and-expression e))
	   ((< <= = >= > !=) (jmespath:compile-comparator-expression e))
	   ((quote) (jmespath:compile-literal-expression e))
	   ((function) (jmespath:compile-function e))
	   (else (error 'compile-jmespath "Unsupported command" e))))))
  
(define (jmespath:compile-identifier s)
  (define key=? (lambda (k&v) (and (string=? s (car k&v)) k&v)))
  (define (get json)
    (cond ((vector-any key=? json) => cdr)
	  (else 'null)))
  (lambda (json context)
    (cond ((vector? json) (get json))
	  ((and (list? json) (jmespath-eval-context-wildcard? context))
	   ;; okay it has to be list of vector
	   ;; FIXME it's ugly...
	   (if (for-all vector? json)
	       (let ((r (map (lambda (e) (if (vector? e) (get e) 'null)) json)))
		 (filter-map (lambda (e) (and (not (eq? e 'null)) e)) r))
	       'null))
	  (else 'null))))

(define (jmespath:compile-wilecard-expression e)
  (lambda (json context)
    (if (vector? json)
	(map cdr (vector->list json))
	'null)))

(define (jmespath:compile-sub-expression e)
  (let ((e* (map compile-expression (cdr e))))
    (lambda (json context)
      (let loop ((json json)
		 (context context)
		 (e* e*)
		 (e (cdr e))
		 (in-wildcard? #f))
	(if (null? e*)
	    json
	    ;; hmmm ugly...
	    (let ((v ((car e*) json context))
		  (in-wildcard? (or (wildcard-expression? (car e))
				    in-wildcard?)))
	      (loop v
		    (make-child-context v context in-wildcard?)
		    (cdr e*)
		    (cdr e)
		    in-wildcard?)))))))

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

(define (jmespath:compile-filter-expression e)
  (let ((e (compile-expression (cadr e))))
    (lambda (json context)
      (if (list? json)
	  (filter-map (lambda (elm)
			(let ((v (e elm (make-child-context elm context))))
			  (and (not (false-value? v)) elm))) json)
	  'null))))
			      
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
  (let ((e* (map compile-expression (cdr e))))
    (lambda (json context)
      (let loop ((e* e*) (v 'null))
	(if (null? e*)
	    v
	    (let ((v2 ((car e*) json context)))
	      (if (false-value? v2)
		  v2
		  (loop (cdr e*) v2))))))))

(define (jmespath:compile-not-expression e)
  (let ((e (compile-expression (cadr e))))
    (lambda (json context)
      (let ((v (e json context)))
	;; false-like values?
	(false-value? v)))))

(define (jmespath:compile-comparator-expression e)
  ;; hmmmm, we need to utilise this
  (define (json=? a b)
    (define (entry=? a b)
      (and (json=? (car a) (car b))
	   (json=? (cdr a) (cdr b))))
    (define (key-compare a b) (string<? (car a) (car b)))
    (cond ((and (string? a) (string? b)) (string=? a b))
	  ;; 1 and 1.0 are not the same so can't be = or equal?
	  ((and (number? a) (number? b)) (eqv? a b))
	  ((and (vector? a) (vector? b))
	   (vector-every entry=?
			 (vector-sort key-compare a)
			 (vector-sort key-compare b)))
	  ((and (list? a) (list? b)) (for-all json=? a b))
	  (else (eq? a b))))
  (let ((cmp (car e))
	(lhse (compile-expression (cadr e)))
	(rhse (compile-expression (caddr e))))
    (lambda (json context)
      (let ((lhs (lhse json context))
	    (rhs (rhse json context)))
	(case cmp
	  ((< <= >= >)
	   (if (and (number? lhs) (number? rhs))
	       (cond ((eq? cmp '<)  (< lhs rhs))
		     ((eq? cmp '>)  (> lhs rhs))
		     ((eq? cmp '<=) (<= lhs rhs))
		     ((eq? cmp '>=) (>= lhs rhs)))
	       'null))
	  ((=) (json=? lhs rhs))
	  ((!=) (not (json=? lhs rhs))))))))

(define (jmespath:compile-literal-expression e)
  (let ((v (cadr e)))
    (lambda (json context) v)))

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
