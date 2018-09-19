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
	   ((not)
	    (jmespath:compile-not-expression (compile-expression (cadr e))))
	   (else (error 'compile-jmespath "Unsupported command"))))))
  
(define (jmespath:compile-identifier s)
  (define key=? (lambda (k&v) (and (string=? s (car k&v)) k&v)))
  (lambda (json context)
    (if (vector? json)
	(cond ((vector-any key=? json) => cdr)
	      (else 'null))
	'null)))

(define (jmespath:compile-not-expression e)
  (lambda (json context)
    (let ((v (e json context)))
      ;; false-like values?
      (false-value? v))))

)
