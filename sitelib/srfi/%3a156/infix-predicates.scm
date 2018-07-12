;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a156/infix-predicates.scm - Syntactic combiners for binary predicates
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

;; the library name is totally my original but I think it describes well enough.

(library (srfi :156 infix-predicates)
    (export is isnt)
    (import (rnrs))
(define-syntax is
  (syntax-rules ()
    ((_ . exprs) (transform identity exprs))))

(define-syntax isnt
  (syntax-rules ()
    ((_ . exprs) (transform not exprs))))

(define-syntax identity
  (syntax-rules ()
    ((_ e) e)))

(define-syntax transform
  (lambda (x)
    (define (transform e renamed formals)
      (define (recur next val arg op?)
	(with-syntax (((v ...) val)
		      ((n ...) arg)
		      (next next)
		      ((expr ...) renamed)
		      ((args ...) formals))
	  (if op?
	      (with-syntax ((op op?))
		(transform next #'(expr ... v ... op) #'(args ... n ...)))
	      (transform next #'(expr ... v ...) #'(args ... n ...)))))
      (syntax-case e ()
	(() (list renamed formals))
	((arg op . rest)
	 (and (identifier? #'arg) (free-identifier=? #'arg #'_))
	 (with-syntax ((tmp (generate-temporaries '(t))))
	   (recur #'rest #'tmp #'tmp #'op)))
	((arg op . rest) (recur #'rest #'(arg) '() #'op))
	((arg)
	 (and (identifier? #'arg) (free-identifier=? #'arg #'_))
	 (with-syntax ((tmp (generate-temporaries '(t))))
	   (recur #'() #'tmp #'tmp #f)))
	((arg) (recur #'() #'(arg) '() #f))))
    (syntax-case x ()
      ((_ wrap body)
       (with-syntax (((renamed formals) (transform #'body '() '())))
	 (if (null? (syntax->datum #'formals))
	     #'(wrap (infix->postfix . renamed))
	     #'(lambda formals (wrap (infix->postfix . renamed)))))))))

(define-syntax infix->postfix
  (syntax-rules ()
    ((_ x p) (p x))
    ((_ l op r) (op l r))
    ((_ l op r rest ...)
     (let ((r* r))
       (and (infix->postfix l op r*)
	    (infix->postfix r* rest ...))))))
)
