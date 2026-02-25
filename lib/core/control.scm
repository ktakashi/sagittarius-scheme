;;; -*- Scheme -*-
;;;
;;; core/control.scm - core controls
;;;  
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
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
(library (core control)
    (export case-lambda)
    (import (core)
	    (core syntax)
	    (sagittarius))
(define-syntax case-lambda-aux
  (lambda (x)
    (define (construct args formals clause*)
      (define _car #'car)
      (define _cdr #'cdr)
      (define (parse-formals formal args inits)
	(syntax-case formal ()
	  (() (reverse! inits))
	  ((a . d)
	   (with-syntax ((arg `(,_car ,args))
			 (args `(,_cdr ,args)))
	     (parse-formals #'d #'args
			    (cons (list #'a #'arg) inits))))
	  (v
	   (reverse! (cons (list #'v args) inits)))))
      (with-syntax ((((var init) ...) (parse-formals formals args '()))
		    ((clause ...) clause*))
	;; Using `lambda` enables type check, immediate apply
	;; will be converted to let by the compiler.
	#'((lambda (var ...) clause ...) init ...)))
    (syntax-case x ()
      ((_ args n)
       #'(assertion-violation #f "unexpected number of arguments" args))
      ((_ args n ((x ...) b ...) more ...)
       (with-syntax ((let-clause (construct #'args #'(x ...) #'(b ...)))
		     (expect-length (length #'(x ...))))
	 #'(if (= n expect-length)
	       let-clause
	       (case-lambda-aux args n more ...))))
      ((_ args n ((x1 x2 ... . r) b ...) more ...)
       (with-syntax ((let-clause (construct #'args #'(x1 x2 ... . r) 
					    #'(b ...)))
		     (expect-length (length #'(x1 x2 ...))))
	 #'(if (>= n expect-length)
	       let-clause
	       (case-lambda-aux args n more ...))))
      ((_ args n (r b ...) more ...)
       #'(let ((r args)) b ...)))))

(define-syntax case-lambda
  (syntax-rules ()
    ((_ (fmls b1 b2 ...))
     (lambda fmls b1 b2 ...))
    ((_ (fmls b1 b2 ...) ...)
     (lambda args
       (let ((n (length args)))
	 (case-lambda-aux args n (fmls b1 b2 ...) ...))))))

)
