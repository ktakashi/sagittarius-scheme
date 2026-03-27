;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; srfi/%3a226/control/exceptions.scm - SRFI-226 exceptions
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
(library (srfi :226 control exceptions)
    (export with-exception-handler
	    exception-handler-stack
	    raise
	    raise-continuable
	    guard
	    else =>)
    (import (except (rename (rnrs)
			    (with-exception-handler %with-exception-handler)
			    (raise-continuable %raise-continuable))
		    guard)
	    (sagittarius continuations))
(define handler-stack-continuation-mark-key
  (let ((mark-key (make-continuation-mark-key 'exception-handler)))
    (lambda () mark-key)))
(define (exception-handler-stack)
  (continuation-mark-set-first (current-continuation-marks)
			       (handler-stack-continuation-mark-key)))
(define (current-exception-handler) (car (exception-handler-stack)))
(define (with-exception-handler handler thunk)
  ;; Use both continuation marks (for SRFI-226 semantics) AND
  ;; native R6RS handler (for C-level exceptions like thread-join!)
  (with-continuation-mark (handler-stack-continuation-mark-key)
      (cons handler (exception-handler-stack))
    (%with-exception-handler handler thunk)))

(define-syntax guard
  (lambda (stx)
    (syntax-case stx ()
      [(_ (id c1 c2 ...) e1 e2 ...)
       (identifier? #'id)
       #`(call-with-current-continuation
	  (lambda (guard-k)
	    (with-exception-handler
	     (lambda (c)
	       (call-with-current-continuation
		(lambda (handler-k)
		  (call-in-continuation guard-k
		   (lambda ()
		     (let ([id c])
		       #,(let f ([c1 #'c1] [c2* #'(c2 ...)])
			   (syntax-case c2* ()
			     [()
			      (with-syntax
				  ([rest
				    #'(call-in-continuation handler-k
					(lambda () (raise-continuable c)))])
				(syntax-case c1 (else =>)
				  [(else e1 e2 ...)
				   #'(begin e1 e2 ...)]
				  [(e0) #'e0]
				  [(e0 => e1)
				   #'(let ([t e0]) (if t (e1 t) rest))]
				  [(e0 e1 e2 ...)
				   #'(if e0
					 (begin e1 e2 ...)
					 rest)]))]
			     [(c2 c3 ...)
			      (with-syntax ([rest (f #'c2 #'(c3 ...))])
				(syntax-case c1 (=>)
				  [(e0) #'(let ([t e0]) (if t t rest))]
				  [(e0 => e1)
				   #'(let ([t e0]) (if t (e1 t) rest))]
				  [(e0 e1 e2 ...)
				   #'(if e0
					 (begin e1 e2 ...)
					 rest)]))]))))))))
	     (lambda ()
	       e1 e2 ...))))]
      [_
       (syntax-violation 'guard "invalid syntax" stx)])))

(define raise-continuable
  (lambda (con)
    ;; Use native raise-continuable which will invoke native exception handlers
    ;; This works with our with-exception-handler that installs both
    ;; continuation mark handlers and native handlers
    (%raise-continuable con)))

)
