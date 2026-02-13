;;; -*- Scheme -*-
;;;
;;; continuations.scm - Continuations
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
(library (sagittarius continuations)
    (export call-with-continuation-prompt call/prompt
	    abort-current-continuation abort/cc
	    call-with-composable-continuation call/comp

	    call/delim-cc
	    call-with-delimited-current-continuation

	    continuation? composable-continuation?
	    continuation-prompt-available?

	    default-continuation-prompt-tag
	    make-continuation-prompt-tag continuation-prompt-tag?
	    shift reset
	    prompt control)
    (import (except (core) call/cc call-with-current-continuation)
	    (core macro)
	    (sagittarius))

;; From SRFI-226 implementation
(define-syntax reset
  (lambda (x)
    (syntax-case x ()
      [(reset e1 e2 ...)
       #'(call-with-continuation-prompt
	  (lambda ()
	    e1 e2 ...))])))

(define-syntax shift
  (lambda (x)
    (syntax-case x ()
      [(shift k e1 e2 ...)
       #'(call-with-composable-continuation
	  (lambda (c)
            (define k (lambda args (reset (apply c args))))
            (abort-current-continuation (default-continuation-prompt-tag)
					(lambda () e1 e2 ...))))])))

(define-syntax prompt
  (lambda (x)
    (syntax-case x ()
      [(prompt e1 e2 ...)
       #'(call-with-continuation-prompt
	  (lambda () e1 e2 ...)
	  (default-continuation-prompt-tag)
	  (lambda (thunk) (thunk)))])))
  
(define-syntax control
  (lambda (x)
    (syntax-case x ()
      [(control k e1 e2 ...)
       #'(call-with-composable-continuation
	  (lambda (k)
	    (abort-current-continuation (default-continuation-prompt-tag)
					(lambda () e1 e2 ...))))])))

)
