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

	    continuation? continuation-prompt-available?

	    default-continuation-prompt-tag
	    make-continuation-prompt-tag continuation-prompt-tag?
	    shift reset)
    (import (except (core) call/cc call-with-current-continuation)
	    (core macro)
	    (sagittarius))

;; (define (call/cc proc :optional (tag (default-continuation-prompt-tag)))
;;   (call-with-composable-continuation
;;    (lambda (ck)
;;      (define (k . args)
;;        (abort-current-continuation tag (lambda () (apply ck args))))
;;      (proc k))
;;    tag))
;; (define call-with-current-continuation call/cc)

(define (abort-current-continuation/keep-prompt tag thunk)
  ((call-with-continuation-prompt
    (lambda ()
      ((call-with-delimited-current-continuation
	(lambda (k) (lambda () k))
	tag)))
    tag)
   thunk))

(define (make-call-with-shift abort-cc inserted-handler)
  (define (call-with-shift f :optional (tag (default-continuation-prompt-tag)))
    (call-with-composable-continuation
     (lambda (k)
       (abort-cc
	tag
	(lambda ()
	  (f (lambda vals
	       (call-with-continuation-prompt
		(lambda () (apply k vals))
		tag
		inserted-handler))))))))
  call-with-shift)

(define call-with-shift
  (make-call-with-shift abort-current-continuation/keep-prompt #f))

(define-syntax shift
  (lambda (x)
    (syntax-case x ()
      ((_ id expr0 expr ...)
       (identifier? #'id)
       #'(call-with-shift (lambda (id) expr0 expr ...))))))

(define-syntax reset
  (lambda (x)
    (syntax-case x ()
      ((_ expr0 expr ...)
       #'(call-with-continuation-prompt
	  (lambda () expr0 expr ...))))))

)
