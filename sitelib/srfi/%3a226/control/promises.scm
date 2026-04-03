;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; srfi/%3a226/control/promises.scm - SRFI-226 promises
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
(library (srfi :226 control promises)
    (export ;; Uncaught exception conditions - re-exported from threads
	    &uncaught-exception
	    make-uncaught-exception-condition
	    uncaught-exception-condition?
	    uncaught-exception-condition-reason
	    
	    ;; Promise procedures
	    delay
	    force
	    make-promise
	    promise?)
    (import (except (rnrs) guard)
	    (rnrs mutable-pairs)
	    (sagittarius parameters)
	    (except (sagittarius continuations) with-continuation-marks)
	    ;; Import SRFI-226 guard for proper exception handling with prompts
	    (only (srfi :226 control exceptions) guard raise-continuable)
	    ;; Import uncaught-exception from threads to use consistent type
	    (srfi :226 control threads))

  ;; SRFI-226 promises support multiple values and capture parameterization
  
  (define-record-type promise-box
    (fields (mutable state) (mutable value) (mutable params)))
  
  (define (promise? obj) (promise-box? obj))
  
  ;; SRFI-226 delay captures the parameterization at creation time
  (define-syntax delay
    (syntax-rules ()
      ((_ expr ...)
       (let ((ps (current-parameterization)))
	 (make-promise-box 'pending
			   (lambda ()
			     (call-with-parameterization ps
			       (lambda () expr ...)))
			   ps)))))
  
  ;; make-promise wraps an already computed value as a promise
  ;; Unlike delay, make-promise does NOT flatten nested promises
  (define make-promise
    (case-lambda
      ((obj)
       ;; Store as already-done promise with the object as value
       ;; Note: if obj is a promise, it stays as is - we don't unwrap it
       (make-promise-box 'done-value (list obj) #f))
      ((obj . objs)
       (make-promise-box 'done-value (cons obj objs) #f))))
  
  ;; force evaluates a promise, handling multiple values and exceptions
  (define (force promise)
    (if (promise? promise)
	(case (promise-box-state promise)
	  ((done done-value)
	   ;; Return stored values
	   (apply values (promise-box-value promise)))
	  ((error)
	   ;; Re-raise stored exception as uncaught (continuable so handlers
	   ;; can return a replacement value)
	   (raise-continuable
	    (make-uncaught-exception-condition (promise-box-value promise))))
	  ((pending)
	   ;; Call the thunk and capture multiple values
	   (let ((thunk (promise-box-value promise)))
	     (guard (exc
		     (else
		      ;; Mark promise as errored and re-raise  
		      ;; Use raise-continuable so exception handlers can
		      ;; return replacement values
		      (promise-box-state-set! promise 'error)
		      (promise-box-value-set! promise exc)
		      (raise-continuable (make-uncaught-exception-condition exc))))
	       (call-with-values thunk
		 (lambda vals
		   (if (and (= (length vals) 1)
			    (promise? (car vals)))
		       ;; If the result is a single promise from delay,
		       ;; force it (delay-force semantics)
		       (let ((inner (force (car vals))))
			 ;; After forcing inner, its result might have
			 ;; updated promise, so re-check
			 (if (memq (promise-box-state promise) '(done done-value))
			     (apply values (promise-box-value promise))
			     (begin
			       (promise-box-state-set! promise 'done)
			       (promise-box-value-set!
				promise
				(call-with-values (lambda () inner) list))
			       inner)))
		       (begin
			 ;; Normal case: store values and return
			 (promise-box-state-set! promise 'done)
			 (promise-box-value-set! promise vals)
			 (apply values vals))))))))
	  (else
	   (error 'force "invalid promise state" (promise-box-state promise))))
	;; Non-promise value, return as-is
	promise))

)
