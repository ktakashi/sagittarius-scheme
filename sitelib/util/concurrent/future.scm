;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; util/concurrent/future.scm - Future
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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
(library (util concurrent future)
    (export (rename (future <future>))
	    future?
	    ;; macro
	    (rename (future-builder future)) class
	    ;; common procedures
	    future-get future-cancel
	    future-done? future-cancelled?

	    &future-terminated future-terminated? terminated-future

	    future-state
	    ;; these should not be used by user but we need to expose it
	    ;; so that future implementation can do flexible state control
	    ;; see util/concurrent/executor.scm
	    future-state-set!
	    future-thunk
	    future-result
	    future-result-set!
	    future-canceller
	    future-canceller-set!

	    make-piped-future
	    make-composable-future
	    make-completed-future
	    
	    ;; implementation specific
	    (rename (simple-future <simple-future>))
	    make-simple-future simple-future?
	    
	    ;; shared-box (not public APIs)
	    make-shared-box shared-box?
	    shared-box-put! shared-box-get!
	    ;; these are non public API as well
	    future-update-state!
	    future-execute-task!
	    future-try-lock!
	    future-lock!
	    future-unlock!)
    (import (rnrs)
	    (sagittarius)
	    (srfi :18))

  ;; lightweight shared-queue to retrieve future result
  (define shared-box-mark (list 'shared-box))
  (define-record-type (<shared-box> make-shared-box shared-box?)
    (fields (mutable value  %sb-value %sb-value-set!)
	    (immutable lock %sb-lock)
	    (immutable cv   %sb-cv)
	    (mutable waiter %sb-waiter %sb-waiter-set!))
    (protocol (lambda (p)
		(lambda ()
		  (p shared-box-mark
		     (make-mutex)
		     (make-condition-variable)
		     0)))))
  (define (shared-box-put! sb o)
    (mutex-lock! (%sb-lock sb))
    (%sb-value-set! sb o)
    (when (> (%sb-waiter sb) 0)
      (condition-variable-broadcast! (%sb-cv sb)))
    (mutex-unlock! (%sb-lock sb)))
  (define (shared-box-get! sb . maybe-timeout)
    (define timeout (if (pair? maybe-timeout) (car maybe-timeout) #f))
    (define timeout-value (if (and (pair? maybe-timeout)
				   (pair? (cdr maybe-timeout)))
			      (cadr maybe-timeout)
			      #f))
    (mutex-lock! (%sb-lock sb))
    (%sb-waiter-set! sb (+ (%sb-waiter sb) 1))
    (let loop ()
      (let ((r (%sb-value sb)))
	(cond ((eq? r shared-box-mark)
	       (cond ((mutex-unlock! (%sb-lock sb) (%sb-cv sb) timeout)
		      (mutex-lock! (%sb-lock sb))
		      (loop))
		     (else (values timeout-value #t))))
	      (else
	       (%sb-waiter-set! sb (- (%sb-waiter sb) 1))
	       (mutex-unlock! (%sb-lock sb))
	       (values r #f))))))

  (define-condition-type &future-terminated &error
    make-future-terminated future-terminated?
    (future terminated-future))

  ;; future
  (define-record-type future
    (fields thunk
	    (mutable result)
	    (mutable state)
	    (mutable canceller)
	    lock)
    (protocol (lambda (p)
		(lambda (thunk result)
		  (p thunk result 'created #f (make-mutex))))))
  (define (future-update-state! future state)
    (let ((lock (future-lock future)))
      (mutex-lock! lock)
      (future-state-set! future state)
      (mutex-unlock! lock)))

  (define (future-try-lock! future)
    (let ((lock (future-lock future)))
      (mutex-lock! lock 0 #f)))

  (define (future-lock! future)
    (let ((lock (future-lock future)))
      (mutex-lock! lock)))

  (define (future-unlock! future)
    (let ((lock (future-lock future)))
      (mutex-unlock! lock)))
    
  (define (future-execute-task! future :optional (cleanup #f))
    (let ((q (future-result future))
	  (thunk (future-thunk future)))
      (when thunk
	(let ((lock (future-lock future)))
	  (mutex-lock! lock)
	  (let ((state (future-state future)))
	    (when (memq state '(created execute-internal))
	      (future-state-set! future 'executing)
	      (guard (e (else
			 (future-canceller-set! future #t) ;; kinda abusing
			 (when cleanup (cleanup future))
			 (future-state-set! future 'finished)
			 (shared-box-put! q e)))
		
		(let ((r (thunk)))
		  (when cleanup (cleanup future))
		  (future-state-set! future 'finished)
		  (shared-box-put! q r)))))
	  (mutex-unlock! lock)))))
  
  (define (make-piped-future)
    (let* ((box (make-shared-box))
	   (f (make-future #f box)))
      (values f
	      (lambda (v) (shared-box-put! box v) f)
	      (lambda (e)
		(future-canceller-set! f #t)
		(shared-box-put! box e)
		f))))

  (define (make-composable-future)
    (define box (make-shared-box))
    (define (composable-result future . opts)
      (apply future-get (shared-box-get! box) opts))
    (let ((f (make-future #f composable-result)))
      (values f (lambda (v)
		  (unless (future? v)
		    (assertion-violation 'composable-future
					 "Result value must be a future" v))
		  (shared-box-put! box v)
		  #f))))

  (define (make-completed-future v)
    (define box (make-shared-box))
    (shared-box-put! box v)
    (make-future #f box))
  
  (define (simple-invoke thunk f q)
    (lambda ()
      (guard (e (else (future-canceller-set! f #t)
		      (future-state-set! f 'finished)
		      (shared-box-put! q e)))
	(let ((r (thunk)))
	  (future-state-set! f 'finished)
	  (shared-box-put! q r)))))
  (define-record-type simple-future
    (parent future)
    (protocol (lambda (n)
		(lambda (thunk)
		  (let ((q (make-shared-box)))
		    (let ((f ((n thunk q))))
		      (thread-start! (make-thread (simple-invoke thunk f q)))
		      f))))))

  (define-syntax class (syntax-rules ()))
  ;; for convenience default using simple future
  (define-syntax future-builder
    (syntax-rules (class)
      ((_ (class cls) expr ...)
       ((record-constructor (record-constructor-descriptor cls))
	(lambda () expr ...)))
      ((_ expr ...)
       (make-simple-future (lambda () expr ...)))))

  (define (raise-future-terminated future)
    (raise (condition
	    (make-future-terminated future)
	    (make-who-condition 'future-get)
	    (make-message-condition "future is terminated")
	    (make-irritants-condition future))))

  ;; kinda silly
  (define (future-get future . opt)
    (define (finish r)
      ;; now we can change the state shared.
      (future-update-state! future 'done)
      (if (eqv? (future-canceller future) #t) ;; kinda silly
	  (raise r)
	  r))
    (when (eq? (future-state future) 'terminated)
      (raise-future-terminated future))
    (let* ((state (future-state future))
	   (r (future-result future)))
      ;; (format #t "fget ~a~%" r)
      (cond ((and (not (eq? state 'done)) (shared-box? r))
	     (let-values (((v timeout?) (apply shared-box-get! r opt)))
	       (cond (timeout? v)
		     (else
		      (future-result-set! future v)
		      (finish (future-result future))))))
	    ((and (not (eq? state 'done)) (procedure? r))
	     (future-result-set! future (apply r future opt))
	     (finish (future-result future)))
	    (else (finish r)))))
  ;; kinda dummy
  (define (future-cancel future)
    (unless (eq? (future-state future) 'done)
      (future-state-set! future 'terminated))
    (let ((c (future-canceller future)))
      (when (procedure? c)
	(c future)
	(future-canceller-set! future #f))))

  (define (future-done? future) (memq (future-state future) '(done finished)))
  (define (future-cancelled? future) (eq? (future-state future) 'terminated))

  )
