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
	    future-claim-task!
	    future-prepare-execution!
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
		     (else
		      (%sb-waiter-set! sb (- (%sb-waiter sb) 1))
		      (values timeout-value #t))))
	      (else
	       (%sb-waiter-set! sb (- (%sb-waiter sb) 1))
	       (mutex-unlock! (%sb-lock sb))
	       (values r #f))))))

  (define-condition-type &future-terminated &error
    make-future-terminated future-terminated?
    (future terminated-future))

  ;; future
  (define-record-type future
    (fields (mutable thunk)
	    (mutable result)
	    (mutable state)
	    (mutable canceller)
	    lock)
    (protocol (lambda (p)
		(lambda (thunk result)
		  (p thunk result 'created #f (make-mutex))))))
  (define (call-with-future-lock future proc)
    (let ((lock (future-lock future)))
      (mutex-lock! lock)
      (guard (e (else (mutex-unlock! lock) (raise e)))
	(call-with-values proc
	  (lambda args
	    (mutex-unlock! lock)
	    (apply values args))))))

  (define (future-update-state! future state)
    (call-with-future-lock future
      (lambda ()
	(future-state-set! future state))))

  (define (future-try-lock! future)
    (let ((lock (future-lock future)))
      (mutex-lock! lock 0 #f)))

  (define (future-lock! future)
    (let ((lock (future-lock future)))
      (mutex-lock! lock)))

  (define (future-unlock! future)
    (let ((lock (future-lock future)))
      (mutex-unlock! lock)))

  (define (future-claim-task! future)
    (call-with-future-lock future
      (lambda ()
	(let ((state (future-state future))
	      (thunk (future-thunk future)))
	  (and thunk
	       (memq state '(created execute-internal))
	       (begin
		 (future-thunk-set! future #f)
		 (future-state-set! future 'executing)
		 thunk))))))

  (define (future-prepare-execution! future :optional (canceller #f))
    (call-with-future-lock future
      (lambda ()
	(let ((state (future-state future)))
	  (if (memq state '(done finished terminated))
	      #f
	      (begin
		(unless (shared-box? (future-result future))
		  (future-result-set! future (make-shared-box)))
		(future-canceller-set! future canceller)
		#t))))))

  (define (future-publish-completion! future result exceptional?)
    (call-with-future-lock future
      (lambda ()
	(let ((state (future-state future)))
	  (if (memq state '(done finished terminated))
	      #f
	      (begin
		(future-canceller-set! future (and exceptional? #t))
		(let ((q (future-result future)))
		  (when (shared-box? q)
		    (shared-box-put! q result)))
		(future-state-set! future 'finished)
		#t))))))

  (define (future-transition-to-terminated! future)
    (call-with-future-lock future
      (lambda ()
	(let ((state (future-state future))
	      (q (future-result future))
	      (canceller (future-canceller future)))
	  (if (memq state '(done finished terminated))
	      (values #f #f #f)
	      (begin
		(future-thunk-set! future #f)
		(future-state-set! future 'terminated)
		(future-canceller-set! future #f)
		(values #t canceller (and (shared-box? q) q))))))))
  
  (define (future-execute-task! future :optional (cleanup #f))
    (define (finalize result exceptional?)
      (let-values (((v e?)
		    (if cleanup
			(guard (e (else (values e #t)))
			  (cleanup future)
			  (values result exceptional?))
			(values result exceptional?))))
	(future-publish-completion! future v e?)))
    (let ((thunk (future-claim-task! future)))
      (when thunk
	(let-values (((result exceptional?)
		      (guard (e (else (values e #t)))
			(values (thunk) #f))))
	  (finalize result exceptional?)))))
  
  (define (make-piped-future)
    (let* ((box (make-shared-box))
	   (f (make-future #f box)))
      (values f
	      (lambda (v) 
		(future-publish-completion! f v #f)
		f)
	      (lambda (e)
		(future-publish-completion! f e #t)
		f))))

  (define (make-composable-future)
    (define box (make-shared-box))
    (define (composable-result future . opts)
      (let-values (((v timeout?) (apply shared-box-get! box opts)))
	(if timeout?
	    v
	    (apply future-get v opts))))
    (let ((f (make-future #f composable-result)))
      (values f
	      (lambda (v)
		(unless (future? v)
		  (assertion-violation 'composable-future
				       "Result value must be a future" v))
		(shared-box-put! box v)
		f))))

  (define (make-completed-future v)
    (let ((f (make-future #f v)))
      (future-state-set! f 'done)
      f))
  
  (define (simple-invoke thunk f)
    (lambda ()
      (let-values (((result exceptional?)
		    (guard (e (else (values e #t)))
		      (values (thunk) #f))))
	(future-publish-completion! f result exceptional?))))
  (define-record-type simple-future
    (parent future)
    (protocol (lambda (n)
		(lambda (thunk)
		  (let ((q (make-shared-box)))
		    (let ((f ((n thunk q))))
		      (thread-start! (make-thread (simple-invoke thunk f)))
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

  (define (future-complete-get! future value value?)
    (call-with-future-lock future
      (lambda ()
	(let ((state (future-state future)))
	  (cond ((eq? state 'terminated)
		 (values 'terminated #f #f))
		(else
		 (when value?
		   (when (shared-box? (future-result future))
		     (future-result-set! future value)))
		 (unless (eq? state 'done)
		   (future-state-set! future 'done))
		 (values 'done
			 (future-result future)
			 (eqv? (future-canceller future) #t))))))))

  (define (future-get future . opt)
    (define (finish v has-value?)
      (let-values (((status result exceptional?)
		    (future-complete-get! future v has-value?)))
	(cond ((eq? status 'terminated) (raise-future-terminated future))
	      (exceptional? (raise result))
	      (else result))))
    (let-values (((state r)
		  (call-with-future-lock future
		    (lambda ()
		      (values (future-state future)
			      (future-result future))))))
      (cond ((eq? state 'terminated)
	     (raise-future-terminated future))
	    ((and (not (eq? state 'done)) (shared-box? r))
	     (let-values (((v timeout?) (apply shared-box-get! r opt)))
	       (if timeout?
		   v
		   (finish v #t))))
	    ((and (not (eq? state 'done)) (procedure? r))
	     (finish (apply r future opt) #t))
	    (else
	     (finish #f #f)))))

  (define future-cancelled-mark (list 'future-cancelled-mark))
  (define (future-cancel future)
    (let-values (((cancelled? canceller q)
		  (future-transition-to-terminated! future)))
      (when q
	(shared-box-put! q future-cancelled-mark))
      (when (procedure? canceller)
	(canceller future))
      cancelled?))

  (define (future-done? future)
    (and (memq (future-state future) '(done finished)) #t))
  (define (future-cancelled? future) (eq? (future-state future) 'terminated))

  )
