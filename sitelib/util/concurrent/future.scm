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

(library (util concurrent future)
    (export <future> future?
	    ;; macro
	    future class
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
	    ;; implementation specific
	    <simple-future> make-simple-future simple-future?
	    
	    ;; shared-box (not public APIs)
	    make-shared-box shared-box?
	    shared-box-put! shared-box-get!
	    )
    (import (rnrs)
	    (srfi :18))

  ;; lightweight shared-queue to retrieve future result
  (define shared-box-mark (list 'shared-box))
  (define-record-type (<shared-box> make-shared-box shared-box?)
    (fields (mutable value %sb-value %sb-value-set!)
	    (immutable lock %sb-lock)
	    (immutable cv   %sb-cv))
    (protocol (lambda (p)
		(lambda ()
		  (p shared-box-mark (make-mutex) (make-condition-variable))))))
  (define (shared-box-put! sb o)
    (mutex-lock! (%sb-lock sb))
    (%sb-value-set! sb o)
    ;; TODO do we need count waiter?
    (condition-variable-broadcast! (%sb-cv sb))
    (mutex-unlock! (%sb-lock sb)))
  (define (shared-box-get! sb)
    (mutex-lock! (%sb-lock sb))
    (let loop ()
      (let ((r (%sb-value sb)))
	(cond ((eq? r shared-box-mark)
	       (cond ((mutex-unlock! (%sb-lock sb) (%sb-cv sb))
		      (mutex-lock! (%sb-lock sb))
		      (loop))
		     (else #f)))
	      (else 
	       (mutex-unlock! (%sb-lock sb))
	       r)))))

  (define-condition-type &future-terminated &error
    make-future-terminated future-terminated?
    (future terminated-future))

  ;; future
  (define-record-type (<future> %make-future future?)
    (fields (mutable thunk future-thunk future-thunk-set!)
	    (mutable result future-result future-result-set!)
	    (mutable state future-state future-state-set!)
	    (mutable canceller future-canceller future-canceller-set!))
    (protocol (lambda (p)
		(lambda (thunk result)
		  (p thunk result 'created #f)))))

  (define (simple-invoke thunk f q)
    (lambda ()
      (guard (e (else (future-canceller-set! f #t)
		      (future-state-set! f 'finished)
		      (shared-box-put! q e)))
	(let ((r (thunk)))
	  (future-state-set! f 'finished)
	  (shared-box-put! q r)))))
  (define-record-type (<simple-future> make-simple-future simple-future?)
    (parent <future>)
    (protocol (lambda (n)
		(lambda (thunk)
		  (let ((q (make-shared-box)))
		    (let ((f ((n thunk q))))
		      (thread-start! (make-thread (simple-invoke thunk f q)))
		      f))))))

  (define-syntax class (syntax-rules ()))
  ;; for convenience default using simple future
  (define-syntax future
    (syntax-rules (class)
      ((_ (class cls) expr ...)
       ((record-constructor (record-constructor-descriptor cls))
	(lambda () expr ...)))
      ((_ expr ...)
       (make-simple-future (lambda () expr ...)))))

  ;; kinda silly
  (define (future-get future)
    (define (finish r)
      ;; now we can change the state shared.
      (future-state-set! future 'done)
      (if (eqv? (future-canceller future) #t) ;; kinda silly
	  (raise r)
	  r))
    (when (eq? (future-state future) 'terminated)
      (raise (condition
	      (make-future-terminated future)
	      (make-who-condition 'future-get)
	      (make-message-condition "future is terminated")
	      (make-irritants-condition future))))
    (let ((state (future-state future)))
      (let ((r (future-result future)))
	(finish 
	 (cond ((and (not (eq? state 'done)) (shared-box? r))
		(future-result-set! future (shared-box-get! r))
		(future-result future))
	       ((and (not (eq? state 'done)) (procedure? r))
		(future-result-set! future (r future))
		(future-result future))
	       (else r))))))
  ;; kinda dummy
  (define (future-cancel future)
    (unless (eq? (future-state future) 'done)
      (future-state-set! future 'terminated))
    (let ((c (future-canceller future)))
      (when (procedure? c)
	(c future)
	(future-canceller-set! future #f))))

  (define (future-done? future) (eq? (future-state future) 'done))
  (define (future-cancelled? future) (eq? (future-state future) 'terminated))

  )
