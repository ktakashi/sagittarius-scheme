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
	    )
    (import (rnrs)
	    (srfi :18)
	    (util concurrent shared-queue))

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
      (guard (e (else (future-canceller-set! f #t) (shared-queue-put! q e)))
	(shared-queue-put! q (thunk)))
      (future-state-set! f 'finished)))
  (define-record-type (<simple-future> make-simple-future simple-future?)
    (fields (mutable thread simple-future-thread %future-thread-set!))
    (parent <future>)
    (protocol (lambda (n)
		(lambda (thunk)
		  (let ((q (make-shared-queue)))
		    (let ((f ((n thunk q) #f)))
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
      (if (eqv? (future-canceller future) #t) ;; kinda silly
	  (raise r)
	  r))
    (when (eq? (future-state future) 'terminated)
      (error 'future-get "future is terminated" future))
    (let ((state (future-state future)))
      (future-state-set! future 'done)
      (let ((r (future-result future)))
	(finish 
	 (cond ((and (not (eq? state 'done)) (shared-queue? r))
		(future-result-set! future (shared-queue-get! r))
		(future-result future))
	       ((and (not (eq? state 'done)) (procedure? r))
		(future-result-set! (r future))
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
