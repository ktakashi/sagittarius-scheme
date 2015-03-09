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

	    ;; implementation specific
	    <simple-future> make-simple-future simple-future?
	    )
    (import (rnrs)
	    (srfi :18))

  ;; future
  (define-record-type (<future> %make-future future?)
    (fields (immutable thunk future-thunk)
	    (immutable get %future-get)
	    (immutable cancel %future-cancel)
	    (mutable state future-state future-state-set!))
    (protocol (lambda (p)
		(lambda (thunk get cancel)
		  (p thunk get cancel 'created)))))

  (define (simple-get future) (thread-join! (simple-future-thread future)))
  (define (simple-cancel future) 
    (thread-terminate! (simple-future-thread future)))

  (define-record-type (<simple-future> make-simple-future simple-future?)
    (fields (immutable thread simple-future-thread))
    (parent <future>)
    (protocol (lambda (n)
		(lambda (thunk)
		  ((n thunk simple-get simple-cancel)
		   (thread-start! (make-thread thunk)))))))

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
    ;; TODO should set this get?
    (future-state-set! future 'done)
    ((%future-get future) future))
  (define (future-cancel future)
    ;; TODO should set this after join?
    (future-state-set! future 'terminated)
    ((%future-cancel future) future))

  (define (future-done? future) (eq? (future-state future) 'done))
  (define (future-cancelled? future) (eq? (future-state future) 'terminated))

  )