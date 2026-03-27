;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; srfi/%3a226/control/threads.scm - SRFI-226 threads
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
(library (srfi :226 control threads)
    (export (rename (&thread-exception &thread)
		    (thread-exception? thread-condition?))
	    make-thread-condition
	    &uncaught-exception
	    make-uncaught-exception-condition
	    (rename (uncaught-exception? uncaught-exception-condition?)
		    (uncaught-exception-reason
		     uncaught-exception-condition-reason))

	    
	    (rename (&terminated-thread-exception &thread-already-terminated)
		    (terminated-thread-exception? thread-already-terminated-condition?))
	    make-thread-already-terminated-condition

	    (rename (&join-timeout-exception &thread-timeout)
		    (join-timeout-exception? thread-timeout-condition?))
	    make-thread-timeout-condition

	    (rename (&abandoned-mutex-exception &thread-abandoned-mutex)
		    (abandoned-mutex-exception? thread-abandoned-mutex-condition?))
	    make-thread-abandoned-mutex-condition

	    &concurrent-modification
	    make-concurrent-modification-violation
	    concurrent-modification-violation?

	    ;; Thread record type for subtyping
	    thread

	    ;; Thread procedures
	    thread?
	    current-thread
	    make-thread
	    thread-start!
	    thread-yield!
	    thread-sleep!
	    thread-terminate!
	    thread-schedule-terminate!
	    thread-join!

	    ;; Mutex
	    make-mutex
	    mutex?
	    mutex-state
	    mutex-lock!
	    mutex-unlock!

	    ;; Condition variables
	    make-condition-variable
	    condition-variable?
	    condition-variable-signal!
	    condition-variable-broadcast!)
    (import (rnrs)
	    (rename (sagittarius threads)
		    (make-thread       %make-thread)
		    (current-thread    %current-thread)
		    (thread?           %thread?)
		    (thread-start!     %thread-start!)
		    (thread-terminate! %thread-terminate!)
		    (thread-join!      %thread-join!))
	    (sagittarius parameters))

;; SRFI-226 thread condition types
(define (make-thread-condition) (make-thread-exception (current-thread)))
(define (make-uncaught-exception-condition obj)
  (make-uncaught-exception (current-thread) obj))
(define (make-thread-already-terminated-condition)
  (make-terminated-thread-exception (current-thread) #f))
(define (make-thread-timeout-condition)
  (make-join-timeout-exception (current-thread)))
(define (make-thread-abandoned-mutex-condition)
  ;; dummy value...
  (make-abandoned-mutex-exception (current-thread) #f))

;; &concurrent-modification
(define-condition-type &concurrent-modification &assertion
  make-concurrent-modification-violation concurrent-modification-violation?)

(define-record-type thread
  (fields (mutable raw-thread))
  (protocol (lambda (p)
	      (lambda (thunk)
		(let ((ps (current-parameterization))
		      (r (p #f)))
		  (thread-raw-thread-set! r
		   (%make-thread
		    (lambda ()
		      (current-thread r)
		      (call-with-parameterization ps thunk))))
		  r)))))
;; bah...
(define %thread
  (let ((rcd (make-record-constructor-descriptor (record-type-descriptor thread)
						 #f
						 (lambda (p)
						   (lambda (thread)
						     (p thread))))))
    (record-constructor rcd)))
(define *root-thread* (%thread (%current-thread)))
(define current-thread (make-thread-parameter *root-thread*))

(define (thread-start! (thread thread?))
  (%thread-start! (thread-raw-thread thread))
  thread)
(define (thread-terminate! (thread thread?))
  (%thread-terminate! (thread-raw-thread thread)))
(define (thread-join! (thread thread?) . opts)
  (apply %thread-join! (thread-raw-thread thread) opts))

;; thread-schedule-terminate! - async version of thread-terminate!
;; Returns immediately without waiting for termination
;; Note: In current implementation, this may block same as thread-terminate!
(define (thread-schedule-terminate! thread)
  ;; Mark thread for termination
  ;; Currently uses thread-terminate! which may block
  (thread-terminate! thread))

)
