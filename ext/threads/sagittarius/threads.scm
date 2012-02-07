;;; -*- Scheme -*-
;;;
;;; threads.scm - multi threads
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

;; load (sagittarius threads impl) library
(load-dynamic-library "sagittarius--threads")
(library (sagittarius threads)
    (export thread?
	    make-thread
	    thread-name
	    thread-specific thread-specific-set!
	    thread-state
	    current-thread
	    ;; TODO implement
	    ;;thread-base-priority thread-base-priority-set!
	    ;;thread-priority-boost thread-priority-boost-set!
	    ;;thread-quantum thread-quantum-set!
	    thread-start! thread-join! thread-yield! thread-sleep!
	    thread-stop! thread-cont!
	    thread-terminate!

	    ;; mutex
	    mutex? make-mutex
	    mutex-name mutex-state
	    mutex-specific mutex-specific-set!
	    mutex-lock! mutex-unlock!

	    ;; condition variable
	    condition-variable? make-condition-variable
	    condition-variable-name
	    condition-variable-specific condition-variable-specific-set!
	    condition-variable-signal! condition-variable-broadcast!

	    ;; time
	    ;;current-time time? time->seconds seconds->time

	    ;; exceptions
	    join-time-out-exception? abandoned-mutex-exception?
	    terminated-thread-exception? uncaught-exception?
	    uncaught-exception-reason

	    ;; sagittarius threads specific
	    ;; from srfi-18
	    mutex-lock-recursively! mutex-unlock-recursively!
	    ;; from Gauche
	    with-locking-mutex

	    sys-nanosleep
	    thread-guard
	    ;; clos
	    <thread> <mutex> <condition-variable>
	    )
    (import (core)
	    (core syntax)
	    (sagittarius)
	    (sagittarius threads impl))

  ;; NB: actually, we can make mutex recursively by default.
  ;;     I'm not sure if it's a good proposal or not, so for now,
  ;;     like this.
  (define (mutex-lock-recursively! mutex)
    (if (eq? (mutex-state mutex) (current-thread))
	(let ((n (mutex-specific mutex)))
	  (mutex-specific-set! mutex (+ n 1)))
	(begin
	  (mutex-lock! mutex)
	  (mutex-specific-set! mutex 0))))
  
  (define (mutex-unlock-recursively! mutex)
    (let ((n (mutex-specific mutex)))
      (if (= n 0)
	  (mutex-unlock! mutex)
	  (mutex-specific-set! mutex (- n 1)))))

  (define (with-locking-mutex mutex thunk)
    (dynamic-wind
	(lambda () (mutex-lock! mutex))
	thunk
	(lambda () (mutex-unlock! mutex))))

  (define-syntax thread-guard
    (syntax-rules ()
      [(thread-guard (var . clauses) . body)
       (with-error-handler
         (lambda (e)
           (let ((var e))
             (%guard-rec var e . clauses)))
	 (lambda () . body))]))

  (define-syntax %guard-rec
    (syntax-rules (else =>)
      [(%guard-rec var exc)
       (raise exc)]
      [(%guard-rec var exc (else . exprs))
       (begin . exprs)]
      [(%guard-rec var exc (test => proc) . more)
       (let ((tmp test))
	 (if tmp
	     (proc tmp)
	     (%guard-rec var exc . more)))]
      [(%guard-rec var exc (test . exprs) . more)
       (if test
	   (begin . exprs)
	   (%guard-rec var exc . more))]
      [(%guard-rec var exc other . more)
       (syntax-error "malformed guard clause" other)]))

)