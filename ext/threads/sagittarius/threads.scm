;;; -*- Scheme -*-
;;;
;;; threads.scm - multi threads
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
	    thread-interrupt!

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
	    join-timeout-exception? abandoned-mutex-exception?
	    terminated-thread-exception? uncaught-exception?
	    thread-interrupt-exception? ;; extra
	    thead-exception-thread
	    abandoned-mutex-exception-mutex
	    terminated-thread-exception-terminator
	    uncaught-exception-reason

	    ;; semaphore
	    semaphore? make-semaphore semaphore-wait! semaphore-post!
	    semaphore-close! semaphore-destroy!
	    semaphore-name

	    ;; sagittarius threads specific
	    ;; from srfi-18
	    mutex-lock-recursively! mutex-unlock-recursively!
	    ;; from Gauche
	    with-locking-mutex with-recursive-locking-mutex

	    sys-nanosleep
	    thread-guard
	    ;; clos
	    <thread> <mutex> <condition-variable> <semaphore>
	    )
    (import (core)
	    (core syntax)
	    (core conditions)
	    (sagittarius)
	    (sagittarius dynamic-module))
  (load-dynamic-module "sagittarius--threads")

  ;; initialise condition
  (initialize-builtin-condition &thread-exception &error thread)
  (initialize-builtin-condition &join-timeout-exception &thread-exception)
  (initialize-builtin-condition &abandoned-mutex-exception &thread-exception mutex)
  (initialize-builtin-condition &terminated-thread-exception &thread-exception terminator)
  (initialize-builtin-condition &uncaught-exception &thread-exception reason)
  
  (define-condition-accessor thead-exception-thread
    &thread-exception &thread-exception-thread)
  (define-condition-accessor abandoned-mutex-exception-mutex
    &abandoned-mutex-exception &abandoned-mutex-exception-mutex)
  (define-condition-accessor terminated-thread-exception-terminator
    &terminated-thread-exception &terminated-thread-exception-terminator)
  (define-condition-accessor uncaught-exception-reason
    &uncaught-exception &uncaught-exception-reason)

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
  
  (define (mutex-unlock-recursively! mutex :optional (cv #f) (timeout #f))
    (let ((n (mutex-specific mutex)))
      (if (= n 0)
	  (mutex-unlock! mutex cv timeout)
	  (mutex-specific-set! mutex (- n 1)))))

  (define (with-locking-mutex mutex thunk)
    (dynamic-wind
	(lambda () (mutex-lock! mutex))
	thunk
	(lambda () (mutex-unlock! mutex))))
  ;; hmmm...
  (define (with-recursive-locking-mutex mutex thunk)
    (dynamic-wind
	(lambda () (mutex-lock-recursively! mutex))
	thunk
	(lambda () (mutex-unlock-recursively! mutex))))

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
