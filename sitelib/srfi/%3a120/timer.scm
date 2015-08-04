;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; srfi/%3a120/timer.scm - Timer SRFI
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

(library (srfi :120 timer)
    (export make-timer timer?
	    timer-cancel!
	    timer-schedule! timer-reschedule!
	    timer-task-remove! timer-task-exists?
	    
	    make-timer-delta timer-delta?)
    (import (rnrs)
	    (srfi :19)
	    (rename (util timer)
		    (make-timer        u:make-timer)
		    (timer-cancel!     u:timer-cancel!)
		    (timer-schedule!   u:timer-schedule!)
		    (timer-reschedule! u:timer-reschedule!)
		    (timer-remove!     timer-task-remove!)
		    (timer-exists?     timer-task-exists?)))

  (define-record-type (<timer-delta> make-timer-delta timer-delta?)
    (fields (immutable time timer-delta-time))
    (protocol
     (lambda (p)
       (lambda (n unit)
	 (define (milliseconds->sec&nano msec)
	   (let ((sec (div msec 1000))
		 (nsec (* (mod msec 1000) 1000000)))
	     (values sec nsec)))
	 (define (microseconds->sec&nano n)
	   (values (div n 1000000)
		   (* (mod n 1000000) 1000)))
	 (define (nanoseconds->sec&nano n)
	   (values (div n 1000000000) (mod n 1000000000)))
	 (let-values (((sec nsec)
		       (case unit
			 ((h)  (values (* 3600 n) 0))
			 ((m)  (values (* n 60) 0))
			 ((s)  (values n 0))
			 ((ms) (milliseconds->sec&nano n))
			 ((us) (microseconds->sec&nano n))
			 ((ns) (nanoseconds->sec&nano n))
			 (else
			  (assertion-violation 'make-timer-delta
					       "unsupported unit" unit)))))
	   (p (make-time time-duration nsec sec)))))))

  ;; SRFI procedures
  (define (make-timer :optional (error-handler raise))
    ;; on the SRFI timer is started
    (timer-start! (u:make-timer :error-handler error-handler)))

  (define (timer-schedule! timer thunk first :optional (period 0))
    (let ((first (if (timer-delta? first) 
		     (add-duration (current-time) (timer-delta-time first))
		     first))
	  (p    (if (timer-delta? period)
		    (timer-delta-time period)
		    period)))
      (u:timer-schedule! timer thunk first p)))

  (define (timer-reschedule! timer id first :optional (period 0))
    (let ((first (if (timer-delta? first) 
		     (add-duration (current-time) (timer-delta-time first))
		     first))
	  (p    (if (timer-delta? period)
		    (timer-delta-time period)
		    period)))
      (u:timer-reschedule! timer id first p)))

  (define (timer-cancel! t)
    (u:timer-cancel! (timer-stop! t)))

)
