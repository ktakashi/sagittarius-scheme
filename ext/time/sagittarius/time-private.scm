;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/time-private.scm - private library of time related libraries
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

(library (sagittarius time-private)
    ;; exports all C procedures + alpha
    (export make-time time? current-time
	    time-type time-nanosecond time-second
	    set-time-type! set-time-nanosecond! set-time-second!
	    copy-time time-resolution
	    current-time
	    time->seconds seconds->time
	    time<=? time<? time=? time>=? time>?
	    time-difference time-difference!
	    add-duration add-duration!
	    subtract-duration subtract-duration!
	    leap-second-delta +leap-second-table+
	    ;; timezone
	    %local-timezone-name
	    ;; time-error
	    &time-error make-time-error time-error? time-error-type
	    )
    (import (core)
	    (core conditions)
	    (sagittarius)
	    (sagittarius dynamic-module)
	    (sagittarius time-util))
  (load-dynamic-module "sagittarius--time")

  (define-condition-type &time-error &error make-time-error time-error?
    (type time-error-type))

  (define-constant +leap-second-table+
    (include "leap-table.scm"))

  (define (leap-second-delta utc-seconds)
    (letrec ( (lsd (lambda (table)
                     (cond
                      ((>= utc-seconds (caar table))
                       (cdar table))
                      (else (lsd (cdr table)))))) )
      (if (< utc-seconds  (* (- 1972 1970) 365 tm:sid)) 0
          (lsd  +leap-second-table+))))

  (define (current-time :optional (type 'time-utc))
    (let-values (((sec usec) (get-time-of-day)))
      (case type
	((time-utc) (make-time 'time-utc (* usec 1000) sec))
	((time-tai) 
	 (make-time 'time-tai (* usec 1000) (+ sec (leap-second-delta sec))))
	((time-monotonic)
	 (make-time 'time-monotonic
		    (* usec 1000) (+ sec (leap-second-delta sec))))
	((time-process)
	 (let-values (((vsec vusec) (vm-process-time)))
	   (make-time 'time-process (* (- usec vusec) 1000) (- sec vsec))))
	((time-thread)
	 (let-values (((vsec vusec) (vm-thread-time)))
	   (make-time 'time-thread (* (- usec vusec) 1000) (- sec vsec))))
	(else
	 (error 'current-time
		"TIME-ERROR type current-time: invalid clock type"
		type)))))
)
