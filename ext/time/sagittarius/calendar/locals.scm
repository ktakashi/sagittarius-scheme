;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/calendar/locals.scm - Local date/time containers
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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

;; it's called local-time or local-date this is because
;; time and date are already taken and calendar-date means differntly
;; on our calendar libraries...
(library (sagittarius calendar locals)
    (export (rename (local-time <local-time>)) local-time?
	    make-common-local-time common-local-time?
	    common-local-time-hour common-local-time-minute
	    common-local-time-second common-local-time-nanosecond
	    (rename (local-date <local-date>)) local-date?
	    make-common-local-date common-local-date?
	    common-local-date-day common-local-date-month
	    common-local-date-year

	    ;; helpers for common-time and date
	    time-components->absolute
	    absolute->time-components
	    absolute->day&nanosecond
	    )
    (import (rnrs)
	    (sagittarius timezone)
	    (sagittarius time-util))

;; local time holds hour, minute second and nanosecond
(define-record-type local-time)

;; this is an abstract type. actual implementations are
;; depending on the calendars
(define-record-type local-date)

(define-record-type common-local-time
  (parent local-time)
  (fields nanosecond second minute hour))

(define-record-type common-local-date
  (parent local-date)
  (fields day month year))


(define (absolute->day&nanosecond gd)
  (if (integer? gd)
      (values (exact gd) 0)
      (let* ((igd (inexact gd))
	     (fgd (floor igd)))
	(values (exact fgd) (exact (* 24 3600 tm:nano (- igd fgd)))))))

;; not sure if we should put it here
(define (absolute->time-components day tz)
  (define day&secs absolute->day&nanosecond)
  (define (compute day nsec)
    (define (parse-nanosec nsec)
      (values (mod nsec tm:nano) (floor (/ nsec tm:nano))))
    (define (fixup nsec s0 m0 h0)
      (define-syntax carry
	(syntax-rules ()
	  ((_ v c?)
	   (let ((c c?)) (if (>= v c) (values (- v c) 1) (values v 0))))))
      (let*-values (((s c) (carry s0 60))
		    ((m c) (carry (+ m0 c) 60))
		    ((h c) (carry (+ h0 c) 24)))
	(values nsec s m h c)))
    (let-values (((nsec sec) (parse-nanosec nsec)))
      (let* ((h (+ (div sec 3600) 12)) ;; sum offset
	     (r (mod sec 3600))
	     (m (div r 60))
	     (s (mod r 60)))
	(fixup (exact (floor nsec)) s m h))))
  (let-values (((d nsec) (day&secs day)))
    (compute d nsec)))

(define (time-components->absolute n s m h tz)
  (+ -1/2 ;; julian day of 0 starts 12:00:00 so subtract it
     (/ (+ (* h 60 60)
	   (* m 60)
	   s
	   (/ n tm:nano)
	   (- (timezone-offset tz)))
	tm:sid)))
  
)
