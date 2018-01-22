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
    (export (rename (partial-time <partial-time>)
		    (partial-date <partial-date>))
	    partial-time? partial-date?

	    (rename (local-time <local-time>)) local-time?
	    make-local-time
	    local-time-hour local-time-minute
	    local-time-second local-time-nanosecond
	    (rename (local-date <local-date>)) local-date?
	    make-local-date
	    local-date-day local-date-month local-date-year

	    ;; helpers for common-time and date
	    time-components->absolute
	    absolute->time-components
	    absolute->day&nanosecond

	    nanosecond->day
	    second->day
	    minute->day
	    hour->day
	    day->nanosecond
	    day->second
	    day->minute
	    day->hour
	    add-common-calendar-unit
	    )
    (import (rnrs)
	    (sagittarius timezone)
	    (sagittarius time-util)
	    (sagittarius calendar constants))

;; these are an abstract type. actual implementations are
;; depending on the calendars

;; interface: partial time
(define-record-type partial-time)

;; interface: partial date
(define-record-type partial-date)

;; TODO maybe we should move this to commons library or so
(define-record-type local-time
  (parent partial-time)
  (fields nanosecond second minute hour))

(define-record-type local-date
  (parent partial-date)
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
      (values (mod nsec tm:nano)
	      (+ (floor (/ nsec tm:nano)) (timezone-offset tz))))
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

(define (nanosecond->day nsec) (/ nsec tm:nano (* 60 60 24)))
(define (second->day sec)      (/ sec (* 60 60 24)))
(define (minute->day min)      (/ min (* 60 24)))
(define (hour->day hour)       (/ hour 24))
(define (day->nanosecond day)  (* day (* 24 60 60 tm:nano)))
(define (day->second day)      (* day (* 24 60 60)))
(define (day->minute day)      (* day (* 24 60)))
(define (day->hour day)        (* day 24))

(define (add-common-calendar-unit absolute unit amount)
  (+ absolute
     (case unit
       ((nanosecond) (nanosecond->day amount))
       ((second)     (second->day amount))
       ((minute)     (minute->day amount))
       ((hour)       (hour->day amount))
       ((day)        amount)
       (else (assertion-violation 'add-common-calendar-unit
				  "unknown unit" unit)))))
)
