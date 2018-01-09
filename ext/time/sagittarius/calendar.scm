;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/calendar.scm - calender
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

#!nounbound
(library (sagittarius calendar)
    (export (rename (calendar <calender>)) calendar?
	    calendar:rfc3339 calendar:gregorian
	    calendar:julian
	    (rename (calendar:rfc3339 calendar:system))
	    calendar-name

	    (rename (calendar-date <calendar-date>))
	    make-calendar-date calendar-date? 
	    time-utc->calendar-date calendar-date->time-utc
	    calendar-date-calendar
	    calendar-date-year
	    calendar-date-month
	    calendar-date-day
	    calendar-date-hour
	    calendar-date-minute
	    calendar-date-second
	    calendar-date-nanosecond
	    calendar-date-timezone

	    calendar-date-add  calendar-date-subtract)
    (import (rnrs)
	    (rnrs r5rs)
	    (clos user)
	    (sagittarius)
	    (sagittarius time-private)
	    (sagittarius time-util)
	    (sagittarius timezone))

(define-record-type calendar
  (fields name
	  decoder
	  encoder))
(define (julian-day-decoder time timezone)
  (define offset (timezone-offset timezone))
  (let-values (((secs date month year)
		(tm:decode-julian-day-number
		 (tm:time->julian-day-number (time-second time) offset))))
    (let* ((hours    (quotient secs (* 60 60)))
	   (rem      (remainder secs (* 60 60)))
	   (minutes  (quotient rem 60))
	   (seconds  (remainder rem 60)))
      (values (time-nanosecond time)
	      seconds
	      minutes
	      hours
	      date
	      month
	      year))))

(define (julian-day-encoder nanosecond second minute hour
			    day month year timezone)
  (define offset (timezone-offset timezone))
  (let ( (jdays (- (tm:encode-julian-day-number day month year)
		   tm:tai-epoch-in-jd)) )
    (make-time time-utc nanosecond
	       (+ (* (- jdays 1/2) 24 60 60)
		  (* hour 60 60)
		  (* minute 60)
		  second
		  (- offset)))))
(define calendar:rfc3339 (make-calendar "RFC 3339"
					julian-day-decoder
					julian-day-encoder))
(define calendar:gregorian (make-calendar "Gregorian"
					  julian-day-decoder
					  julian-day-encoder))
;; TBD
(define calendar:julian (make-calendar "Julian" #f #f))

(define (calendar-normalize calendar
			    nanosecond second minute hour day month year
			    timezone)
  (define encoder (calendar-encoder calendar))
  (define decoder (calendar-decoder calendar))
  ;; it's inefficient but works perfectly fine
  (decoder (encoder nanosecond second minute hour day month year timezone)
	   timezone))
(define (calendar-decode-time-utc calendar time timezone)
  ((calendar-decoder calendar) time timezone))

(define-record-type calendar-date
  ;; absent = #f
  (fields nanosecond
	  second
	  minute
	  hour
	  day
	  month
	  year
	  timezone ;; timezone
	  calendar ;; calendar-type
	  )
  (protocol
   (lambda (p)
     (lambda (n s m h d M y :optional (timezone (local-timezone))
		(calendar calendar:rfc3339))
       (let-values (((n s m h d M y) (calendar-normalize
				      calendar
				      n s m h d M y timezone)))
	 (p n s m h d M y timezone calendar))))))

(define (time-utc->calendar-date time :optional (timezone (local-timezone))
				 (calendar calendar:rfc3339))
  (unless (eq? (time-type time) 'time-utc)
    (assertion-violation 'time-utc->calendar "invalid time type" time))
  (let-values (((n s m h d M y)
		(calendar-decode-time-utc calendar time timezone)))
    (make-calendar-date n s m h d M y timezone calendar)))
(define (calendar-date->time-utc cd)
  (define encoder (calendar-encoder (calendar-date-calendar cd)))
  (encoder (calendar-date-nanosecond cd)
	   (calendar-date-second cd)
	   (calendar-date-minute cd)
	   (calendar-date-hour cd)
	   (calendar-date-day cd)
	   (calendar-date-month cd)
	   (calendar-date-year cd)
	   (calendar-date-timezone cd)))

(define (convert-calendar-date cd calendar :optional (timezone #f))
  (define encoder (calendar-encoder calendar))
  (define tz (or timezone (calendar-date-timezone cd)))
  (let ((time (encoder (calendar-date-nanosecond cd)
		       (calendar-date-second cd)
		       (calendar-date-minute cd)
		       (calendar-date-hour cd)
		       (calendar-date-day cd)
		       (calendar-date-month cd)
		       (calendar-date-year cd)
		       tz)))
    (time-utc->calendar-date time timezone calendar)))

;; API
(define (calendar-date-add calendar-date unit amount)
  (error 'calendar-date-add "not-yet"))

;; API
(define (calendar-date-subtract calendar-date unit amount)
  (error 'calendar-date-subtract "not-yet"))

;; print
(define-method write-object ((c calendar) out)
  (format out "#<calendar ~a>" (calendar-name c)))
(define-method write-object ((cd calendar-date) out)
  (format
   out "#<calendar-date ~a ~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d.~9,'0d (~a)>"
   (calendar-date-calendar cd)
   (calendar-date-year cd)
   (calendar-date-month cd)
   (calendar-date-day cd)
   (calendar-date-hour cd)
   (calendar-date-minute cd)
   (calendar-date-second cd)
   (calendar-date-nanosecond cd)
   (calendar-date-timezone cd)))
)
