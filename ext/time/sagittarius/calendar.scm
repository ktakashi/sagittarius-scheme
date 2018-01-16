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
	    calendar:gregorian
	    calendar:julian
	    (rename (calendar:gregorian calendar:system))
	    calendar-name

	    (rename (calendar-date <calendar-date>))
	    make-calendar-date calendar-date? 
	    time-utc->calendar-date calendar-date->time-utc
	    calendar-date-calendar
	    calendar-date-absolute-date
	    calendar-date-timezone

	    calendar-date->julian-day
	    
	    calendar-date-add  calendar-date-subtract)
    (import (rnrs)
	    (rnrs r5rs)
	    (clos user)
	    (sagittarius)
	    (sagittarius time-private)
	    (sagittarius time-util)
	    (sagittarius timezone)
	    (sagittarius calendar constants)
	    (sagittarius calendar gregorian)
	    (sagittarius calendar iso))

(define-record-type calendar
  (fields name
	  components->absolute
	  absolute->components))

(define calendar:gregorian (make-calendar "Gregorian"
					  gregorian-components->absolute
					  absolute->gregorian-components))
(define calendar:iso (make-calendar "ISO"
				    iso-components->absolute
				    absolute->iso-components))
;; TBD
(define calendar:julian (make-calendar "Julian" #f #f))

;; time-utc -> absolute date
(define (time-utc->absolute time timezone)
  (define (nanosecond->date nsec) (/ nsec tm:nano 60 60 24))
  (let ((sec (+ (time-second time) (timezone-offset timezone)
		+epoch-in-utc-second+)))
    (+ (nanosecond->date (+ (* sec tm:nano) (time-nanosecond time))) 1)))

(define (absolute->time-utc absolute timezone)
  (define (date->nanosecond date) (* date tm:sid tm:nano))
  (let ((nsec (date->nanosecond (- absolute 1))))
    (make-time time-utc
	       (mod nsec tm:nano)
	       (- (floor (/ nsec tm:nano))
		  +epoch-in-utc-second+
		  (timezone-offset timezone)))))

(define-record-type calendar-date
  ;; absent = #f
  (fields absolute-date ;; 1 = 1/1/1 12:00:00
	  timezone ;; timezone
	  calendar ;; calendar-type
	  ))

(define (%make-calendar-date calendar . components)
  (let-values (((absolute tz)
		(apply (calendar-components->absolute calendar)
		       components)))
    (make-calendar-date absolute tz calendar)))
(define (make-gregorian-calendar-date n s m h d M y
				      :optional (timezone (local-timezone)))
  (%make-calendar-date calendar:gregorian n s m h d M y timezone))
(define (make-iso-calendar-date n s m h d w y
				:optional (timezone (local-timezone)))
  (%make-calendar-date calendar:iso n s m h d w y timezone))


(define (time-utc->calendar-date time :optional (timezone (local-timezone))
				 (calendar calendar:gregorian))
  (unless (eq? (time-type time) 'time-utc)
    (assertion-violation 'time-utc->calendar "invalid time type" time))
  (let ((absolute (time-utc->absolute time timezone)))
    (make-calendar-date absolute timezone calendar)))

(define (calendar-date->time-utc cd)
  (let ((absolute (calendar-date-absolute-date cd))
	(timezone (calendar-date-timezone cd)))
    (absolute->time-utc absolute timezone)))

(define (calendar-date->julian-day cd)
  (+ (calendar-date-absolute-date cd) +julian-day-offset+))

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
  (format out "#<calendar-date ~a ~d (~a)>"
	  (calendar-date-calendar cd)
	  (calendar-date-absolute-date cd)
	  (calendar-date-timezone cd)))
)
