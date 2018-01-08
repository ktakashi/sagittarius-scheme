;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/calender.scm - calender
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

(library (sagittarius calender)
    (export (rename (calendar-type <calender-type>)) calendar-type?
	    calendar-type:rfc3339 calendar-type:gregorian
	    calendar-type:julian
	    (rename (calendar-type:rfc3339 calendar-type:system))

	    (rename (calendar <calendar>)) make-calendar calendar?
	    calendar-add! calendar-add
	    calendar-subtract! calendar-subtract)
    (import (rnrs)
	    (sagittarius time-private)
	    (sagittarius time-util)
	    (sagittarius timezone))

(define-record-type calendar-type
  (fields decoder))
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
(define calendar-type:rfc3339 (make-calendar-type julian-day-decoder))
(define calendar-type:gregorian (make-calendar-type julian-day-decoder))
;; TBD
(define calendar-type:julian (make-calendar-type #f))

(define (calendar-type:normalize nanosecond second minute hour day month year)
  ;; e.g. 69 sec -> 1min + 9 sec
  (values nanosecond second minute hour day month year))
(define (calendar-type-decode-time calendar-type time timezone)
  ((calendar-type-decoder calendar-type) time timezone))

(define-record-type calendar
  ;; absent = #f
  (fields (mutable nanosecond)
	  (mutable second)
	  (mutable minute)
	  (mutable hour)
	  (mutable day)
	  (mutable month)
	  (mutable year)
	  (mutable timezone) ;; timezone
	  type		     ;; calendar-type
	  )
  (protocol
   (lambda (p)
     (lambda (n s m h d M y :optional (timezone (local-timezone))
		(type calendar-type:rfc3339))
       ;; TODO check integer or #f
       (p n s m h d M y timezone type)))))

(define (time-utc->calendar time :optional (timezone (local-timezone))
			    (type calendar-type:rfc3339))
  (unless (eq? (time-type time) 'time-utc)
    (assertion-violation 'time-utc->calendar "invalid time type" time))
  (let-values (((n s m h d M y) (calendar-type-decode-time type time timezone)))
    (make-calendar n s m h d M y timezone type)))

(define (convert-calendar cal type)
  (error 'convert-calendar "not yet"))

;; API
(define (calendar-add! calendar unit amount)
  (error 'calendar-add "not-yet"))

;; API
(define (calendar-subtract! calendar unit amount)
  (error 'calendar-add "not-yet"))

;; API
(define (calendar-add calendar unit amount)
  (calendar-add! (copy-calendar calendar) unit amount))

;; API
(define (calendar-subtract calendar unit amount)
  (calendar-subtract! (copy-calendar calendar) unit amount))


)
