;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/calendar/iso.scm - ISO calendar calculation
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

(library (sagittarius calendar iso)
    (export iso->absolute absolute->iso
	    make-iso-local-date iso-local-date?
	    iso-local-date-day iso-local-date-week iso-local-date-year

	    ;; aux APIs
	    iso-component->absolute
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius timezone)
	    (sagittarius time-util)
	    (sagittarius calendar gregorian)
	    (sagittarius calendar locals))

;; like 2018-W52-7
;; day  = 7
;; week = 52
;; year = 2018
(define-record-type iso-local-date
  (parent <local-date>)
  (fields day week year))

;;; Aux APIs
(define (absolute->day-of-week date) (mod date 7))
(define (kday-on-or-before k date) (- date (absolute->day-of-week (- date k))))
(define (kday-on-or-after k date) (kday-on-or-before k (+ date 6)))
(define (kday-before k date) (kday-on-or-before k (- date 1)))
(define (kday-after k date) (kday-on-or-after k (+ date 7)))
(define (nth-kday n k d m y tz)
  ;; do we need tz or can be GMT?
  (let ((d (gregorian-components->absolute 0 0 0 12 d m y tz)))
    (if (> n 0)
	(+ (* n 7) (kday-before k d))
	(+ (* n 7) (kday-after k d)))))

;;; Aux API
(define (iso-component->absolute n s m h d w y tz)
  (let ((hid (time-components->absolute n s m h tz)))
    (+ (nth-kday w 0 28 12 (- y 1) tz) d hid)))

;;; API
(define (iso->absolute local-time iso-date . maybe-tz)
  (define tz (if (null? maybe-tz) (local-timezone) (car maybe-tz)))
  (iso-component->absolute (local-time-nanosecond local-time)
			   (local-time-second local-time)
			   (local-time-minute local-time)
			   (local-time-hour local-time)
			   (iso-local-date-day iso-date)
			   (iso-local-date-week iso-date)
			   (iso-local-date-year iso-date)
			   tz))

;;; API
(define (absolute->iso date . maybe-tz)
  (define tz (if (null? maybe-tz) (local-timezone) (car maybe-tz)))
  (define (nsec->day nsec)
    (/ nsec tm:nano ;; -> sec
       60 ;; -> min
       60 ;; -> hour
       24 ;; -> day
       ))
  (let* ((approx (absolute->gregorian-year (- date 3) tz))
	 (tmp (iso-component->absolute 0 0 0 12 1 1 (+ approx 1) tz))
	 (year (if (>= date tmp) (+ approx 1) approx))
	 (week (+ (div (- date (iso-component->absolute 0 0 0 12 1 1 year tz))
		       7)
		  1))
	 (day (let ((d (mod date 7)))
		(if (zero? d)
		    (+ d 7)
		    d))))
    (let*-values (((d nsec) (absolute->day&nanosecond date))
		  ((n s m h c) (absolute->time-components (nsec->day nsec) tz)))
      (if (> c 0)
	  (fixup n s m h day week year c)
	  (values (make-common-local-time n s m h)
		  (make-iso-local-date day week year)
		  tz)))))
)
