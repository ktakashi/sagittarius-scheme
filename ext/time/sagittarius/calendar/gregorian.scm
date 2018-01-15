;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/calendar/gregorian.scm - Gregorian calculation
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

;; reference
;; * Calendrical Calculations - Nachum Dershowitz and Edward M. Reingold 
;;   http://reingold.co/cc-paper.pdf
;; * Calendrical Calculations, II: Three Historical Calendars
;;     Edward M. Reingold, Nachum Dershowitz, and Stewart M. Clamen
;;   http://reingold.co/cc2-paper.pdf
;; * Year zero - Wikipedia
;;   https://en.wikipedia.org/wiki/Year_zero
(library (sagittarius calendar gregorian)
    (export gregorian->absolute absolute->gregorian
	    ;; helpers for other calendars
	    absolute->gregorian-component gregorian-components->absolute
	    gregorian-leap-year? gregorian-new-year absolute->gregorian-year
	    +gregorian-epoch+)
    (import (rnrs)
	    (sagittarius) ;; for define-constant
	    (sagittarius time-util)
	    (sagittarius timezone)
	    (sagittarius calendar locals))

(define-constant +gregorian-epoch+ 1)
(define *timezone/gmt* (timezone "GMT"))

;;; Aux API
(define (gregorian-leap-year? y)
  (and (zero? (mod y 4))
       (not (memv (mod y 400) '(100 200 300)))))

;;; Aux API
(define (gregorian-components->absolute n s m h d M y tz)
  (define y-1 (- y 1))
  (+ (- +gregorian-epoch+ 1)
     (* 365 y-1)
     (div y-1 4)
     (- (div y-1 100))
     (div y-1 400)
     (div (- (* 367 M) 362) 12)
     (cond ((<= M 2) 0)
	   ((gregorian-leap-year? y) -1)
	   (else -2))
     d
     (time-components->absolute n s m h tz)))

;;; API
(define (gregorian->absolute local-time local-date . maybe-tz)
  (define tz (if (null? maybe-tz) (local-timezone) (car maybe-tz)))
  (gregorian-components->absolute (common-local-time-nanosecond local-time)
				  (common-local-time-second local-time)
				  (common-local-time-minute local-time)
				  (common-local-time-hour local-time)
				  (common-local-date-day local-date)
				  (common-local-date-month local-date)
				  (common-local-date-year local-date)
				  tz))
;;; Aux API
(define (gregorian-new-year y)
  (gregorian-components->absolute 0 0 0 12 1 1 y *timezone/gmt*))

;;; Aux API
(define (absolute->gregorian-year d tz)
  (define dis (/ (timezone-offset tz) tm:sid))
  (let* ((d0 (- (+ d dis) +gregorian-epoch+))
	 (n400 (div d0 146097))
	 (d1 (mod d0 146097))
	 (n100 (div d1 36524))
	 (d2 (mod d1 36524))
	 (n4 (div d2 1461))
	 (d3 (mod d2 1461))
	 (n1 (div d3 365))
	 (year (+ (* 400 n400) (* 100 n100) (* 4 n4) n1)))
    (if (or (= n100 4) (= n1 4))
	year
	(+ year 1))))

(define (last-day-of-gregorian-month month year)
  (if (and (= month 2) (gregorian-leap-year? year))
      29
      (vector-ref '#(31 28 31 30 31 30 31 31 30 31 30 31) (- month 1))))

;;; Aux API
(define (absolute->gregorian-component date tz)
  (let* ((d (exact date)) ;; make sure it's exact number
	 (year (absolute->gregorian-year d tz))
	 (prior-days (- d (gregorian-new-year year)))
	 (correction-d (gregorian-components->absolute 0 0 0 12 1 3 year tz))
	 (correction (cond ((< d correction-d) 0)
			   ((gregorian-leap-year? year) 1)
			   (else 2)))
	 (month (div (+ (* 12 (+ prior-days correction)) 373) 367))
	 (day (+ (- d (gregorian-components->absolute 0 0 0 12 1
						      month year tz)) 1)))
    (define (fixup n s m h day month year)
      (let ((last-day (last-day-of-gregorian-month month year)))
	(if (> day last-day)
	    (let ((m (+ month 1))
		  (d 1))
	      (if (> m 12)
		  (values n s m h d 1 (+ year 1))
		  (values n s m h d m year)))
	    (values n s m h day month year))))
    (let*-values (((n s m h carry) (absolute->time-components day tz))
		  ((month year) (if (> month 12)
				    (values 1 (+ year 1))
				    (values month year))))
      ;; 1 BC is followed by 1 AD so no 0 AD or 0 BC.
      (let ((year (if (<= d 0) (- year 1) year))
	    (day (exact (floor (+ day carry)))))
	  (if (> carry 0)
	      (fixup n s m h day month year)
	      (values n s m h day month year))))))

;;; API
(define (absolute->gregorian date  . maybe-tz)
  (define tz (if (null? maybe-tz) (local-timezone) (car maybe-tz)))
  (let-values (((n s m h d M y) (absolute->gregorian-component date tz)))
    (values (make-common-local-time n s m h)
	    (make-gregorian-local-date d M y)
	    tz)))
)

