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
(library (sagittarius calendar gregorian)
    (export gregorian->absolute absolute->gregorian)
    (import (rnrs)
	    (sagittarius timezone))

;; TODO move
(define-syntax sum
  (syntax-rules ()
    ((_ expr index init cond)
     (do ((tmp 0 (+ tmp expr)) (index init (+ index 1)))
	 ((not cond) tmp)))))

(define (last-day-of-gregorian-month month year)
  (if (and (= month 2)
	   (zero? (mod year 4))
	   (not (memv (mod year 400) '(100 200 300))))
      29
      (vector-ref '#(31 28 31 30 31 30 31 31 30 31 30 31) (- month 1))))

;; we put absolute date 1 is 1/Jan/1 12:00:00.0 UTC+0
(define (gregorian->absolute n s m h d M y . maybe-tz)
  (define timezone (if (null? maybe-tz) (local-timezone) (car maybe-tz)))
  (define y-1 (- y 1))
  (+ d 
     (sum (last-day-of-gregorian-month m* y) m* 1 (< m* M))
     (* 365 y-1)
     (div y-1 4)
     (- (div y-1 100))
     (div y-1 400)
     -1/2 ;; julian day of 0 starts 12:00:00 so subtract it
     (/ (+ (* h 60 60)
	   (* m 60)
	   s
	   (/ n tm:nano)
	   (- (timezone-offset timezone)))
	tm:sid)))

(define *timezone/gmt* (timezone "GMT"))

(define (absolute->gregorian gd . maybe-tz)
  (define tz (if (null? maybe-tz) (local-timezone) (car maybe-tz)))
  ;; the following 2 are using GMT timezone for simplicity
  (define (compute-year d apprx)
    (+ apprx
       (sum 1 y apprx 
	    (>= d (gregorian->absolute 0 0 0 12 1 1 (+ 1 y) *timezone/gmt*)))))
  (define (compute-month d year)
    (+ 1 (sum 1 m 1 (> d (gregorian->absolute 0 0 0 12
			  (last-day-of-gregorian-month m year) m year
			  *timezone/gmt*)))))
  (define (compute-day d month year)
    ;; use passed timezone here to compute offset properly
    (- d (- (gregorian->absolute 0 0 0 12 1 month year tz) 1)))
  
  (define (day&secs gd)
    (if (and (exact? gd) (integer? gd))
	(values gd 0)
	(let* ((igd (inexact gd))
	       (fgd (floor igd)))
	  (values (exact fgd) (exact (* 24 3600 tm:nano (- igd fgd)))))))

  (define (compute-hours day nsec tz)
    (define offset-day (* day 24))
    (define (compute day nsec)
      (define (parse-nanosec nsec)
	(values (mod nsec tm:nano) (floor (/ nsec tm:nano))))
      (let-values (((nsec sec) (parse-nanosec nsec)))
	(let* ((h (+ (div sec 3600) 12)) ;; sum offset
	       (r (mod sec 3600))
	       (m (div r 60))
	       (s (mod r 60)))
	  (values (exact (floor nsec)) s m h day))))
    (let-values (((d ns) (day&secs day)))
      (let ((nsec (+ nsec ns)))
	(compute d nsec))))
  (unless (positive? gd)
    (assertion-violation 'absolute->gregorian "Positive number required" gd))
  (let-values (((d nsec) (day&secs gd)))
    (let* ((approx (div d 366))
	   (year (compute-year d approx))
	   (month (compute-month d year))
	   (day (compute-day d month year)))
      (let-values (((n s m h day) (compute-hours day nsec tz)))
	(values n s m h day month year)))))

)

