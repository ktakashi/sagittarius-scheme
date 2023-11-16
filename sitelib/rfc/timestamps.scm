;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/timestamps.scm - RFC 3339 Date and Time on the Internet: Timestamps
;;;  
;;;   Copyright (c) 2023  Takashi Kato  <ktakashi@ymail.com>
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

;; RFC 3339 is *not* subset of ISO 8601.
;; see: https://ijmacd.github.io/rfc3339-iso8601/
;; But the format is widely used especially web application
;; (most of the time, ISO 8601 format means RFC 3339.
;;  e.g. yyyy-MM-ddThh:MM:ss.nnnZ format, this is defined in the
;;       both specification though).
;; TODO If we support complete ISO 8601 format, then use the parser
;;      in this library and enhance it.

;; for now, only parsers

;; reference
;; https://datatracker.ietf.org/doc/html/rfc3339#section-5.6
#!nounbound
(library (rfc timestamps)
    (export parse-date parse-time parse-date-time)
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (sagittarius timezone)
	    (srfi :14 char-sets)
	    (srfi :19 time)
	    (srfi :144 flonums))

;; APIs
(define (parse-date s) 
  (define (->date v)
    (let-values (((d m h) (apply values v)))
      (make-date 0 0 0 0 d m h (timezone-offset (local-timezone)))))
  (parse s $full-date ->date))
(define (parse-time s)
  (define (->date v)
    (let-values (((off f s m h) (apply values v)))
      (make-date f s m h 0 0 0 off)))
  (parse s $full-time ->date))
(define (parse-date-time s)
  (define (->date v) (apply make-date v))
  (parse s $date-time ->date))

;; parsers
(define $digit ($char-set-contains? (string->char-set "0123456789")))
(define ($number n)
  ($let ((d* ($repeat $digit n)))
    ($return (string->number (list->string d*)))))

(define $date-fullyear ($number 4))
(define $date-month
  ($let ((n ($number 2)))
    (if (in-range? n 1 12) ($return n) ($expect "2 digits of month"))))
(define $date-mday
  ($let ((n ($number 2)))
    (if (in-range? n 1 31) ($return n) ($expect "2 digits of date"))))

(define $time-hour
  ($let ((n ($number 2)))
    (if (in-range? n 0 23) ($return n) ($expect "2 digits of hour"))))
(define $time-minute
  ($let ((n ($number 2)))
    (if (in-range? n 0 59) ($return n) ($expect "2 digits of minute"))))
(define $time-second
  ($let ((n ($number 2)))
    (if (in-range? n 0 60) ($return n) ($expect "2 digits of second"))))

(define $time-secfrac
  ($let (( ($eqv? #\.) )
	 (d* ($many $digit 1)))
    (let ((l (length d*))
	  (n (string->number (list->string d*))))
      (let-values (((i n) (flinteger-fraction (* n (expt 10. (- 9 l))))))
	($return (exact i))))))
(define $time-numoffset
  ($let ((s ($or ($eqv? #\+) ($eqv? #\-)))
	 (h $time-hour)
	 ( ($eqv? #\:) )
	 (m $time-minute))
   ($return (let ((n (+ (* 3600 h) (* 60 m)))) (if (eqv? s #\-) (- n) n)))))
(define $time-offset
  ($or ($seq ($or ($eqv? #\z) ($eqv? #\Z)) ($return 0))
       $time-numoffset))

(define $partial-time
  ($let ((h $time-hour)
	 ( ($eqv? #\:) )
	 (m $time-minute)
	 ( ($eqv? #\:) )
	 (s $time-second)
	 (f ($optional $time-secfrac 0)))
   ($return (list f s m h))))
(define $full-date
  ($let ((y $date-fullyear)
	 ( ($eqv? #\-) )
	 (m $date-month)
	 ( ($eqv? #\-) )
	 (d $date-mday))
   (if (valid-date? y m d)
       ($return (list d m y))
       ($fail "Invalid date"))))
(define $full-time
  ($let ((pt $partial-time)
	 (off $time-offset))
    (if (valid-time? pt off)
	($return (cons off pt))
	($fail "Invalid time"))))

;; do we want to support configurable separator?
(define $date-time
  ($let ((d $full-date)
	 ( ($or ($eqv? #\t) ($eqv? #\T)) )
	 (t $full-time))
    ($return `(,@(cdr t) ,@d ,(car t)))))


;; utilities
(define (parse s parser ->date)
  (let-values (((s v nl) (parser (string->list s))))
    (and (parse-success? s)
	 (null? nl)
	 (->date v))))

(define (in-range? v s e) (and (<= s v) (<= v e)))
(define (valid-date? y m d)
  (define (leap-year? y)
    (and (zero? (mod y 4))
	 (or (not (zero? (mod y 100)))
	     (zero? (mod y 400)))))
  (if (and (leap-year? y) (= m 2))
      (<= d 29)
      (cond ((assv m *mday-map*) => (lambda (s) (<= d (cdr s))))
	    ;; invalid month
	    (else #f))))

(define (valid-time? pt offset)
  (let-values (((f s m h) (apply values pt)))
    (let* ((d0 (make-date f s m h 1 1 1970 offset))
	   (t0 (date->time-utc d0))
	   (d1 (time-utc->date t0 offset)))
      (or (and (= (date-nanosecond d0) (date-nanosecond d1))
	       (= (date-second d0) (date-second d1))
	       (= (date-minute d0) (date-minute d1))
	       (= (date-hour d0) (date-hour d1)))
	  ;; leap second...
	  (let ((d3 (time-utc->date t0 0)))
	    (and (= (date-second d3) 0)
		 (= (date-minute d3) 0)
		 (= (date-hour d3) 0)
		 (time=? t0 (date->time-utc d1))))))))

(define *mday-map*
  `(( 1 . 31)
    ( 2 . 28)
    ( 3 . 31)
    ( 4 . 30)
    ( 5 . 31)
    ( 6 . 30)
    ( 7 . 31)
    ( 8 . 31)
    ( 9 . 30)
    (10 . 31)
    (11 . 30)
    (12 . 31)))
)
