;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/schema/types.scm - XML Schema Datatypes
;;;
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
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

;; ref:
;;  W3C XML Schema Definition Language (XSD) 1.1 Part 2: Datatypes
;;  https://www.w3.org/TR/xmlschema11-2/

#!nounbound
(library (text xml schema types)
    (export xs:any-type xs:any-type?
	    xs:any-simple-type xs:any-simple-type?
	    xs:any-atomic-type xs:any-atomic-type?

	    xs:duration xs:duration? (rename make-xs:duration xs:make-duration)
	    xs:duration-months xs:duration-seconds

	    xs:day-time-duration xs:day-time-duration?
	    (rename make-xs:day-time-duration xs:make-day-time-duration)

	    xs:year-month-duration xs:year-month-duration?
	    (rename make-xs:year-month-duration xs:make-year-month-duration)
	    
	    xs:qname xs:qname? (rename make-xs:qname xs:make-qname)
	    xs:qname-namespace-uri xs:qname-local-part xs:qname-prefix
	    xs:qname->node-name xs:qname->expanded-qname

	    xs:time xs:time? (rename make-xs:time xs:make-time)
	    xs:date xs:date? (rename make-xs:date xs:make-date)
	    xs:datetime xs:datetime? (rename make-xs:datetime xs:make-datetime)
	    (rename (xs:base-date-hour            xs:time-hour)
		    (xs:base-date-minute          xs:time-minute)
		    (xs:base-date-second          xs:time-second)
		    (xs:base-date-timezone-offset xs:time-timezone-offset)
		    (xs:base-date=?               xs:time=?)
		    (xs:base-date<?               xs:time<?)
		    (xs:base-date>?               xs:time>?)
		    (xs:base-date-year            xs:date-year)
		    (xs:base-date-month           xs:date-month)
		    (xs:base-date-day             xs:date-day)
		    (xs:base-date-timezone-offset xs:date-timezone-offset)
		    (xs:base-date=?               xs:date=?)
		    (xs:base-date<?               xs:date<?)
		    (xs:base-date>?               xs:date>?)
		    (xs:base-date-year            xs:datetime-year)
		    (xs:base-date-month           xs:datetime-month)
		    (xs:base-date-day             xs:datetime-day)
		    (xs:base-date-hour            xs:datetime-hour)
		    (xs:base-date-minute          xs:datetime-minute)
		    (xs:base-date-second          xs:datetime-second)
		    (xs:base-date-timezone-offset xs:datetime-timezone-offset)
		    (xs:base-date=?               xs:datetime=?)
		    (xs:base-date<?               xs:datetime<?)
		    (xs:base-date>?               xs:datetime>?))
	    xs:g-year xs:g-year? (rename make-xs:g-year xs:make-g-year)
	    xs:g-year-month xs:g-year-month?
	    (rename make-xs:g-year-month xs:make-g-year-month)
	    xs:g-month xs:g-month? (rename make-xs:g-month xs:make-g-month)
	    xs:g-month-day xs:g-month-day?
	    (rename make-xs:g-month-day xs:make-g-month-day)
	    xs:g-day xs:g-day? (rename make-xs:g-day xs:make-g-day)
	    (rename
	     (xs:base-date-year            xs:g-year-year)
	     (xs:base-date-timezone-offset xs:g-year-timezone-offset)
	     (xs:base-date-year            xs:g-year-month-year)
	     (xs:base-date-month           xs:g-year-month-month)
	     (xs:base-date-timezone-offset xs:g-year-month-timezone-offset)
	     (xs:base-date-month           xs:g-month-month)
	     (xs:base-date-timezone-offset xs:g-month-timezone-offset)
	     (xs:base-date-month           xs:g-month-day-month)
	     (xs:base-date-day             xs:g-month-day-day)
	     (xs:base-date-timezone-offset xs:g-month-day-timezone-offset)
	     (xs:base-date-day             xs:g-day-day)
	     (xs:base-date-timezone-offset xs:g-day-timezone-offset))
	    )
    (import (rnrs)
	    (sagittarius calendar)
	    (sagittarius timezone)
	    (srfi :13 strings)
	    (srfi :19 time)
	    (srfi :115 regexp))

;; 3 Built-in Datatypes and Their Definitions
;; for now, I implement what we need
;; we don't consider lexical space here
(define-record-type xs:any-type)

(define-record-type xs:any-simple-type
  (parent xs:any-type))

(define-record-type (xs:any-atomic-type dummy %any-atomic-type?)
  (parent xs:any-simple-type))

(define (xs:any-atomic-type? o)
  (or (%any-atomic-type? o)
      (string? o)  ;; anyURI base64Binary string
      (integer? o) ;; decimal and its hierarchies
      (real? o)	   ;; float double
      (boolean? o) ;; boolean
      ))


(define +duration-regex+
  (regexp
   '(: (? #\-) "P"
       (or (: (or (: ($ (+ (/ "09"))) "Y"      ;; 1
		     (? ($ (+ (/ "09"))) "M")  ;; 2
		     (? ($ (+ (/ "09"))) "D")) ;; 3
		  (: ($ (+ (/ "09"))) "M"      ;; 4
		     (? ($ (+ (/ "09"))) "D")) ;; 5
		  (: ($ (+ (/ "09"))) "D"))    ;; 6
	      (? "T"
		 (or (: ($ (+ (/ "09"))) "H"		  ;; 7 
			(? ($ (+ (/ "09"))) "M")	  ;; 8
			(? ($ (+ (/ "09")))		  ;; 9
			   (? "." ($ (+ (/ "09")))) "S")) ;; 10
		     (: ($ (+ (/ "09"))) "M"		  ;; 11
			(? ($ (+ (/ "09")))		  ;; 12
			   (? "." ($ (+ (/ "09")))) "S")) ;; 13
		     (: ($ (+ (/ "09")))		  ;; 14
			(? "." ($ (+ (/ "09")))) "S"))))  ;; 15

	   (: "T" (or (: ($ (+ (/ "09"))) "H"		     ;; 16
			 (? ($ (+ (/ "09"))) "M")	     ;; 17
			 (? ($ (+ (/ "09")))		     ;; 18
			    (? "." ($ (+ (/ "09")))) "S"))   ;; 19
		      (: ($ (+ (/ "09"))) "M"		     ;; 20
			 (? ($ (+ (/ "09")))		     ;; 21
			    (? "." ($ (+ (/ "09")))) "S"))   ;; 22
		      (: ($ (+ (/ "09")))		     ;; 23
			 (? "." ($ (+ (/ "09")))) "S"))))))) ;; 24
(define +duration-ymd-matches+
  '((1  2  3)
    (#f 4  5)
    (#f #f 6)))
(define +duration-hms-matches+
  '(( 7  8  9 10)
    (#f 11 12 13)
    (#f #f 14 15)
    (16 17 18 19)
    (#f 20 21 22)
    (#f #f 23 24)))

(define (parse-duration duration rx ymd hms)
  (define (submatch m p)
    ;; it's a bit of waste
    (let loop ((r '()) (p p) (matched? #f))
      (cond ((null? p) (and matched? (reverse r)))
	    ((not (car p)) (loop (cons #f r) (cdr p) matched?))
	    ((regexp-match-submatch m (car p)) =>
	     (lambda (v)
	       (loop (cons v r) (cdr p) #t)))
	    (else (loop (cons #f r) (cdr p) matched?)))))
  
  (define (parse m matchers)
    (define sample (car matchers))
    (cond ((exists (lambda (p) (submatch m p)) matchers) =>
	   (lambda (result) (apply values result)))
	  (else (apply values (map (lambda (_) #f) sample)))))
    
  (cond ((regexp-matches +duration-regex+ duration) =>
	 (lambda (m)
	   (let-values (((y mo d) (parse m ymd))
			((h mi s f) (parse m hms)))		     
	     (define neg? (char=? (string-ref duration 0) #\-))
	     (define (neg n) (if neg? (- n) n))
	     (values (neg (+ (or (and y (* 12 (string->number y))) 0)
			     (or (and mo (string->number mo)) 0)))
		     (neg (+ (or (and d (* 24 60 60 (string->number d))) 0)
			     (or (and h (* 60 60 (string->number h))) 0)
			     (or (and mi (* 60 (string->number mi))) 0)
			     (or (and s (string->number s)) 0)
			     ;; fraction?
			     (or (and f (string->number (string-append "0." f)))
				 0.0)))))))
	(else (assertion-violation 'parse-duration "Invalid duration"
				   duration))))

(define-record-type xs:duration
  (parent xs:any-atomic-type)
  (fields months seconds)
  (protocol (lambda (p)
	      (define (ctr m s)
		(when (or (and (negative? m) (positive? s))
			  (and (positive? m) (negative? s)))
		  (assertion-violation 'xs:make-duration
				       "Invalid months and seconds" m s))
		((p) m s))
	      (case-lambda
	       ((s)
		(let-values (((m s) (parse-duration s +duration-regex+
						    +duration-ymd-matches+
						    +duration-hms-matches+)))
		  (ctr m s)))
	       ((m s) (ctr m s))))))

(define-record-type xs:day-time-duration
  (parent xs:duration)
  (protocol (lambda (p)
	      (case-lambda 
	       ((d)
		(let-values (((m s) (if (string? d)
					(parse-duration d +duration-regex+
							+duration-ymd-matches+
							+duration-hms-matches+)
					(values 0 d))))
		  (unless (zero? m)
		    (assertion-violation 'xs:make-day-time-duration
					 "Month must not be specified" d))
		  ((p 0 s))))))))

(define-record-type xs:year-month-duration
  (parent xs:duration)
  (protocol (lambda (p)
	      (case-lambda
	       ((d)
		(let-values (((m s) (if (string? d)
					(parse-duration d +duration-regex+
							+duration-ymd-matches+
							+duration-hms-matches+)
					(values d 0.0))))
		  (unless (zero? s)
		    (assertion-violation 'xs:make-year-month-duration
					 "Second must not be specified" d))
		  ((p m 0.0))))))))

(define-record-type xs:qname
  (parent xs:any-atomic-type)
  ;; QName is defined rather weirdly on the specification
  ;; we take these fields from common sense
  (fields namespace-uri
	  local-part
	  prefix
	  ;; cache
	  >node-name
	  >expanded-qname)
  (protocol (lambda (p)
	      (define (make namespace-uri local-part prefix)
		((p) namespace-uri local-part prefix
		     (if (zero? (string-length prefix))
			 local-part
			 (string-append prefix ":" local-part))
		     (list prefix namespace-uri local-part)))
	      (case-lambda
	       ((namespace-uri local-part)
		(make namespace-uri local-part ""))
	       ((namespace-uri local-part prefix)
		(make namespace-uri local-part prefix))))))

(define-record-type xs:base-date
  (parent xs:any-atomic-type)
  (fields date has-tz? calendar-date)
  (protocol
   (lambda (p)
     (define (normalize d)
       ;; check 24:00:00
       (let ((hour (date-hour d))   
	     (mins (date-minute d))
	     (secs (date-second d)))
	 (cond ((< hour 24) d)
	       ((and (= hour 24) (zero? mins) (zero? secs))
		(make-date 0 0 0 0
			   (if (zero? (date-day d))
			       0
			       (+ (date-day d) 1))
			   (date-month d)
			   (date-year d) (date-zone-offset d)))
	       (else
		;; error
		(assertion-violation 'xs:normalize-date "Invalid time" d)))))
     (define (find-etc/gmt tz*)
       (or (find (lambda (tz) (string-prefix? "Etc" (timezone-name tz))) tz*)
	   (car tz*)))
     (lambda (d)
       (let* ((off (date-zone-offset d))
	      (tz (if off
		      (find-etc/gmt (zone-offset->timezones off))
		      (local-timezone)))
	      (nd (normalize
		   (if off
		       d
		       (make-date (date-nanosecond d) (date-second d)
				  (date-minute d) (date-hour d)
				  (date-day d) (date-month d)
				  (date-year d)
				  (timezone-offset tz)))))
	      (cd (time-utc->calendar-date (date->time-utc nd) tz)))
	 ((p)
	  (time-utc->date (calendar-date->time-utc cd) (timezone-offset tz))
	  (and off #t) cd))))))

(define-syntax define-base-date-accessor
  (lambda (x)
    (define (gen k prop)
      (define s (symbol->string (syntax->datum prop)))
      (datum->syntax k
       (list (string->symbol (string-append "xs:base-date-" s))
	     (string->symbol (string-append "date-" s)))))
    (syntax-case x ()
      ((k prop)
       (with-syntax (((name acc) (gen #'k #'prop)))
	 #'(k name acc)))
      ((k name acc)
       #'(define (name d) (acc (xs:base-date-date d)))))))
(define-base-date-accessor year)
(define-base-date-accessor month)
(define-base-date-accessor day)
(define-base-date-accessor hour)
(define-base-date-accessor minute)
(define-base-date-accessor second)
(define (xs:base-date-timezone-offset xd)
  (and (xs:base-date-has-tz? xd)
       (/ (date-zone-offset (xs:base-date-date xd)) 60)))

(define (xs:base-date=? d1 d2)
  (and
   (or (and (not (xs:base-date-timezone-offset d1))
	    (not (xs:base-date-timezone-offset d2)))
       (and (xs:base-date-timezone-offset d1)
	    (xs:base-date-timezone-offset d2)))
   (calendar-date=? (xs:base-date-calendar-date d1)
		    (xs:base-date-calendar-date d2))))
(define (xs:base-date<? d1 d2)
  (calendar-date<? (xs:base-date-calendar-date d1)
		   (xs:base-date-calendar-date d2)))
(define (xs:base-date>? d1 d2) (xs:base-date<? d2 d1))

(define (make-date-argument->date who len fmt)
  (define fmt/tz (string-append fmt "~z"))
  (lambda (s)
    (cond ((string? s)
	   (if (> (string-length s) len)
	       (string->date s fmt/tz)
	       ;; string->date without tz converts local tz
	       ;; but we don't want to it, so recreate
	       (let ((d (string->date s fmt)))
		 (make-date (date-nanosecond d) (date-second d)
			    (date-minute d) (date-hour d)
			    (date-day d) (date-month d) (date-year d)
			    #f))))
	  ((date? s) s)
	  (else (assertion-violation who "Invalid argument" s)))))
(define (get-offset maybe-offset)
  (cond ((null? maybe-offset) #f)
	((not (car maybe-offset)) #f)
	(else (* (car maybe-offset) 60))))

;; maybe we should use calander libraryy...
(define-record-type xs:time
  (parent xs:base-date)
  (protocol (lambda (p)
	      (define ->date
		(make-date-argument->date 'xs:make-time 8 "~H:~M:~S"))
	      (case-lambda
	       ((s) ((p (->date s))))
	       ((h m s . offset)
		(let ((off (get-offset offset)))
		  ((p (make-date 0 s m h 0 0 0 off)))))))))

(define-record-type xs:date
  (parent xs:base-date)
  (protocol (lambda (p)
	      (define ->date
		(make-date-argument->date 'xs:make-date 10 "~Y-~m-~d"))
	      (case-lambda
	       ((s) ((p (->date s))))
	       ((y m d . offset)
		(let ((off (get-offset offset)))
		  ((p (make-date 0 0 0 0 d m y off)))))))))

(define-record-type xs:datetime
  (parent xs:date)
  (protocol (lambda (p)
	      (define ->date
		(make-date-argument->date 'xs:make-datetime 19
					  "~Y-~m-~dT~H:~M:~S"))
	      (case-lambda
	       ((s) ((p (->date s))))
	       ((y m d h mi s . offset)
		(let ((off (get-offset offset)))
		  ((p (make-date 0 s mi h d m y off)))))))))

(define-record-type xs:g-year
  (parent xs:base-date)
  (protocol (lambda (p)
	      (define ->date
		(make-date-argument->date 'xs:make-g-year 4 "~Y"))
	      (case-lambda
	       ((s) ((p (->date s))))
	       ((y . offset)
		(let ((off (get-offset offset)))
		  ((p (make-date 0 0 0 0 0 0 y off)))))))))
(define-record-type xs:g-year-month
  (parent xs:base-date)
  (protocol (lambda (p)
	      (define ->date
		(make-date-argument->date 'xs:make-g-year-month 7 "~Y-~m"))
	      (case-lambda
	       ((s) ((p (->date s))))
	       ((y m . offset)
		(let ((off (get-offset offset)))
		  ((p (make-date 0 0 0 0 0 m y off)))))))))
(define-record-type xs:g-month
  (parent xs:base-date)
  (protocol (lambda (p)
	      (define ->date
		(make-date-argument->date 'xs:make-g-month 4 "--~m"))
	      (case-lambda
	       ((s) ((p (->date s))))
	       ((m . offset)
		(let ((off (get-offset offset)))
		  ((p (make-date 0 0 0 0 0 m 0 off)))))))))

(define-record-type xs:g-month-day
  (parent xs:base-date)
  (protocol (lambda (p)
	      (define ->date
		(make-date-argument->date 'xs:make-g-month 7 "--~m-~d"))
	      (case-lambda
	       ((s) ((p (->date s))))
	       ((m d . offset)
		(let ((off (get-offset offset)))
		  ((p (make-date 0 0 0 0 d m 0 off)))))))))

(define-record-type xs:g-day
  (parent xs:base-date)
  (protocol (lambda (p)
	      (define ->date
		(make-date-argument->date 'xs:make-g-day 5 "---~d"))
	      (case-lambda
	       ((s) ((p (->date s))))
	       ((d . offset)
		(let ((off (get-offset offset)))
		  ((p (make-date 0 0 0 0 d 0 0 off)))))))))

)