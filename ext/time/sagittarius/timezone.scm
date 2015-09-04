;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/timezone.scm - Timezone library
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (sagittarius timezone)
    (export local-timezone
	    timezone?
	    timezone
	    timezone-name
	    timezone-short-name
	    timezone-raw-offset
	    timezone-offset
	    timezone-dst?
	    <timezone>

	    ;; utilities
	    timezone-name-list

	    ;; for debug
	    timezone-rules
	    timezone-rule-name
	    )
    (import (core)
	    (core base)
	    (core io)
	    (core errors)
	    (core record)
	    (sagittarius)
	    (sagittarius time-private)
	    (sagittarius time-util)
	    (clos user))
	    
  (define-constant +tzdata+ (include "tzdata.scm"))
  (define-constant +tz-zone+ 0)
  (define-constant +tz-rules+ 1)
  (define-constant +tz-alias+ 2)

  ;; should we copy them each time?
  (define timezone-name-list
    (let ((list (vector->list 
		 (vector-map car (vector-ref +tzdata+ +tz-zone+)))))
      (lambda () list)))
  
  ;; timezone record. this is a immutable record
  (define-record-type (<timezone> make-timezone timezone?)
    (fields (immutable name timezone-name)
	    ;; these 4 slots are for convenience
	    ;; it contains *latest* timezone info
	    (immutable short-name %timezone-short-name)
	    (immutable raw-offset %timezone-raw-offset)
	    (immutable rule-name  timezone-rule-name)
	    (immutable start-offet timezone-start-offset)
	    ;; histories
	    (immutable histories  timezone-histories)
	    (immutable rules      timezone-rules)))

  (define-method write-object ((tz <timezone>) out)
    (format out "#<timezone ~a>" (timezone-name tz)))

  (define (binary-search array name)
    (let loop ((from 0) (to (- (vector-length array) 1)))
      (and (<= from to)
	   (let* ((mid (div (+ from to) 2))
		  (t (vector-ref array mid))
		  (r (compare (car t) name)))
	     (cond ((zero? r) t)
		   ((positive? r) (loop from (- mid 1)))
		   (else (loop (+ mid 1) to)))))))

  (define (create-timezone name)
    (define (find-rules histories)
      (define rules (vector-ref +tzdata+ +tz-rules+))
      (let loop ((h histories) (r '()))
	(if (null? h)
	    (reverse! r)
	    (or (and-let* ((name (cadar h))
			   ( (not (assoc name r)) )
			   (rule (binary-search rules name)))
		  (loop (cdr h) (cons rule r)))
		;; no rule for this name
		(loop (cdr h) r)))))
    ;; zone history contains [UNTIL]. we want to have [FROM]
    ;; so make [UNTIL] to [FROM] here
    (define (shiftup-histories histories)
      (define (drop-last p) (take p (- (length p) 1)))
	
      (let loop ((h histories) (p #f) (r '()))
	(cond ((null? h)
	       (if p
		   (if (= (length p) 3)
		       (reverse! (cons `(,@p #f) r))
		       (reverse! (cons (drop-last p) r)))
		   (reverse! r)))
	      (p 
	       (let ((c (car h)))
		 (if (= (length p) 3)
		     (loop (cdr h) (car h) (cons `(,@p ,(cadddr c)) r))
		     (loop (cdr h) (car h) 
			   (cons `(,@(drop-last p) ,(cadddr c)) r)))))
	      (else (loop (cdr h) (car h) r)))))
    (and-let* ((zone (binary-search (vector-ref +tzdata+ +tz-zone+) name))
	       (rules (find-rules (cdr zone)))
	       (shifted (shiftup-histories (cdr zone)))
	       (current (car shifted))
	       (histories (cdr shifted)))
      (make-timezone name (caddr current) (car current) (cadr current)
		     (cadddr current) 
		     histories 
		     ;; ->vector
		     (map (lambda (rule)
			    (cons (car rule) (map list->vector (cdr rule))))
			  rules))))

  (define timezone
    (let ((cache (make-string-hashtable))
	  (alias (vector-ref +tzdata+ +tz-alias+)))
      (lambda (name)
	;; resolve alias first
	(let ((name (or (cond ((assoc name alias) => cdr) (else #f)) name)))
	  (cond ((hashtable-ref cache name #f))
		((create-timezone name) =>
		 (lambda (tz) (hashtable-set! cache name tz) tz))
		;; fallback to "GMT"
		(else (timezone "GMT")))))))

  ;; this considers DST.
  (define (timezone-offset tz :optional (when (current-time)))
    (let-values (((zone rule) (timezone-zone&rule tz when)))
      (let ((raw-offset (%timezone-raw-offset zone)))
	(+ raw-offset (vector-ref rule 5)))))

  (define (timezone-dst? tz :optional (when (current-time)))
    (let-values (((zone rule) (timezone-zone&rule tz when)))
      (let ((dst (vector-ref rule 5)))
	(and dst (positive? dst)))))

  ;; TODO cache it?
  (define (timezone-short-name tz :optional (when (current-time)))
    (let-values (((zone rule) (timezone-zone&rule tz when)))
      (let ((letter (vector-ref rule 6)))
	(if (and letter (string-scan (%timezone-short-name zone) "~"))
	    (format (%timezone-short-name zone) letter)
	    (%timezone-short-name zone)))))

  (define (timezone-raw-offset tz :optional (when #f))
    (if when
	(let-values (((zone rule) (timezone-zone&rule tz when)))
	  (%timezone-raw-offset zone))
	;; this is current
	(%timezone-raw-offset tz)))

  (define (local-timezone) (timezone (local-timezone-name)))

  (cond-expand
   ((or windows cygwin)
    (define-constant +win-mappings+ (include "win-mappings.scm"))
    (define (local-timezone-name)
      (let* ((zone-id (%local-timezone-name))
	     ;; first comes first serve...
	     (tzid (binary-search +win-mappings+ zone-id)))
	(if tzid
	    (cdr tzid)
	    ;; fallback
	    "Etc/GMT"))))
   (else (define local-timezone-name %local-timezone-name)))

;;; private stuff
  (define (timezone-zone&rule tz when)
    (define (find-rule-with-sec sec offset rules)
      (let-values (((secs date month year)
		    (tm:decode-julian-day-number
		     (tm:time->julian-day-number sec offset))))
	(find-rule tz date month year rules)))

    (define (find-start sec histories)
      (let loop ((hs histories))
	(let ((h (car hs)))
	  (cond ((= (length h) 3) h)
		((> sec (cadddr h)) h)
		(else (loop (cdr hs)))))))
    ;; for GMT without DST (fallback)
    (define default-rule #(0 9999 1 1 0 0 #f))
    (define (timezone-from-start start tz rules)
      (make-timezone (timezone-name tz)
		     (caddr start)
		     (car start)
		     (cadr start)
		     (if (>= (length start) 4)
			 (car (cdddr start))
			 ;; FIXME this isn't right
			 #f)
		     '()
		     ;; this paticular rule is needed
		     rules))

    (let ((sec (time-second when)) ;; ignore nanosecond
	  (rules (timezone-rules tz))
	  (raw-offset (%timezone-raw-offset tz))
	  (starts (timezone-start-offset tz)))
      (if (or (not starts) (> sec starts)) ;; if starts is #f then means always
	  (or (and-let* ((rule-name (timezone-rule-name tz))
			 (rules (assoc rule-name rules))
			 (rule  (find-rule-with-sec sec raw-offset rules)))
		(values tz rule))
	      (values tz default-rule))
	  (let-values (((secs date month year)
			(tm:decode-julian-day-number
			 ;; FIXME this is slightly wrong
			 (tm:time->julian-day-number sec raw-offset))))
	    (let ((start (find-start sec (timezone-histories tz))))
	      (or (and-let* ((rule-name (cadr start))
			     (rules (assoc rule-name rules))
			     (rule (find-rule tz date month year rules)))
		    ;; TODO don't create this each time...
		    (values (timezone-from-start start tz (list rules)) rule))
		  (values (timezone-from-start start tz (list rules))
			  default-rule)))))))

  ;; copy&paste from compile-tzdatabase.scm
  (define-constant +days-of-months+
    ;; 1  2  3  4  5  6  7  8  9 10 11 12
    #(31 28 31 30 31 30 31 31 30 31 30 31))
  (define (leap-year? y) (and (not (zero? (mod y 1000))) (zero? (mod y 4))))

  (define (resolve-date y m d)
    (define (last-day-of m)
      (let ((d (vector-ref +days-of-months+ (- m 1))))
	(if (and (= m 2) (leap-year? y))
	    (+ d 1)
	    d)))
    (if (number? d)
	d 
	(let* ((day (car d))
	       (when (cdr d))
	       (last (if (eq? when 'last) (last-day-of m) (+ when 7)))
	       (last-day (day-of-week y m last))
	       (target (cdr (assq day +day-of-week+)))
	       (off (abs (- last-day target))))
	  (- last off))))

  (define-constant +day-of-week+
    '((Sun . 0) (Mon . 1) (Tue . 2) (Wed . 3) (Thu . 4) (Fri . 5) (Sat . 6)))
  (define (day-of-week y m d)
    #|
      dayofweek(y, m, d)/* 1 <= m <= 12,  y > 1752 (in the U.K.) */
      {
          static int t[] = {0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4};
          y -= m < 3;
          return (y + y/4 - y/100 + y/400 + t[m-1] + d) % 7;
      }
      from https://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week
    |#
    (define t #(0 3 2 5 0 3 5 1 4 6 2 4))
    (let ((y (if (< m 3) (- y 1) y)))
      (mod (+ (- (+ y (div y 4)) (div y 100))
	      (div y 400) (vector-ref t (- m 1)) d)
	   7)))

  ;; TODO better solution, this is ugly.
  (define (find-rule tz date month year orules)
    (define (check-date r date)
      (let* ((effective-date (cdr r))
	     (effective-month (vector-ref (car r) 2)))
	(or (not (= month effective-month))
	    ;; TODO consider time
	    (<= effective-date date))))

    (let loop ((rules (cdr orules))
	       (dst #f)
	       (std #f))
      (cond ((null? rules)
	     ;; e.g. if standard time starts September and current time
	     ;;      is April, then std can be #f.
	     (cond ((and dst (check-date dst date)) (car dst))
		   ((and std (check-date std date)) (car std))
		   (else
		    (error 'timezone-offset "can't find a rule for tz" tz))))
	    ((and dst std)
	     ;; check
	     (let* ((dd (cdr dst))
		    (dr (car dst))
		    (dm (vector-ref dr 2))
		    (sd (cdr std))
		    (sr (car std))
		    (sm (vector-ref sr 2)))
	       (define (check m1 m2 d1 d2 r1 r2)
		 (if (and (<= m1 month m2)
			  ;; TODO consider time
			  (cond ((= month m1) (>= d1 date))
				((= month m2) (< date d2))
				(else #t)))
		     r1
		     r2))
	       ;; we have northern and southern hemisphere
	       (if (> sm dm)
		   (check dm sm dd sd dr sr)
		   (check sm dm sd dd sr dr))))
	    (else
	     (let ((rule (car rules)))
	       (or (and-let* ((start (vector-ref rule 0))
			      (end (vector-ref rule 1))
			      ( (<= start year end) )
			      (m (vector-ref rule 2))
			      ( (<= m month) )
			      ;; rule must always have date otherwise
			      ;; we don't know when to start.
			      ;; in case fallback is 1
			      (d (or (vector-ref rule 3) 1))
			      (on-date (resolve-date year month d))
			      ;; ( (<= on-date date) )
			      ;; TODO consider time
			      (off (vector-ref rule 5)))
		     ;; if there's a rule means it has DST (right?)
		     (if (zero? off)
			 ;; std
			 (loop (cdr rules) dst (cons rule on-date))
			 (loop (cdr rules) (cons rule on-date) std)))
		   (loop (cdr rules) dst std)))))))
  

)
