;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators/format.scm - JSON schema format validator
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

;; reference:
;; Draft-7, 2019-09 and 2020-12: https://json-schema.org/
#!nounbound
(library (text json schema validators format)
    (export json-schema:format
	    *json-schema:validate-format?*)
    (import (rnrs)
	    (sagittarius regex)
	    (srfi :14 char-sets)
	    (srfi :19 time)
	    (srfi :39 parameters)
	    (peg)
	    (peg chars)
	    (rfc smtp format) ;; smtp-valid-address?
	    (rfc uri)
	    (rfc uri-template)
	    (rfc uuid)
	    (text json pointer)
	    (only (text json schema validators primitives)
		  json-schema:pattern))

(define *json-schema:validate-format?* (make-parameter #t))

(define (json-schema:format v)
  (cond ((assoc v +json-schema-defined-formats+) =>
	 (lambda (slot)
	   (let ((validator (cdr slot)))
	     (lambda (e)
	       (if (*json-schema:validate-format?*)
		   (or (not (string? e)) (validator e))
		   #t)))))
	;; not supported, so ignore
	(else (lambda (e) #t))))


(define digit-set (string->char-set "0123456789"))
(define $digit ($char-set-contains? digit-set))
(define (valid-date? y m d)
  (let* ((d0 (make-date 0 0 0 0 d m y 0))
	 (d1 (time-utc->date (date->time-utc d0) 0)))
    (and (= (date-day d0) (date-day d1))
	 (= (date-month d0) (date-month d1))
	 (= (date-year d0) (date-year d1)))))
(define ($number n)
  ($let ((d* ($repeat $digit n)))
    ($return (string->number (list->string d*)))))
  

(define $date
  ($let ((y ($number 4))
	 ( ($eqv? #\-) )
	 (m ($number 2))
	 ( ($eqv? #\-) )
	 (d ($number 2)))
    (if (valid-date? y m d) 
	($return (list y m d))
	($fail "Invalid date"))))

(define (valid-time? h m s f offset)
  (let* ((d0 (make-date f s m h 1 1 1970 offset))
	 (d1 (time-utc->date (date->time-utc d0) offset)))
    (and (<= 0 h) (< h 24) ;; basic checks
	 (or (and (= (date-nanosecond d0) (date-nanosecond d1))
		  (= (date-second d0) (date-second d1))
		  (= (date-minute d0) (date-minute d1))
		  (= (date-hour d0) (date-hour d1)))
	     ;; leap second...
	     (let ((d3 (time-utc->date (date->time-utc d0) 0)))
	       (and (= (date-second d3) 0)
		    (= (date-minute d3) 0)
		    (= (date-hour d3) 0)
		    (time=? (date->time-utc d0) (date->time-utc d1))))))))
(define (->nano f)
  (define l (length f))
  (let ((n (string->number (list->string f))))
    ;; nano 10^-9
    ;; truncate if it's too big
    (* n (expt 10 (- 9 l)))))

(define $time
  ($let ((h ($number 2))
	 ( ($eqv? #\:) )
	 (m ($number 2))
	 ( ($eqv? #\:) )
	 (s ($number 2))
	 (f ($optional
	     ($let (( ($eqv? #\.) ) (d* ($many $digit 1)))
	       (cond ((->nano d*) => $return)
		     (else ($fail "Invalid fraction"))))
	     0))
	 (offset ($or ($seq ($eqv? #\z) ($return 0))
		      ($seq ($eqv? #\Z) ($return 0))
		      ($let ((sign ($or ($eqv? #\+) ($eqv? #\-)))
			     (h ($number 2))
			     ( ($eqv? #\:) )
			     (l ($number 2)))
			(if (and (<= 0 h) (< h 24) (<= 0 l) (< l 60))
			    ($return 
			     (let ((v (+ (* 3600 h) (* 60 l))))
			       (if (eqv? #\- sign)
				   (- v)
				   v)))
			    ($fail "Invalid offset"))))))
    (if (valid-time? h m s f offset)
	($return (list h m s f offset))
	($fail "Invalid time"))))

(define $date-time
  ($let ((d $date)
	 ( ($or ($eqv? #\t) ($eqv? #\T)) )
	 (t $time))
    ($return (append d t))))

(define (parser->validator p)
  (define (check p e)
    (let-values (((s v nl) (p (string->list e))))
      (and (parse-success? s)
	   (null? nl))))
  (lambda (e)
    (or (not (string? e))
	(check p e))))

(define json-schema:format-date (parser->validator $date))
(define json-schema:format-time (parser->validator $time))
(define json-schema:format-date-time (parser->validator $date-time))

(define (json-schema:format-email e) (smtp-valid-address? e))
;; lazy...
(define json-schema:format-idn-email (json-schema:pattern "[^@]+@[^@]+"))

;; hostname
(define json-schema:format-hostname
  (json-schema:pattern "^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]*[a-zA-Z0-9])\\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\\-]*[A-Za-z0-9])$"))
(define json-schema:format-idn-hostname
  (json-schema:pattern "^(([^\\.-]|[^\\.-][^\\.]*[^\\.\\-])\\.)*([^\\.\\-]|[^\\.\\-][^\\.]*[^\\.\\-])$"))

;; IP address
(define json-schema:format-ipv4
  (json-schema:pattern "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"))
;; from https://stackoverflow.com/a/17871737/4377398
(define json-schema:format-ipv6
  (json-schema:pattern "(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))"))

;; we don't check if the scheme requires authority or not
(define (json-schema:format-uri e)
  (let-values (((scheme specific) (uri-scheme&specific e)))
    (and scheme specific #t)))
(define (json-schema:format-uri-reference e) #t)
(define (json-schema:format-uri-template e)
  (guard (e (else #f))
    (pair? (parse-uri-template e))))

(define (json-schema:format-json-pointer v)
  (guard (e (else #f)) (json-pointer v)))
(define (json-schema:format-relative-json-pointer v)
  ;; very simple check
  ;; TODO make relative JSON pointer parser
  (and (string->number (string (string-ref v 0)))
       (or (eqv? #\# (string-ref v (- (string-length v) 1)))
	   (guard (e (else #f)) (json-pointer (string-append "/" v))))))
(define (json-schema:format-regex v)
  (guard (e (else #f)) (and (regex v 0 #t) #t)))

(define (json-schema:format-uuid v)
  (guard (e (else #f)) (uuid? (string->uuid v))))

(define +json-schema-defined-formats+
  `(
    ("date" . ,json-schema:format-date)
    ("time" . ,json-schema:format-time)
    ("date-time" . ,json-schema:format-date-time)
    ("email" . ,json-schema:format-email)
    ("idn-email" . ,json-schema:format-idn-email)
    ("hostname" . ,json-schema:format-hostname)
    ("idn-hostname" . ,json-schema:format-idn-hostname)
    ("ipv4" . ,json-schema:format-ipv4)
    ("ipv6" . ,json-schema:format-ipv6)
    ("uri" . ,json-schema:format-uri)
    ("uri-reference" . ,json-schema:format-uri-reference)
    ("iri" . ,json-schema:format-uri)
    ("iri-reference" . ,json-schema:format-uri-reference)
    ("uri-template" . ,json-schema:format-uri-template)
    ("json-pointer" . ,json-schema:format-json-pointer)
    ("relative-json-pointer" . ,json-schema:format-relative-json-pointer)
    ("regex" . ,json-schema:format-regex)
    ;; from draft 2019-09, but make it common
    ("uuid" . ,json-schema:format-uri)
    ;; TODO duration
    ))

)
