;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/schema/parsers.scm - XML Schema parsers
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
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
;;    https://www.w3.org/TR/xmlschema11-2/
;;  RFC 3339: https://datatracker.ietf.org/doc/html/rfc3339
#!nounbound
(library (text xml schema parsers)
    (export xs:parse-duration
	    *xs:duration-parser*
	    *rfc:duration-parser*)
    (import (rnrs)
	    (srfi :115 regexp))

(define-record-type xs:duration-parser
  (fields regexp ymdw hmsf))
  
(define +duration-regex+
  (regexp
   '(: bol
       (? #\-) "P"
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
			 (? "." ($ (+ (/ "09")))) "S")))     ;; 24
	   ;; Extra, not XML schema, but ISO 8601
	   (: ($ (/ "09")) "W"))
       eol)))

(define +duration-ymdw-matches+
  '((1  2  3  #f)
    (#f 4  5  #f)
    (#f #f 6  #f)
    (#f #f #f 25)))
(define +duration-hms-matches+
  '(( 7  8  9 10)
    (#f 11 12 13)
    (#f #f 14 15)
    (16 17 18 19)
    (#f 20 21 22)
    (#f #f 23 24)))

(define *xs:duration-parser*
  (make-xs:duration-parser +duration-regex+
			   +duration-ymdw-matches+
			   +duration-hms-matches+))

;; RFC 3339 has a bit more strict rule
(define +rfc3339-duration-regex+
  (regexp
   '(: bol
       "P"
       (or (: (or (: ($ (+ (/ "09"))) "Y"	       ;; 1
		     (? ($ ($ (+ (/ "09"))) "M"	       ;; 2
			   (? ($ (+ (/ "09"))) "D")))) ;; 3
		  (: ($ (+ (/ "09"))) "M"	       ;; 4
		     (? ($ (+ (/ "09"))) "D"))	       ;; 5
		  (: ($ (+ (/ "09"))) "D"))	       ;; 6
	      (? "T"
		 (or (: ($ (+ (/ "09"))) "H"		  ;; 7
			(? ($ ($ (+ (/ "09"))) "M"	  ;; 8
			      (? ($ (+ (/ "09"))) "S")))) ;; 9
		     (: ($ (+ (/ "09"))) "M"		  ;; 10
			(? ($ (+ (/ "09"))) "S"))	  ;; 11
		     (: ($ (+ (/ "09")))  "S"))))	  ;; 12

	   (: "T" (or (: ($ (+ (/ "09"))) "H"		  ;; 13
			 (? ($ ($ (+ (/ "09"))) "M"	  ;; 14
			       (? ($ (+ (/ "09")))"S")))) ;; 15
		      (: ($ (+ (/ "09"))) "M"		  ;; 16
			 (? ($ (+ (/ "09"))) "S"))	  ;; 17
		      (: ($ (+ (/ "09")))  "S")))	  ;; 18
	   ;; Extra, not XML schema, but ISO 8601
	   (: ($ (/ "09")) "W")) ;; 19
       eol)))

(define +rfc3339-ymdw-matches+
  '((1  2  3  #f)
    (#f 4  5  #f)
    (#f #f 6  #f)
    (#f #f #f 19)))
(define +rfc3339-hms-matches+
  '(( 7  8  9 #f)
    (#f 10 11 #f)
    (#f #f 12 #f)
    (13 14 15 #f)
    (#f 16 17 #f)
    (#f #f 18 #f)))

(define *rfc:duration-parser*
  (make-xs:duration-parser +rfc3339-duration-regex+
			   +rfc3339-ymdw-matches+
			   +rfc3339-hms-matches+))

(define (xs:parse-duration duration :optional (parser *xs:duration-parser*))
  (parse-duration duration
		  (xs:duration-parser-regexp parser)
		  (xs:duration-parser-ymdw parser)
		  (xs:duration-parser-hmsf parser)))

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

  (define (parse matchers m)
    (define sample (car matchers))
    (cond ((exists (lambda (p) (submatch m p)) matchers) =>
	   (lambda (result) (apply values result)))
	  (else (apply values (map (lambda (_) #f) sample)))))
  (cond ((regexp-matches rx duration) =>
	 (lambda (m)
	   (let-values (((y mo d w) (parse ymd m))
			((h mi s f) (parse hms m)))
	     (define neg? (char=? (string-ref duration 0) #\-))
	     (define (neg n) (if neg? (- n) n))
	     (values (neg (+ (or (and y (* 12 (string->number y))) 0)
			     (or (and mo (string->number mo)) 0)))
		     (neg (+ (or (and d (* 24 60 60 (string->number d))) 0)
			     (or (and w (* 24 60 60 7 (string->number w))) 0)
			     (or (and h (* 60 60 (string->number h))) 0)
			     (or (and mi (* 60 (string->number mi))) 0)
			     (or (and s (string->number s)) 0)
			     ;; fraction?
			     (or (and f (string->number (string-append "0." f)))
				 0.0)))))))
	(else (assertion-violation 'parse-duration "Invalid duration"
				   duration))))

)
