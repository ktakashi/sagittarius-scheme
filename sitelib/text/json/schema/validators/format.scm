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
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :19 time)
	    (srfi :39 parameters)
	    (srfi :115 regexp)
	    (peg)
	    (peg chars)
	    (rfc smtp format)
	    (rfc timestamps)
	    (rfc uri parser)
	    (rfc uri-template)
	    (rfc uuid)
	    (text json pointer)
	    (only (text json schema validators primitives)
		  json-schema:pattern)
	    (text xml schema))

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

(define (parser->validator p)
  (lambda (e)
    (or (not (string? e))
	(date? (p e)))))

(define json-schema:format-date (parser->validator parse-date))
(define json-schema:format-time (parser->validator parse-time))
(define json-schema:format-date-time (parser->validator parse-date-time))

(define (json-schema:format-email e) (smtp-valid-address? e))
(define (json-schema:format-idn-email e) (smtp-valid-address? e))

;; hostname
;; label must be max 63
;; see https://datatracker.ietf.org/doc/html/rfc1034#section-3.1
(define json-schema:format-hostname
  (json-schema:pattern "^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])\\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\\-]{0,61}[A-Za-z0-9])$"))
(define json-schema:format-idn-hostname
  (json-schema:pattern 
   "^(?u:([^\\.-]|[^\\.-][^\\.]*[^\\.\\-])\\.)*([^\\.\\-]|[^\\.\\-][^\\.]*[^\\.\\-])$"))

;; IP address
(define (json-schema:format-ipv4 e) (ipv4-address? e))
(define (json-schema:format-ipv6 e) (ipv6-address? e))

;; we don't check if the scheme requires authority or not
(define (json-schema:format-uri e)
  (let*-values (((scheme specific) (uri-scheme&specific e))
		((auth path query frag) (uri-decompose-hierarchical specific)))
    (and scheme specific (or auth path)
	 (string-every (lambda (c) (not (char-whitespace? c))) specific))))
(define (json-schema:format-uri-reference e)
  (and (string-every (lambda (c) (not (eqv? c #\\))) e)
       (let-values (((scheme ui host port path query frag)
		     (uri-reference-parse e)))
	 (and (or scheme ui host port path query frag) #t))))
(define (json-schema:format-uri-template e)
  (guard (e (else #f))
    (pair? (parse-uri-template e))))

(define (json-schema:format-json-pointer v)
  (guard (e (else #f)) (and (json-pointer v) #t)))

(define $non-negative-integer
  ($or ($eqv? #\0)
       ($seq ($char-set-contains? (ucs-range->char-set #x31 #x40))
	     ($many ($char-set-contains? (ucs-range->char-set #x30 #x40))))))
(define $index-manipulation
  ($seq ($or ($eqv? #\+) ($eqv? #\-)) $non-negative-integer))
(define $relative-json-pointer
  ($or ($seq $non-negative-integer ($eqv? #\#))
       ($seq $non-negative-integer
	     ($optional $index-manipulation)
	     $json-pointer)))

(define (json-schema:format-relative-json-pointer v)
  (let-values (((s v nl) ($relative-json-pointer (string->list v))))
    (and (parse-success? s) (null? nl))))
(define (json-schema:format-regex v)
  (guard (e (else #f)) (and (regex v 0 #t) #t)))

(define (json-schema:format-uuid v)
  (guard (e (else #f)) (uuid? (string->uuid v))))

(define (json-schema:format-duration v)
  (guard (e (else #f)) (xs:duration? (xs:make-duration v))))

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
    ("uuid" . ,json-schema:format-uuid)
    ("duration" . ,json-schema:format-duration)
    ))

)
