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

	    )
    (import (rnrs))

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
      ;; TODO date and dateTime (maybe srfi 19?)
      ))


(define-record-type xs:duration
  (parent xs:any-atomic-type)
  (fields months seconds)
  (protocol (lambda (p)
	      (lambda (m s)
		(when (or (and (negative? m) (positive? s))
			  (and (positive? m) (negative? s)))
		  (assertion-violation 'xs:make-duration
				       "Invalid months and seconds" m s))
		((p) m s)))))
(define-record-type xs:day-time-duration
  (parent xs:duration)
  (protocol (lambda (p) (lambda (s) ((p 0 s))))))
(define-record-type xs:year-month-duration
  (parent xs:duration)
  (protocol (lambda (p) (lambda (m) ((p m 0))))))


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

)
