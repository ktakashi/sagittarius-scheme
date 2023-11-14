;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators/primitives.scm - JSON schema primitive validators
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
(library (text json schema validators primitives)
    (export json-schema:type
	    json-schema:enum
	    json-schema:const

	    json-schema:multiple-of
	    json-schema:maximum
	    json-schema:exclusive-maximum
	    json-schema:minimum
	    json-schema:exclusive-minimum

	    json-schema:max-length
	    json-schema:min-length
	    json-schema:pattern

	    json-schema:max-items
	    json-schema:min-items
	    json-schema:unique-items

	    json-schema:max-properties
	    json-schema:min-properties
	    json-schema:required)
    (import (rnrs)
	    (sagittarius regex)
	    (srfi :1 lists)
	    (srfi :133 vectors)
	    (text json schema version)
	    (text json compare))

;; utilities (maybe move to somewhere)
(define unique?
  (case-lambda
   ((v) (unique? v json=?))
   ((v =) (equal? v (delete-duplicates v =)))))

;; validator = (obj) -> boolean
;; Common validators (among draft-7, 2019-09 and 2020-12)

;; sections from draft 2020-12
;; (implementations can be used in common among the drafts)

;;; 6. A Vocabulary for Structural Validation
;;; 6.1 Validation Keywords for Any Instance Type
;;; 6.1.1. type
;; `type` validator: (type) -> (obj) -> boolean
;; e.g. schema = {"type": "object", "properties": ...}
;;      type = "object" (for json-schema:type)
(define (json-schema:type type)
  (define (check type)
    (when (zero? (length type))
      (assertion-violation 'json-schema:type
			   "Array type must contain one element"))
    (unless (for-all string? type)
      (assertion-violation 'json-schema:type
			   "Array type must contain only string element" type))
    ;; it says MUST so check it
    (unless (unique? type string=?)
      (assertion-violation 'json-schema:type
			   "Array type contains duplicate value" type)))
  (define (predicate-of type)
    (cond ((string=? "string" type) string?)
	  ((string=? "integer" type) integer?)
	  ((string=? "number" type) real?)	;; TODO exclude rational?)
	  ;; we use vector json for validation
	  ;; TODO should we check content?
	  ((string=? "object" type) vector?)
	  ((string=? "array" type) list?)
	  ((string=? "boolean" type) boolean?)
	  ((string=? "null" type) (lambda (e) (eq? e 'null)))
	  (else (assertion-violation 'json-schema:type "Unknown type" type))))
  (cond ((list? type)
	 (check type)
	 (fold-left (lambda (acc t)
		      (let ((v (predicate-of t)))
			(lambda (e) (or (v e) (acc e)))))
		    (lambda (e) #f) type))
	((string? type) (predicate-of type))
	(else (assertion-violation 'json-schema:type
				   "Type must be array or string" type))))

;;; 6.1.2 enum
;; `enum` validator: (vals) -> (obj) -> boolean
(define (json-schema:enum vals)
  (unless (list? vals)
    (assertion-violation 'json-schema:enum "Enum must be an array" vals))
  (when (zero? (length vals))
    (assertion-violation 'json-schema:enum "Enum should not be empty"))
  (unless (unique? vals)
    (assertion-violation 'json-schema:enum
			 "Enum should contain unique value" vals))
  (lambda (e) (exists (lambda (v) (json=? e v)) vals)))

;;; 6.1.2 const 
;; `const` validator: (v) -> (obj) -> boolean
(define (json-schema:const v) (lambda (e) (json=? e v)))


;;; 6.2. Validation Keywords for Numeric Instances (number and integer)
;;; 6.2.1 multipleOf
;; `multipleOf` validator: (n) -> (obj) -> boolean
(define (json-schema:multiple-of v)
  (define (->integer e v)
    (if (not (or (integer? e) (integer? v)))
	(->integer (* e 10) (* v 10))
	(values e v)))
  (unless (and (real? v) (positive? v))
    (assertion-violation 'json-schema:multiple-of
			 "MultipleOf must be a number greater than 0" v))
  (lambda (e)
    (or (not (real? e))
	(let-values (((e v) (->integer e v)))
	  (zero? (mod e v))))))

(define (min/max who compare)
  (define name (symbol->string who))
  (define err-who (string->symbol (string-append "json-schema:" name)))
  (define err-msg (string-append (string-titlecase name) " must be a number"))
  (lambda (v)
    (unless (real? v) (assertion-violation err-who err-msg v))
    (lambda (e) (or (not (real? e)) (compare e v)))))

;;; 6.2.2 maximum
;; `maximum` validator: (n) -> (obj) -> boolean
(define json-schema:maximum (min/max 'maximum <=))
;;; 6.2.3 exclusiveMaximum
;; `exclusiveMaximum` validator: (n) -> (obj) -> boolean
(define json-schema:exclusive-maximum (min/max 'exclusive-maximum <))
;;; 6.2.4 minimum
;; `minimum` validator: (n) -> (obj) -> boolean
(define json-schema:minimum (min/max 'minimum >=))
;;; 6.2.5 exclusiveMinimum
;;  `exclusiveMinimu` validator: (n) -> (obj) -> boolean
(define json-schema:exclusive-minimum (min/max 'exclusive-minimum >))

;;; 6.3. Validation Keywords for Strings
;;; 6.3.1. maxLength
;; `maxLength` validator: (n) -> (obj) -> boolean
(define (json-schema:max-length v)
  (when (or (not (integer? v)) (negative? v))
    (assertion-violation 'json-schema:max-length
			 "maxLength must be a non negative integer" v))
  (lambda (e) (or (not (string? e)) (<= (string-length e) v))))

;;; 6.3.2. minLength
;; `minLength` validator: (n) -> (obj) -> boolean
(define (json-schema:min-length v)
  (when (or (not (integer? v)) (negative? v))
    (assertion-violation 'json-schema:min-length
			 "minLength must be a non negative integer" v))
  (lambda (e) (or (not (string? e)) (<= v (string-length e)))))

;;; 6.3.3. pattern
;; `pattern` validator: (pattern) -> (obj) -> boolean
(define (json-schema:pattern p)
  (unless (string? p)
    (assertion-violation 'json-schema:pattern "pattern must be a string" p))
  (guard (e (else
	     (assertion-violation 'json-schema:pattern
				  (if (message-condition? e)
				      (condition-message e)
				      "Invalid regex pattern") p)))
    (let ((rx (regex p)))
      (lambda (e) (or (not (string? e))
		      (and (looking-at rx e) #t))))))

;;; 6.4. Validation Keywords for Arrays
;;; 6.4.1. maxItems
(define (json-schema:max-items n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:max-items
			 "maxItems must be a non negative integer" n))
  (lambda (e) (or (not (list? e)) (<= (length e) n))))
;;; 6.4.2. minItems
(define (json-schema:min-items n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:max-items
			 "minItems must be a non negative integer" n))
  (lambda (e) (or (not (list? e)) (<= n (length e)))))
;;; 6.4.3. uniqueItems
(define (json-schema:unique-items b)
  (unless (boolean? b)
    (assertion-violation 'json-schema:unique-items
			 "uniqueItems must be a boolean" b))
  (if b
      (lambda (e) (and (list? e) (unique? e)))
      (lambda (e) #t)))

;;; 6.5. Validation Keywords for Objects
;;; 6.5.1. maxProperties
(define (json-schema:max-properties n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:max-properties
			 "maxProperties must be a non negative integer" n))
  (lambda (e) (or (not (vector? e)) (<= (vector-length e) n))))
;;; 6.5.2. minProperties
(define (json-schema:min-properties n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:min-properties
			 "minProperties must be a non negative integer" n))
  (lambda (e) (or (not (vector? e)) (<= n (vector-length e)))))
;;; 6.5.3. required
(define (json-schema:required e*)
  (unless (and (list? e*) (for-all string? e*) (unique? e*))
    (assertion-violation 'json-schema:required
			 "Required must be an array of unique strings" e*))
  (lambda (e)
    (or (not (vector? e))
	;; TODO inefficient
	(for-all (lambda (k)
		   (vector-any
		    (lambda (v)
		      ;; not sure if we need to handle invalid JSON
		      ;; structure...
		      (and (pair? v) (string? (car v)) 
			   (string=? (car v) k)))
		    e))
		 e*))))

)
