;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators.scm - JSON schema validators
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

;; reference:
;; Draft-7: http://json-schema.org/
;; TODO: follow the final version when published
#!nounbound
(library (text json schema validators)
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
	    json-schema:required

	    +json-schema-validators+
	    +json-schema-type-sub-validators+
	    )
    (import (rnrs)
	    (sagittarius regex)
	    (srfi :1 lists)
	    (srfi :133 vectors))
;; utilities
(define unique?
  (case-lambda
   ((v) (unique? v equal?))
   ((v =) (equal? v (delete-duplicates v =)))))
(define (json=? a b)
  (define (entry=? a b)
    (and (json=? (car a) (car b))
	 (json=? (cdr a) (cdr b))))
  (cond ((and (string? a) (string? b)) (string=? a b))
	;; 1 and 1.0 are not the same so can't be = or equal?
	((and (number? a) (number? b)) (eqv? a b))
	((and (vector? a) (vector? b)) (vector-every entry=? a b))
	((and (list? a) (list? b)) (for-all json=? a b))
	(else (eq? a b))))

;;; 6.1. Validation Keywords for Any Instance Type
;; 6.1.1 type
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
  (cond ((list? type)
	 (check type)
	 (fold-left (lambda (acc t)
		      (let ((v (json-schema:type t)))
			(lambda (e) (or (v e) (acc e))))) (lambda (e) #f) type))
	((string? type)
	 (cond ((string=? "string" type) string?)
	       ((string=? "integer" type) integer?)
	       ((string=? "number" type) real?)	;; TODO exclude rational?)
	       ;; we use vector json for validation
	       ;; TODO should we check content?
	       ((string=? "object" type) vector?)
	       ((string=? "array" type) list?)
	       ((string=? "boolean" type) boolean?)
	       ((string=? "null" type) (lambda (e) (eq? e 'null)))
	       (else (assertion-violation 'json-schema:type "Unknown type"))))
	(else (assertion-violation 'json-schema:type
				   "Type must be array or string" type))))

;; 6.1.2 enum
(define (json-schema:enum vals)
  (unless (list? vals)
    (assertion-violation 'json-schema:enum "Enum must be an array" vals))
  (when (zero? (length vals))
    (assertion-violation 'json-schema:enum "Enum should not be empty"))
  (unless (unique? vals)
    (assertion-violation 'json-schema:enum
			 "Enum should contain unique value" vals))
  (lambda (e) (exists (lambda (v) (json=? e v)) vals)))

;; 6.1.3 const
(define (json-schema:const v) (lambda (e) (json=? e v)))

;;; 6.2. Validation Keywords for Numeric Instances (number and integer)
;; 6.2.1 multipleOf
(define (json-schema:multiple-of v)
  (unless (and (real? v) (positive? v))
    (assertion-violation 'json-schema:multiple-of
			 "MultipleOf must be a number greater than 0" v))
  (lambda (e)
    (and (real? e) (zero? (mod e v)))))

;; 6.2.2. maximum
(define (json-schema:maximum v)
  (unless (real? v)
    (assertion-violation 'json-schema:maximum "Maximum must be a number" v))
  (lambda (e) (and (real? e) (<= e v))))
;; 6.2.3. exclusiveMaximum
(define (json-schema:exclusive-maximum v)
  (unless (real? v)
    (assertion-violation 'json-schema:exclusive-maximum
			 "ExclusiveMaximum must be a number" v))
  (lambda (e) (and (real? e) (< e v))))
;; 6.2.4. minimum
(define (json-schema:minimum v)
  (unless (real? v)
    (assertion-violation 'json-schema:minimum "Minimum must be a number" v))
  (lambda (e) (and (real? e) (<= v e))))
;; 6.2.5. exclusiveMinimum
(define (json-schema:exclusive-minimum v)
  (unless (real? v)
    (assertion-violation 'json-schema:exclusive-minimum
			 "ExclusiveMinimum must be a number" v))
  (lambda (e) (and (real? e) (< v e))))

;;; 6.3. Validation Keywords for Strings
;; 6.3.1. maxLength
(define (json-schema:max-length v)
  (when (or (not (integer? v)) (negative? v))
    (assertion-violation 'json-schema:max-length
			 "maxLength must be a non negative integer" v))
  (lambda (e) (and (string? e) (<= (string-length e) v))))
;; 6.3.2. minLength
(define (json-schema:min-length v)
  (when (or (not (integer? v)) (negative? v))
    (assertion-violation 'json-schema:min-length
			 "minLength must be a non negative integer" v))
  (lambda (e) (and (string? e) (<= v (string-length e)))))
;; 6.3.3. pattern
(define (json-schema:pattern p)
  (unless (string? p)
    (assertion-violation 'json-schema:pattern "pattern must be a string" p))
  (guard (e (else
	     (assertion-violation 'json-schema:pattern
				  "Invalid regex pattern" p)))
    (let ((rx (regex p)))
      (lambda (e)
	(and (string? e) (matches rx e) #t)))))

;;; 6.4. Validation Keywords for Arrays
;; items, additionalItems, and contains are handled on validator creation
;; (Those require understanding of the schema)
;; 6.4.3. maxItems
(define (json-schema:max-items n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:max-items
			 "maxItems must be a non negative integer" n))
  (lambda (e) (and (list? e) (<= (length e) n))))
;; 6.4.4. minItems
(define (json-schema:min-items n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:max-items
			 "minItems must be a non negative integer" n))
  (lambda (e) (and (list? e) (<= n (length e)))))
;; 6.4.5. uniqueItems
(define (json-schema:unique-items b)
  (unless (boolean? b)
    (assertion-violation 'json-schema:unique-items
			 "uniqueItems must be a boolean" b))
  (if b
      (lambda (e) (and (list? e) (unique? e)))
      (lambda (e) #t)))

;;; 6.5. Validation Keywords for Objects
;; 6.5.1. maxProperties
(define (json-schema:max-properties n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:max-properties
			 "maxProperties must be a non negative integer" n))
  (lambda (e) (and (vector? e) (<= (vector-length e) n))))
;; 6.5.2. minProperties
(define (json-schema:min-properties n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:min-properties
			 "minProperties must be a non negative integer" n))
  (lambda (e) (and (vector? e) (<= n (vector-length e)))))
;; 6.5.3. required
(define (json-schema:required e*)
  (unless (and (list? e*) (for-all string? e*) (unique? e*))
    (assertion-violation 'json-schema:required
			 "Required must be an array of unique strings" e*))
  (lambda (e)
    (and (vector? e)
	 ;; TODO inefficient
	 (for-all (lambda (k)
		    (vector-any
		     (lambda (v)
		       ;; not sure if we need to handle invalid JSON
		       ;; structure...
		       (and (pair? v) (string? (car v)) (string=? (car v) k)))
		     e))
		  e*))))
;; properties, patternProperties, additionalPropperties,
;; dependencies, propertyNames are handled on validator creation

(define +json-schema-any-instance-validators+
  `(
    ("type" ,json-schema:type)
    ("enum" ,json-schema:enum)
    ("const" ,json-schema:const)
    ))
(define +json-schema-numeric-instance-validators+
  `(
    ("multipleOf" ,json-schema:multiple-of)
    ("maximum" ,json-schema:maximum)
    ("exclusiveMaximum" ,json-schema:exclusive-maximum)
    ("minimum" ,json-schema:minimum)
    ("exclusiveMinimum" ,json-schema:exclusive-minimum)
    ))
(define +json-schema-string-validators+
  `(
    ("maxLength" ,json-schema:max-length)
    ("minLength" ,json-schema:min-length)
    ("pattern" ,json-schema:pattern)
    ))
(define +json-schema-array-validators+
  `(
    ("items" #f)
    ("additionalItems" #f)
    ("maxItems" ,json-schema:max-items)
    ("minItems" ,json-schema:min-items)
    ("uniqueItems" ,json-schema:unique-items)
    ("contains" #f)
    ))
(define +json-schema-object-validators+
  `(
    ("maxProperties" ,json-schema:max-properties)
    ("minProperties" ,json-schema:min-properties)
    ("required" ,json-schema:required)
    ("properties" #f)
    ("patternProperties" #f)
    ("additionalPropperties" #f)
    ("dependencies" #f)
    ("propertyNames" #f)
    ))
(define +json-schema-validators+
  `(
    ,@+json-schema-any-instance-validators+
    ,@+json-schema-numeric-instance-validators+
    ,@+json-schema-string-validators+
    ,@+json-schema-array-validators+
    ,@+json-schema-object-validators+
    ))

(define +json-schema-type-sub-validators+
  `(
    ("string" ,@+json-schema-string-validators+)
    ("number" ,@+json-schema-numeric-instance-validators+)
    ("integer" ,@+json-schema-numeric-instance-validators+)
    ("array" ,@+json-schema-array-validators+)
    ("object" ,@+json-schema-object-validators+)
    ))
)
