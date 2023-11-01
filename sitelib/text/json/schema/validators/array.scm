;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators/array.scm - JSON schema array validators
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

#!nounbound
(library (text json schema validators array)
    (export json-schema:items
	    json-schema:prefix-items
	    json-schema:additional-items
	    json-schema:unevaluated-items
	    json-schema:contains

	    json-schema:draft-7-items
	    json-schema:draft-7-contains)
    (import (rnrs)
	    (srfi :1 lists)
	    (text json pointer)
	    (text json schema validators api))


(define (schema->core-validator schema context schema-path)
  (schema-validator->core-validator
   (schema-context->schema-validator (make-schema-context schema context)
				     schema-path)))

(define (json-schema:items value context schema-path)
  (lambda (e ctx) #t))

(define (json-schema:prefix-items value context schema-path)
  (lambda (e ctx) #t))


;; additionalItems and unevaluatedItems
(define (handle-extra-items who value context schema-path pred)
  (define schema (schema-context-schema context))
  (define (filter ctx schema o)
    (if (validator-context:marked? ctx o schema)
	(let loop ((i 0) (e o) (r '()))
	  (cond ((null? e) (reverse! r))
		((pred ctx o (cons i (car e)) schema)
		 (loop (+ i 1) (cdr e) r))
		(else
		 (loop (+ i 1) (cdr e) (cons (cons i (car e)) r)))))
	'()))
  
  (unless (json-schema? value)
    (assertion-violation who "JSON Schema is required" value))
  (let ((validator (schema->core-validator value context schema-path)))
    (lambda (e ctx)
      (or (not (list? e))
	  (and (validator-context:mark! ctx e schema)
	       (for-all
		(lambda (v)
		  (validator-context:mark-element! ctx e v schema
		   (validator (cdr v) ctx))) (filter ctx schema e)))))))

(define items-pointer (json-pointer "/items"))
(define (json-schema:additional-items value context schema-path)
  (define schema (schema-context-schema context))
  (define items (items-pointer schema))
  (and (not (json-pointer-not-found? items))
       (not (json-schema? items)) ;; must be list to be effective
       (handle-extra-items 'json-schema:additiona-items value context
			   (build-schema-path schema-path "additionalItems")
			   validator-context:marked-element?)))

(define (json-schema:unevaluated-items value context schema-path)
  (handle-extra-items 'json-schema:unevaluated-items value context
		      (build-schema-path schema-path "unevaluatedItems")
		      validator-context:unevaluated?))

(define (handle-contains value context schema-path)
  (unless (json-schema? value)
    (assertion-violation 'json-schema:contains "JSON Schema is required" value))
  (let ((path (build-schema-path schema-path "contains")))
    (schema->core-validator value context path)))
(define (json-schema:draft-7-contains value context schema-path)
  (define validator (handle-contains value context schema-path))  
  (lambda (e ctx)
    (or (not (list? e))
	(exists (lambda (v) (validator v ctx)) e))))

(define max-contains-pointer (json-pointer "/maxContains"))
(define min-contains-pointer (json-pointer "/minContains"))
(define (json-schema:contains value context schema-path)
  (define schema (schema-context-schema context))
  (define contains-validator (handle-contains value context schema-path))
  (define (obtain-value who pointer schema)
    (let ((r (pointer schema)))
      (and (not (json-pointer-not-found? r))
	   (or (and (integer? r) (not (negative? r)) r)
	       (assertion-violation who "Non negative integer is required" r)))))
  (define (count validator e ctx)
    (length (filter-map (lambda (v) (validator v ctx)) e)))
  (let ((max-contains
	 (or (obtain-value 'json-schema:max-contains max-contains-pointer schema)
	     +inf.0))
	(min-contains
	 (or (obtain-value 'json-schema:min-contains min-contains-pointer schema)
	     1)))
    (lambda (e ctx)
      (or (not (list? e))
	  (let ((n (count contains-validator e ctx)))
	    (and (<= min-contains n) (<= n max-contains)))))))

;; Draft 7 and 2019-09
(define (json-schema:draft-7-items value context schema-path)
  (define schema (schema-context-schema context))
  (define path (build-schema-path schema-path "items"))
  (define (mark-all ctx o schema r)
    (let loop ((i 0) (e o))
      (if (null? e)
	  r
	  (let ((v (car e)))
	    (and (validator-context:mark-element! ctx o (cons i v) schema r)
		 (loop (+ i 1) (cdr e)))))))

  (define (validate validators o ctx schema)
    (let loop ((i 0) (validators validators) (e o))
      (cond ((null? validators)) ;; ok
	    ((null? e))		 ;; permitted
	    (else
	     (let ((validator (car validators))
		   (v (car e)))
	       (and (validator-context:mark-element! ctx o (cons i v) schema
		     (validator v ctx))
		    (loop (+ i 1) (cdr validators) (cdr e))))))))

  (cond ((json-schema? value)
	 (let ((validator (schema->core-validator value context path)))
	   (lambda (e ctx)
	     (or (not (list? e))
		 (and (validator-context:mark! ctx e schema)
		      (mark-all ctx e schema
				(for-all (lambda (v) (validator v ctx)) e)))))))
	((and (list? value) (for-all json-schema? value))
	 (let ((validators
		(map (lambda (schema)
		       (schema->core-validator schema context path)) value)))
	   (lambda (e ctx)
	     (or (not (list? e))
		 (and (validator-context:mark! ctx e schema)
		      (validate validators e ctx schema))))))
	(else (assertion-violation 'json-schema:items
		"Either JSON Schema or list of JSON Schema is required" value))))

)
