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
	    json-schema:draft-7-contains

	    json-schema:draft-2019-09-contains)
    (import (rnrs)
	    (srfi :1 lists)
	    (text json pointer)
	    (text json schema validators api))


(define (schema->core-validator schema context schema-path)
  (schema-validator->core-validator
   (schema-context->schema-validator (make-schema-context schema context)
				     schema-path)))

(define (items-handler value context schema-path)
  (define schema (schema-context-schema context))
  (define (validate validators o ctx context)
    (define lint-mode? (validator-context-lint-mode? ctx))
    (let loop ((i 0) (validators validators) (e o) (r #t))
      (cond ((null? validators) r) ;; ok
	    ((null? e) r)	   ;; permitted
	    (else
	     (let ((validator (car validators))
		   (ctx (validator-context:add-path! ctx i))
		   (v (car e)))
	       (let ((t (validator-context:mark-element! ctx o (cons i v) context
							 (validator v ctx))))
		 (if lint-mode?
		     (and r (loop (+ i 1) (cdr validators) (cdr e) (and r t)))
		     (loop (+ i 1) (cdr validators) (cdr e) (and r t)))))))))
  (unless (and (list? value) (for-all json-schema? value))
    (assertion-violation 'json-schema:prefix-items
			 "Array of JSON Schema is required" value))
  (let ((validators (map (lambda (schema)
			   (schema->core-validator schema context schema-path))
			 value)))
    (lambda (e ctx)
      (or (not (list? e))
	  (and (validator-context:mark! ctx e context)
	       (validate validators e ctx context))))))

(define prefix-items-pointer (json-pointer "/prefixItems"))
(define (json-schema:items value context schema-path)
  (define schema (schema-context-schema context))
  (unless (json-schema? value)
    (assertion-violation 'json-schema:items "JSON Schema is required" value))
  (if (json-pointer-not-found? (prefix-items-pointer schema))
      (json-schema:draft-7-items value context schema-path)
      (handle-extra-items 'json-schema:items value context
			  schema-path
			  validator-context:marked-element?)))

(define (json-schema:prefix-items value context schema-path)
  (when (null? value)
    (assertion-violation 'json-schema:prefix-items
			 "At least one element is required" value))
  (items-handler value context schema-path))


;; additionalItems and unevaluatedItems
(define (handle-extra-items who value context schema-path pred)
  (define (filter ctx context o)
    (if (validator-context:marked? ctx o context)
	(let loop ((i 0) (e o) (r '()))
	  (cond ((null? e) (reverse! r))
		((pred ctx o (cons i (car e)) context)
		 (loop (+ i 1) (cdr e) r))
		(else
		 (loop (+ i 1) (cdr e) (cons (cons i (car e)) r)))))
	'()))
  
  (unless (json-schema? value)
    (assertion-violation who "JSON Schema is required" value))
  (let ((validator (schema->core-validator value context schema-path)))
    (lambda (e ctx)
      (define lint-mode? (validator-context-lint-mode? ctx))
      (or (not (list? e))
	  (and (validator-context:mark! ctx e context)
	       (if lint-mode?
		   (fold-left
		    (lambda (acc v)
		      (and (validator-context:mark-element!
			    ctx e v context (validator (cdr v) ctx))
			   acc))
		    #t (filter ctx context e))
		   (for-all
		    (lambda (v)
		      (validator-context:mark-element! ctx e v context
		       (validator (cdr v) ctx))) (filter ctx context e))))))))

(define items-pointer (json-pointer "/items"))
(define (json-schema:additional-items value context schema-path)
  (define schema (schema-context-schema context))
  (define items (items-pointer schema))
  (and (not (json-pointer-not-found? items))
       (not (json-schema? items)) ;; must be list to be effective
       (handle-extra-items 'json-schema:additiona-items value context
			   schema-path
			   validator-context:marked-element?)))

(define (json-schema:unevaluated-items value context schema-path)
  (handle-extra-items 'json-schema:unevaluated-items value context
		      schema-path
		      validator-context:unevaluated?))

(define (contains-validator value context schema-path)
  (unless (json-schema? value)
    (assertion-violation 'json-schema:contains "JSON Schema is required" value))
  (let ((path schema-path))
    (schema->core-validator value context path)))

(define (json-schema:draft-7-contains value context schema-path)
  (define validator (contains-validator value context schema-path))
  (lambda (e ctx)
    (or (not (list? e))
	(exists (lambda (v) (validator v ctx)) e))))

(define max-contains-pointer (json-pointer "/maxContains"))
(define min-contains-pointer (json-pointer "/minContains"))
(define (handle-contains value context schema-path need-mark?)
  (define schema (schema-context-schema context))
  (define validator (contains-validator value context schema-path))
  (define (obtain-value who pointer schema)
    (let ((r (pointer schema)))
      (and (not (json-pointer-not-found? r))
	   (or (and (integer? r) (not (negative? r)) r)
	       (assertion-violation who "Non negative integer is required" r)))))
  (define (count validator e ctx)
    (length (filter-map (lambda (v) (validator v ctx)) e)))
  (define (count/mark validator e ctx)
    (validator-context:mark! ctx e context)
    (let loop ((i 0) (n 0) (v e))
      (if (null? v)
	  n
	  (let* ((t (car v))
		 (r (validator t ctx)))
	    (validator-context:mark-element! ctx e (cons i t) context r)
	    (loop (+ i 1) (+ n (if r 1 0)) (cdr v))))))
  (define (make-validator validator counter max-contains min-contains)
    (lambda (e ctx)
      (or (not (list? e))
	  (let ((n (counter validator e ctx)))
	    (and (<= min-contains n) (<= n max-contains))))))
  (let ((max-contains
	 (or (obtain-value 'json-schema:max-contains max-contains-pointer schema)
	     +inf.0))
	(min-contains
	 (or (obtain-value 'json-schema:min-contains min-contains-pointer schema)
	     1)))
    (make-validator validator (if need-mark? count/mark count)
		    max-contains min-contains)))
(define (json-schema:draft-2019-09-contains value context schema-path)
  (handle-contains value context schema-path #f))
(define (json-schema:contains value context schema-path)
  (handle-contains value context schema-path #t))

;; Draft 7 and 2019-09
(define (json-schema:draft-7-items value context schema-path)
  (define path schema-path)
  (define (validate e ctx validator)
    (define lint-mode? (validator-context-lint-mode? ctx))
    (let loop ((i 0) (e e) (r #t))
      (if (null? e)
	  r
	  (let ((ctx (validator-context:add-path! ctx i)))
	    (loop (+ i 1) (cdr e)
		  (if lint-mode?
		      (and (validator (car e) ctx) r)
		      (and r (validator (car e) ctx))))))))
  (define (mark-all ctx o context r)
    (let loop ((i 0) (e o))
      (if (null? e)
	  r
	  (let* ((v (car e)))
	    (and (validator-context:mark-element! ctx o (cons i v) context r)
		 (loop (+ i 1) (cdr e)))))))

  (cond ((json-schema? value)
	 (let ((validator (schema->core-validator value context path)))
	   (lambda (e ctx)
	     (or (not (list? e))
		 (and (validator-context:mark! ctx e context)
		      (mark-all ctx e context (validate e ctx validator)))))))
	((and (list? value) (for-all json-schema? value))
	 (items-handler value context path))
	(else (assertion-violation 'json-schema:items
		"Either JSON Schema or list of JSON Schema is required" value))))

)
