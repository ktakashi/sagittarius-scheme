;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators/object.scm - JSON schema object validators
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
(library (text json schema validators object)
    (export json-schema:properties
	    json-schema:pattern-properties
	    json-schema:property-names
	    json-schema:additional-properties
	    json-schema:unevaluated-properties
	    json-schema:dependencies
	    json-schema:dependent-schemas
	    json-schema:dependent-required)
    (import (rnrs)
	    (sagittarius regex)
	    (srfi :1 lists)
	    (srfi :133 vectors)
	    (text json pointer)
	    (text json schema validators api))

(define (compile-properties properties context schema-path regexp?)
  (define (entry->validator e)
    (cons (if regexp? (regex (car e)) (car e))
	  (schema-validator->core-validator
	   (schema-context->schema-validator
	    (make-schema-context (cdr e) context)
	    (build-schema-path schema-path (car e))))))
  (map entry->validator (vector->list properties)))

(define (matching-properties prop properties)
  (define (match-property? p)
    (let ((name (car p)))
      (if (regex-pattern? name)
	  (looking-at name prop)
	  (equal? name prop))))
  (let ((r (map cdr (filter match-property? properties))))
    (and (not (null? r)) r)))
(define (check-entry e context entry ctx properties)
  (define lint-mode? (validator-context-lint-mode? ctx))
  (let ((key (car entry))
	(value (cdr entry)))
    (cond ((matching-properties key properties) =>
	   (lambda (validators)
	     (validator-context:mark-element!
	      ctx e entry context
	      (if lint-mode?
		  (fold-left
		   (lambda (acc v)
		     (and (v value (validator-context:add-path! ctx key)) acc))
		   #t validators)
		  (for-all (lambda (v) 
			     (v value (validator-context:add-path! ctx key)))
			   validators)))))
	  (else #t))))

(define ((properties-handler name regexp?) value context schema-path)
  (define path schema-path)
  (let ((properties (compile-properties value context path regexp?)))
    (lambda (e ctx)
      (define lint-mode? (validator-context-lint-mode? ctx))
      (or (not (vector? e))
	  (if lint-mode?
	      (vector-fold
	       (lambda (acc v)
		 (and (check-entry e context v ctx properties) acc)) #t e)
	      (vector-every
	       (lambda (v) (check-entry e context v ctx properties))
	       e))))))
(define json-schema:properties (properties-handler "properties" #f))
(define json-schema:pattern-properties
  (properties-handler "patternProperties" #t))

(define (json-schema:property-names value context schema-path)
  (define schema (schema-context-schema context))
  (let ((validator (schema-validator->core-validator
		    (schema-context->schema-validator
		     (make-schema-context value context)
		     schema-path))))
    (lambda (e ctx)
      (define lint-mode? (validator-context-lint-mode? ctx))
      (or (not (vector? e))
	  (if lint-mode?
	      (vector-fold
	       (lambda (acc v)
		 (let ((k (car v)))
		   (and (validator k (validator-context:add-path! ctx k)) acc)))
	       #t e)
	      (vector-every
	       (lambda (v)
		 (let ((k (car v)))
		   (validator k (validator-context:add-path! ctx k))))
	       e))))))

(define (handle-extras who value context schema-path pred)
  (define schema (schema-context-schema context))
  (define (filter-marked-items ctx context e)
    (filter (lambda (v) (not (pred ctx e v context)))
	    (vector->list e)))
  (unless (json-schema? value)
    (assertion-violation who
			 "JSON Schema is required" value))
  (let ((validator (schema-validator->core-validator
		    (schema-context->schema-validator
		     (make-schema-context value context)
		     schema-path))))
    (lambda (e ctx)
      (define lint-mode? (validator-context-lint-mode? ctx))
      (or (not (vector? e))
	  (if lint-mode?
	      (fold-left
	       (lambda (acc v)
		 (validator-context:mark-element! ctx e v context
		  (validator (cdr v) (validator-context:add-path! ctx (car v)))))
	       #t (filter-marked-items ctx context e))
	      (for-all (lambda (v)
			 (validator-context:mark-element! ctx e v context
			  (validator (cdr v) 
				     (validator-context:add-path! ctx (car v)))))
		       (filter-marked-items ctx context e)))))))

(define (json-schema:additional-properties value context schema-path)
  (handle-extras 'json-schema:additional-properties
		 value context
		 schema-path
		 validator-context:marked-element?))

(define (json-schema:unevaluated-properties value context schema-path)
  (handle-extras 'json-schema:unevaluated-properties
		 value context
		 schema-path
		 validator-context:unevaluated?))


(define (compile-dependent-required e context schema-path)
  (define check-pointer (json-pointer (string-append "/" (car e))))
  (define required-pointers
    (map (lambda (v) (json-pointer (string-append "/" v))) (cdr e)))
  ;; TODO should we make it schema-validator so that the
  ;; result can be tracked?
  (lambda (e path)
    ;; TODO should be make efficient by retrieving all keys first
    ;;      then check with lset-intersection or so?
    (or (json-pointer-not-found? (check-pointer e))
	(for-all (lambda (p) (not (json-pointer-not-found? (p e))))
		 required-pointers))))

(define (compile-dependent-schema e context schema-path)
  (define pointer (json-pointer (string-append "/" (car e))))
  (let ((validator (schema-validator->core-validator
		    (schema-context->schema-validator
		     (make-schema-context (cdr e) context)
		     (build-schema-path schema-path (car e))))))
    (lambda (e ctx)
      (or (json-pointer-not-found? (pointer e))
	  (validator e ctx)))))

;; `dependencies`
(define (json-schema:dependencies value context schema-path)
  (define (compile-dependency e)
    (if (list? (cdr e))
	(compile-dependent-required e context schema-path)
	(compile-dependent-schema e context schema-path)))
  (unless (vector? value)
    (assertion-violation 'json-schema:dependencies
			 "A JSON object required" value))
  (let ((validators (map compile-dependency (vector->list value))))
    (lambda (e ctx)
      (for-all (lambda (v) (v e ctx)) validators))))

;; `dependentRequired`
(define (json-schema:dependent-required value context schema-path)
  (define (check-value value)
    (unless (for-all string? value)
      (assertion-violation 'json-schema:dependent-required
			   "Must be a list of string" value))
    (unless (equal? value (delete-duplicates value string=?))
      (assertion-violation 'json-schema:dependent-required
			   "Value contains duplicates" value)))
  (define (compile-required e)
    (compile-dependent-required e context schema-path))
  (unless (vector? value)
    (assertion-violation 'json-schema:dependent-required
			 "Must be a JSON object" value))
  (vector-for-each (lambda (e) (check-value (cdr e))) value)
  (let ((validators (map compile-required (vector->list value))))
    (lambda (e ctx)
      (for-all (lambda (v) (v e ctx)) validators))))

;; `dependentSchemas`
(define (json-schema:dependent-schemas value context schema-path)
  (define (compile-property e) (compile-dependent-schema e context schema-path))
  (define (valid-object? v)
    (and (vector? v)
	 (vector-fold (lambda (acc e) (and acc (json-schema? (cdr e)))) #t v)))

  (unless (valid-object? value)
    (assertion-violation 'json-schema:dependent-schemas
			 "A JSON Schema Object is required" value))
  (let ((validators (map compile-property (vector->list value))))
    (lambda (e ctx)
      (for-all (lambda (v) (v e ctx)) validators))))

)
