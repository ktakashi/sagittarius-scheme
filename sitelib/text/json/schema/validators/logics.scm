;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators/logcs.scm - JSON schema logc subschemas
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
(library (text json schema validators logics)
    (export json-schema:all-of json-schema:any-of json-schema:one-of
	    json-schema:not

	    json-schema:if
	    json-schema:then
	    json-schema:else)
    (import (rnrs)
	    (srfi :1 lists)
	    (text json pointer)
	    (text json schema validators api))

(define (and-merger . validators)
  (lambda (e ctx)
    (define ctx2 (validator-context:detatch-report! ctx))
    (for-all (lambda (validator) (validator e ctx2)) validators)))
(define (or-merger . validators)
  (lambda (e ctx)
    (define ctx2 (validator-context:detatch-report! ctx))
    (let ((r (filter-map (lambda (validator) (validator e ctx2)) validators)))
      (not (null? r)))))
(define (unique-merger . validators)
  (lambda (e ctx)
    (define ctx2 (validator-context:detatch-report! ctx))
    (let ((r (filter-map (lambda (validator) (validator e ctx2)) validators)))
      (= (length r) 1))))

(define (array-of-schema-handler name merger)
  (define (compile schema context path)
    (schema-context->schema-validator
     (make-schema-context schema context) path))
  (lambda (value context schema-path)
    (unless (list? value)
      (assertion-violation 'array-of-schema-handler
			   "Must be a list of schema" value))
    (when (null? value)
      (assertion-violation 'array-of-schema-handler
			   "Must have at least one schema" value))
    (let ((path (build-schema-path schema-path name)))
      (do ((i 0 (+ i 1)) (schema value (cdr schema))
	   (r '() (cons (compile (car schema) context
				 (build-schema-path path (number->string i)))
			r)))
	  ((null? schema)
	   (wrap-core-validator
	    (apply merger (reverse! (map schema-validator-validator r)))
	    path))))))

(define json-schema:all-of (array-of-schema-handler "allOf" and-merger))
(define json-schema:any-of (array-of-schema-handler "anyOf" or-merger))
(define json-schema:one-of (array-of-schema-handler "oneOf" unique-merger))

(define (json-schema:not value context schema-path)
  (unless (or (json-schema? value))
    (assertion-violation 'json-schema:not "JSON Schema is required" value))
  (let* ((path (build-schema-path schema-path "not"))
	 (validator (schema-validator-validator
		     (schema-context->schema-validator
		      (make-schema-context value context) path))))
    (wrap-core-validator
     (lambda (e ctx)
       (let ((snapshot (validator-context:marks ctx e)))
	 (validator-context:update-difference! ctx e snapshot (not (validator e ctx)))))
     path)))

;; if-then-else is handled a bit differently from the other keywords.
(define then-pointer (json-pointer "/then"))
(define else-pointer (json-pointer "/else"))
(define (json-schema:if value context schema-path)
  (define build-path (lambda (path) (build-schema-path schema-path path)))
  (define (schema->core-validator schema context schema-path)
    (schema-validator->core-validator
     (schema-context->schema-validator (make-schema-context schema context)
				       schema-path)))
  (unless (json-schema? value)
    (assertion-violation 'json-schema:if "JSON Schema is required" value))
  (let* ((parent-schema (schema-context-schema context))
	 (then-schema (then-pointer parent-schema))
	 (else-schema (else-pointer parent-schema))
	 (if-validator (schema->core-validator value context (build-path "if")))
	 (then-validator (and (not (json-pointer-not-found? then-schema))
			      (schema->core-validator
			       then-schema context (build-path "then"))))
	 (else-validator (and (not (json-pointer-not-found? else-schema))
			      (schema->core-validator
			       else-schema context (build-path "else")))))
    (lambda (e ctx)
      (if (if-validator e ctx)
	  (if then-validator
	      (then-validator e ctx)
	      #t)
	  (if else-validator
	      (else-validator e ctx)
	      #t)))))

;; It does nothing basically but in case of *without* `if` keyword
;; i.e. test suits
(define (json-schema:then value context schema-path)
  (unless (json-schema? value)
    (assertion-violation 'json-schema:then "JSON Schema is required" value))
  (schema-context->schema-validator
   (make-schema-context value context)
   (build-schema-path schema-path "then"))
  #f)
(define (json-schema:else value context schema-path)
  (unless (json-schema? value)
    (assertion-violation 'json-schema:else "JSON Schema is required" value))
  (schema-context->schema-validator
   (make-schema-context value context)
   (build-schema-path schema-path "else"))
  #f)
  
)
