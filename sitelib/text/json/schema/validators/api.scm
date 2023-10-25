;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators/api.scm - JSON schema base
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
(library (text json schema validators api)
    (export validator-context->schema-validator
	    make-schema-validator schema-validator?
	    schema-validator->core-validator
	    wrapped-validator
	    
	    make-validator-context validator-context?
	    make-initial-validator-context
	    validator-context-version validator-context-id
	    validator-context-schema validator-context-parent
	    validator-context-cache
	    *json-schema:version*
	    
	    )
    (import (rnrs)
	    (sagittarius regex)
	    (srfi :1 lists)
	    (srfi :39 parameters)
	    (srfi :133 vectors)
	    (text json pointer)
	    (rename (text json schema conditions)
		    (raise-json-schema-report report))
	    (text json schema version))

(define *json-schema:version* (make-parameter (json-schema:version draft-7)))
(define $schema-pointer (json-pointer "/$schema"))
(define $id-pointer (json-pointer "/$id"))

(define-record-type
    (validator-context make-initial-validator-context validator-context?)
  (fields version      ;; schema version
	  id	       ;; $id for convenience
	  schema       ;; raw schema (sexp JSON)
	  parent       ;; parent context (for $vocabulary handling, mainly)
	  cache	       ;; validator cache
	  (mutable $vocabularies)
	  schema->validator
	  )
  (protocol (lambda (p)
	      (define (parent-version context)
		(cond ((not context) (*json-schema:version*))
		      ((validator-context-version context))
		      ((validator-context-parent context) => parent-version)
		      (else (*json-schema:version*))))
	      (define (get-version schema parent)
		(let ((schema-id ($schema-pointer schema)))
		  (cond ((json-pointer-not-found? schema-id)
			 (if parent
			     (cond ((validator-context-version parent))
				   ((get-version
				     (validator-context-schema parent)
				     (validator-context-parent parent))))
			     (parent-version parent)))
			((json-schema->version schema-id))
			(else (*json-schema:version*)))))
	      (define (get-id schema)
		(let ((id ($id-pointer schema)))
		  (cond ((json-pointer-not-found? id) #f)
			(else id))))
	      (define (make schema parent cache ->validator)
		(cond ((vector? schema)
		       (p (get-version schema parent)
			  (get-id schema) schema parent cache #f ->validator))
		      (parent
		       (p (get-version parent #f) #f schema parent cache #f
			  ->validator))
		      (else
		       (p #f (*json-schema:version*) schema parent cache #f
			  ->validator))))
	      (case-lambda
	       ((schema schema->validator)
		(make schema #f (make-eq-hashtable) schema->validator))
	       ((schema parent schema->validator)
		(make schema parent
		      (validator-context-cache parent)
		      schema->validator))))))

(define (make-validator-context schema parent)
  (make-initial-validator-context schema parent
   (validator-contnext-schema->validator parent)))

(define (validator-context->schema-validator context schema-path)
  ((validator-context-schema->validator context) context schema-path))

(define-record-type schema-validator
  (fields validator schema-path))

(define (schema-validator->core-validator schema-validator)
  (define core-validator (schema-validator-validator schema-validator))
  (define schema-path (schema-validator-schema-path schema-validator))
  (lambda (e path)
    (or (core-validator e path)
	(report e (if (equal? path "") "/" path) schema-path))))
(define (wrapped-validator validator schema-path)
  (schema-validator->core-validator
   (make-schema-validator validator schema-path)))
)
