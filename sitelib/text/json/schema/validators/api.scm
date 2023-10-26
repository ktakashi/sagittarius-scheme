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
    (export schema-context->schema-validator
	    schema-validator->core-validator
	    schema-validator-validator
	    wrap-core-validator

	    ;; utilities
	    json-schema?
	    build-schema-path
	    boolean->validator
	    
	    ;; contexts
	    make-root-context root-context?
	    schema-context?
	    schema-context-version schema-context-schema-id
	    schema-context-schema
	    make-initial-schema-context
	    make-schema-context

	    make-validator-context validator-context

	    ;; paremters
	    *json-schema:default-version*)
    (import (rnrs)
	    (srfi :2 and-let*)
	    (srfi :39 parameters)
	    (srfi :117 list-queues)
	    (text json pointer)
	    (text json schema version))

(define *json-schema:default-version* 
  (make-parameter (json-schema:version draft-7)))

;; Root context
(define (jp name) (json-pointer (string-append "/" name)))
(define (->configuration entry)
  (define (->config config) (cons* (car config) (jp (car config)) (cdr config)))
  (cons (car entry) (map ->config (cdr entry))))
(define-record-type root-context
  (fields configuration
	  (mutable schema-contexts))
  (protocol (lambda (p)
	      (lambda (configuration)
		(p (map ->configuration configuration)
		   '())))))

(define (root-context:add-schema-context! root schema-context)
  (let ((contexts (root-context-schema-contexts root)))
    (root-context-schema-contexts-set! root (cons schema-context contexts))))

(define (configuration-keywords configuration version)
  (cond ((assq version configuration) => cdr)
	(else 
	 ;; This must never happen, but for sanity
	 (assertion-violation 'configuration-keywords
			      "[BUG] The version is not known" version))))

;; Schema context
;; A context contains
;; - schema        - Current JSON schema
;; - schema-id     - This can be propagated from the parent context
;; - version       - The schema version
;; - root          - root context, which contains version config et.al
;; - parent        - parent schema context
;; - anchors       - $anchor or $id with fragment, hashtable
;; - vocabularies  - $vocabularies, hashtable
;; - validator     - the validator of this context
(define $schema-pointer (json-pointer "/$schema"))
(define $id-pointer (json-pointer "/$id"))
(define-record-type (schema-context make-raw-schema-context schema-context?)
  (fields schema
	  schema-id
	  version
	  root
	  parent
	  anchors
	  vocabularies
	  (mutable validator))
  (protocol (lambda (p)
	      (define (id schema parent)
		(or (and-let* ( ( (vector? schema) )
				(r ($id-pointer schema))
				( (not (json-pointer-not-found? r)) ))
		      r)
		    (and parent (schema-context-schema-id parent))))
	      (define (version schema parent)
		(or (and-let* ( ( (vector? schema) )
				(r ($schema-pointer schema))
				( (not (json-pointer-not-found? r)) ))
		      (json-schema->version r))
		    (and parent (schema-context-version parent))
		    (*json-schema:default-version*)))
	      (lambda (schema root parent)
		(p schema
		   (id schema parent)
		   (version schema parent)
		   root parent
		   (make-hashtable string-hash string=?)
		   (make-hashtable string-hash string=?)
		   #f)))))

(define (make-initial-schema-context schema root)
  (let ((ctx (make-raw-schema-context schema root #f)))
    (root-context:add-schema-context! root ctx)
    ctx))

(define (make-schema-context schema parent)
  (make-raw-schema-context schema (schema-context-root parent) parent))

(define (schema-context:keywords context)
  (let ((root (schema-context-root context))
	(version (schema-context-version context)))
    (configuration-keywords (root-context-configuration root) version)))

;; validator context
;; validation time context
(define-record-type validator-context
  (fields path
	  parent
	  reports
	  lint-mode?)
  (protocol (lambda (p)
	      (case-lambda
	       ((lint-mode?) (p "/" #f (list-queue) lint-mode?))
	       ((path parent)
		;; share the report
		(p (build-validation-path (validator-context-path parent) path)
		   parent
		   (validator-context-reports parent)
		   (validator-context-lint-mode? parent)))))))
(define (build-validation-path base path)
  (string-append base "/" (if (number? path) (number->string path) path)))
(define (validator-context:report! context obj schema-path)
  (list-queue-add-front! (validator-context-reports context)
			 (list obj
			       (validator-context-path context)
			       schema-path))
  (validator-context-lint-mode? context))

;; schema-context -> validator
;; schema-path is debug or reporting purpose
(define (schema-context->schema-validator context schema-path)
  (define schema (schema-context-schema context))
  (define (compile schema context schema-path)
    (define keywords (schema-context:keywords context))
    (define len (vector-length schema))
    (let loop ((keywords keywords) (acc (lambda (e ctx) #t)))
      (if (null? keywords)
	  acc
	  (let* ((config (car keywords))
		 (path (build-schema-path schema-path (car config)))
		 (v ((cadr config) schema))
		 (handler (cddr config)))
	    (if (json-pointer-not-found? v)
		(loop (cdr keywords) acc)
		(let-values (((validator continue?) (handler v context path)))
		  (let ((v (lambda (e ctx) (and (acc e ctx) (validator e ctx)))))
		  (if continue?
		      (loop (cdr keywords) v)
		      v))))))))
  (define (update-cache! context validator)
    (schema-context-validator-set! context validator)
    validator)
  (cond ((vector? schema)
	 (update-cache! context
	  (make-schema-validator (compile schema context schema-path)
				 schema-path)))
	((boolean? schema)
	 (update-cache! context
	  (make-schema-validator (boolean->validator schema)
				 schema-path)))
	(else
	 (assertion-violation 'schema-context->schema-validator
			      "Invalid JSON Schema" schema))))

(define-record-type schema-validator
  (fields validator schema-path))
(define (schema-validator->core-validator schema-validator)
  (define core-validator (schema-validator-validator schema-validator))
  (define schema-path (schema-validator-schema-path schema-validator))
  (lambda (e ctx)
    (or (core-validator e ctx)
	(validator-context:report! ctx e schema-path))))

(define (wrap-core-validator validator schema-path)
  (schema-validator->core-validator
   (make-schema-validator validator schema-path)))

;; utilities
(define (json-schema? v) (or (vector? v) (boolean? v)))
(define (build-schema-path base child)
  (string-append base "/" child))
(define (boolean->validator b) (lambda (e ctx) b))

)
