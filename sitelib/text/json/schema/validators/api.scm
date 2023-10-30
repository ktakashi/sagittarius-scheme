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
	    schema-context->cached-validator
	    schema-validator->core-validator
	    schema-validator-validator
	    
	    schema-context:delayed-validator
	    wrap-core-validator

	    ;; utilities
	    json-schema?
	    build-schema-path
	    boolean->validator
	    uri->id&fragment
	    
	    ;; contexts
	    make-root-context root-context?
	    schema-context?
	    schema-context-version schema-context-version-set!
	    schema-context-schema-id
	    schema-context-in-id
	    schema-context-schema
	    schema-context-anchors
	    schema-context-validator
	    make-initial-schema-context initial-schema-context->schema-validator
	    make-disjoint-context
	    make-schema-context

	    schema-context:find-by-id schema-context:set-id!
	    schema-context:root-schema
	    schema-context:find-by-anchor schema-context:add-anchor!

	    make-validator-context validator-context
	    validator-context:mark!
	    validator-context:mark-element!
	    validator-context:marked?
	    validator-context:marked-element?
	    validator-context:unevaluated?

	    ;; paremters
	    *json-schema:default-version*)
    (import (rnrs)
	    (rfc uri)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (srfi :45 lazy)
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
	  ids
	  cache
	  late-inits
	  (mutable schema-contexts))
  (protocol (lambda (p)
	      (lambda (configuration)
		(p (map ->configuration configuration)
		   (make-hashtable string-hash string=?)
		   (make-eq-hashtable)
		   (list-queue)
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
;; - in-id         - The $id belong to this context, can be the same as schema-id
;;                   FIXME: Bad naming... 
;; - version       - The schema version
;; - root          - root context, which contains version config et.al
;; - parent        - parent schema context
;; - anchors       - $anchor or $id with fragment, hashtable
;; - vocabularies  - $vocabularies, hashtable
;; - validator     - the validator of this context
(define-record-type (schema-context make-raw-schema-context schema-context?)
  (fields schema
	  (mutable schema-id)
	  (mutable in-id)
	  (mutable version)
	  root
	  parent
	  (mutable anchors)
	  definitions
	  vocabularies
	  (mutable validator)
	  cache)
  (protocol (lambda (p)
	      (define (in-id parent)
		(and parent (schema-context-schema-id parent)))
	      (define (version parent)
		(or (and parent (schema-context-version parent))
		    (*json-schema:default-version*)))
	      (lambda (schema root parent)
		(p schema
		   #f
		   (in-id parent)
		   (version parent)
		   root parent
		   (or (and parent (schema-context-anchors parent))
		       (make-hashtable string-hash string=?))
		   (make-hashtable string-hash string=?)
		   (make-hashtable string-hash string=?)
		   #f
		   (or (and parent (schema-context-cache parent))
		       (make-eq-hashtable))
		   )))))

(define (make-initial-schema-context schema root)
  (let ((ctx (make-raw-schema-context schema root #f)))
    (root-context:add-schema-context! root ctx)
    ctx))

(define (make-disjoint-context schema context)
  (let ((root (schema-context-root context)))
    (make-initial-schema-context schema root)))

(define (make-schema-context schema parent)
  (make-raw-schema-context schema (schema-context-root parent) parent))

(define (schema-context:keywords context)
  (let ((root (schema-context-root context))
	(version (schema-context-version context)))
    (configuration-keywords (root-context-configuration root) version)))

;; id must be FQDN (or at least merged)
(define (schema-context:set-id! context id)
  (schema-context-schema-id-set! context id)
  (schema-context-in-id-set! context id)
  (schema-context-anchors-set! context (make-hashtable string-hash string=?))
  (let ((root (schema-context-root context)))
    ;; TODO should we check duplicate $id?
    (hashtable-set! (root-context-ids root) id context)))

(define (schema-context:find-by-id context id)
  (let ((root (schema-context-root context)))
    (hashtable-ref (root-context-ids root) id #f)))

(define (schema-context:root-schema context)
  (if (not (schema-context-parent context))
      context
      (schema-context:root-schema (schema-context-parent context))))

(define (schema-context:add-anchor! context anchor)
  ;; TODO should we check duplicate $anchor?
  (hashtable-set! (schema-context-anchors context) anchor context))

(define (schema-context:find-by-anchor context anchor)
  (let ((anchors (schema-context-anchors context)))
    (hashtable-ref anchors anchor #f)))

(define (schema-context:cache-schema! context)
  (let ((root (schema-context-root context)))
    (hashtable-set! (root-context-cache root)
		    (schema-context-schema context) context)))

(define (schema-context:execute-late-init! context)
  (let ((root (schema-context-root context)))
    (list-queue-for-each (lambda (thunk) (thunk))
			 (root-context-late-inits root))))

;; validator context
;; validation time context
(define-record-type validator-context
  (fields path
	  parent
	  reports
	  marks
	  lint-mode?)
  (protocol (lambda (p)
	      (case-lambda
	       ((lint-mode?)
		(p "/" #f (list-queue) (make-eq-hashtable) lint-mode?))
	       ((path parent)
		;; share the report
		(p (build-validation-path (validator-context-path parent) path)
		   parent
		   (validator-context-reports parent)
		   (validator-context-marks parent)
		   (validator-context-lint-mode? parent)))))))
(define (build-validation-path base path)
  (string-append base "/" (if (number? path) (number->string path) path)))
(define (validator-context:report! context obj schema-path)
  (list-queue-add-front! (validator-context-reports context)
			 (list obj
			       (validator-context-path context)
			       schema-path))
  (validator-context-lint-mode? context))

(define (validator-context:mark! context obj schema)
  (let ((mark (validator-context-marks context)))
    (hashtable-update! mark obj
		       (lambda (v)
			 (cond ((assq schema v) v)
			       (else (cons (cons schema (list-queue)) v))))
		       '())))
(define (validator-context:mark-element! context obj element schema)
  (let ((mark (validator-context-marks context)))
    (hashtable-update! mark obj
		       (lambda (v)
			 (cond ((assq schema v) =>
				(lambda (slot)
				  (list-queue-add-front! (cdr slot) element))))
			 v)
		       '())))

(define (validator-context:marked? context obj schema)
  (let ((slots (hashtable-ref (validator-context-marks context) obj '())))
    (cond ((assq schema slots))
	  (else #f))))

(define (validator-context:marked-element? context obj element schema)
  (let ((slots (hashtable-ref (validator-context-marks context) obj '())))
    (cond ((assq schema slots) =>
	   (lambda (slot) (memq element (list-queue-list (cdr slot)))))
	  (else #f))))

(define (validator-context:unevaluated? context obj element schema)
  (let ((elements (append-map (lambda (s) (list-queue-list (cdr s)))
		   (hashtable-ref (validator-context-marks context) obj '()))))
    (memq element elements)))

;; Schema validator
(define (update-cache! context validator)
  (schema-context-validator-set! context validator)
  (schema-context:cache-schema! context)
  validator)

(define (initial-schema-context->schema-validator initial-context)
  (define (finish validator)
    (schema-context:execute-late-init! initial-context)
    validator)
  (finish (schema-context->schema-validator initial-context "#")))

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
		 (path schema-path)
		 (v ((cadr config) schema))
		 (handler (cddr config)))
	    (if (json-pointer-not-found? v)
		(loop (cdr keywords) acc)
		(let-values (((v continue?) (handler v context path)))
		  (let ((next (or (and v (lambda (e ctx)
					   (and (acc e ctx) (v e ctx))))
				  acc)))
		    (if continue?
			(loop (cdr keywords) next)
			next))))))))
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

(define (schema-context->cached-validator context schema-path)
  (define cache (schema-context-cache context))
  (define (retriever)
    (cond ((schema-context-validator context) =>
	   schema-validator->core-validator)
	  (else true-validator)))
  (cond ((hashtable-ref cache context #f) =>
	 (lambda (promise)
	   (make-schema-validator
	    (lambda (e ctx) ((force promise) e ctx)) schema-path)))
	(else
	 (hashtable-set! cache context (lazy (retriever)))
	 (let* ((v (schema-context->schema-validator context schema-path))
		(core-validator (schema-validator->core-validator v)))
	   (hashtable-set! cache context (delay (lambda () core-validator)))
	   v))))

(define (schema-context:delayed-validator context initializer schema-path)
  (define root (schema-context-root context))
  (define validator #f)
  (define (delayed-validator)
    (lambda (e ctx) (validator e ctx)))
  (list-queue-add-front! (root-context-late-inits root)
			 (lambda () (set! validator (initializer))))
  (update-cache! context
   (make-schema-validator (delayed-validator) schema-path)))


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
(define true-validator (boolean->validator #t))

(define (uri->id&fragment uri)
  (define anchor-index (string-index uri #\#))
  (define len (string-length uri))
  (let ((v (substring uri 0 (or anchor-index len))))
    (values (and (not (string-null? v)) v)
	    (and anchor-index (substring uri (+ anchor-index 1) len)))))

(define (merge-id base new)
  (if (and base (fqdn? base))
      (uri-merge base new)
      new))
(define (fqdn? uri)
  (let-values (((scheme specific) (uri-scheme&specific uri)))
    ;; at least mergeable I think
    (and scheme #t)))

)
