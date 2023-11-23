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
	    schema-context->cached-schema-validator
	    schema-validator->core-validator
	    
	    schema-context:delayed-validator
	    schema-context:dynamic-validator
	    schema-context:recursive-validator
	    core-validator->reporting-validator

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
	    schema-context-parent
	    make-initial-schema-context initial-schema-context->schema-validator
	    make-disjoint-context
	    make-schema-context

	    schema-context->schema-validator/late-initiation

	    schema-context:find-by-id schema-context:set-id!
	    schema-context:root-schema
	    schema-context:find-by-anchor schema-context:add-anchor!

	    schema-context:mark-dynamic-anchor!
	    schema-context:has-dynamic-anchor?
	    schema-context:find-by-dynamic-anchor
	    schema-context:mark-recursive-anchor!
	    schema-context:recursive-anchor-enabled?

	    schema-context:cache-vocabulary! schema-context:vocabulary-loaded?

	    make-validator-context validator-context
	    (rename (validator-context:reports validator-context-reports))
	    validator-context-lint-mode?
	    validator-context:add-path!
	    validator-context:detatch-report!
	    validator-context:mark-element!
	    validator-context:update-marks!
	    validator-context:marked-element?
	    validator-context:unevaluated?
	    validator-context:set-dynamic-context!
	    validator-context:set-recursive-context!

	    validation-report-object
	    validation-report-path
	    validation-report-schema-path
	    
	    ;; paremters
	    *json-schema:default-version*)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (rfc uri)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (srfi :117 list-queues)
	    (srfi :126 hashtables)
	    (text json pointer)
	    (text json schema version))

(define *json-schema:default-version* 
  (make-parameter (json-schema:version draft-7)))

(define *recursive-anchor* "")

;; Root context
(define (jp name) (json-pointer (string-append "/" name)))
(define (->configuration entry)
  (define (->config config) (cons* (car config) (jp (car config)) (cdr config)))
  (cons (car entry) (map ->config (cdr entry))))

;; - vocabularies    - retrieved vocabularies, to avoid infinite loop
;; - dynamic-anchors - dynamic anchors (incl. recursive anchor)
(define-record-type root-context
  (fields configuration
	  ids
	  cache
	  vocabularies
	  dynamic-anchors
	  schema-contexts)
  (protocol (lambda (p)
	      (lambda (configuration)
		(p (map ->configuration configuration)
		   (make-hashtable string-hash string=?)
		   (make-eq-hashtable)
		   (make-hashtable string-hash string=?)
		   (make-hashtable string-hash string=?)
		   (list-queue))))))

(define (root-context:add-schema-context! root schema-context)
  (let ((contexts (root-context-schema-contexts root)))
    (list-queue-add-back! contexts schema-context)))

(define (configuration-keywords configuration version)
  (cond ((assq version configuration) => cdr)
	(else 
	 ;; This must never happen, but for sanity
	 (assertion-violation 'configuration-keywords
			      "[BUG] The version is not known" version))))

;; $schema needs to be handled before validator compilation
;; due to the vocabularies selection.
(define $schema-pointer (json-pointer "/$schema"))

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
;; - validator     - the validator of this context
(define-record-type (schema-context make-raw-schema-context schema-context?)
  (fields schema
	  (mutable schema-id)
	  (mutable in-id)
	  (mutable version)
	  root
	  parent
	  (mutable anchors)
	  (mutable validator)
	  cache
	  late-inits)
  (protocol (lambda (p)
	      (define (in-id parent)
		(and parent
		     (or (schema-context-schema-id parent)
			 (in-id (schema-context-parent parent)))))
	      (define (version schema parent)
		(define uri ($schema-pointer schema))
		(define (uri->version uri)
		  (cond ((json-schema->version uri))
			(else (assertion-violation 'json-schema:$schema
						   "Unknown schema" uri))))
		(or (and (not (json-pointer-not-found? uri))
			 (uri->version uri))
		    (and parent (schema-context-version parent))
		    (*json-schema:default-version*)))
	      (lambda (schema root parent)
		(p schema
		   #f
		   (in-id parent)
		   (version schema parent)
		   root parent
		   (or (and parent (schema-context-anchors parent))
		       (make-hashtable string-hash string=?))
		   #f
		   (or (and parent (schema-context-cache parent))
		       (make-eq-hashtable))
		   (or (and parent (schema-context-late-inits parent))
		       (list-queue))
		   )))))

(define (make-initial-schema-context schema root)
  (let ((ctx (make-raw-schema-context schema root #f)))
    (root-context:add-schema-context! root ctx)
    ctx))

(define (make-disjoint-context schema context)
  (let ((root (schema-context-root context)))
    (parameterize ((*json-schema:default-version*
		    (schema-context-version context)))
      (make-initial-schema-context schema root))))

(define (make-schema-context schema parent)
  (let* ((root (schema-context-root parent))
	 (cache (root-context-cache root)))
    (cond ((hashtable-ref cache schema #f))
	  (else
	   (let ((r (make-raw-schema-context schema root parent)))
	     (hashtable-set! cache schema r)
	     r)))))

(define (schema-context:keywords context)
  (let ((root (schema-context-root context))
	(version (schema-context-version context)))
    (configuration-keywords (root-context-configuration root) version)))

;; id must be FQDN (or at least merged)
(define (schema-context:set-id! context id)
  (define (search-parent-in-id context)
    (and context
	 (or (schema-context-schema-id context)
	     (search-parent-in-id (schema-context-parent context)))))
  (define (get-in-id parent)
    (cond ((and parent (search-parent-in-id parent)))
	  (else #f)))
  (let* ((in-id (get-in-id (schema-context-parent context)))
	 (id (or (and in-id (uri-merge in-id id)) id)))
    (schema-context-schema-id-set! context id)
    (schema-context-in-id-set! context (or in-id id))
    (schema-context-anchors-set! context (make-hashtable string-hash string=?))
    (let ((root (schema-context-root context)))
      ;; TODO should we check duplicate $id?
      (hashtable-set! (root-context-ids root) id context))))

(define (schema-context:find-by-id context id)
  (let ((root (schema-context-root context)))
    (hashtable-ref (root-context-ids root) id #f)))

(define (schema-context:root-schema context)
  (if (not (schema-context-parent context))
      context
      (schema-context:root-schema (schema-context-parent context))))

(define (schema-context:same-root? context0 context1)
  (eq? (schema-context:root-schema context0)
       (schema-context:root-schema context1)))

(define (schema-context:add-anchor! context anchor)
  ;; TODO should we check duplicate $anchor?
  (hashtable-set! (schema-context-anchors context) anchor context))

(define (schema-context:find-by-anchor context anchor)
  (let ((anchors (schema-context-anchors context)))
    (hashtable-ref anchors anchor #f)))

(define (schema-context:find-by-dynamic-anchor context anchor)
  (define id (or (schema-context-schema-id context)
		 (schema-context-in-id context)
		 ""))
  (let ((anchors (root-context-dynamic-anchors (schema-context-root context))))
    (cond ((hashtable-ref anchors (string-append id "#" anchor) #f))
	  (else #f))))

(define (schema-context:mark-dynamic-anchor! context anchor)
  (define id (or (schema-context-schema-id context)
		 (schema-context-in-id context)
		 ""))
  (let ((anchors (root-context-dynamic-anchors (schema-context-root context))))
    (schema-context:add-anchor! context anchor)
    ;; Anchor per dynamic scope, we prepare like this here
    ;; maybe we should make a different field...
    (hashtable-set! anchors (string-append id "#" anchor) context)
    (hashtable-update! anchors anchor
		       (lambda (r)
			 (cond ((memq context r) r)
			       (else (cons context r))))
		       '())))

(define (schema-context:mark-recursive-anchor! context)
  (let ((root (schema-context-root context)))
    (hashtable-update! (root-context-dynamic-anchors root) *recursive-anchor*
		       (lambda (r)
			 (cond ((memq context r) r)
			       (else (cons context r))))
		       '())))

(define (schema-context:has-dynamic-anchor? context anchor)
  (define id (or (schema-context-schema-id context)
		 (schema-context-in-id context)
		 ""))
  (let ((anchors (root-context-dynamic-anchors (schema-context-root context))))
    (cond ((hashtable-ref anchors (string-append id "#" anchor) #f))
	  (else #f))))

(define (schema-context:recursive-anchor-enabled? context)
  (let ((anchors (root-context-dynamic-anchors (schema-context-root context))))
    (cond ((hashtable-ref anchors *recursive-anchor* #f) =>
	   (lambda (v)
	     (let* ((id (schema-context-in-id context))
		    (target (schema-context:find-by-id context id)))
	       (and (memq target v) #t))))
	  (else #f))))

(define (schema-context:cache-schema! context)
  (let ((root (schema-context-root context)))
    (hashtable-set! (root-context-cache root)
		    (schema-context-schema context) context)))

(define (schema-context:cache-vocabulary! context uri)
  (let ((root (schema-context-root context)))
    (hashtable-set! (root-context-vocabularies root) uri context)))

(define (schema-context:vocabulary-loaded? context uri)
  (let ((root (schema-context-root context)))
    (hashtable-contains? (root-context-vocabularies root) uri)))

(define (schema-context:execute-late-init! context)
  (for-each (lambda (thunk) (thunk))
	    (list-queue-remove-all! (schema-context-late-inits context))))

;; validator context
;; validation time context
(define-record-type (validator-context
		     make-raw-validator-context
		     validator-context?)
  (fields path
	  parent
	  reports
	  dynamic-contexts
	  evaluating-scopes  ;; schema-ids
	  evaluating-schemas ;; schema
	  lint-mode?))
(define (make-validator-context lint-mode?)
  (make-raw-validator-context
   "/" #f
   (list-queue)
   (make-hashtable equal-hash equal?)
   (list-queue)
   (list-queue)
   lint-mode?))

(define (build-validation-path base path)
  (define segment (json-pointer-encode
		   (if (number? path) (number->string path) path)))
  (if (string=? "/" base)
      (string-append base segment)
      (string-append base "/" segment)))

(define (validator-context:add-path! context path)
  (define base (validator-context-path context))
  (make-raw-validator-context
   (build-validation-path base path)
   context
   (validator-context-reports context)
   (validator-context-dynamic-contexts context)
   (validator-context-evaluating-scopes context)
   (validator-context-evaluating-schemas context)
   (validator-context-lint-mode? context)))

(define (validator-context:detatch-report! context)
  (make-raw-validator-context
   (validator-context-path context)
   context
   (list-queue)
   (validator-context-dynamic-contexts context)
   (validator-context-evaluating-scopes context)
   (validator-context-evaluating-schemas context)
   (validator-context-lint-mode? context)))

(define validation-report list)
(define validation-report-object car)
(define validation-report-path cadr)
(define validation-report-schema-path caddr)

(define (validator-context:report! context obj schema-path)
  (list-queue-add-front! (validator-context-reports context)
			 (validation-report obj
			  (validator-context-path context)
			  schema-path))
  (validator-context-lint-mode? context))

(define (validator-context:reports context)
  (list-queue-list (validator-context-reports context)))

(define (validator-context:mark-element! context obj element schema success?)
  (define target-schema (schema-context-schema schema))
  (let-values (((schema marks) (validator-context:evaluating-schema context)))
    (when (eq? target-schema schema)
      (let ((v (cons* schema element success?)))
	(hashtable-update! marks obj (lambda (r) (cons v r)) '()))))
  success?)

(define (validator-context:update-marks! context obj schema success?)
  (define target-schema (schema-context-schema schema))
  (define (flip-result v)
    (filter-map (lambda (s)
		  (cond ((eq? target-schema (car s))
			 (cond (success? (set-cdr! (cdr s) success?) s)
			       (else #f)))
			(else s))) v))
  (let-values (((schema marks) (validator-context:evaluating-schema context)))
    (hashtable-update! marks obj flip-result '())
    success?))

(define (validator-context:marked-element? context obj element schema-context)
  (define target-schema (schema-context-schema schema-context))
  (let-values (((schema marks) (validator-context:evaluating-schema context)))
    (let ((slots (hashtable-ref marks obj '())))
      (cond ((find (lambda (v)
		     (and (eq? (car v) target-schema) (equal? (cadr v) element)))
		     slots) #t)
	    (else #f)))))

(define (validator-context:unevaluated? context obj element schema-context)
  (let-values (((schema marks) (validator-context:evaluating-schema context)))
    (and (eq? schema (schema-context-schema schema-context))
	 (cond ((hashtable-ref marks obj #f) =>
		(lambda (v*)
		  (find (lambda (v)
			  (and (cddr v) (equal? (cadr v) element))) v*)))
	       (else #f)))))

(define (validator-context:set-dynamic-context! context schema-context anchor)
  (let ((root (schema-context-root schema-context))
	(dynamic-contexts (validator-context-dynamic-contexts context)))
    (cond ((hashtable-ref (root-context-dynamic-anchors root) anchor #f) =>
	   (lambda (v)
	     (when (and (not (hashtable-ref dynamic-contexts anchor #f))
			(memq schema-context v))
	       (hashtable-set! dynamic-contexts anchor schema-context)))))))

(define (validator-context:set-recursive-context! context schema-context)
  (let ((anchors
	 (root-context-dynamic-anchors (schema-context-root schema-context)))
	(contexts (validator-context-dynamic-contexts context)))
    (cond ((hashtable-ref anchors *recursive-anchor* #f) =>
	   (lambda (v)
	     (when (and (not (hashtable-ref contexts *recursive-anchor* #f))
			(memq schema-context v))
	       (hashtable-set! contexts *recursive-anchor* schema-context)))))))

(define (validator-context:dynamic-context context anchor)
  (let ((dynamic-contexts (validator-context-dynamic-contexts context)))
    (hashtable-ref dynamic-contexts anchor #f)))

(define (validator-context:push-scope! context id)
  (let ((queue (validator-context-evaluating-scopes context)))
    (or (and (list-queue-empty? queue)
	     (list-queue-add-front! queue id))
	(and (not (equal? (list-queue-front queue) id))
	     (list-queue-add-front! queue id)))))

(define (validator-context:push-schema! context schema)
  (let ((queue (validator-context-evaluating-schemas context)))
    (list-queue-add-front! queue (cons schema (make-eq-hashtable)))))

(define (validator-context:pop-scope! context)
  (let ((queue (validator-context-evaluating-scopes context)))
    (list-queue-remove-front! queue)))

(define (validator-context:pop-schema! context)
  (let* ((queue (validator-context-evaluating-schemas context))
	 (e (list-queue-remove-front! queue)))
    (unless (list-queue-empty? queue)
      (let ((ht (cdr (list-queue-front queue))))
	(hashtable-walk (cdr e)
	 ;; k = obj, v = (schema . element)
	 ;; it's a bit redundant, but keep it like this for my convenience
	 (lambda (k v)
	   (hashtable-update! ht k (lambda (r) (append v r)) '())))))))

(define (validator-context:evaluating-schema context)
  (let ((v (list-queue-front (validator-context-evaluating-schemas context))))
    (values (car v) (cdr v))))

;; Schema validator
(define (update-cache! context validator)
  (schema-context-validator-set! context validator)
  (schema-context:cache-schema! context)
  validator)

(define (initial-schema-context->schema-validator initial-context)
  (schema-context->schema-validator/late-initiation initial-context '("#")))

(define (schema-context->schema-validator/late-initiation context schema-path)
  (define (finish validator)
    (schema-context:execute-late-init! context)
    validator)
  (finish (schema-context->schema-validator context schema-path)))


;; schema-context -> validator
;; schema-path is debug or reporting purpose
(define (schema-context->schema-validator context schema-path)
  (define schema (schema-context-schema context))
  (define (compile schema context schema-path)
    (define keywords (schema-context:keywords context))
    (define len (vector-length schema))
    (define (merge acc v)
      (lambda (e ctx)
	(if (validator-context-lint-mode? ctx)
	    (let ((r0 (acc e ctx)) (r1 (v e ctx)))
	      (and r0 r1))
	    (and (acc e ctx) (v e ctx)))))
	      
    (let loop ((keywords keywords) (acc (lambda (e ctx) #t)))
      (if (null? keywords)
	  acc
	  (let* ((config (car keywords))
		 (path (cons (car config) schema-path))
		 (v ((cadr config) schema))
		 (handler (cddr config)))
	    (if (json-pointer-not-found? v)
		(loop (cdr keywords) acc)
		(let-values (((v continue?) (handler v context path)))
		  (let ((next (or (and v (merge acc v)) acc)))
		    (if continue?
			(loop (cdr keywords) next)
			next))))))))
  (cond ((vector? schema)
	 (update-cache! context
	  (make-schema-validator (compile schema context schema-path)
				 schema-path
				 context)))
	((boolean? schema)
	 (update-cache! context
	  (make-schema-validator (boolean->validator schema)
				 schema-path)))
	(else
	 (assertion-violation 'schema-context->schema-validator
			      "Invalid JSON Schema" schema))))

(define (schema-context->cached-schema-validator context schema-path)
  (define cache (schema-context-cache context))
  (define (initializer)
    (schema-validator->core-validator
     (schema-context->schema-validator context schema-path)))
  (cond ((hashtable-ref cache context #f))
	(else
	 (let* ((core-validator (schema-context:delayed-validator
				 context initializer schema-path))
		(validator (make-schema-validator core-validator
						  schema-path context)))
	   (hashtable-set! cache context validator)
	   validator))))

(define (schema-context:delayed-validator context initializer schema-path)
  (define validator #f)
  (define (delayed-validator) (lambda (e ctx) (validator e ctx)))
  (list-queue-add-front! (schema-context-late-inits context)
			 (lambda () (set! validator (initializer))))
  (delayed-validator))

(define (schema-context:recursive-validator context schema-path)
  (define (validator ctx)
    (schema-validator->core-validator
     (schema-context-validator
      (validator-context:dynamic-context ctx *recursive-anchor*))))
  (handle-dynamic-validator context schema-path validator))

(define (schema-context:dynamic-validator context dynamic-anchor schema-path)
  (define root (schema-context-root context))
  (define dynamic-anchors (root-context-dynamic-anchors root))
  ;; Now, we are doing a bit sloppy way of resolving dynamic scope here.
  ;; First we get the farthest scope, by reversing pushed scopes.
  ;; Then checks if there's a possible dynamic anchor per scope
  ;; i.e., we set dynamic anchor with `id#anchor` format during complation
  ;;       so just referring the key
  (define (search-dynamic-scope anchors ctx)
    (define scopes
      (reverse!
       (filter values
	       (list-queue-list (validator-context-evaluating-scopes ctx)))))
    (define ->schema schema-context-schema)
    
    (let loop ((s* scopes))
      (if (null? s*)
	  ;; no scope? should never happen
	  (assertion-violation 'schema-context:dynamic-validator
			       "No dynamic anchor found" dynamic-anchor
			       schema-path)
	  (let ((schema (schema-context:find-by-id context (car s*))))
	    (cond ((not schema) (loop (cdr s*)))
		  ((schema-context:find-by-dynamic-anchor schema dynamic-anchor))
		  (else (loop (cdr s*))))))))
	    
  (define (schema-context ctx dynamic-anchor)
    ;; The first case is that root schema contains $dynamicAnchor
    ;; which means, it is the farthest dynamic scope, easy.
    (cond ((validator-context:dynamic-context ctx dynamic-anchor))
	  ;; Now, $dynamicAnchor is one of the schema, most likely
	  ;; $defs, so we need to search the farthest dynamic scope
	  ((hashtable-ref dynamic-anchors dynamic-anchor #f) =>
	   (lambda (anchors) (search-dynamic-scope anchors ctx)))
	  (else (assertion-violation 'schema-context:dynamic-validator
				     "[BUG] No dynamic anchor found"
				     dynamic-anchor))))
  (define (validator ctx)
    (schema-validator->core-validator
     (schema-context-validator
      (schema-context ctx dynamic-anchor))))
  (handle-dynamic-validator context schema-path validator))

(define (handle-dynamic-validator context schema-path validator-provier)
  (define (dynamic-validator e ctx) ((validator-provier ctx) e ctx))
  dynamic-validator)

(define-record-type schema-validator
  (fields validator schema-path schema-context)
  (protocol (lambda (p)
	      (case-lambda
	       ((validator schema-path) (p validator schema-path #f))
	       ((validator schema-path schema-context)
		(p validator schema-path schema-context))))))
(define (schema-validator->core-validator schema-validator)
  (define core-validator (schema-validator-validator schema-validator))
  (define schema-path
    (string-join
     (reverse (schema-validator-schema-path schema-validator)) "/"))
  (define context (schema-validator-schema-context schema-validator))
  (define id (and (schema-context? context)
		  (or (schema-context-schema-id context)
		      (schema-context-in-id context))))
  (define schema (and (schema-context? context) (schema-context-schema context)))
  (lambda (e ctx)
    (define pushed? (validator-context:push-scope! ctx id))
    (validator-context:push-schema! ctx schema)
    (let ((r (core-validator e ctx)))
      (unless r (validator-context:report! ctx e schema-path))
      (validator-context:pop-schema! ctx)
      (when pushed? (validator-context:pop-scope! ctx))
      r)))

(define (core-validator->reporting-validator validator schema-path)
  (define path (string-join (reverse schema-path) "/"))
  (lambda (e ctx)
    (let ((r (validator e ctx)))
      (unless r (validator-context:report! ctx e path))
      r)))

;; utilities
(define (json-schema? v) (or (vector? v) (boolean? v)))
(define (in-scope? scope-context child-context)
  (cond ((not child-context) #f)
	((eq? scope-context child-context))
	(else (in-scope? scope-context (schema-context-parent child-context)))))

(define (build-schema-path base child) (cons child base))
(define (boolean->validator b) (lambda (e ctx) b))

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
