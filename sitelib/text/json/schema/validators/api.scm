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
	    schema-context:dynamic-validator
	    schema-context:recursive-validator
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
	    schema-context-parent
	    make-initial-schema-context initial-schema-context->schema-validator
	    make-disjoint-context
	    make-schema-context

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
	    validator-context:marks
	    validator-context:mark!
	    validator-context:mark-element!
	    validator-context:update-difference!
	    validator-context:marked?
	    validator-context:marked-element?
	    validator-context:unevaluated?
	    validator-context:set-dynamic-context!
	    validator-context:set-recursive-context!

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
	  ;; first one, above should resolve I think...
	  ((hashtable-ref anchors anchor #f) => car)
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
  (let ((root (schema-context-root context)))
    (cond ((hashtable-ref (root-context-dynamic-anchors root) anchor #f))
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
(define-record-type validator-context
  (fields path
	  parent
	  reports
	  marks
	  (mutable dynamic-contexts)
	  evaluating-schemas
	  lint-mode?)
  (protocol (lambda (p)
	      (case-lambda
	       ((lint-mode?)
		(p "/"
		   #f
		   (list-queue)
		   ;; relay on the fact that JSON must not have duplicate keys
		   (make-hashtable equal-hash equal?)
		   (make-hashtable equal-hash equal?)
		   (list-queue)
		   lint-mode?))))))
(define (build-validation-path base path)
  (string-append base "/" (if (number? path) (number->string path) path)))
(define (validator-context:report! context obj schema-path)
  (list-queue-add-front! (validator-context-reports context)
			 (list obj
			       (validator-context-path context)
			       schema-path))
  (validator-context-lint-mode? context))

(define (validator-context:reports context)
  (list-queue-list (validator-context-reports context)))

(define (validator-context:mark! context obj schema)
  (let ((mark (validator-context-marks context)))
    (hashtable-update! mark obj
      (lambda (v)
	(cond ((assq schema v) v)
	      (else (cons (cons schema (list-queue)) v))))
      '())))
(define (validator-context:mark-element! context obj element schema success?)
  (let ((mark (validator-context-marks context)))
    (hashtable-update! mark obj
      (lambda (v)
	(cond ((assq schema v) =>
	       (lambda (slot)
		 (list-queue-add-front! (cdr slot) (cons element success?)))))
	v)
      '())
    success?))

(define (validator-context:update-difference! context obj snapshot success?)
  (define (swap-marks! q base diff)
    (list-queue-clear! q)
    (for-each (lambda (v) (list-queue-add-back! q v)) base)
    ;; strip out failed validation
    ;; NB: this is for unevaludated with `not not` case
    ;;     I think it should be invalid test case, but it's listed
    ;;     in the official test suite, so no argue.
    (for-each (lambda (v)
		(when success?
		  (set-cdr! v #t)
		  (list-queue-add-back! q v))) diff))
  (let ((slots (hashtable-ref (validator-context-marks context) obj '())))
    (for-each (lambda (slot)
		(let ((q (cdr slot)))
		  (cond ((memq (car slot) snapshot) =>
			 (lambda (base)
			   (let* ((marks (list-queue-list q))
				  (diff (drop-right marks (length base))))
			     (swap-marks! q base diff))))
			(else
			 (swap-marks! q '() (list-queue-list q))))))
	      slots)
    success?))

(define (validator-context:marks context obj)
  (define (->snapshot slot)
    ;; convert (schema (e result) ...)
    (cons (car slot) (list-queue-list (cdr slot))))
    
  (let ((mark (validator-context-marks context)))
    (map ->snapshot (hashtable-ref mark obj '()))))

(define (validator-context:marked? context obj schema-context)
  (let ((slots (hashtable-ref (validator-context-marks context) obj '())))
    (cond ((assq schema-context slots))
	  (else #f))))

(define (validator-context:marked-element? context obj element schema-context)
  (let ((slots (hashtable-ref (validator-context-marks context) obj '())))
    (cond ((assq schema-context slots) =>
	   (lambda (slot) (assoc element (list-queue-list (cdr slot)))))
	  (else #f))))

(define (validator-context:unevaluated? context obj element schema-context)
  (define (collect element elements)
    (define (check r element v) (if (equal? (car v) element) (cons v r) r))
    (do ((elements elements (cdr elements))
	 (r '() (check r element (car elements))))
	((null? elements) r)))

  (define (check slot)
    (let ((marked-context (car slot)))
      (or (not (schema-context:same-root? schema-context marked-context))
	  ;; We need to exclude cousins, so check subschema
	  (subschema? (schema-context-schema schema-context)
		      (schema-context-schema marked-context)))))
  (let ((e* (append-map (lambda (s) (list-queue-list (cdr s)))
	     (filter check
	      (hashtable-ref (validator-context-marks context) obj '())))))
    ;; because of allOf, anyOf or oneOf applicators, the elements may contain
    ;; multiple of the same element. So, collect everything and check if
    ;; there's a successful evaluation or not
    (not (null? (filter-map cdr (collect element e*))))))

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

(define (validator-context:push-schema! context id schema path)
  (let ((queue (validator-context-evaluating-schemas context)))
    (if (list-queue-empty? queue)
	(list-queue-add-front! queue (cons* id path schema))
	(let ((e (list-queue-front queue)))
	  (and (not (equal? (car e) id))
	       (list-queue-add-front! queue (cons* id path schema)))))))

(define (validator-context:pop-schema! context)
  (let ((queue (validator-context-evaluating-schemas context)))
    (list-queue-remove-front! queue)))

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
				 schema-path
				 context)))
	((boolean? schema)
	 (update-cache! context
	  (make-schema-validator (boolean->validator schema)
				 schema-path)))
	(else
	 (assertion-violation 'schema-context->schema-validator
			      "Invalid JSON Schema" schema))))

(define (schema-context->cached-validator context schema-path)
  (define cache (schema-context-cache context))
  (define (initializer)
    (schema-validator->core-validator
     (schema-context->schema-validator context schema-path)))

  (cond ((hashtable-ref cache context #f))
	(else
	 (let ((validator (schema-context:delayed-validator
			   context initializer schema-path)))
	   (hashtable-set! cache context validator)
	   validator))))

(define (schema-context:delayed-validator context initializer schema-path)
  (define validator #f)
  (define (delayed-validator) (lambda (e ctx) (validator e ctx)))
  (list-queue-add-front! (schema-context-late-inits context)
			 (lambda () (set! validator (initializer))))
  (update-cache! context
   (make-schema-validator (delayed-validator) schema-path context)))

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
  ;; The idea is that, we filter the dynamic anchors against the schema
  ;; of the scope, then check the number of the anchors. If it contains
  ;; only one anchor, that's (probably, hopefully) the scope
  (define (search-dynamic-scope anchors ctx)
    (define scopes
      (reverse!
       (filter-map car (list-queue-list
			(validator-context-evaluating-schemas ctx)))))
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
    ;; (newline)
    ;; (display dynamic-anchor) (newline)
    ;; (for-each (lambda (s) (display s) (newline))
    ;; 	      (list-queue-list (validator-context-evaluating-schemas ctx)))
    ;; (display "----") (newline)
    ;; (for-each (lambda (s) (display s) (newline))
    ;; 	      (map schema-context-schema
    ;; 	      (hashtable-ref dynamic-anchors dynamic-anchor #f)))

    (schema-validator->core-validator
     (schema-context-validator
      (schema-context ctx dynamic-anchor))))
  (handle-dynamic-validator context schema-path validator))

(define (handle-dynamic-validator context schema-path validator-provier)
  (define (dynamic-validator e ctx) ((validator-provier ctx) e ctx))
  (cond ((schema-context-validator context))
	(else
	 (update-cache! context
	  (make-schema-validator dynamic-validator schema-path context)))))

(define-record-type schema-validator
  (fields validator schema-path schema-context)
  (protocol (lambda (p)
	      (case-lambda
	       ((validator schema-path) (p validator schema-path #f))
	       ((validator schema-path schema-context)
		(p validator schema-path schema-context))))))
(define (schema-validator->core-validator schema-validator)
  (define core-validator (schema-validator-validator schema-validator))
  (define schema-path (schema-validator-schema-path schema-validator))
  (define context (schema-validator-schema-context schema-validator))
  (define id (and (schema-context? context)
		  (or (schema-context-schema-id context)
		      (schema-context-in-id context))))
  (define schema (and (schema-context? context) (schema-context-schema context)))
  (lambda (e ctx)
    (define pushed? (validator-context:push-schema! ctx id schema schema-path))
    (let ((r (or (core-validator e ctx)
		 (validator-context:report! ctx e schema-path))))
      (when pushed? (validator-context:pop-schema! ctx))
      r)))

(define (wrap-core-validator validator schema-path)
  (schema-validator->core-validator
   (make-schema-validator validator schema-path)))

;; utilities
(define (json-schema? v) (or (vector? v) (boolean? v)))
(define (subschema? root-schema schema)
  (and (subschema-depth root-schema schema) #t))
(define (subschema-depth root-schema schema)
  (define (rec root-schema schema depth)
    ;; we don't modify the schema, so `eq?` works.
    (cond ((eq? root-schema schema) depth)
	  ((vector? root-schema)
	   ;; For now DFS, might be better to do BFS
	   (let ((len (vector-length root-schema)))
	     (let loop ((i 0))
	       (cond ((= i len) #f)
		     ((rec (cdr (vector-ref root-schema i)) schema (+ depth 1)))
		     (else (loop (+ i 1)))))))
	  ((list? root-schema)
	   (exists (lambda (r) (rec r schema (+ depth 1))) root-schema))
	  (else #f)))
  (rec root-schema schema 0))

(define (build-schema-path base child)
  (string-append base "/" child))
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
