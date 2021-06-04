;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators.scm - JSON schema validators
;;;
;;;   Copyright (c) 2018-2021  Takashi Kato  <ktakashi@ymail.com>
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
    (export json-schema->json-validator
	    json-schema-validator?
	    json-schema-validator-id json-schema-validator-schema
	    json-schema-validator-source

	    json-schema:version *json-schema:version*

	    ;; simple validators
	    json-schema:type
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

	    json-schema:items
	    json-schema:max-items
	    json-schema:min-items
	    json-schema:unique-items
	    json-schema:contains

	    json-schema:max-properties
	    json-schema:min-properties
	    json-schema:required
	    json-schema:dependencies
	    json-schema:property-names
	    json-schema:properties

	    json-schema:if

	    json-schema:all-of
	    json-schema:any-of
	    json-schema:one-of
	    json-schema:not

	    json-schema:format
	    *json-schema:resolve-external-schema?*
	    *json-schema:validate-format?*
	    *json-schema:validator-error-reporter*
	    *json-schema:lint-mode?*

	    json-schema-report?
	    json-schema-report-path
	    json-schema-report-target
	    json-schema-report-parameter
	    simple-json-schema-error-reporter
	    *json-schema:report-port*
	    )
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (text json validator)
	    (text json pointer)
	    (text json parse) ;; for *json-map-type*
	    (text json convert)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius control)
	    (scheme lazy)
	    (rfc uri)
	    (rfc uri-template)
	    (rfc smtp format) ;; for smtp-valid-address?
	    (util uri)
	    (srfi :1 lists)
	    (srfi :39 parameters)
	    (srfi :117 list-queues)
	    (srfi :133 vectors))

(define-enumeration json-schema:version
  (draft-7 2019-09 2020-12)
  json-schema-version)
(define +json-schema-version-settings+
  `(
    (,(json-schema:version draft-7)
     "http://json-schema.org/draft-07/schema#"
     ,(lambda () +json-schema-draft-7-validators+)
     ("definitions"))
    
    (,(json-schema:version 2019-09)
     ;; not sure if we should use this
     "https://json-schema.org/draft/2019-09/schema"
     ,(lambda () +json-schema-draft-7-validators+)
     ("$defs" "definitions"))
    
    (,(json-schema:version 2020-12)
     ;; not sure if we should use this
     "https://json-schema.org/draft/2020-12/schema"
     ,(lambda () +json-schema-draft-7-validators+)
     ("$defs" "definitions")))
  )

(define (json-schema:version-schema version)
  (cond ((assq version +json-schema-version-settings+) => cadr)
	(else (assertion-violation 'json-schema:version-definitions
				   "Unknown version" version))))
;; damn...
(define (json-schema:version-schema-validators version)
  ((cond ((assq version +json-schema-version-settings+) => caddr)
	 (else (assertion-violation 'json-schema:version-schema-validators
				    "Unknown version" version)))))
(define (json-schema:version-definitions version)
  (cond ((assq version +json-schema-version-settings+) => cadddr)
	(else (assertion-violation 'json-schema:version-definitions
				   "Unknown version" version))))
;; default draft-7
(define *json-schema:version* (make-parameter (json-schema:version draft-7)))
(define *json-schema:resolve-external-schema?* (make-parameter #f))
;;; 7.2. Implementation Requirements
;; they SHOULD offer an option to disable validation for this
;; ('format') keyword.
(define *json-schema:validate-format?* (make-parameter #t))

;; internal parameter to handle shared structure
;; for unit test we need to have default hashtable
(define *validators* (make-parameter (make-eq-hashtable)))
(define *referencing-validators* (make-parameter (make-eq-hashtable)))
(define *context* (make-parameter #f))
(define *lazy-queue* (make-parameter #f))

;; reporting parameter
;; json pointer
(define *current-path* (make-parameter ""))
;; reporter we don't do anything by default
(define *json-schema:validator-error-reporter* (make-parameter #f))
(define *json-schema:report-port* (make-parameter #f))
(define *json-schema:lint-mode?* (make-parameter #f))

(define-condition-type &json-schema-report &condition
  make-json-schema-report json-schema-report?
  (path json-schema-report-path)
  (target json-schema-report-target)
  (parameter json-schema-report-parameter))

(define (report obj parameter)
  (raise-continuable
   (make-json-schema-report (*current-path*) obj parameter)))
(define (build-pointer base next)
  (string-append base "/" (if (number? next) (number->string next) next)))

(define (simple-json-schema-error-reporter e)
  (define out (or (*json-schema:report-port*) (current-error-port)))
  (display (json-schema-report-path e) out) (newline out)
  (display "\tobject: " out) (write (json-schema-report-target e) out)
  (newline out)
  (let ((parameter (json-schema-report-parameter e)))
    (display "\t" out)
    (display (car parameter) out) (display ": " out)
    (display (cadr parameter) out) (newline out)))
      
(define (reporting-validator validator)
  (lambda (e)
    (let ((reporter (*json-schema:validator-error-reporter*)))
      (with-exception-handler
       (lambda (e)
	 (unless (json-schema-report? e) (raise e))
	 (and reporter (reporter e) (*json-schema:lint-mode?*)))
       (lambda () (validator e))))))
(define-record-type json-schema-validator
  (parent <json-validator>)
  (fields source ;; Raw sexp JSON (vector)
	  schema ;; $schema
	  id	 ;; $id
	  )
  (protocol (lambda (n)
	      (lambda (validator source schema id)
		((n (reporting-validator validator)) source schema id)))))

(define (key=? key) (lambda (e) (and (pair? e) (string=? (car e) key) e)))
(define value-of
  (case-lambda
   ((key schema) (value-of key schema #f))
   ((key schema default)
    (cond ((vector-any (key=? key) schema) => cdr)
	  (else default)))))

(define (json-schema->json-validator schema . referencing-validators)
  (define default-schema
    (json-schema:version-schema (*json-schema:version*)))
  (define (convert schema)
  (let ((r (alist-json->vector-json schema)))
    (unless (vector? r)
      (assertion-violation 'json-schema->json-validator
			   "JSON object is required" schema))
    r))
  (define (->validator-map)
    (unless (for-all json-schema-validator? referencing-validators)
      (assertion-violation 'json-schema->json-validator
       "referencing-validators must be a list of json-schema-validators"
       referencing-validators))
    (let ((ht (make-hashtable equal-hash equal?)))
      (for-each (lambda (v)
		  (hashtable-set! ht (json-schema-validator-id v)
				  (json-validator-validator v)))
		referencing-validators)
      ht))

  (cond ((vector? schema)
	 (parameterize ((*validators* (make-eq-hashtable))
			(*referencing-validators* (->validator-map))
			(*lazy-queue* (list-queue))
			(*context* #f))
	   (let (($schema (value-of "$schema" schema default-schema))
		 ($id (value-of "$id" schema))
		 (v (schema->validator 'json-schema->json-validator schema)))
	     (for-each force (list-queue-list (*lazy-queue*)))
	     (make-json-schema-validator v schema $schema $id))))
	((boolean? schema)
	 (make-json-schema-validator (boolean->validator schema)
				     schema default-schema #f))
	(else (json-schema->json-validator (convert schema)))))

;;; internal
(define-record-type context
  (fields schema
	  id
	  ids
	  ref-cache
	  schema-parents
	  parent)
  (protocol (lambda (p)
	      (define (collect c)
		(collect-ids c)
		c)
	      (lambda (schema parent)
		(collect (p schema
			    (and (vector? schema) (value-of "$id" schema))
			    (or (and parent (context-ids parent))
				(make-hashtable string-hash string=?))
			    (or (and parent (context-ref-cache parent))
				(make-hashtable string-hash string=?))
			    (or (and parent (context-schema-parents parent))
				(make-eq-hashtable))
			    parent))))))
(define (root-context context)
  (let loop ((c context))
    (if (not (context-parent c))
	c
	(loop (context-parent c)))))

;; this is for sure only called the schema is an object
(define (parse-uri id)
  (let*-values (((scheme specific) (uri-scheme&specific id))
		((auth path query frag)
		 (uri-decompose-hierarchical specific)))
    ;; decode fragment for relative JSON pointer.
    (values scheme auth path query (and frag (uri-decode-string frag)))))

(define (collect-ids context)
  (define root-schema (context-schema context))
  (define root-id (context-id context))
  ;; TODO $defs is the latest... fuck
  (define $defs (json-schema:version-definitions (*json-schema:version*)))
  (define schema-validators
    (json-schema:version-schema-validators (*json-schema:version*)))
  (define ids (context-ids context))
  (define schema-parents (context-schema-parents context))
  (define (check-id schema root-id ids)
    (let (($id (value-of "$id" schema)))
      (if (and $id (not (zero? (string-length $id))) (not (equal? "#" $id)))
	  (let-values (((scheme auth path q frag) (parse-uri $id)))
	    (hashtable-set! ids $id schema)
	    (cond (scheme
		   ;; switch root-id
		   (uri-compose :scheme scheme :authority auth
				:path path :query q))
		  (path
		   (let-values (((rscheme rauth rpath rq rfrag)
				 (parse-uri root-id)))
		     (let ((id (uri-compose :scheme rscheme :authority rauth
					    :path path :query rq)))
		       (hashtable-set! ids id schema)
		       id)))
		  (frag
		   (let ((id (string-append (or root-id "") $id)))
		     (hashtable-set! ids id schema)
		     root-id))
		  ;; somethng is wrong but proceed
		  (else root-id)))
	  root-id)))
  
  (define (check-$defs schema root-id ids)
    (let ((defs (map (lambda (d) (value-of d schema)) $defs)))
      (for-each (lambda (def)
		  (when (vector? def)
		    (vector-for-each
		     (lambda (e)
		       (when (vector? (cdr e))
			 (hashtable-set! schema-parents (cdr e) schema)
			 (let ((id (check-id (cdr e) root-id ids)))
			   (check-$defs (cdr e) id ids)))) def))) defs)))

  (when (vector? root-schema)
    (let ((base-id (check-id root-schema root-id ids)))
      (check-$defs root-schema base-id ids))))
				
(define (->json-validator schema)
  (define schema-validators
    (json-schema:version-schema-validators (*json-schema:version*)))
  (define (generate-validator schema)
    (define ignore (make-hashtable string-hash string=?))
    (vector-fold
     (lambda (combined-validator e)
       ;; TODO consider schema version
       (cond ((and (not (hashtable-contains? ignore (car e)))
		   (assoc (car e) schema-validators)) =>
	      (lambda (slot)
		(cond ((cadr slot) =>
		       (lambda (g)
			 ;; some of the validators are pretty much
			 ;; associated if this is such a propety, then
			 ;; it should have the ignore list behind the
			 ;; slot, so add them.
			 (for-each (lambda (i) (hashtable-set! ignore i #t))
				   (cddr slot))
			 (let ((validator (g schema (cdr e))))
			   (lambda (e)
			     (and (combined-validator e) (validator e))))))
		      ;; for unknown property we ignore
		      (else combined-validator))))
	     (else combined-validator)))
     (lambda (e) #t) schema))
  
  (define validators (*validators*))
  (cond ((hashtable-ref validators schema #f))
	(else
	 (hashtable-set! validators schema
			 (lambda (e)
			   ((hashtable-ref validators schema #f) e)))
	 (let ((validator (generate-validator schema)))
	   (hashtable-set! validators schema validator)
	   validator))))

(define (schema? v) (or (boolean? v) (vector? v)))
(define (schema->validator who schema)
  (parameterize ((*context* (make-context schema (*context*))))
    (cond ((boolean? schema) (boolean->validator schema))
	  ((vector? schema)
	   (or ($ref->validator who schema) (->json-validator schema)))
	  (else
	   (assertion-violation who "JSON schema is required" schema)))))

(define ($ref->validator who schema)
  (define (handle-$ref $ref)
    (define context (*context*))
    (define ids (context-ids context))
    (define ref-cache (context-ref-cache context))
    (define (->validator schema) (schema->validator who schema))
    
    (define (retrieve-from-uri uri frag)
      (define (read-from-http uri)
	(call-with-port (transcoded-port (open-uri uri)
					 (native-transcoder))
			json-read))
      (guard (e (else #f))
	(let ((schema (cond ((hashtable-ref ref-cache uri #f))
			    (else
			     (let ((s (read-from-http uri)))
			       (hashtable-set! ref-cache uri s)
			       s)))))
	  (if frag
	      (parameterize ((*context* (make-context schema #f)))
		(handle-$ref (string-append "#" frag)))
	      ;; TODO referencing validator?
	      (let ((v (json-schema->json-validator schema)))
		(lambda (e) (validate-json v e)))))))
    
    (define (search-parent-schema child)
      (define schema-parents (context-schema-parents context))
      (cond ((hashtable-ref schema-parents child #f) =>
	     ;; okay we know this one already
	     (lambda (schema)
	       (if (value-of "$id" schema)
		   schema
		   (search-parent-schema schema))))
	    (else
	     (let loop ((context context))
	       (cond ((not (context-parent context)) (context-schema context))
		     ((context-id context) (context-schema context))
		     (else (loop (context-parent context))))))))
    
    (define (search-parent-id child)
      (define schema-parents (context-schema-parents child))
      (define (handle-context context)
	(let loop ((context context) (r ""))
	  (cond ((not context) r)
		((context-id context) =>
		 (lambda (id)
		   (let-values (((scheme auth path q frag) (parse-uri id)))
		     (cond (scheme (uri-compose :scheme scheme
						:authority auth
						:path r
						:query q))
			   (path (loop (context-parent context)
				       (uri-merge r path)))
			   ;; should we?
			   (else id)))))
		(else (loop (context-parent context) r)))))
      (define (handle-known schema)
	(let loop ((schema schema) (r ""))
	  (if schema
	      (let ((id (value-of "$id" schema)))
		(if id
		    (let-values (((scheme auth path query frag) (parse-uri id)))
		      (cond (scheme
			     (uri-compose :scheme scheme
					  :authority auth
					  :path r
					  :query query))
			    (path (loop (hashtable-ref schema-parents schema #f)
					(uri-merge r path)))
			    (else id)))
		    (loop (hashtable-ref schema-parents schema #f) r)))
	      r)))
      ;; parent of the child = e.g. #(("$ref" . "boo")) might be
      ;; in the schema-parents
      (let ((parent-schema (cond ((context-parent child) => context-schema)
				 (else #f))))
	(if (hashtable-contains? schema-parents parent-schema)
	    (handle-known parent-schema)
	    (handle-context child))))
    ;; Handling $id
    ;; ref:
    ;; https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-01
    ;; section 8.2.4
    (define (handle-ref $ref)
      (let-values (((scheme auth path query frag) (parse-uri $ref)))
	(cond (scheme
	       (and (*json-schema:resolve-external-schema?*)
		    (retrieve-from-uri (uri-compose :scheme scheme
						    :authority auth
						    :path path
						    :query query)
				       frag)))
	      
	      (path
	       ;; this is wrong in case of under different id's
	       ;; definition
	       (let ((root-id (search-parent-id context)))
		 (let-values (((scheme auth p q f)
			       (parse-uri root-id)))
		   (let ((id (uri-compose :scheme scheme
					  :authority auth
					  :path (or (and p (uri-merge p path))
						    path)
					  :query query)))
		     (cond ((hashtable-ref ids id #f) => ->validator)
			   ((and (*json-schema:resolve-external-schema?*)
				 (retrieve-from-uri id frag)))
			   (else #f))))))
	      (frag
	       ;; must be a valid json pointer, otherwise an ID (or anchor)
	       (and (or (zero? (string-length frag))
			(eqv? #\/ (string-ref frag 0)))
		    (let* ((parent (search-parent-schema schema))
			   (s ((json-pointer frag) parent)))
		      (and (not (json-pointer-not-found? s))
			   (->validator s)))))
	      (else #f))))
    
    (cond ((hashtable-ref ids $ref #f) => ->validator)
	  ((handle-ref $ref))
	  (else
	   ;; suppose this is a reference used before the definition
	   ;; make it lazy evaluated
	   (let ((v (delay
		      (cond ((hashtable-ref ids $ref #f) => ->validator)
			    (else (lambda (e) #t))))))
	     (list-queue-add-front! (*lazy-queue*) v)
	     (lambda (e) ((force v) e))))))
  (cond ((and (vector? schema) (value-of "$ref" schema)) => handle-$ref)
	(else #f)))

;; utilities
(define unique?
  (case-lambda
   ((v) (unique? v json=?))
   ((v =) (equal? v (delete-duplicates v =)))))
(define (json=? a b)
  (define (entry=? a b)
    (and (json=? (car a) (car b))
	 (json=? (cdr a) (cdr b))))
  (define (key-compare a b) (string<? (car a) (car b)))
  (cond ((and (string? a) (string? b)) (string=? a b))
	((and (number? a) (number? b)) (= a b))
	((and (vector? a) (vector? b))
	 (vector-every entry=?
		       (vector-sort key-compare a)
		       (vector-sort key-compare b)))
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
  (define (predicate-of type)
    (cond ((string=? "string" type) string?)
	  ((string=? "integer" type) integer?)
	  ((string=? "number" type) real?)	;; TODO exclude rational?)
	  ;; we use vector json for validation
	  ;; TODO should we check content?
	  ((string=? "object" type) vector?)
	  ((string=? "array" type) list?)
	  ((string=? "boolean" type) boolean?)
	  ((string=? "null" type) (lambda (e) (eq? e 'null)))
	  (else (assertion-violation 'json-schema:type "Unknown type" type))))
  (define (wrap-report v)
    (lambda (e) (or (v e) (report e `(type, type)))))
  (cond ((list? type)
	 (check type)
	 (wrap-report
	  (fold-left 
	   (lambda (acc t)
	     (let ((v (predicate-of t)))
	       (lambda (e) (or (v e) (acc e))))) (lambda (e) #f) type)))
	((string? type) (wrap-report (predicate-of type)))
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
  (lambda (e)
    (or (exists (lambda (v) (json=? e v)) vals)
	(report e `(enum ,vals)))))

;; 6.1.3 const
(define (json-schema:const v)
  (lambda (e) (or (json=? e v) (report e `(const v)))))

;;; 6.2. Validation Keywords for Numeric Instances (number and integer)
;; 6.2.1 multipleOf
(define (json-schema:multiple-of v)
  (define (->integer e v)
    (if (not (or (integer? e) (integer? v)))
	(->integer (* e 10) (* v 10))
	(values e v)))
  (unless (and (real? v) (positive? v))
    (assertion-violation 'json-schema:multiple-of
			 "MultipleOf must be a number greater than 0" v))
  (lambda (e)
    (or (and (real? e)
	     (let-values (((e v) (->integer e v)))
	       (zero? (mod e v))))
	(report e `(multiple-of ,v)))))

(define (min/max who compare)
  (define name (symbol->string who))
  (define err-who (string->symbol (string-append "json-schema:" name)))
  (define err-msg (string-append (string-titlecase name) " must be a number"))
  (lambda (v)
    (unless (real? v) (assertion-violation err-who err-msg v))
    (lambda (e)
      (or (and (real? e) (compare e v)) (report e `(,who ,v))))))
;; 6.2.2. maximum
(define json-schema:maximum (min/max 'maximum <=))
;; 6.2.3. exclusiveMaximum
(define json-schema:exclusive-maximum (min/max 'exclusive-maximum <))
;; 6.2.4. minimum
(define json-schema:minimum (min/max 'minimum >=))
;; 6.2.5. exclusiveMinimum
(define json-schema:exclusive-minimum (min/max 'exclusive-minimum >))

;;; 6.3. Validation Keywords for Strings
;; 6.3.1. maxLength
(define (json-schema:max-length v)
  (when (or (not (integer? v)) (negative? v))
    (assertion-violation 'json-schema:max-length
			 "maxLength must be a non negative integer" v))
  (lambda (e)
    (or (and (string? e) (<= (string-length e) v))
	(report e `(max-length ,v)))))
;; 6.3.2. minLength
(define (json-schema:min-length v)
  (when (or (not (integer? v)) (negative? v))
    (assertion-violation 'json-schema:min-length
			 "minLength must be a non negative integer" v))
  (lambda (e)
    (or (and (string? e) (<= v (string-length e)))
	(report e `(min-length ,v)))))
;; 6.3.3. pattern
(define (json-schema:pattern p)
  (unless (string? p)
    (assertion-violation 'json-schema:pattern "pattern must be a string" p))
  (guard (e (else
	     (assertion-violation 'json-schema:pattern
				  (if (message-condition? e)
				      (condition-message e)
				      "Invalid regex pattern") p)))
    (let ((rx (regex p)))
      (lambda (e)
	(or (and (string? e) (looking-at rx e) #t)
	    (report e `(pattern ,p)))))))

;;; 6.4. Validation Keywords for Arrays
;; 6.4.1. items
;; 6.4.2. additionalItems
(define (json-schema:items schema items)
  (define (->validator schema) (schema->validator 'json-schema:items schema))
  (define (get-additional-validator schema)
    (if (vector? schema)
	(->validator (value-of "additionalItems" schema #t))
	(boolean->validator #t)))
  (define (validate path i validator e)
    (parameterize ((*current-path* (build-pointer path i)))
      ;; TODO should we also add definition of items, etc. for report here?
      (validator e)))
  (define (validate-elements path validator i e*)
    (let loop ((i i) (e* e*))
      (cond ((null? e*))
	    ((validate path i validator (car e*)) (loop (+ i 1) (cdr e*)))
	    (else #f))))
  (cond ((boolean? items)
	 (lambda (e*) (or (null? e*) items (report e* `(items ,items)))))
	((and (list? items) (for-all schema? items))
	 (let ((additional-validator (get-additional-validator schema))
	       (validators (map (lambda (i) (->validator i)) items)))
	   (lambda (e*)
	     (define current-path (*current-path*))
	     (let loop ((i 0) (e* e*) (validators validators))
	       (cond ((null? e*))
		     ((null? validators)
		      (validate-elements current-path
					 additional-validator i e*))
		     (else
		      (and (validate current-path i (car validators) (car e*))
			   (loop (+ i 1) (cdr e*) (cdr validators)))))))))
	((vector? items)
	 (let ((validator (->validator items)))
	   (lambda (e*)
	     (validate-elements (*current-path*) validator 0 e*))))
	(else
	 (assertion-violation 'json-schema:items
	  "Items must be a JSON schema or array of JSON schema" items))))
;; 6.4.3. maxItems
(define (json-schema:max-items n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:max-items
			 "maxItems must be a non negative integer" n))
  (lambda (e)
    (or (and (list? e) (<= (length e) n)) (report e `(max-items ,n)))))
;; 6.4.4. minItems
(define (json-schema:min-items n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:max-items
			 "minItems must be a non negative integer" n))
  (lambda (e)
    (or (and (list? e) (<= n (length e))) (report e `(min-items ,n)))))
;; 6.4.5. uniqueItems
(define (json-schema:unique-items b)
  (unless (boolean? b)
    (assertion-violation 'json-schema:unique-items
			 "uniqueItems must be a boolean" b))
  (if b
      (lambda (e)
	(or (and (list? e) (unique? e)) (report e `(unique-items ,b))))
      (lambda (e) #t)))
;; 6.4.6. contains
(define (json-schema:contains value)
  (let ((validator (schema->validator 'json-schema:contains value)))
    (lambda (e) (or (exists validator e) (report e `(contains ,value))))))

;;; 6.5. Validation Keywords for Objects
;; 6.5.1. maxProperties
(define (json-schema:max-properties n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:max-properties
			 "maxProperties must be a non negative integer" n))
  (lambda (e)
    (or (and (vector? e) (<= (vector-length e) n))
	(report e `(max-properties ,n)))))
;; 6.5.2. minProperties
(define (json-schema:min-properties n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:min-properties
			 "minProperties must be a non negative integer" n))
  (lambda (e)
    (or (and (vector? e) (<= n (vector-length e)))
	(report e `(min-properties ,n)))))
;; 6.5.3. required
(define (json-schema:required e*)
  (unless (and (list? e*) (for-all string? e*) (unique? e*))
    (assertion-violation 'json-schema:required
			 "Required must be an array of unique strings" e*))
  (lambda (e)
    (or (and (vector? e)
	     ;; TODO inefficient
	     (for-all (lambda (k)
			(vector-any
			 (lambda (v)
			   ;; not sure if we need to handle invalid JSON
			   ;; structure...
			   (and (pair? v) (string? (car v)) 
				(string=? (car v) k)))
			 e))
		      e*))
	(report e `(required ,e*)))))
  
;; 6.5.4. properties
;; 6.5.5. patternProperties
;; 6.5.6. additionalProperties
;; we handle properties and patternProperties simultaneously this may
;; results the same validator twice, but it's okay since both
;; validator returns the same result (only performance penalty)
(define (json-schema:properties schema value)
  ;; we don't know which one it is so retrieve it again
  (define properties (value-of "properties" schema (eof-object)))
  (define pattern-properties (value-of "patternProperties" schema (eof-object)))
  (define additional-properties (value-of "additionalProperties" schema #t))

  (define (->validator schema)
    (schema->validator 'json-schema:properties schema))
  (define (validate path validator key value)
    (parameterize ((*current-path* (build-pointer path key)))
      ;; TODO should we also add definition of properties, etc for report here?
      (validator value)))
  (define (object->validator obj regex?)
    (define (check vec key=? validator)
      (define len (vector-length vec))
      (define path (*current-path*))
      (let loop ((i 0) (found? #f) (ok? #t))
	(cond ((= i len) (or (not found?) ok?))
	      ((key=? (vector-ref vec i)) =>
	       (lambda (k&v)
		 (loop (+ i 1) #t
		       (and ok? (validate path validator
					  (car k&v) (cdr k&v))))))
	      (else (loop (+ i 1) found? ok?)))))
    (define (->key=? reg)
      (define rx (regex reg))
      (lambda (e) (and (looking-at rx (car e)) e)))
    (cond ((eof-object? obj)
	   (values '() (boolean->validator #t))) ;; always #t
	  ((boolean? obj)
	   (values '() (boolean->validator obj)))
	  (else
	   (let ((k=?&v (vector-map
			 (lambda (e)
			   (cons (if regex? (->key=? (car e)) (key=? (car e)))
				 (cdr e))) obj)))
	     (values
	      (vector-fold (lambda (acc e) (cons (car e) acc)) '() k=?&v)
	      (vector-fold (lambda (combined k&v)
			     (let ((validator (->validator (cdr k&v)))
				   (key=? (car k&v)))
			       (lambda (e)
				 ;; first this, to check vector or not
				 (and (combined e)
				      (check e key=? validator)))))
			   (lambda (e) (vector? e))
			   k=?&v))))))
  (define (wrap props pprops validator)
    (define pred
      (lambda (e)
	(and (not (exists (lambda (key=?) (key=? e)) props))
	     (not (exists (lambda (key=?) (key=? e)) pprops))
	     e)))
    (lambda (e)
      (define len (vector-length e))
      (define path (*current-path*))
      (let loop ((i 0) (ok? #t) (found? #f))
	(cond ((= i len) (or (not found?) ok?))
	      ((pred (vector-ref e i)) =>
	       (lambda (v)
		 (loop (+ i 1)
		       (and ok? (validate path validator (car v) (cdr v))) #t)))
	      (else (loop (+ i 1) ok? found?))))))

  (let-values (((props validator) (object->validator properties #f))
	       ((pprops pvalidator) (object->validator pattern-properties #t)))
    (let ((additional-validator (wrap props pprops
				      (->validator additional-properties))))
      (lambda (e)
	(and (vector? e)
	     (validator e)
	     (pvalidator e)
	     (additional-validator e))))))
;; 6.5.7. dependencies
(define (json-schema:dependencies v)
  (define (handle-array v)
    ;; for debug purpose, we check value here as well...
    (unless (and (for-all string? v)
		 (unique? v string=?))
      (assertion-violation 'json-schema:dependencies
			   "Array of dependencies must be a unique string array"
			   v))
    (json-schema:required v))
  (define (handle-dependency d)
    (if (list? d)
	(handle-array d)
	(schema->validator 'json-schema:dependencies d)))
  (unless (vector? v)
    (assertion-violation 'json-schema:dependencies
			 "Dependencies must be an object" v))
  (vector-fold (lambda (combined e)
		 (let ((prop (car e))
		       (dependency (handle-dependency (cdr e))))
		   (lambda (e)
		     (let ((v (value-of prop e (eof-object))))
		       (and (combined e)
			    (or (eof-object? v)
				(dependency e)))))))
	       (boolean->validator #t) v))
;; 6.5.8. propertyNames
(define (json-schema:property-names v)
  (unless (or (boolean? v) (vector? v))
    (assertion-violation 'json-schema:propperty-names
			 "PropertyNames must be a JSON schema"))
  (let ((validator (schema->validator 'json-schema:propperty-names v)))
    (lambda (e)
      (define path (*current-path*))
      (define (validate path validator k)
	(parameterize ((*current-path* (build-pointer path k)))
	  (or (validator k)
	      (report k `(property-names ,v)))))
      (vector-every (lambda (k&v)
		      ;; the key must always be a string, otherwise it's an
		      ;; invalid JSON but we don't check that 
		      (or (not (string? (car k&v)))
			  (validate path validator (car k&v)))) e))))

(define-syntax no-report
  (syntax-rules ()
    ((_ expr)
     (with-exception-handler
      (lambda (e)
	(unless (json-schema-report? e) (raise e))
	#f)
      (lambda () expr)))))
;;; 6.6. Keywords for Applying Subschemas Conditionally
(define (json-schema:if schema v)
  (define $then (schema->validator 'json-schema:if (value-of "then" schema #t)))
  (define $else (schema->validator 'json-schema:if (value-of "else" schema #t)))
  (define $if (schema->validator 'json-schema:if v))
  (lambda (e)
    (if (no-report ($if e))
	($then e)
	($else e))))

;;; 6.7. Keywords for Applying Subschemas With Boolean Logic
;; 6.7.1. allOf
(define (json-schema:all-of v)
  (define (->validator v) (schema->validator 'json-schema:all-of v))
  (unless (and (list? v) (not (null? v)) (for-all schema? v))
    (assertion-violation 'json-schema:all-of
			 "AllOf must be non empty array of JSON schema" v))
  (let ((validators (map ->validator v)))
    (lambda (e)
      (or (no-report (for-all (lambda (v) (v e)) validators))
	  (report e `(all-of ,v))))))
;; 6.7.2. anyOf
(define (json-schema:any-of v)
  (define (->validator v) (schema->validator 'json-schema:any-of v))
  (unless (and (list? v) (not (null? v)) (for-all schema? v))
    (assertion-violation 'json-schema:any-of
			 "AnyOf must be non empty array of JSON schema" v))
  (let ((validators (map ->validator v)))
    (lambda (e)
      (or (no-report (exists (lambda (v) (v e)) validators))
	  (report e `(any-of ,v))))))
;; 6.7.3. oneOf
(define (json-schema:one-of v)
  (define (->validator v) (schema->validator 'json-schema:one-of v))
  (define (check validators e)
    (no-report (fold-left (lambda (n v) (if (v e) (+ n 1) n)) 0 validators)))
  (unless (and (list? v) (not (null? v)) (for-all schema? v))
    (assertion-violation 'json-schema:any-of
			 "OneOf must be non empty array of JSON schema" v))
  (let ((validators (map ->validator v)))
    (lambda (e)
      (or (= 1 (check validators e)) (report e `(one-of ,v))))))
;; 6.7.4. not
(define (json-schema:not v)
  (let ((validator (schema->validator 'json-schema:not v)))
    (lambda (e)
      (or (not (no-report (validator e)))
	  (report e `(not ,v))))))

;;; 7. Semantic Validation With "format"
(define (json-schema:format v)
  (cond ((assoc v +json-schema-defined-formats+) =>
	 (lambda (slot)
	   (let ((validator (cdr slot)))
	     (lambda (e)
	       (if (*json-schema:validate-format?*)
		   (and (validator e) #t)
		   #t)))))
	;; not supported, so ignore
	(else (boolean->validator #t))))

;; NOTE: for date-time related, we only check format not validity
(define date-pattern "\\d{4}-\\d{2}-\\d{2}")
(define time-pattern
  "\\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?(?:Z|(?:\\+|-)\\d{2}:\\d{2})")
(define json-schema:format-date
  (json-schema:pattern (string-append "^" date-pattern "$")))
(define json-schema:format-time
  (json-schema:pattern (string-append "^" time-pattern "$")))
(define json-schema:format-date-time
  (json-schema:pattern (string-append "^" date-pattern "T" time-pattern "$")))

(define (json-schema:format-email e) (smtp-valid-address? e))
;; lazy...
(define json-schema:format-idn-email (json-schema:pattern "[^@]+@[^@]+"))

;; hostname
(define json-schema:format-hostname
  (json-schema:pattern "^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]*[a-zA-Z0-9])\\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\\-]*[A-Za-z0-9])$"))
(define json-schema:format-idn-hostname
  (json-schema:pattern "^(([^\\.-]|[^\\.-][^\\.]*[^\\.\\-])\\.)*([^\\.\\-]|[^\\.\\-][^\\.]*[^\\.\\-])$"))

;; IP address
(define json-schema:format-ipv4
  (json-schema:pattern "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"))
;; from https://stackoverflow.com/a/17871737/4377398
(define json-schema:format-ipv6
  (json-schema:pattern "(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))"))

;; we don't check if the scheme requires authority or not
(define (json-schema:format-uri e)
  (let-values (((scheme specific) (uri-scheme&specific e)))
    (and scheme specific #t)))
(define (json-schema:format-uri-reference e) #t)
(define (json-schema:format-uri-template e)
  (guard (e (else #f))
    (pair? (parse-uri-template e))))

(define (json-schema:format-json-pointer v)
  (guard (e (else #f)) (json-pointer v)))
(define (json-schema:format-relative-json-pointer v)
  (and (string->number (string (string-ref v 0)))
       (guard (e (else #f)) (json-pointer v))))
(define (json-schema:format-regex v)
  (guard (e (else  #f)) (regex v)))

(define +json-schema-defined-formats+
  `(
    ("date" . ,json-schema:format-date)
    ("time" . ,json-schema:format-time)
    ("date-time" . ,json-schema:format-date-time)
    ("email" . ,json-schema:format-email)
    ("idn-email" . ,json-schema:format-idn-email)
    ("hostname" . ,json-schema:format-hostname)
    ("idn-hostname" . ,json-schema:format-idn-hostname)
    ("ipv4" . ,json-schema:format-ipv4)
    ("ipv6" . ,json-schema:format-ipv6)
    ("uri" . ,json-schema:format-uri)
    ("uri-reference" . ,json-schema:format-uri-reference)
    ("iri" . ,json-schema:format-uri)
    ("iri-reference" . ,json-schema:format-uri-reference)
    ("uri-template" . ,json-schema:format-uri-template)
    ("json-pointer" . ,json-schema:format-json-pointer)
    ("relative-json-pointer" . ,json-schema:format-relative-json-pointer)
    ("regex" . ,json-schema:format-regex)
    ))

(define (boolean->validator b) (if b (lambda (_) #t) (lambda (_) #f)))

(define (type-wrap type? validator)
  (lambda (e) (or (not (type? e)) (validator e))))
;; simple wrap 
(define (s/w simple-validator-generator)
  (lambda (schema v)
    (simple-validator-generator v)))
;; type check
(define (t/w type? simple-validator-generator)
  (lambda (schema v)
    (type-wrap type? (simple-validator-generator v))))
;; JSON schema of true/false is {} or { "not": {} }
(define (a/w type? schema-validator-generator)
  (lambda (schema v)
    (type-wrap type? 
	       (if (boolean? v)
		   (boolean->validator v)
		   (schema-validator-generator schema v)))))
(define (b/w type? schema-validator-generator)
  (lambda (schema v)
    (type-wrap type? (schema-validator-generator schema v))))

(define +json-schema-any-instance-validators+
  `(
    ("type" ,(s/w json-schema:type))
    ("enum" ,(s/w json-schema:enum))
    ("const" ,(s/w json-schema:const))
    ))
(define +json-schema-numeric-instance-validators+
  `(
    ("multipleOf" ,(t/w real? json-schema:multiple-of))
    ("maximum" ,(t/w real?  json-schema:maximum))
    ("exclusiveMaximum" ,(t/w real? json-schema:exclusive-maximum))
    ("minimum" ,(t/w real?  json-schema:minimum))
    ("exclusiveMinimum" ,(t/w real? json-schema:exclusive-minimum))
    ))
(define +json-schema-string-validators+
  `(
    ("maxLength" ,(t/w string? json-schema:max-length))
    ("minLength" ,(t/w string? json-schema:min-length))
    ("pattern" ,(t/w string? json-schema:pattern))
    ;; well it's string validation anyway...
    ("format" ,(t/w string? json-schema:format))
    ))
(define +json-schema-array-validators+
  `(
    ("items" ,(b/w list? json-schema:items))
    ("additionalItems" #f) ;; this is handled by items
    ("maxItems" ,(t/w list? json-schema:max-items))
    ("minItems" ,(t/w list? json-schema:min-items))
    ("uniqueItems" ,(t/w list? json-schema:unique-items))
    ("contains" ,(t/w list? json-schema:contains))
    ))
(define +json-schema-object-validators+
  `(
    ("maxProperties" ,(t/w vector? json-schema:max-properties))
    ("minProperties" ,(t/w vector? json-schema:min-properties))
    ("required" ,(t/w vector? json-schema:required))
    ("properties" ,(a/w vector? json-schema:properties)
     "patternProperties" "additionalProperties")
    ("patternProperties" ,(a/w vector? json-schema:properties)
     "properties" "additionalProperties")
    ("additionalProperties" ,(a/w vector? json-schema:properties)
     "properties" "patternProperties")
    ("dependencies" ,(t/w vector? json-schema:dependencies))
    ("propertyNames" ,(t/w vector? json-schema:property-names))
    ))
(define +json-schema-conditional-validators+
  `(
    ("if" ,json-schema:if)
    ))
(define +json-schema-boolean-logic-validators+
  `(
    ("allOf" ,(s/w json-schema:all-of))
    ("anyOf" ,(s/w json-schema:any-of))
    ("oneOf" ,(s/w json-schema:one-of))
    ("not" ,(s/w json-schema:not))
    ))
(define +json-schema-draft-7-validators+
  `(
    ,@+json-schema-any-instance-validators+
    ,@+json-schema-numeric-instance-validators+
    ,@+json-schema-string-validators+
    ,@+json-schema-array-validators+
    ,@+json-schema-object-validators+
    ,@+json-schema-conditional-validators+
    ,@+json-schema-boolean-logic-validators+
    ))
)
