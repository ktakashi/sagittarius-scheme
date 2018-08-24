;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators.scm - JSON schema validators
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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

	    ;; for testing
	    resolve-$ref
	    )
    (import (rnrs)
	    (text json validator)
	    (text json pointer)
	    (text json parse) ;; for *json-map-type*
	    (text json convert)
	    (sagittarius regex)
	    (sagittarius control)
	    (rfc uri)
	    (srfi :1 lists)
	    (srfi :133 vectors))

(define-record-type json-schema-validator
  (parent <json-validator>)
  (fields source ;; RAW sexp JSON (vector)
	  schema ;; $schema
	  id	 ;; $id
	  ))

(define (key=? key) (lambda (e) (and (pair? e) (string=? (car e) key) e)))
(define value-of
  (case-lambda
   ((key schema) (value-of key schema #f))
   ((key schema default)
    (cond ((vector-any (key=? key) schema) => cdr)
	  (else default)))))

(define +json-schema-uri+  "http://json-schema.org/schema#")
(define (json-schema->json-validator schema)
  (define (convert schema)
    (let ((r (alist-json->vector-json schema)))
      (unless (vector? r)
	(assertion-violation 'json-schema->json-validator
			     "JSON object is required" schema))
      r))
  (if (vector? schema)
      (let* (($schema (value-of "$schema" schema +json-schema-uri+))
	     ($id (value-of "$id" schema))
	     (resolved-schema (resolve-$ref $id schema)))
	(make-json-schema-validator (->json-validator resolved-schema)
				    schema $schema $id))
      (json-schema->json-validator (convert schema))))

;; internal
;; The $ref resolution takes the following 2 passes:
;;   1. collect and merge all $id's
;;   2. resolve $ref (incl. removing other properties)
;; The first pass collects and stores the absolute id of the target
;; JSON object which contains the id.
;; The second pass resolves the '$ref's as either JSON pointer or
;; absolute id.
;;
;; NOTE: 
;; The specification is rather vague for $id and $ref, especially
;; merging URI. For example, 2 of the online validator behave
;; differently. We only merge URI to root id, iff it has hostname, and
;; if there's an $id with hostname, we treat it as if it's just an
;; absolute URI.
;;
;; NOTE 2:
;; An absolute URI in this case is an URI containing scheme.
;;
;; FIXME: This followes the tree twice 
(define (resolve-$ref root-id schema)
  (define (parse-id id)
    (if id
	(let*-values (((scheme specific) (uri-scheme&specific id))
		      ((auth path query frag)
		       (uri-decompose-hierarchical specific)))
	  (values scheme auth path query frag))
	(values #f #f #f #f #f)))
  (define-values (root-scheme root-auth root-path root-query root-frag)
    (parse-id root-id))
  (define current-path root-path)
  ;; absolute id storage
  (define ids (make-hashtable string-hash string=?))
  (define (merge-id id)
    (let-values (((scheme auth path query frag) (parse-id id)))
      (if (or scheme (not root-scheme))
	  (if frag id (string-append id "#"))
	  (uri-compose :scheme root-scheme :authority root-auth
		       :path (or path current-path)
		       :query query :fragment frag))))
  (define (collect-ids object)
    (define (collect-id parent-object id)
      (unless (string? id)
	(assertion-violation 'json-schema->json-validator
			     "$id must be a string" id))
      (when (or (string=? "" id) (string=? "#" id))
	(assertion-violation 'json-schema->json-validator
			     "$id should not be an empty string or '#'" id))
      ;; root-id might be #f so use equal? instead of string=?
      (unless (equal? id root-id)
	(hashtable-set! ids (merge-id id) parent-object)
	;; FIXME parsing twice...
	(let-values (((scheme auth path query frag) (parse-id id)))
	  ;; if we have path, then this object belongs to the path
	  (when path (set! current-path path)))))
    (let (($id (value-of "$id" object)))
      (when $id (collect-id object $id))
      (vector-for-each (lambda (e)
			 (when (vector? (cdr e))
			   (let ((current current-path)) 
			     (collect-ids (cdr e))
			     (set! current-path current))))
		       object)))
  (define (handle-$ref e)
    (define (err id)
      (assertion-violation 'json-schema->json-validator "Unknown $ref" id))
    (define (refer-absolute id maybe-external? error?)
      (or (hashtable-ref ids id)
	  (hashtable-ref ids (string-append id "#")) ;; check with fragment
	  ;; TODO handle external
	  (and error? (err id))))
    (let-values (((scheme auth path query frag)
		  (parse-id (cdr e))))
      (cond (scheme
	     (or (refer-absolute (cdr e) #t #f)
		 ;; okay as it as doesn't exist so try JSON pointer
		 (let* ((uri (uri-compose :scheme scheme :authority auth
					  :path path :query query))
			(obj (refer-absolute uri #t #t)))
		   (if frag
		       ((json-pointer frag) obj)
		       obj))))
	    (path
	     (let ((obj (refer-absolute (merge-id path) #f #t)))
	       (if frag
		   ((json-pointer frag) obj)
		   obj)))
	    (frag ((json-pointer frag) schema))
	    ;; should not happen, ...I think...
	    (else (refer-absolute (cdr e) #f #t)))))
  (define (resolve-reference o)
    (define len (vector-length o))
    (define object (vector-copy o))
    (let loop ((i 0) (found? #f) (refs '()))
      (if (= len i)
	  (if found?
	      (vector-concatenate refs)
	      object)
	  (let ((e (vector-ref object i)))
	    (cond ((string=? "$ref" (car e))
		   (loop (+ i 1) #t (cons (handle-$ref e) refs)))
		  ((vector? (cdr e))
		   (vector-set! object i
				(cons (car e) (resolve-reference (cdr e))))
		   (loop (+ i 1) found? refs))
		  (else (loop (+ i 1) found? refs)))))))
  (when root-id (hashtable-set! ids root-id schema))
  (collect-ids schema)
  (resolve-reference schema))

(define (->json-validator schema)
  (define ignore (make-hashtable string-hash string=?))
  (vector-fold
   (lambda (combined-validator e)
     ;; TODO consider schema version
     (cond ((and (not (hashtable-contains? ignore (car e)))
		 (assoc (car e) +json-schema-validators+)) =>
	    (lambda (slot)
	      (cond ((cadr slot) =>
		     (lambda (g)
		       ;; some of the validators are pretty much associated
		       ;; if this is such a propety, then it should have the
		       ;; ignore list behind the slot, so add them.
		       (for-each (lambda (i) (hashtable-set! ignore i #t))
				 (cddr slot))
		       (let ((validator (g schema (cdr e))))
			 (lambda (e)
			   (and (combined-validator e) (validator e))))))
		    ;; for unknown property we ignore
		    (else combined-validator))))
	   (else combined-validator)))
   (lambda (e) #t) schema))

(define (schema->validator who schema)
  (cond ((boolean? schema) (boolean->validator schema))
	((vector? schema) (->json-validator schema))
	(else
	 (assertion-violation who "JSON schema is required" schema))))

;; utilities
(define unique?
  (case-lambda
   ((v) (unique? v equal?))
   ((v =) (equal? v (delete-duplicates v =)))))
(define (json=? a b)
  (define (entry=? a b)
    (and (json=? (car a) (car b))
	 (json=? (cdr a) (cdr b))))
  (cond ((and (string? a) (string? b)) (string=? a b))
	;; 1 and 1.0 are not the same so can't be = or equal?
	((and (number? a) (number? b)) (eqv? a b))
	((and (vector? a) (vector? b)) (vector-every entry=? a b))
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
  (cond ((list? type)
	 (check type)
	 (fold-left (lambda (acc t)
		      (let ((v (json-schema:type t)))
			(lambda (e) (or (v e) (acc e))))) (lambda (e) #f) type))
	((string? type)
	 (cond ((string=? "string" type) string?)
	       ((string=? "integer" type) integer?)
	       ((string=? "number" type) real?)	;; TODO exclude rational?)
	       ;; we use vector json for validation
	       ;; TODO should we check content?
	       ((string=? "object" type) vector?)
	       ((string=? "array" type) list?)
	       ((string=? "boolean" type) boolean?)
	       ((string=? "null" type) (lambda (e) (eq? e 'null)))
	       (else (assertion-violation 'json-schema:type "Unknown type"))))
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
  (lambda (e) (exists (lambda (v) (json=? e v)) vals)))

;; 6.1.3 const
(define (json-schema:const v) (lambda (e) (json=? e v)))

;;; 6.2. Validation Keywords for Numeric Instances (number and integer)
;; 6.2.1 multipleOf
(define (json-schema:multiple-of v)
  (unless (and (real? v) (positive? v))
    (assertion-violation 'json-schema:multiple-of
			 "MultipleOf must be a number greater than 0" v))
  (lambda (e)
    (and (real? e) (zero? (mod e v)))))

;; 6.2.2. maximum
(define (json-schema:maximum v)
  (unless (real? v)
    (assertion-violation 'json-schema:maximum "Maximum must be a number" v))
  (lambda (e) (and (real? e) (<= e v))))
;; 6.2.3. exclusiveMaximum
(define (json-schema:exclusive-maximum v)
  (unless (real? v)
    (assertion-violation 'json-schema:exclusive-maximum
			 "ExclusiveMaximum must be a number" v))
  (lambda (e) (and (real? e) (< e v))))
;; 6.2.4. minimum
(define (json-schema:minimum v)
  (unless (real? v)
    (assertion-violation 'json-schema:minimum "Minimum must be a number" v))
  (lambda (e) (and (real? e) (<= v e))))
;; 6.2.5. exclusiveMinimum
(define (json-schema:exclusive-minimum v)
  (unless (real? v)
    (assertion-violation 'json-schema:exclusive-minimum
			 "ExclusiveMinimum must be a number" v))
  (lambda (e) (and (real? e) (< v e))))

;;; 6.3. Validation Keywords for Strings
;; 6.3.1. maxLength
(define (json-schema:max-length v)
  (when (or (not (integer? v)) (negative? v))
    (assertion-violation 'json-schema:max-length
			 "maxLength must be a non negative integer" v))
  (lambda (e) (and (string? e) (<= (string-length e) v))))
;; 6.3.2. minLength
(define (json-schema:min-length v)
  (when (or (not (integer? v)) (negative? v))
    (assertion-violation 'json-schema:min-length
			 "minLength must be a non negative integer" v))
  (lambda (e) (and (string? e) (<= v (string-length e)))))
;; 6.3.3. pattern
(define (json-schema:pattern p)
  (unless (string? p)
    (assertion-violation 'json-schema:pattern "pattern must be a string" p))
  (guard (e (else
	     (assertion-violation 'json-schema:pattern
				  "Invalid regex pattern" p)))
    (let ((rx (regex p)))
      (lambda (e)
	(and (string? e) (looking-at rx e) #t)))))

;;; 6.4. Validation Keywords for Arrays
;; 6.4.1. items
;; 6.4.2. additionalItems
(define (json-schema:items schema items)
  (define (->validator schema) (schema->validator 'json-schema:items schema))
  (define (get-additional-validator schema)
    (->validator (value-of "additionalItems" schema #t)))
  
  (cond ((boolean? items) (boolean->validator items))
	((and (list? items) (for-all vector? items))
	 (let ((additional-validator (get-additional-validator schema))
	       (validators (map ->json-validator items)))
	   (lambda (e*)
	     (let loop ((e* e*) (validators validators))
	       (cond ((null? e*) #t)
		     ((null? validators) (for-all additional-validator e*))
		     (else (and ((car validators) (car e*))
				(loop (cdr e*) (cdr validators)))))))))
	((vector? items)
	 (let ((validator (->validator items)))
	   (lambda (e*)
	     (for-all validator e*))))
	(else
	 (assertion-violation 'json-schema:items
	  "Items must be a JSON schema or array of JSON schema" items))))
;; 6.4.3. maxItems
(define (json-schema:max-items n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:max-items
			 "maxItems must be a non negative integer" n))
  (lambda (e) (and (list? e) (<= (length e) n))))
;; 6.4.4. minItems
(define (json-schema:min-items n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:max-items
			 "minItems must be a non negative integer" n))
  (lambda (e) (and (list? e) (<= n (length e)))))
;; 6.4.5. uniqueItems
(define (json-schema:unique-items b)
  (unless (boolean? b)
    (assertion-violation 'json-schema:unique-items
			 "uniqueItems must be a boolean" b))
  (if b
      (lambda (e) (and (list? e) (unique? e)))
      (lambda (e) #t)))
;; 6.4.6. contains
(define (json-schema:contains value)
  (let ((validator (schema->validator 'json-schema:contains value)))
    (lambda (e) (exists validator e))))

;;; 6.5. Validation Keywords for Objects
;; 6.5.1. maxProperties
(define (json-schema:max-properties n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:max-properties
			 "maxProperties must be a non negative integer" n))
  (lambda (e) (and (vector? e) (<= (vector-length e) n))))
;; 6.5.2. minProperties
(define (json-schema:min-properties n)
  (when (or (not (integer? n)) (negative? n))
    (assertion-violation 'json-schema:min-properties
			 "minProperties must be a non negative integer" n))
  (lambda (e) (and (vector? e) (<= n (vector-length e)))))
;; 6.5.3. required
(define (json-schema:required e*)
  (unless (and (list? e*) (for-all string? e*) (unique? e*))
    (assertion-violation 'json-schema:required
			 "Required must be an array of unique strings" e*))
  (lambda (e)
    (and (vector? e)
	 ;; TODO inefficient
	 (for-all (lambda (k)
		    (vector-any
		     (lambda (v)
		       ;; not sure if we need to handle invalid JSON
		       ;; structure...
		       (and (pair? v) (string? (car v)) (string=? (car v) k)))
		     e))
		  e*))))

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
  (define (object->validator obj regex?)
    (define (check vec key=? validator)
      (define len (vector-length vec))
      (let loop ((i 0) (found? #f) (ok? #t))
	(cond ((= i len) (and found? ok?))
	      ((key=? (vector-ref vec i)) =>
	       (lambda (k&v) (loop (+ i 1) #t (validator (cdr k&v)))))
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
	     (cdr e))))
    (lambda (e)
      (define len (vector-length e))
      (let loop ((i 0) (ok? #f) (found? #f))
	(cond ((= i len) (or (not found?) ok?))
	      ((pred (vector-ref e i)) =>
	       (lambda (v) (loop (+ i 1) (validator v) #t)))
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
    (cond ((list? d) (handle-array d))
	  ((vector? d) (->json-validator d))
	  ((boolean? d) (boolean->validator d))
	  (else (assertion-violation 'json-schema:dependencies
				     "Dependency must be an array or schema"
				     d))))
  (unless (vector? v)
    (assertion-violation 'json-schema:dependencies
			 "Dependencies must be an object" v))
  (vector-fold (lambda (combined e)
		 (let ((prop (car e))
		       (dependency (handle-dependency (cdr e))))
		   (lambda (e)
		     (let ((v (value-of prop e (eof-object))))
		       (and (combined e)
			    (if (not (eof-object? v))
				(dependency e)
				#t))))))
	       (boolean->validator #t) v))
;; 6.5.8. propertyNames
(define (json-schema:property-names v)
  (unless (or (boolean? v) (vector? v))
    (assertion-violation 'json-schema:propperty-names
			 "PropertyNames must be a JSON schema"))
  (let ((validator (schema->validator 'json-schema:propperty-names v)))
    (lambda (e)
      (vector-every (lambda (k&v)
		      ;; the key must always be a string, otherwise it's an
		      ;; invalid JSON but we don't check that 
		      (or (not (string? (car k&v)))
			  (validator (car k&v)))) e))))

;;; 6.6. Keywords for Applying Subschemas Conditionally

;;; 6.7. Keywords for Applying Subschemas With Boolean Logic

;;; 7. Semantic Validation With "format"


(define (boolean->validator b) (if b (lambda (_) #t) (lambda (_) #f)))
(define (type-wrap type? validator)
  (lambda (e) (or (not (type? e)) (validator e))))
;; simple wrap 
(define (s/w simple-validator-generator)
  (lambda (schema v) (simple-validator-generator v)))
;; type check
(define (t/w type? simple-validator-generator)
  (lambda (schema v) (type-wrap type? (simple-validator-generator v))))
;; JSON schema of true/false is {} or { "not": {} }
(define (a/w type? schema-validator-generator)
  (lambda (schema v)
    (type-wrap type? 
	       (if (boolean? v)
		   (boolean->validator v)
		   (schema-validator-generator schema v)))))

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
    ))
(define +json-schema-array-validators+
  `(
    ("items" ,(a/w list? json-schema:items))
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
(define +json-schema-validators+
  `(
    ,@+json-schema-any-instance-validators+
    ,@+json-schema-numeric-instance-validators+
    ,@+json-schema-string-validators+
    ,@+json-schema-array-validators+
    ,@+json-schema-object-validators+
    ))
)
