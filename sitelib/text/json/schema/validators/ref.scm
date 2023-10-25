;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators/ref.scm - JSON schema $ref handling
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
(library (text json schema validators ref)
    (export json-schema:$ref json-schema:$recursive-ref
	    json-schema:draft-7-$ref

	    *json-schema:resolve-external-schema?*
	    *json-schema:external-schema-resolver*)
    (import (rnrs)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (text json parse)
	    (text json pointer)
	    (rfc uri)
	    (util uri))
(define *json-schema:resolve-external-schema?* (make-parameter #f))

(define (json-schema:default-external-schema-resolver uri)
  (call-with-port (transcoded-port (open-uri uri) (native-transcoder))
    json-read))
(define *json-schema:external-schema-resolver* (make-parameter #f))


;; probably better to check URI and only fragment but for now
(define (mere-json-pointer? s) (string-prefix? "#/" s))

(define ($ref-handler value context schema-path allow-recursive?)
  (define (external-resolver)
    (or (*json-schema:external-schema-resolver*)
	(and (*json-schema:resolve-external-schema?*)
	     json-schema:default-external-schema-resolver)))
  (define (resolve-external-schema context uri frag)
    (let ((resolver (external-resolver)))
      (unless resolver
	(assertion-violation '$ref-handler "$ref not found" value schema-path))
      (let* ((schema (resolver uri))
	     ;; for recursive ref, we need to make fresh context without
	     ;; parent.
	     (ctx (make-validator-context schema)))
	;; if fragmentation is defined, then only partial schema
	;; should be compiled (I think), so use $ref-handler for that.
	;; Otherwise we can simply compile the entire schema
	(if (and frag (not (zero? (string-length frag))))
	    ($ref-handler (string-append "#" frag) ctx
			  (string-append uri "#" frag)
			  allow-recursive?)
	    (schema-validator->core-validator
	     (schema->schema-validator ctx "#"))))))
  
  (define (handle-json-pointer context jp path)
    (define decoded (uri-decode-string jp))
    (cond ((search-schema context (json-pointer decoded)) =>
	   (lambda (c&s)
	     (schema-validator->core-validator
	      (schema->schema-validator
	       (make-validator-context (cdr c&s) (car c&s))
	       path))))
	  (else
	   (assertion-violation '$ref-handler
				"$ref not found" value schema-path))))
  (define (->validator id ctx)
    (let* ((cache (validator-context-cache ctx))
	   (retriever (lambda () (hashtable-ref cache ctx true-validator))))
      (cond ((hashtable-ref cache ctx #f) =>
	     (lambda (p)
	       (lambda (e path)
		 ((force p) e path))))
	    (else
	     ;; put dummy promise
	     (hashtable-set! cache ctx (lazy retriever))
	     (let ((v (schema-validator->core-validator
		       (schema->schema-validator ctx (string-append id "#")))))
	       ;; update
	       (hashtable-set! cache ctx (delay (lambda () v)))
	       v)))))
  (define (find-anchor ctx frag)
    (let ((id (string-append "#" frag)))
      (search-definitions ctx id id)))
  (cond ((string=? value "#")
	 (unless allow-recursive?
	   (assertion-violation '$ref-handler "Recursive $ref is not allowed"))
	 (json-schema:$recursive-ref value context schema-path))
	((mere-json-pointer? value)
	 (handle-json-pointer context
			      (substring value 1 (string-length value))
			      value))
	(else
	 (let-values (((scheme ui host port path query frag) (uri-parse value)))
	   (let* ((id (if (and (not scheme) (not host) path)
			  (compose-id context path)
			  (uri-compose :scheme scheme :userinfo ui
				       :host host :port port :path path
				       :query query)))
		  (ctx (search-context context id value)))
	     (cond ((not ctx)
		    (resolve-external-schema context
		     (uri-compose :scheme scheme :userinfo ui :host host
				  :path path :port port :query query) frag))
		   ;; is fragment json pointer
		   ((and frag (string-prefix? "/" frag))
		    (handle-json-pointer ctx frag (string-append id "#" frag)))
		   ;; possibly an anchor
		   ((and frag (find-anchor ctx frag)) =>
		    (lambda (new-ctx)
		      (schema-validator->core-validator
		       ;; TODO schema-path
		       (schema->schema-validator new-ctx value))))
		   ;; okay then we convert this to a validator
		   ;; here, we also handle cross referencing schemas...
		   (else (->validator id ctx))))))))

(define (json-schema:draft-7-$ref value context schema-path)
  ($ref-handler value context schema-path #t))

(define (json-schema:$ref value context schema-path)
  ($ref-handler value context schema-path #f))


(define (json-schema:$recursive-ref value context schema-path)
  (define schema (do ((c context (validator-context-parent c)))
		     ((not (validator-context-parent c))
		      (validator-context-schema c))))
  (define cache (validator-context-cache context))
  (define fallback-validator
    (make-schema-validator true-validator schema-path))
  (define (validator-retriever)
    (let ((v (hashtable-ref cache schema fallback-validator)))
      (schema-validator->core-validator v)))
  (let ((validator (delay (validator-retriever))))
    (lambda (e path)
      (let ((v (force validator)))
	(v e path)))))


;; utilities
;; $ref handlers
(define (search-schema context pointer)
  (define schema (validator-context-schema context))
  (let ((r (pointer schema)))
    (cond ((json-pointer-not-found? r)
	   (cond ((validator-context-parent context) =>
		  (lambda (context) (search-schema context pointer)))
		 (else #f)))
	  (else (cons context r)))))
(define (search-context context id org-id)
  (define context-id (validator-context-id context))
  (define (find-vocabularies context id)
    (define vocabularies (validator-context-$vocabularies context))
    (and vocabularies
	 (not (null? vocabularies))
	 (find (lambda (context)
		 (equal? (validator-context-id context) id))
	       vocabularies)))
 
  (cond ((equal? context-id id) context)
	((find-vocabularies context id))
	((search-definitions context id org-id))
	((validator-context-parent context) =>
	 (lambda (context) (search-context context id org-id)))
	(else #f)))

(define (search-definitions context id org-id)
  (define version-specific (validator-context:version-specific context))
  (define context-id (validator-context-id context))
  (define definitions-pointers
    (version-specific-definitions-pointers version-specific))
  (define (anchor? s)
    (and (string? s)
	 (> (string-length s) 1)
	 (not (string-prefix? "#/" s))))
  (define (compute-possible-id id org-id context-id)
    (cond ((and context-id (string-prefix? context-id id))
	   (list id org-id))
	  ;; uri-compose may crete this if scheme host and path are missing...
	  ((and (string=? id "///") (anchor? org-id))
	   (list org-id))
	  (else (list id))))
  (define (find-schema-id obj id context)
    (define context-id (validator-context-id context))
    (define len (vector-length obj))
    (define ids (compute-possible-id id org-id context-id))
    (let loop ((i 0))
      (if (= i len)
	  #f
	  (let* ((e (vector-ref obj i))
		 ;; TODO in case of #foo case, for draft-7 $id
		 ;;      can also be an anchor, but after that it must
		 ;;      be $anchor
		 (r ($id-pointer (cdr e))))
	    (cond ((json-pointer-not-found? r) (loop (+ i 1)))
		  ((or (member r ids)
		       (and context-id (member (uri-merge context-id r) ids)))
		   (make-validator-context (cdr e) context))
		  (else (loop (+ i 1))))))))

  (define (check-definitions context id pointers)
    (define schema (validator-context-schema context))
    (let loop ((p pointers))
      (if (null? p)
	  #f
	  (let ((r ((car p) schema)))
	    (cond ((json-pointer-not-found? r) (loop (cdr p)))
		  ((and (vector? r) (find-schema-id r id context)))
		  (else (loop (cdr p))))))))
  (check-definitions context id definitions-pointers))

(define (compose-id context path)
  (define (rec context)
    (define context-id (validator-context-id context))
    (cond (context-id (uri-merge context-id path))
	  ((validator-context-parent context) => rec)
	  ;; maybe this is the id
	  (else path)))
  (rec context))

)
