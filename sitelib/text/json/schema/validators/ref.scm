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
	    json-schema:$dynamic-ref
	    json-schema:draft-7-$ref

	    *json-schema:resolve-external-schema?*
	    *json-schema:external-schema-resolver*)
    (import (rnrs)
	    (rfc uri)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (srfi :45 lazy)
	    (text json parse)
	    (text json pointer)
	    (text json schema validators api)
	    (text json schema validators core)
	    (util uri))

(define *json-schema:resolve-external-schema?* (make-parameter #f))

(define (json-schema:default-external-schema-resolver uri)
  (guard (e (else (error 'json-schema:default-external-schema-resolver
			 "Failed to retrieve external schema"
			 uri
			 e)))
    (call-with-port (transcoded-port (open-uri uri) (native-transcoder))
      json-read)))
(define *json-schema:external-schema-resolver* (make-parameter #f))

;; probably better to check URI and only fragment but for now
(define (mere-json-pointer? s) (string-prefix? "/" s))
(define (->cached-validator schema id)
  (let ((path (or (and id (string-append id "#")) "#")))
    (schema-context->cached-validator schema path)))

(define (ref-not-found value schema-path)
  (assertion-violation 'json-schema:$ref "$ref not found" value schema-path))
(define (check-anchor schema id anchor schema-path dynamic?)
  (cond ((not anchor)
	 (cond ((schema-context-validator schema))
	       ;; in case of cross reference or self $id reference
	       (else (->cached-validator schema id))))
	((mere-json-pointer? anchor)
	 ;; a bit inefficient but just compile it
	 (let ((s ((json-pointer (uri-decode-string anchor))
		   (schema-context-schema schema))))
	   (when (json-pointer-not-found? s) (ref-not-found id schema-path))
	   (let ((schema (make-schema-context s schema)))
	     (cond ((schema-context-validator schema))
		   (else (schema-context->schema-validator schema
			  (string-append (or id "") "#" anchor)))))))
	((string-null? anchor)
	 ;; recursive
	 (cond ((schema-context-validator schema))
	       (else (->cached-validator schema id))))
	((and (not dynamic?) (schema-context:find-by-anchor schema anchor)) =>
	 schema-context-validator)
	((and dynamic? (schema-context:find-by-dynamic-anchor schema anchor)) =>
	 schema-context-validator)
	(else #f)))

(define (resolve-external-schema context id anchor schema-path dynamic?)
  (let ((resolver (or (*json-schema:external-schema-resolver*)
		      (and (*json-schema:resolve-external-schema?*)
			   json-schema:default-external-schema-resolver))))
    (unless resolver
      (assertion-violation 'json-schema:$ref
			   "External schema! Enable external resolver" id))
    (let ((this-context (make-disjoint-context (resolver id) context)))
      ;; Resolve given id as the value of $id.
      ;; If the schema has it, then it'd be overwritten anyway.
      (json-schema:$id id this-context "#")
      (let ((validator (initial-schema-context->schema-validator this-context)))
	(cond ((check-anchor this-context id anchor schema-path dynamic?))
	      (else validator))))))

(define ($ref-handler value context schema-path dynamic?)
  (define schema-id (schema-context-schema-id context))
  (define in-id (schema-context-in-id context))

  (let-values (((this-id anchor) (uri->id&fragment value)))
    (let* ((id (or (and this-id (if in-id (uri-merge in-id this-id) this-id))
		   (and schema-id in-id (uri-merge in-id schema-id))
		   in-id))
	   (schema (if id
		       (schema-context:find-by-id context id)
		       ;; then root schema
		       (schema-context:root-schema context))))
      (schema-validator->core-validator
       (cond ((not schema)
	      (schema-context:delayed-validator context
	       (lambda ()
		 (let ((schema (schema-context:find-by-id context id)))
		   (schema-validator->core-validator
		    (cond ((not schema)
			   (resolve-external-schema context id
						    anchor schema-path dynamic?))
			  ((check-anchor schema id anchor schema-path dynamic?))
			  (else (schema-context-validator schema))))))
	       (string-append (or id "") "#")))
	     ((check-anchor schema id anchor schema-path dynamic?))
	     (else
	      (schema-context:delayed-validator context
	       (lambda ()
		 (schema-validator->core-validator
		  (cond ((schema-context:find-by-anchor schema anchor) =>
			 schema-context-validator)
			(else (ref-not-found id schema-path)))))
	       (string-append (or id "") "#"))))))))

(define (json-schema:draft-7-$ref value context schema-path)
  ($ref-handler value context schema-path #f))

(define (json-schema:$ref value context schema-path)
  ($ref-handler value context schema-path #f))

(define (json-schema:$recursive-ref value context schema-path)
  (unless (equal? value "#")
    (assertion-violation 'json-schema:$recursive-ref
			 "$resursiveRef must have value of '#'" value))
  (cond ((schema-context:recursive-anchor-enabled? context)
	 (schema-validator->core-validator
	  (schema-context:recursive-validator context schema-path)))
	(else ($ref-handler value context schema-path #f))))

(define (json-schema:$dynamic-ref value context schema-path)
  (define (dynamic-anchor? context value)
    (cond ((string-prefix? "#" value)
	   (let ((anchor (substring value 1 (string-length value))))
	     (and (schema-context:has-dynamic-anchor? context anchor)
		  anchor)))
	  ;; TODO id#frag case, not sure how we should handle it...
	  (else #f)))
	 
  (unless (string? value)
    (assertion-violation 'json-schema:$dynamic-ref "Must be string" value))
  (cond ((dynamic-anchor? context value) =>
	 (lambda (anchor)
	   (schema-validator->core-validator
	    (schema-context:dynamic-validator context anchor schema-path))))
	(else ($ref-handler value context schema-path #t))))

)
