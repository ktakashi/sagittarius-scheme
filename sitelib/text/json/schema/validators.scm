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
	    json-schema-validator-id
	    json-schema-validator-version
	    (rename (json-schema-validator-version json-schema-validator-schema))
	    json-schema-validator-source
	    json-schema:version

	    simple-json-schema-error-reporter

	    *json-schema:default-version*
	    (rename (*json-schema:default-version* *json-schema:version*))
	    *json-schema:vocabulary-handler*
	    *json-schema:resolve-external-schema?*
	    *json-schema:external-schema-resolver*
	    *json-schema:validate-format?*
	    *json-schema:lint-mode?*
	    *json-schema:validator-error-reporter*
	    *json-schema:report-port*
	    )
    (import (rnrs)
	    (srfi :39 parameters)
	    (text json pointer)
	    (text json validator)
	    (text json schema validators api)
	    (text json schema validators array)
	    (text json schema validators core)
	    (text json schema validators ref)
	    (text json schema validators logics)
	    (text json schema validators primitives)
	    (text json schema validators object)
	    (text json schema validators format)
	    (text json schema version)
	    (text json schema vocabularies)
	    )

(define *json-schema:lint-mode?* (make-parameter #f))
(define *json-schema:report-port* (make-parameter #f))
(define *json-schema:validator-error-reporter* (make-parameter #f))

(define (run-validator validator)
  (lambda (v)
    (define lint-mode? (*json-schema:lint-mode?*))
    (define ctx (make-validator-context lint-mode?))
    (let ((r (validator v ctx)))
      (or (and lint-mode?
	       (cond ((*json-schema:validator-error-reporter*) =>
		      (lambda (reporter) (reporter ctx) #f))
		     (else r)))
	  r))))

(define (simple-json-schema-error-reporter ctx)
  (define out (or (*json-schema:report-port*) (current-error-port)))
  (define reports (validator-context-reports ctx))
  (define (report-error report)
    (display "\t     object: " out) (write (car report) out) (newline out)
    (display "\t  json path: " out) (display (cadr report) out) (newline out)
    (display "\tschema path: " out) (display (caddr report) out) (newline out))
  (display "[Validation result]" out) (newline out)
  (for-each report-error reports))

(define-record-type json-schema-validator
  (parent <json-validator>)
  (fields source  ;; Raw sexp JSON (vector)
	  version ;; value of $schema
	  id	  ;; $id
	  )
  (protocol (lambda (n)
	      (lambda (validator source version id)
		((n (run-validator validator)) source version id)))))

(define (json-schema->json-validator schema . referencing-validators)
  (let* ((root (make-root-context *version-specifics*))
	 (context (make-initial-schema-context schema root)))
    (make-json-schema-validator
     (schema-validator->core-validator
      (initial-schema-context->schema-validator context))
     schema
     (schema-context-version context)
     (schema-context-schema-id context))))

(define ((simple-handler ->validator) value context schema-path)
  (let ((validator (->validator value)))
    (values (wrap-core-validator
	     (lambda (e ctx) (validator e)) schema-path) #t)))
(define ((schema-handler ->validator) value context schema-path)
  (values (->validator value context schema-path) #t))
(define ((no-continue-handler ->validator) value context schema-path)
  (values (->validator value context schema-path) #f))

(define *common-vocabularies*
  `(("$schema" . ,(schema-handler json-schema:$schema))

    ("type" . ,(simple-handler json-schema:type))
    ("enum" . ,(simple-handler json-schema:enum))
    ("const" . ,(simple-handler json-schema:const))

    ("multipleOf" . ,(simple-handler json-schema:multiple-of))
    ("maximum" . ,(simple-handler json-schema:maximum))
    ("exclusiveMaximum" . ,(simple-handler json-schema:exclusive-maximum))
    ("minimum" . ,(simple-handler json-schema:minimum))
    ("exclusiveMinimum" . ,(simple-handler json-schema:exclusive-minimum))

    ("maxLength" . ,(simple-handler json-schema:max-length))
    ("minLength" . ,(simple-handler json-schema:min-length))
    ("pattern" . ,(simple-handler json-schema:pattern))
    ("format" . ,(simple-handler json-schema:format))

    ("maxItems" . ,(simple-handler json-schema:max-items))
    ("minItems" . ,(simple-handler json-schema:min-items))
    ("uniqueItems" . ,(simple-handler json-schema:unique-items))

    ("maxProperties" . ,(simple-handler json-schema:max-properties))
    ("minProperties" . ,(simple-handler json-schema:min-properties))
    ("required" . ,(simple-handler json-schema:required))
    ("properties" . ,(schema-handler json-schema:properties))
    ("patternProperties" . ,(schema-handler json-schema:pattern-properties))
    ("propertyNames" . ,(schema-handler json-schema:property-names))
    ("additionalProperties" . ,(schema-handler json-schema:additional-properties))
    ;; only 2019-09 have backward compatibility? I couldn't find explicit
    ;; sentence :(
    ("dependencies" . ,(schema-handler json-schema:dependencies))
    
    ;; 10.2.1 Keywords for Applying Subschemas With Logic
    ("allOf" . ,(schema-handler json-schema:all-of))
    ("anyOf" . ,(schema-handler json-schema:any-of))
    ("oneOf" . ,(schema-handler json-schema:one-of))
    ("not" . ,(schema-handler json-schema:not))
    ("if" . ,(schema-handler json-schema:if))
    ("then" . ,(schema-handler json-schema:then))
    ("else" . ,(schema-handler json-schema:else))
    ))

;; order matters
(define *draft-7-vocabularies*
  `(("$id" . ,(schema-handler json-schema:draft-7-$id))
    ("definitions" . ,(schema-handler json-schema:definitions))
    ("$ref" . ,(no-continue-handler json-schema:draft-7-$ref))
    ,@*common-vocabularies*
    ("items" . ,(schema-handler json-schema:draft-7-items))
    ("contains" . ,(schema-handler json-schema:draft-7-contains))
    ("additionalItems" . ,(schema-handler json-schema:additional-items))
    ))

(define *draft-2019-09-vocabularies*
  `(("$id" . ,(schema-handler json-schema:$id))
    ("$vocabulary" . ,(schema-handler json-schema:$vocabulary))
    ("$defs" . ,(schema-handler json-schema:$defs))
    ("definitions" . ,(schema-handler json-schema:definitions))
    ("$anchor" . ,(schema-handler json-schema:$anchor))
    ("$recursiveAnchor" . ,(schema-handler json-schema:$recursive-anchor))
    ("$ref" . ,(schema-handler json-schema:$ref))
    ("$recursiveRef" . ,(no-continue-handler json-schema:$recursive-ref))

    ,@*common-vocabularies*
    ("items" . ,(schema-handler json-schema:draft-7-items))
    ;; order is important
    ("additionalItems" . ,(schema-handler json-schema:additional-items))
    ("unevaluatedItems" . ,(schema-handler json-schema:unevaluated-items))
    ("contains" . ,(schema-handler json-schema:contains))
    ("dependentSchemas" . ,(schema-handler json-schema:dependent-schemas))
    ("dependentRequired" . ,(schema-handler json-schema:dependent-required))
    ))

(define *draft-2020-12-vocabularies*
  `(("$id" . ,(schema-handler json-schema:$id))
    ("$defs" . ,(schema-handler json-schema:$defs))
    ("$ref" . ,(schema-handler json-schema:$ref))
    ,@*common-vocabularies*
    ("dependentSchemas" . ,(schema-handler json-schema:dependent-schemas))
    ("dependentRequired" . ,(schema-handler json-schema:dependent-required))
    ))

(define *version-specifics*
  `((draft-7 . ,*draft-7-vocabularies*)
    (2019-09 . ,*draft-2019-09-vocabularies*)
    (2020-12 . ,*draft-2020-12-vocabularies*)))
)
