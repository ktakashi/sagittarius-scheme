;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators/vocabularies.scm -
;;;           JSON schema validator vocabularies
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

;; reference:
;; Draft-7, 2019-09 and 2020-12: https://json-schema.org/
#!nounbound
(library (text json schema validators vocabularies)
    (export *json-schema:draft-7-vocabularies*

	    *json-schema:draft-2019-09-vocabularies*
	    *json-schema:draft-2019-09-vocab-core*
	    *json-schema:draft-2019-09-vocab-applicator*
	    *json-schema:draft-2019-09-vocab-validation*
	    *json-schema:draft-2019-09-vocab-meta-data*
	    *json-schema:draft-2019-09-vocab-format*
	    *json-schema:draft-2019-09-vocab-content*

	    *json-schema:draft-2020-12-vocabularies*
	    *json-schema:draft-2020-12-vocab-core*
	    *json-schema:draft-2020-12-vocab-applicator*
	    *json-schema:draft-2020-12-vocab-unevaluated*
	    *json-schema:draft-2020-12-vocab-validation*
	    *json-schema:draft-2020-12-vocab-meta-data*
	    *json-schema:draft-2020-12-vocab-format-annotation*
	    *json-schema:draft-2020-12-vocab-content*
	    
	    )
    (import (rnrs)
	    (text json schema validators api)
	    (text json schema validators array)
	    (text json schema validators core)
	    (text json schema validators format)
	    (text json schema validators logics)
	    (text json schema validators object)
	    (text json schema validators primitives)
	    (text json schema validators ref))

(define ((simple-handler ->validator) value context schema-path)
  (let ((validator (->validator value)))
    (values (wrap-core-validator
	     (lambda (e ctx) (validator e)) schema-path) #t)))
(define ((schema-handler ->validator) value context schema-path)
  (values (->validator value context schema-path) #t))
(define ((no-continue-handler ->validator) value context schema-path)
  (values (->validator value context schema-path) #f))

(define *json-schema:draft-7-vocabularies*
  `(("$schema" . ,(schema-handler json-schema:$schema))
    ("$id" . ,(schema-handler json-schema:draft-7-$id))
    ("definitions" . ,(schema-handler json-schema:definitions))
    ("$ref" . ,(no-continue-handler json-schema:draft-7-$ref))
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
    ("additionalProperties" .
     ,(schema-handler json-schema:additional-properties))
    ("dependencies" . ,(schema-handler json-schema:dependencies))
    ("allOf" . ,(schema-handler json-schema:all-of))
    ("anyOf" . ,(schema-handler json-schema:any-of))
    ("oneOf" . ,(schema-handler json-schema:one-of))
    ("not" . ,(schema-handler json-schema:not))
    ("if" . ,(schema-handler json-schema:if))
    ("then" . ,(schema-handler json-schema:then))
    ("else" . ,(schema-handler json-schema:else))
    ("items" . ,(schema-handler json-schema:draft-7-items))
    ("contains" . ,(schema-handler json-schema:draft-7-contains))
    ("additionalItems" . ,(schema-handler json-schema:additional-items))
    ))

(define *json-schema:draft-2019-09-vocab-core*
  `(
    ("$schema" . ,(schema-handler json-schema:$schema))
    ("$id" . ,(schema-handler json-schema:draft-7-$id))
    ("$recursiveAnchor" . ,(schema-handler json-schema:$recursive-anchor))
    ("$vocabulary" . ,(schema-handler json-schema:$vocabulary))
    ("$defs" . ,(schema-handler json-schema:$defs))
    ("$anchor" . ,(schema-handler json-schema:$anchor))
    ("$ref" . ,(schema-handler json-schema:$ref))
    ("$recursiveRef" . ,(schema-handler json-schema:$recursive-ref))
    ))

(define *json-schema:draft-2019-09-vocab-applicator*
  `(
    ("contains" . ,(schema-handler json-schema:draft-2019-09-contains))
    ("items" . ,(schema-handler json-schema:draft-7-items))
    ("additionalItems" . ,(schema-handler json-schema:additional-items))
    ("properties" . ,(schema-handler json-schema:properties))
    ("patternProperties" . ,(schema-handler json-schema:pattern-properties))
    ("dependentSchemas" . ,(schema-handler json-schema:dependent-schemas))
    ("propertyNames" . ,(schema-handler json-schema:property-names))
    ("additionalProperties" .
     ,(schema-handler json-schema:additional-properties))
    ("if" . ,(schema-handler json-schema:if))
    ("then" . ,(schema-handler json-schema:then))
    ("else" . ,(schema-handler json-schema:else))
    ("allOf" . ,(schema-handler json-schema:all-of))
    ("anyOf" . ,(schema-handler json-schema:any-of))
    ("oneOf" . ,(schema-handler json-schema:one-of))
    ("not" . ,(schema-handler json-schema:not))
    ("unevaluatedItems" . ,(schema-handler json-schema:unevaluated-items))
    ("unevaluatedProperties" .
     ,(schema-handler json-schema:unevaluated-properties))
    ))
(define *json-schema:draft-2019-09-vocab-validation*
  `(
    ("multipleOf" . ,(simple-handler json-schema:multiple-of))
    ("maximum" . ,(simple-handler json-schema:maximum))
    ("exclusiveMaximum" . ,(simple-handler json-schema:exclusive-maximum))
    ("minimum" . ,(simple-handler json-schema:minimum))
    ("exclusiveMinimum" . ,(simple-handler json-schema:exclusive-minimum))
    ("maxLength" . ,(simple-handler json-schema:max-length))
    ("minLength" . ,(simple-handler json-schema:min-length))
    ("pattern" . ,(simple-handler json-schema:pattern))
    ("maxItems" . ,(simple-handler json-schema:max-items))
    ("minItems" . ,(simple-handler json-schema:min-items))
    ("uniqueItems" . ,(simple-handler json-schema:unique-items))
    ;; maxContains and minContains are handled in contains, so no validator
    ("maxProperties" . ,(simple-handler json-schema:max-properties))
    ("minProperties" . ,(simple-handler json-schema:min-properties))
    ("required" . ,(simple-handler json-schema:required))
    ("dependentRequired" . ,(schema-handler json-schema:dependent-required))
    ("const" . ,(simple-handler json-schema:const))
    ("enum" . ,(simple-handler json-schema:enum))
    ("type" . ,(simple-handler json-schema:type))
    ))
;; We don't do anything for meta-data (it's not used by validation)
;; so just ignroe
(define *json-schema:draft-2019-09-vocab-meta-data*'())
(define *json-schema:draft-2019-09-vocab-format*
  `(
    ("format" . ,(simple-handler json-schema:format))
    ))
;; The same as meta-data
(define *json-schema:draft-2019-09-vocab-content* '())

;; Order matters, unfortunately, so keep it like this
(define *json-schema:draft-2019-09-vocabularies*
  `(,@*json-schema:draft-2019-09-vocab-core*
    ("definitions" . ,(schema-handler json-schema:definitions))
    ,@*json-schema:draft-2019-09-vocab-validation*
    ,@*json-schema:draft-2019-09-vocab-format*
    ,@*json-schema:draft-2019-09-vocab-applicator*
     ))

(define *json-schema:draft-2020-12-vocab-core*
  `(
    ("$schema" . ,(schema-handler json-schema:$schema))
    ("$id" . ,(schema-handler json-schema:draft-7-$id))
    ("$dynamicAnchor" . ,(schema-handler json-schema:$dynamic-anchor))
    ("$vocabulary" . ,(schema-handler json-schema:$vocabulary))
    ("$defs" . ,(schema-handler json-schema:$defs))
    ("$anchor" . ,(schema-handler json-schema:$anchor))
    ("$ref" . ,(schema-handler json-schema:$ref))
    ("$dynamicRef" . ,(schema-handler json-schema:$dynamic-ref))
    ))
(define *json-schema:draft-2020-12-vocab-applicator*
  `(
    ("contains" . ,(schema-handler json-schema:contains))
    ("prefixItems" . ,(schema-handler json-schema:prefix-items))
    ("items" . ,(schema-handler json-schema:items))
    ("properties" . ,(schema-handler json-schema:properties))
    ("patternProperties" . ,(schema-handler json-schema:pattern-properties))
    ("dependentSchemas" . ,(schema-handler json-schema:dependent-schemas))
    ("propertyNames" . ,(schema-handler json-schema:property-names))
    ("additionalProperties" .
     ,(schema-handler json-schema:additional-properties))
    ("if" . ,(schema-handler json-schema:if))
    ("then" . ,(schema-handler json-schema:then))
    ("else" . ,(schema-handler json-schema:else))
    ("allOf" . ,(schema-handler json-schema:all-of))
    ("anyOf" . ,(schema-handler json-schema:any-of))
    ("oneOf" . ,(schema-handler json-schema:one-of))
    ("not" . ,(schema-handler json-schema:not))
    ))

(define *json-schema:draft-2020-12-vocab-unevaluated*
  `(
    ("unevaluatedItems" . ,(schema-handler json-schema:unevaluated-items))
    ("unevaluatedProperties" .
     ,(schema-handler json-schema:unevaluated-properties))
    ))
(define *json-schema:draft-2020-12-vocab-validation*
  `(
    ("multipleOf" . ,(simple-handler json-schema:multiple-of))
    ("maximum" . ,(simple-handler json-schema:maximum))
    ("exclusiveMaximum" . ,(simple-handler json-schema:exclusive-maximum))
    ("minimum" . ,(simple-handler json-schema:minimum))
    ("exclusiveMinimum" . ,(simple-handler json-schema:exclusive-minimum))
    ("maxLength" . ,(simple-handler json-schema:max-length))
    ("minLength" . ,(simple-handler json-schema:min-length))
    ("pattern" . ,(simple-handler json-schema:pattern))
    ("maxItems" . ,(simple-handler json-schema:max-items))
    ("minItems" . ,(simple-handler json-schema:min-items))
    ("uniqueItems" . ,(simple-handler json-schema:unique-items))
    ;; maxContains and minContains are handled in contains, so no validator
    ("maxProperties" . ,(simple-handler json-schema:max-properties))
    ("minProperties" . ,(simple-handler json-schema:min-properties))
    ("required" . ,(simple-handler json-schema:required))
    ("dependentRequired" . ,(schema-handler json-schema:dependent-required))
    ("const" . ,(simple-handler json-schema:const))
    ("enum" . ,(simple-handler json-schema:enum))
    ("type" . ,(simple-handler json-schema:type))
    ))

(define *json-schema:draft-2020-12-vocab-meta-data* '())

(define *json-schema:draft-2020-12-vocab-format-annotation*
  `(
    ("format" . ,(simple-handler json-schema:format))
    ))
(define *json-schema:draft-2020-12-vocab-content* '())

(define *json-schema:draft-2020-12-vocabularies*
  `(
    ,@*json-schema:draft-2020-12-vocab-core*
    ("$recursiveAnchor" . ,(schema-handler json-schema:$recursive-anchor))
    ("definitions" . ,(schema-handler json-schema:definitions))
    ("$recursiveRef" . ,(schema-handler json-schema:$recursive-ref))
    ,@*json-schema:draft-2020-12-vocab-validation*
    ,@*json-schema:draft-2020-12-vocab-format-annotation*
    ,@*json-schema:draft-2020-12-vocab-applicator*
    ,@*json-schema:draft-2020-12-vocab-unevaluated*
    ))
  
)
