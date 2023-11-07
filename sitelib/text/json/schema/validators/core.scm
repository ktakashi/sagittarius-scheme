;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators/core.scm - JSON schema core
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
(library (text json schema validators core)
    (export json-schema:$id json-schema:$schema
	    json-schema:$anchor json-schema:$recursive-anchor
	    json-schema:$dynamic-anchor
	    json-schema:$defs
	    json-schema:$vocabulary
	    json-schema:draft-7-$id json-schema:definitions)
    (import (rnrs)
	    (srfi :13 strings)
	    (text json pointer)
	    (text json schema version)
	    (text json schema vocabularies)
	    (text json schema validators api))

(define (($id-handler allow-anchor?) value context schema-path)
  (define in-id (schema-context-in-id context))
  ;; split id and anchor
  (let-values (((id anchor) (uri->id&fragment value)))
    (when (and anchor (not allow-anchor?))
      (assertion-violation 'json-schema:$id
			   "$id mustn't contain fragment" value))
    ;; #foo case, allowed on Draft 7
    (when id (schema-context:set-id! context id))
    (when anchor (schema-context:add-anchor! context anchor)))
  ;; we don't need validator for this
  #f)


(define json-schema:$id ($id-handler #f))
  
(define json-schema:draft-7-$id ($id-handler #t))

;; This does nothing, but for consistency :)
(define (json-schema:$schema value context schema-path)
  (cond ((json-schema->version value) =>
	 (lambda (version)
	   (schema-context-version-set! context version)))
	(else
	 (assertion-violation 'json-schema:$schema "Unknown schema" value)))
  #f)

(define (json-schema:$anchor value context schema-path)
  (unless (string? value)
    (assertion-violation 'json-schema:$anchor "Invalid anchor" value))
  (schema-context:add-anchor! context value)
  #f)

(define (json-schema:$recursive-anchor value context schema-path)
  (unless (boolean? value)
    (assertion-violation 'json-schema:$recursive-anchor "Must be boolean" value))
  (when value (schema-context:mark-dynamic-anchor! context value))
  (lambda (e ctx)
    (when value (validator-context:set-dynamic-context! ctx context value))
    #t))

(define (json-schema:$dynamic-anchor value context schema-path)
  (unless (string? value)
    (assertion-violation 'json-schema:$dynamic-anchor "Must be string" value))
  (schema-context:mark-dynamic-anchor! context value)
  (schema-context:add-anchor! context value)
  (lambda (e ctx)
    (validator-context:set-dynamic-context! ctx context value)
    #t)
  )

(define (($defs-handler name) value context schema-path)
  (define this-path (build-schema-path schema-path name))
  (define (compile-definition e)
    (let ((path (build-schema-path this-path (car e)))
	  (ctx (make-schema-context (cdr e) context)))
      ;; This will be stored in the cache, so we don't use the
      ;; return value here :)
      (schema-context->schema-validator ctx path)))
  (unless (vector? value)
    (assertion-violation 'json-schema:$defs
			 "$defs must contain JSON object" value))
  (vector-for-each compile-definition value)
  #f
  )

(define json-schema:definitions ($defs-handler "definitions"))
(define json-schema:$defs ($defs-handler "$defs"))


(define (json-schema:$vocabulary value context schema-path)
  (define (handle-vocabulary e)
    (define uri (car e))
    (unless (schema-context:vocabulary-loaded? context uri)
      (let* ((schema (retrieve-json-schema-vocabulary uri))
	     (new-context (make-disjoint-context schema context)))
	(schema-context:cache-vocabulary! new-context uri)
	(initial-schema-context->schema-validator new-context))))
  (unless (vector? value)
    (assertion-violation 'json-schema:$vocabulary
			 "JSON object is required" value))
  ;; we don't check the value of vocabulary
  (vector-for-each handle-vocabulary value)
  #f)

)
