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
	    (text json schema validators vocabularies)
	    ;; for parameters (maybe merge them into api)
	    (text json schema validators format)
	    (text json schema validators ref)
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

(define *version-specifics*
  `((draft-7 . ,*json-schema:draft-7-vocabularies*)
    (2019-09 . ,*json-schema:draft-2019-09-vocabularies*)
    (2020-12 . ,*json-schema:draft-2020-12-vocabularies*)))
)
