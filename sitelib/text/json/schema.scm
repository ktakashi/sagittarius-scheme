;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema.scm - JSON schema
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
(library (text json schema)
    (export json-schema->json-validator json-validator?
	    json-validator-id json-validator-schema
	    json-validator-source

	    validate-json)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius compiler)
	    (text json parse) ;; for *json-map-type*
	    (text json convert)
	    (text json schema validators)
	    (srfi :133 vectors))

(define-record-type json-validator
  (fields source ;; RAW sexp JSON (vector)
	  schema ;; $schema
	  id	 ;; $id
	  validator))

(define (validate-json json-validator json)
  (if (eq? (*json-map-type*) 'vector)
      ((json-validator-validator json-validator) json)
      (validate-json json-validator (alist-json->vector-json json))))

(define (json-schema->json-validator schema)
  (define (convert schema)
    (let ((r (alist-json->vector-json schema)))
      (unless (vector? r)
	(assertion-violation 'json-schema->json-validator
			     "JSON object is required" schema))
      r))
  (if (vector? schema)
      (->json-validator schema)
      (->json-validator (convert schema))))

(define (->json-validator schema)
  (define (key=? key) (lambda (e) (and (pair? e) (string=? (car e) key) e)))
  (define value-of
    (case-lambda
     ((key schema) (value-of key schema #f))
     ((key schema default)
      (cond ((vector-any (key=? key) schema) => cdr)
	    (else default)))))
  (define $schema (value-of "$schema" schema "http://json-schema.org/schema#"))
  ;; we don't do draft-4 or earlier
  (define $id (value-of "$id" schema))
  (define type (value-of "type" schema))

  (make-json-validator schema $schema $id
   (cond ((and type (assoc type +json-schema-type-sub-validators+)) =>
	  (lambda (slot)
	    ;; (for-each (lambda (kv) (disasm (cadr kv))) (cdr slot))
	    (fold-left
	     (lambda (combined-validator prop&generator)
	       (cond ((value-of (car prop&generator) schema) =>
		      (lambda (v)
			(let ((generator (cadr prop&generator)))
			  (if generator
			      (let ((validator (generator v)))
				(lambda (e)
				  (and (combined-validator e) (validator e))))
			      (error '->json-validator "not yet")))))
		     ;; for unknown property we ignore
		     (else combined-validator)))
	     (lambda (e) #t) (cdr slot))))
	 (else (lambda (e) #t)))))

)
