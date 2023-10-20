;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators/containers.scm - JSON schema container validators
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
(library (text json schema validators containers)
    (export json-schema:array json-schema:object)
    (import (rnrs)
	    (sagittarius regex)
	    (text json schema validators primitives)
	    (srfi :133 vectors))

;; Array validator
;; (items, additional-items) -> (obj, path) -> boolean
;; `items` can be a list of validator or a validator
;; `additional-items` must be a validator
;; It is caller's responsibility to handle which draft it's using
(define (json-schema:array items additional-items)
  (define (check-additional-items validator path i o)
    (do ((i i (+ i 1)) (o o (cdr o))
	 (r #t (and r (validator (car o) (build-pointer path i)))))
	((null? o) r)))
  (if (pair? items)
      (lambda (e path)
	(and (pair? e)
	     (let loop ((i 0) (o e) (v items))
	       (cond ((null? items)
		      (check-additional-items additional-items path i o))
		     ((null? o))
		     (else
		      (let ((this-path (build-pointer path i)))
			(and ((car v) (car o) this-path)
			     (loop (+ i 1) (cdr o) (cdr v)))))))))
      ;; single items
      (lambda (e path)
	(and (pair? e)
	     (let loop ((i 0) (o e) (v items))
	       (or (null? o)
		   (let ((this-path (build-pointer path i)))
		     (and (items (car o) this-path)
			  (loop (+ i 1) (cdr o) (cdr v))))))))))
		     
;; Object validator
;; we need to have three types of validators,
;; - property names
;; - properties (as well as pattern properties)
;; - additional properties
;; The first one will be applied to the property name itself (dominant)
;; The second one will be applied matching properties
;; The third one will be applied to the rest of the properties, which means
;; if the second one didn't match, then it'd be applied
;;
;; `property-names` must be a validator
;; `propertie` must be alist of string/regex as key, validator as value
;; `additional-properties` must be a validator
(define (json-schema:object property-names properties additional-properties)
  (define (match-properties prop properties)
    (define (match-property slot)
      (let ((name (car slot)))
	(cond ((string? name) (string=? name prop))
	      ((regex-pattern? name) (matches name prop))
	      ;; ??? shouldn't be just ignore for now
	      (else #f))))
    (cond ((find match-property properties) => cdr)
	  (else #f)))
  (define (check-object path v)
    (let* ((prop (car v))
	   (value (cdr v))
	   (this-path (build-pointer path prop)))
      (and (property-names prop path)
	   (cond ((match-properties prop properties) =>
		  (lambda (validator) (validator value this-path)))
		 (else
		  (additional-properties value this-path))))))
  (lambda (e path)
    (and (vector? e)
	 (vector-fold (lambda (acc v) (and acc (check-object path v))) #t e))))

;;; Utilities
(define (build-pointer base next)
  (string-append base "/" (if (number? next) (number->string next) next)))
)
