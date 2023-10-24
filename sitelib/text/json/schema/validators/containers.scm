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
;; `additional-items` must be a validator or #f
;; `unevaluated-items` must be a validator or #f
;; It is caller's responsibility to handle which draft it's using
(define (json-schema:array items additional-items unevaluated-items)
  (define (check-additional-items validator path i o)
    (do ((i i (+ i 1)) (o o (cdr o))
	 (r #t (and r (validator (car o) (build-pointer path i)))))
	((null? o) r)))
  (define (check-extras additional unevaluated path i o)
    (cond (additional
	   (check-additional-items additional-items path i o))
	  (unevaluated
	   (check-additional-items unevaluated-items path i o))
	  ;; default true :)
	  (else #t)))
  (cond ((not items)
	 (lambda (e path)
	   ;; additionaItems without items must not be evaluated...
	   (check-extras #f unevaluated-items path 0 e)))
	((pair? items)
	 (lambda (e path)
	   (and (list? e)
		(let loop ((i 0) (o e) (v items))
		  (cond ((null? v)
			 (check-extras additional-items unevaluated-items
				       path i o))
			((null? o))
			(else
			 (let ((this-path (build-pointer path i)))
			   (and ((car v) (car o) this-path)
				(loop (+ i 1) (cdr o) (cdr v))))))))))
	(else
	 ;; single items
	 (lambda (e path)
	   (or (not (list? e))
	       (let loop ((i 0) (o e))
		 (or (null? o)
		     (let ((this-path (build-pointer path i)))
		       (and (items (car o) this-path)
			    (loop (+ i 1) (cdr o)))))))))))
		     
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
;; `additional` must be a validator or #f
;; `unevaluated` must be a validator or #f
(define (json-schema:object property-names properties additional unevaluated)
  (define (match-properties prop properties)
    (define (match-property slot)
      (let ((name (car slot)))
	(cond ((string? name) (string=? name prop))
	      ((regex-pattern? name) (looking-at name prop))
	      ;; ??? shouldn't be just ignore for now
	      (else #f))))
    (let ((r (map cdr (filter match-property properties))))
      (and (not (null? r)) r)))
  (define (check-object path v)
    (let* ((prop (car v))
	   (value (cdr v))
	   (this-path (build-pointer path prop)))
      (and (property-names prop path)
	   (cond ((match-properties prop properties) =>
		  (lambda (validators)
		    (for-all (lambda (v) (v value this-path)) validators)))
		 (additional (additional value this-path))
		 (unevaluated (unevaluated value this-path))
		 (else #t)))))
  (lambda (e path)
    (or (not (vector? e))
	(vector-fold (lambda (acc v) (and acc (check-object path v))) #t e))))

;;; Utilities
(define (build-pointer base next)
  (string-append base "/" (if (number? next) (number->string next) next)))
)
