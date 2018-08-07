;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/yaml/builder.scm - YAML - SEXP builder
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

(library (text yaml builder)
    (export yaml->sexp)
    (import (rnrs)
	    (text yaml conditions)
	    (text yaml nodes)
	    (text yaml tags)
	    (rfc base64))

(define yaml->sexp
  (case-lambda
   ((node) (yaml->sexp node +default-yaml-builders+))
   ((node builders)
    (cond ((yaml-document? node)
	   (yaml->sexp (yaml-document-root-node node) builders))
	  ((yaml-node? node) (build-sexp node builders))
	  (else (assertion-violation 'yaml->sexp
				     "YAML node or document required" node))))))

(define (build-sexp node builders)
  (define tag (yaml-node-tag node))
  (define (find-builder builder)
    (let ((pred (car builder))
	  (expected-tag (cadr builder)))
      (and (pred node) (string=? expected-tag tag))))
  (let ((builder (find find-builder builders)))
    (unless builder
      (raise (condition
	      (make-yaml-error)
	      (make-who-condition 'yaml->sexp)
	      (make-message-condition
	       "The node/tag is not supported with the given set of builders")
	      (make-irritants-condition node))))
    ((cddr builder) node
	     (lambda (next-node)
	       (when (eq? next-node node)
		 (assertion-violation 'yaml->sexp
				      "Recursive building of the node" node))
	       (build-sexp next-node builders)))))

;; the builder doesn't validate input, this is because we don't expect
;; builder to be used without parser
(define (->binary v) (base64-decode-string v :transcoder #f))
(define (->bool v)
  (or (and (memv (char-downcase (string-ref v 0)) '(#\y #\t)) #t)
      (string=? (string-downcase v) "on")))

(define (entry pred tag builder) (cons* pred tag builder))
(define (single-valued p) (lambda (node builders) (p (yaml-node-value node))))
(define (single-entry pred tag p) (cons* pred tag (single-valued p)))
(define +default-yaml-builders+
  `(
    ,(single-entry yaml-scalar-node? +yaml-tag:str+ values)
    ,(single-entry yaml-scalar-node? +yaml-tag:binary+ ->binary)
    ,(single-entry yaml-scalar-node? +yaml-tag:bool+ ->bool)
    ))

)
