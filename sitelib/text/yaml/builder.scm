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
	    (rfc base64)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :115 regexp))

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

(define (->binary v) (base64-decode-string v :transcoder #f))
(define (->bool v)
  (or (and (memv (char-downcase (string-ref v 0)) '(#\y #\t)) #t)
      (string=? (string-downcase v) "on")))
(define (->int v)
  (define (strip-sign v)
    (case (string-ref v 0)
      ((#\-) (values #t (substring v 1 (string-length v))))
      ((#\+) (values #f (substring v 1 (string-length v))))
      (else  (values #f v))))
  (define (handle-number n radix) (string->number (string-delete #\_ n) radix))
  (define (handle-binary n) (handle-number n 2))
  (define (handle-hex n) (handle-number n 16))
  (define (handle-octet n) (handle-number n 8))
  (define (handle-decimal n) (handle-number n 10))
  (define (handle-sexagesimal n)
    (let ((tokens (reverse
		   (map string->number (string-tokenize n char-set:digit)))))
      (do ((tokens tokens (cdr tokens))
	   (coef 0 (+ coef 1))
	   (r 0 (+ (* (car tokens) (expt 60 coef)) r)))
	  ((null? tokens) r))))
  (let-values (((neg n) (strip-sign v)))
    (let ((i (case (string-ref n 0)
	       ((#\0)
		(case (string-ref n 1)
		  ((#\b) (handle-binary (substring n 2 (string-length n))))
		  ((#\x) (handle-hex (substring n 2 (string-length n))))
		  (else
		   (if (> (string-length n) 1)
		       (handle-octet (substring n 1 (string-length n)))
		       (handle-decimal n)))))
	       (else
		(if (string-index n #\:)
		    (handle-sexagesimal n)
		    (handle-decimal n))))))
      (if neg
	  (- i)
	  i))))
(define (entry pred tag builder) (cons* pred tag builder))
(define (single-valued p) (lambda (node builders) (p (yaml-node-value node))))
(define (single-entry pred tag p) (cons* pred tag (single-valued p)))
(define (regexp-pred pred sre)
  (define re (regexp sre))
  (lambda (node)
    (and (pred node)
	 (regexp-matches re (yaml-node-value node)))))

(define +default-yaml-builders+
  `(
    ,(single-entry yaml-scalar-node? +yaml-tag:str+ values)
    ,(single-entry yaml-scalar-node? +yaml-tag:binary+ ->binary)
    ,(single-entry (regexp-pred yaml-scalar-node? +yaml-regexp:bool+)
		   +yaml-tag:bool+ ->bool)
    ,(single-entry (regexp-pred yaml-scalar-node? +yaml-regexp:int+)
		   +yaml-tag:int+ ->int)
    ))

)
