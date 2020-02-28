;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/xpath/compiler.scm - XPath compiler
;;;
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
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
(library  (text xml xpath compiler)
    (export xpath:compile
	    xpath:compile-string)
    (import (rnrs)
	    (text xml errors)
	    (text xml dom nodes)
	    (text xml xpath parser)
	    (srfi :1)
	    (srfi :127)
	    (sagittarius generators))

(define (xpath:compile-string xpath-string)
  (xpath:compile (open-string-input-port xpath-string)))

(define (xpath:compile in)
  (define (wrap evaluator)
    (lambda (dom)
      ;; TODO chheck dom?
      (evaluator (make-xpath-context (list (document-document-element dom))
				     '()))))
  (let ((expr* (xpath:parse (generator->lseq (port->char-generator in)))))
    ;; TODO how should we handle multiple expression?
    ;;      for now, pipe
    (wrap 
     (fold-left (lambda (acc expr)
		  (let ((evaluator (xpath:compile1 expr)))
		    (lambda (context)
		      (acc (evaluator context)))))
		(lambda (context) (xpath-context-targets context)) expr*))))

;; internal
(define-record-type xpath-context
  (fields targets ;; also the results
	  ;; for now alist
	  variables))

(define (xpath:compile1 expr)
  (if (and (pair? expr) (pair? (car expr)))
      (xpath:compile-path expr)
      (lambda (context)
	(xqt-error 'unknown 'xpath:compile "not yet"))))

(define (node-list->list node-list)
  (do ((len (node-list-length node-list))
       (i 0 (+ i 1))
       (r '() (cons (node-list:item node-list i) r)))
      ((= i len) (reverse r))))

(define (xpath:compile-path expr)
  (fold-left (lambda (acc segment)
	       (let ((evaluator (xpath:compile-path-segment segment)))
		 (lambda (context)
		   (acc (evaluator context)))))
	     (lambda (context) context) expr))

(define (xpath:compile-path-segment segment)
  (define (filter-any pred)
    (if (string? pred)
	(lambda (dom)
	  (node-list->list (element:get-elements-by-tag-name dom pred)))
	(assertion-violation 'xpath:compile-path-segment "Not yet" pred)))
  
  (define (filter-root pred)
    (if (string? pred)
	(lambda (dom)
	  (or (and (string=? (node-node-name dom) pred) (list dom)) '()))
	(assertion-violation 'xpath:compile-path-segment "Not yet" pred)))
  
  (define (make-filter type pred)
    (case type
      ((//) (filter-any pred))
      ((/)  (filter-root pred))
      (else
       (assertion-violation
	'xpath:compile-path-segment "Invalid type"
	segment))))
  (let ((filter (make-filter (car segment) (cadr segment))))
    (lambda (context)
      (define dom (xpath-context-targets context))
      (let ((r (append-map filter dom)))
	(make-xpath-context r (xpath-context-variables context))))))
	     
)
