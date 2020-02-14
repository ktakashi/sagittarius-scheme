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
	    (srfi :127)
	    (sagittarius generators))

(define (xpath:compile-string xpath-string)
  (xpath:compile (open-string-input-port xpath-string)))

(define (xpath:compile in)
  (define (wrap evaluator)
    (lambda (dom)
      ;; TODO chheck dom?
      (evaluator (make-xpath-context (document-document-element dom)))))
  (let ((expr* (xpath:parse (generator->lseq (port->char-generator in)))))
    ;; TODO how should we handle multiple expression?
    ;;      for now, pipe
    (wrap 
     (fold-left (lambda (acc expr)
		  (let ((evaluator (xpath:compile1 expr)))
		    (lambda (context)
		      (acc (evaluator context)))))
		(lambda (context) (xpath-context-dom context)) expr*))))

;; internal
(define-record-type xpath-context
  (fields dom))

(define (xpath:compile1 expr)
  (if (and (pair? expr) (pair? (car expr)))
      (xpath:compile-path expr)
      (lambda (context)
	(xqt-error 'unknown 'xpath:compile "not yet"))))

(define (xpath:compile-path expr)
  (fold-left (lambda (acc segment)
	       (let ((evaluator (xpath:compile-path-segment segment)))
		 (lambda (context)
		   (acc (evaluator context)))))
	     (lambda (context) context) expr))
(define (xpath:compile-path-segment segment)
  (define (element-searcher type)
    (case type
      ;; ((//) deep-searcher)
      ((/)  #f)
      (else
       (assertion-violation
	'xpath:compile-path-segment "Invalid type"
	segment))))
  (define (element-predicator pred)
    (if (string? pred)
	(lambda (dom)
	  (and (equal? pred (node-node-name dom))
	       dom))
	(assertion-violation 'xpath:compile-path-segment
			     "Not yet" pred)))
  (let ((searcher (element-searcher (car segment)))
	(predicator (element-predicator (cadr segment))))
    (lambda (context)
      (define dom (xpath-context-dom context))
      ;; I'll think // later on...
      (let ((r (predicator dom)))
	(make-xpath-context r)))))
	     
)
