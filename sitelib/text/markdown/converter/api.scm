;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/converter/api.scm - Markdown converter API
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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
(library (text markdown converter api)
    (export (rename (markdown-converter <markdown-converter>))
	    make-markdown-converter markdown-converter?

	    (rename (markdown-conversion <markdown-conversion>))
	    make-markdown-conversion markdown-conversion?

	    markdown-conversion-options?
	    markdown-conversion-options-builder
	    
	    define-markdown-converter
	    markdown-converter:convert
	    markdown-converter:merge
	    )
    (import (rnrs)
	    (record builder)
	    (srfi :1 lists)
	    (text markdown parser nodes))

(define-record-type markdown-conversion
  (fields type
	  predicate
	  processor))

(define-record-type markdown-converter
  (fields processors))

(define-record-type markdown-conversion-options
  (fields unknown-node-handler
	  context-data))

(define-syntax markdown-conversion-options-builder
  (make-record-builder markdown-conversion-options))
(define default-option (markdown-conversion-options-builder))

(define markdown-converter:convert
  (case-lambda
   ((converter type node)
    (markdown-converter:convert converter type node default-option))
   ((converter type node options)
    (define processors
      (filter (lambda (p) (eq? type (markdown-conversion-type p)))
	      (markdown-converter-processors converter)))
    (define context-data (markdown-conversion-options-context-data options))
    
    (define (default-unknown-node-handler node)
      (raise-continuable
       (condition
	(make-who-condition 'markdown-converter:convert)
	(make-message-condition "Unknown markdown node")
	(make-irritants-condition (list node type)))))
  
    (define (find-processor processors node)
      (find (lambda (processor)
	      ((markdown-conversion-predicate processor) node)) processors))
    
    (define (convert node)
      (define (process processor node)
	((markdown-conversion-processor processor) node context-data convert))
      (let ((processor (find-processor processors node)))
	(or (and processor (process processor node))
	    (cond ((markdown-conversion-options-unknown-node-handler options) =>
		   (lambda (handler) (handler node)))
		  (else (default-unknown-node-handler node))))))
    (convert node))))

(define (markdown-converter:merge converter . converter*)
  (if (null? converter*)
      converter
      (make-markdown-converter
       (append-map markdown-converter-processors (cons converter converter*)))))

(define-syntax define-markdown-converter
  (syntax-rules ()
    ((_ name type (pred proc) ...)
     (define name
       (make-markdown-converter
	(list (make-markdown-conversion 'type pred proc) ...))))))

)
