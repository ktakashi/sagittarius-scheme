;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/output.scm - Document output
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius document output)
    (export (rename (document-output-options <document-output-options>))
	    document-output-options?
	    document-output-options-default-codeblock-language
	    document-output-options-link-source-callback
	    document-output-options-unknown-node-handler
	    document-output-options-builder

	    document-output:make-file-link-callback
	    document-decompose
	    document-element-of?

	    document-output-error document-output-error? &document-output

	    ;; For writer implementation
	    (rename (document-writer <document-writer>))
	    document-writer?
	    document-writer-output-port
	    document-writer-options
	    document-writer-node-handlers
	    document-writer-string-handler
	    document-writer-datum-handler
	    default-unknown-node-handler
	    traverse-document
	    )
    (import (rnrs)
	    (record builder)
	    (sagittarius document conditions)
	    (text sxml tools)
	    (util file))

(define-condition-type &document-output &document
  make-document-output-error document-output-error?)
(define (document-output-error who message . irr)
  (raise (condition (make-document-output-error)
		    (make-who-condition who)
		    (make-message-condition message)
		    (make-irritants-condition irr))))

(define-record-type document-output-options
  (fields default-codeblock-language
	  link-source-callback
	  unknown-node-handler))

(define (default-unknown-node-handler writer name attr content travarse)
  (document-output-error 'write-markdown "Unknown element"
			 (cons* name attr content)))

(define-syntax document-output-options-builder
  (make-record-builder document-output-options
    ((unknown-node-handler default-unknown-node-handler))))

(define (document-output:make-file-link-callback ext)
  (lambda (source format writer)
    (let ((file (string-append (path-sans-extension source) ext)))
      (when writer (writer (list file) file))
      file)))

(define (document-decompose doc)
  (let ((name (car doc))
	(attr (sxml:attr-list doc))
	(content (sxml:content doc)))
    (values name attr content)))

(define (document-element-of? e name)
  (and (pair? e) (eq? (car e) name)))

(define-record-type document-writer
  (fields output-port
	  options
	  node-handlers
	  string-handler
	  datum-handler))

(define (traverse-document writer e)
  (define (noop e))
  (define (call-node-handler writer name attr content)
    (define node-handlers (document-writer-node-handlers writer))
    (let ((handler (cond ((assq name node-handlers) => cadr)
			 (else (document-output-options-unknown-node-handler
				(document-writer-options writer))))))
      (handler writer name attr content traverse)))
  (define (traverse e)
    (cond ((string? e)
	   ((document-writer-string-handler writer) writer e noop))
	  ((pair? e)
	   (let-values (((name attr content) (document-decompose e)))
	     (call-node-handler writer name attr content)))
	  (else
	   ((document-writer-datum-handler writer) writer e noop))))
  (traverse e))


)
