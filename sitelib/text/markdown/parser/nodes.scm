;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/nodes.scm - Node of markdown
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
(library (text markdown parser nodes)
    (export (rename (document? markdown-document?))
	    make-markdown-document
	    make-paragraph-node paragraph-node?
	    make-block-quote-node block-quote-node?
	    make-list-node list-node?
	    make-code-block-node code-block-node?
	    make-heading-node heading-node?
	    make-thematic-break-node thematic-break-node?
	    make-html-block-node html-block-node?
	    make-custom-block-node custom-block-node?

	    *commonmark-namespace*
	    (rename (node:append-child! markdown-node:append-child!))
	    markdown-node:set-attribute!
	    )
    (import (rnrs)
	    (text xml dom))

(define *commonmark-namespace* "http://commonmark.org/xml/1.0")

(define-syntax namespace (syntax-rules ()))
(define-syntax element (syntax-rules ()))
(define-syntax define-markdown-node
  (lambda (x)
    (define (parse-clause name clause*)
      (define (rec clause* ns e)
	(syntax-case clause* (namespace element)
	  (((namespace ns) rest ...)
	   (rec #'(rest ...) #'ns e))
	  (((element e) rest ...)
	   (rec #'(rest ...) ns #'e))
	  (() (list ns e))
	  (_ (syntax-violation 'define-markdown-node "malformed clause"
			       (syntax->datum x)
			       (syntax->datum clause*)))))
      (rec clause* #'*commonmark-namespace* name))
    (define (make-ctr&pred k name)
      (define n (symbol->string (syntax->datum name)))
      (datum->syntax k (map string->symbol
			    (list (string-append "make-" n "-node")
				  (string-append n "-node?")))))
    (syntax-case x ()
      ((k name)
       #'(k name (namespace *commonmark-namespace*) (element name)))
      ((k name clause* ...)
       (with-syntax (((ns element) (parse-clause #'name #'(clause* ...)))
		     ((ctr pred) (make-ctr&pred #'k #'name)))
	 #'(begin
	     (define element-name (symbol->string 'element))
	     (define (ctr parent)
	       (define doc (if (document? parent)
			       parent
			       (node-owner-document parent)))
	       (document:create-element-ns doc ns element-name))
	     (define (pred e)
	       (and (element? e)
		    (equal? (element-namespace-uri e) ns)
		    (equal? (element-local-name e) element-name)))))))))

(define markdown-node:set-attribute!
  (case-lambda
   ((node name value)
    (markdown-node:set-attribute! node *commonmark-namespace* name value))
   ((node ns name value)
    (element:set-attribute-ns! node ns name value))))

(define-markdown-node document)
(define (make-markdown-document)
  (let* ((doc (make-xml-document))
	 (e (make-document-node doc)))
    (node:append-child! doc e)
    e))
(define-markdown-node paragraph)
(define-markdown-node block-quote (element block_quote))
(define-markdown-node list)
(define-markdown-node code-block (element code_block))
(define-markdown-node heading)
(define-markdown-node thematic-break (element thematic_break))
(define-markdown-node html-block)
(define-markdown-node custom-block (element custom_block))

;; TODO inline block

)
