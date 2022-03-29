;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dom/converter.scm - DOM -> SXML
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
(library (text xml dom converter)
    (export dom->sxml
	    dom->sxml-options?
	    dom->sxml-options-builder)
    (import (rnrs)
	    (record builder)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :117 list-queues)
	    (text xml dom nodes)
	    (text xml dom util))

(define-record-type dom->sxml-options
  (fields use-prefix?
	  document-processor
	  element-processor))
(define-syntax dom->sxml-options-builder
  (make-record-builder dom->sxml-options))

(define (default-processor next element) (next element))

(define default-option (dom->sxml-options-builder))

(define (option->node-processor options)
  (define document-processor
    (or (dom->sxml-options-document-processor options) default-processor))
  (define element-processor
    (or (dom->sxml-options-element-processor options) default-processor))
  (lambda (next node)
    (define type (node-node-type node))
    (cond ((= +element-node+ type) (element-processor next node))
	  ((= +document-node+ type) (document-processor next node))
	  (else (default-processor next node)))))

(define dom->sxml
  (case-lambda
   ((dom) (dom->sxml dom default-option))
   ((dom options)
    (define (emit dom body)
      ;; TODO collect namespace from the document
      `(*TOP* 
	      ;; TODO emit xml decl here
	      ,body))
    (let ((prefix? (dom->sxml-options-use-prefix? options))
	  (node-processor (option->node-processor options)))
      (node-processor
       (lambda (doc)
	 (let ((tag-emitter (if prefix? qname->tag namespace-tag->tag)))
	   (emit doc
	    (if (document? doc)     
		(filter-map (lambda (node)
			      (node->sxml doc node
					  tag-emitter
					  node-processor))
			    (list-queue-list (node-children doc)))
		(node->sxml doc doc tag-emitter node-processor)))))
       dom)))))

(define (qname->tag element) (string->symbol (element-tag-name element)))
(define (namespace-tag->tag element)
  (let ((ns (element-namespace-uri element)))
    (if ns
	(string->symbol (string-append ns ":" (element-local-name element)))
	(string->symbol (element-local-name element)))))

(define (node->sxml root node tag-emiter node-processor)
  (let ((type (node-node-type node)))
    (cond ((= +element-node+ type)
	   (element->sxml root node tag-emiter node-processor))
	  ((= +attribute-node+ type)
	   (attribute->sxml root node tag-emiter node-processor))
	  ((or (= +cdata-section-node+ type) (= +text-node+ type))
	   (text->sxml root node tag-emiter node-processor))
	  ((= +entity-reference-node+ type)
	   (entity->sxml root node tag-emiter node-processor))
	  ((= +comment-node+ type)
	   (comment->sxml root node tag-emiter node-processor))
	  ((= +processing-instruction-node+ type)
	   (pi->sxml root node tag-emiter node-processor))
	  ((= +document-type-node+ type)
	   (doctype->sxml root node tag-emiter node-processor))
	  (else (assertion-violation 'node->sxml "Unknown type" type)))))

  
(define (element->sxml root node tag-emiter node-processor)
  (define (proceed elm)
    (define tag (tag-emiter elm))
    (define attr (element-attributes elm))
    (define (namespace-prefix? attr)
      (define name (attr-name attr))
      (or (string-prefix? "xmlns" name) (string-prefix? "xml:" name)))
    (define (do-attr attr v r)
      (if (namespace-prefix? attr)
	  r
	  (cons (node->sxml root attr tag-emiter node-processor) r)))
    (define (do-ns attr v r)
      (define (strip-prefix n)
	(cond ((string-prefix? "xmlns" n)
	       (if (= (string-length n) 5) ;; xmlns
		   '*default*
		   (string->symbol (substring n 6 (string-length n)))))
	      ((string-prefix? "xml:" n)
	       (string->symbol (substring n 4 (string-length n))))))
      (if (and (namespace-prefix? attr)
	       (not (element:skip-namespace? elm attr root #t)))
	  (cons (list (strip-prefix (attr-name attr)) (attr-value attr)) r)
	  r))
    (define (do-it elm)
      (define (merge-attr ns attrs)
	(cond ((and (null? ns) (null? attrs)) '())
	      ((null? ns) `((@ ,@attrs)))
	      (else `((@ (@ (*NAMESPACES* ,@ns)) ,@attrs)))))
      
      (let ((attrs (named-node-map:fold attr '() do-attr))
	    (ns (named-node-map:fold attr '() do-ns)))
	`(,tag ,@(merge-attr ns attrs)
	       ,@(map (lambda (c)
			(node->sxml root c tag-emiter node-processor))
		      (list-queue-list (node-children elm))))))
    (let ((ns (element:collect-parent-namespaces elm #t)))
      (if (null? ns)
	  (do-it elm)
	  (dynamic-wind
	      (lambda ()
		(for-each (lambda (a) (element:set-attribute-node! elm a)) ns))
	      (lambda () (do-it elm))
	      (lambda ()
		(for-each (lambda (a) (element:remove-attribute-node! elm a))
			  ns))))))
  (node-processor proceed node))
  

(define (attribute->sxml root node tag-emiter node-processor)
  (define (proceed attr)
    (define tag (tag-emiter attr))
    `(,tag ,(attr-value attr)))
  (node-processor proceed node))

(define (text->sxml root node tag-emiter node-processor)
  (define (proceed text)
    (if (char-ref-text? text)
	(string (integer->char (caddr (node-source text))))
	(character-data-data text)))
  (node-processor proceed node))

(define (entity->sxml root node tag-emiter node-processor)
  (define (proceed en) `(& ,(node-node-name en)))
  (node-processor proceed node))

(define (comment->sxml root node tag-emiter node-processor)
  (define (proceed c) `(*COMMENT* ,(character-data-data c)))
  (node-processor proceed node))

(define (pi->sxml root node tag-emiter node-processor)
  (define (proceed pi)
    (let ((data (character-data-data pi)))
      `(*PI* ,(processing-instruction-target pi)
	     ,@(if (zero? (string-length data))
		   '()
		   (list data)))))
  (node-processor proceed node))

(define (doctype->sxml root node tag-emiter node-processor)
  (define (proceed doctype) #f) ;; not sure what to do
  (node-processor proceed node))
)
