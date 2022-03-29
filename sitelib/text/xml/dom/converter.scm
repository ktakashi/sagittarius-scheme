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
	    (text xml dom nodes))

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
    (define (emit dom namespaces body)
      `(*TOP* ,@(if namespaces
		    `((@ (*NAMESPACES*
			  ,@(delete-duplicates (list-queue-list namespaces)))))
		    '())
	      ;; TODO emit xml decl here
	      ,body))
    (let ((prefix? (dom->sxml-options-use-prefix? options))
	  (namespaces (list-queue))
	  (node-processor (option->node-processor options)))
      (node-processor
       (lambda (doc)
	 (let ((tag-emitter (if prefix? qname->tag namespace-tag->tag)))
	   (emit doc (and prefix? namespaces)
	    (if (document? doc)     
		(filter-map (lambda (node)
			      (node->sxml node
					  tag-emitter
					  namespaces
					  node-processor))
			    (list-queue-list (node-children doc)))
		(node->sxml doc tag-emitter namespaces node-processor)))))
       dom)))))

(define (qname->tag element) (string->symbol (element-tag-name element)))
(define (namespace-tag->tag element)
  (let ((ns (element-namespace-uri element)))
    (if ns
	(string->symbol (string-append ns ":" (element-local-name element)))
	(string->symbol (element-local-name element)))))

(define (node->sxml node tag-emiter namespaces node-processor)
  (let ((type (node-node-type node)))
    (cond ((= +element-node+ type)
	   (element->sxml node tag-emiter namespaces node-processor))
	  ((= +attribute-node+ type)
	   (attribute->sxml node tag-emiter namespaces node-processor))
	  ((or (= +cdata-section-node+ type) (= +text-node+ type))
	   (text->sxml node tag-emiter namespaces node-processor))
	  ((= +entity-reference-node+ type)
	   (entity->sxml node tag-emiter namespaces node-processor))
	  ((= +comment-node+ type)
	   (comment->sxml node tag-emiter namespaces node-processor))
	  ((= +processing-instruction-node+ type)
	   (pi->sxml node tag-emiter namespaces node-processor))
	  ((= +document-type-node+ type)
	   (doctype->sxml node tag-emiter namespaces node-processor))
	  (else (assertion-violation 'node->sxml "Unknown type" type)))))

;; SSAX doesn't provide any capability of using the same namespace prefix
;; multiple times in different elements, so this is okay and keep it
;; users responsibility for now... (i.e. use fully qualified name)
(define (collect-namespace! node namespaces)
  (define ns (namespace-aware-namespace-uri node))
  (define prefix (namespace-aware-prefix node))
  ;; Unfortunately SSAX doesn't provide capability to handle default namespace
  ;; (maybe I overlooked), for now I put special mark *default*
  (cond ((and ns (not (zero? (string-length prefix))))
	 (list-queue-add-back! namespaces (list (string->symbol prefix) ns)))
	;; okay default namespace
	(ns (list-queue-add-back! namespaces (list '*default* ns)))))
  
(define (element->sxml node tag-emiter namespaces node-processor)
  (define (proceed elm)
    (define tag (tag-emiter elm))
    (define attr (element-attributes elm))
    (define (do-attr attr v r)
      (define name (attr-name attr))
      (if (or (string-prefix? "xmlns" name) (string-prefix? "xml:" name))
	  r
	  (cons (node->sxml attr tag-emiter namespaces node-processor) r)))
    (collect-namespace! elm namespaces)
    (let ((attrs (named-node-map:fold attr '() do-attr)))
    `(,tag ,@(if (null? attrs) '() `((@ ,@attrs)))
	   ,@(map (lambda (c)
		    (node->sxml c tag-emiter namespaces node-processor))
		  (list-queue-list (node-children elm))))))
  (node-processor proceed node))
  

(define (attribute->sxml node tag-emiter namespaces node-processor)
  (define (proceed attr)
    (define tag (tag-emiter attr))
    (collect-namespace! node namespaces)
    `(,tag ,(attr-value attr)))
  (node-processor proceed node))

(define (text->sxml node tag-emiter namespaces node-processor)
  (define (proceed text)
    (if (char-ref-text? text)
	(string (integer->char (caddr (node-source text))))
	(character-data-data text)))
  (node-processor proceed node))

(define (entity->sxml node tag-emiter namespaces node-processor)
  (define (proceed en) `(& ,(node-node-name en)))
  (node-processor proceed node))

(define (comment->sxml node tag-emiter namespaces node-processor)
  (define (proceed c) `(*COMMENT* ,(character-data-data c)))
  (node-processor proceed node))
(define (pi->sxml node tag-emiter namespaces node-processor)
  (define (proceed pi)
    (let ((data (character-data-data pi)))
      `(*PI* ,(processing-instruction-target pi)
	     ,@(if (zero? (string-length data))
		   '()
		   (list data)))))
  (node-processor proceed node))

(define (doctype->sxml node tag-emiter namespaces node-processor)
  (define (proceed doctype) #f) ;; not sure what to do
  (node-processor proceed node)
  )
)
