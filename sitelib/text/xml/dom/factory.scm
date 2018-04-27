;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dom/factory.scm - DOM tree factory
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

#!nounbound
(library (text xml dom factory)
    (export input-port->dom-tree
	    xml-file->dom-tree
	    ;; sxml->dom-tree

	    ;; write-dom-tree

	    ;; options
	    make-xml-document-factory-options
	    xml-document-factory-options?)
    (import (rnrs)
	    (peg)
	    (text xml dom parser)
	    (text xml dom nodes)
	    (sagittarius generators)
	    (srfi :1 lists)
	    (srfi :39 parameters)
	    (srfi :127 lseqs))

;;; utility 
(define (parse-xml in)
  (let-values (((s v n)
		($xml:document (generator->lseq (port->char-generator in)))))
    (cond ((and (parse-success? s) (null? n)) v)
	  ((and (parse-success? s) (not (null? n)))
	   (error 'parse-xml "XML document contains extra data" n))
	  (else (error 'parse-xml "Failed to parse XML document" v)))))

;; TODO maybe this should be move to constructing part
(define-record-type xml-document-factory-options
  (fields namespace-aware?
	  xinclude-aware?
	  validating?
	  whitespace?
	  expand-entity-reference?
	  expand-character-reference?
	  ignore-comments?
	  coalescing?)
  (protocol (lambda (p)
	      ;; the options are taken from Java.
	      (lambda (:key (namespace-aware? #t)
			    (xinclude-aware? #f)
			    (validating? #f)
			    (whitespace? #f)
			    (expand-entity-reference? #t)
			    (expand-character-reference? #t)
			    (ignore-comments? #f)
			    (coalescing? #f))
		(p namespace-aware? xinclude-aware?
		   validating? whitespace? expand-entity-reference?
		   expand-character-reference?
		   ignore-comments? coalescing?)))))
(define-syntax %expand-entity?
  (syntax-rules ()
    ((_)
     (xml-document-factory-options-expand-entity-reference?
      (*factory-options*)))))
(define-syntax %expand-char-ref?
  (syntax-rules ()
    ((_)
     (xml-document-factory-options-expand-character-reference?
      (*factory-options*)))))

(define +default-factory-option+ (make-xml-document-factory-options))

;; internal parameter
(define *factory-options* (make-parameter #f))
(define *root-document* (make-parameter #f))

(define (input-port->dom-tree in :optional (option +default-factory-option+))
  (let ((parsed (parse-xml in))
	(document (make-root-document #f)))
    (parameterize ((*factory-options* option)
		   (*root-document* document))
      (dispatch-factory parsed)
      document)))

(define (xml-file->dom-tree file . opt)
  (call-with-input-file file
    (lambda (in) (apply input-port->dom-tree in opt))))

(define *factory-table* (make-eq-hashtable))
(define (dispatch-factory tree)
  (cond ((pair? tree)
	 (let ((name (car tree)))
	   (cond ((hashtable-ref *factory-table* name #f) =>
		  (lambda (proc) (proc tree (*root-document*))))
		 (else tree))))
	((string? tree)
	 (document:create-text-node (*root-document*) tree))
	(else 
	 (assertion-violation 'document-factory "Unknown value" tree))))
(define-syntax define-factory
  (lambda (x)
    (define (->name name)
      (string->symbol
       (string-append (symbol->string (syntax->datum name)) "-factory")))
    (syntax-case x ()
      ((k (name root-document) body ...)
       (with-syntax ((defname (datum->syntax #'k (->name #'name))))
	 #'(define defname
	     (let ((proc (lambda (name root-document) body ...)))
	       (hashtable-set! *factory-table* 'name proc)
	       proc)))))))

;; The factories depend on the correctness of the input tree.
;; at this moment, we don't validate.
(define-factory (document root-document)
  (dispatch-factory (cadr document)) ;; prolog
  (let ((element (dispatch-factory (caddr document)))
	(misc (map dispatch-factory (cdddr document))))
    (node:append-child! root-document element)
    (document-document-element-set! root-document element)
    (for-each (lambda (node) (node:append-child! root-document node)) misc)))

;; prolog will be handled destructively
(define-factory (prolog root-document)
  (cond ((cadr prolog) => dispatch-factory))
  (let ((misc1 (map dispatch-factory (cdaddr prolog)))
	(doctype (cond ((cadddr prolog) => dispatch-factory) (else #f)))
	(misc2 (map dispatch-factory (cdar (cddddr prolog)))))
    (for-each (lambda (node) (node:append-child! root-document node)) misc1)
    (when doctype
      (document-doctype-set! root-document doctype)
      (node:append-child! root-document doctype))
    (for-each (lambda (node) (node:append-child! root-document node)) misc2)))

(define-factory (xml-decl root-document)
  (let ((version (cadr xml-decl))
	(encode (caddr xml-decl))
	(standalone (cadddr xml-decl)))
    ;; put info to root-document
    (document-xml-version-set! root-document (cadr version))
    (when encode (document-character-set-set! root-document (cadr encode)))
    (when standalone
      (document-xml-standalone?-set! root-document
				     (string=? (cadr standalone) "yes")))))

(define-factory (comment root-document)
  (document:create-comment root-document (cadr comment)))

(define-factory (PI root-document)
  (document:create-processing-instruction root-document (cadr PI) (caddr PI)))

(define-factory (!doctype root-document)
  (define (parse-id id)
    (cond ((not id) (values #f #f))
	  ((eq? (car id) 'system) (values #f (cadr id)))
	  ((eq? (car id) 'public) (values (cadr id) (caddr id)))
	  (else (assertion-violation '!doctype "Invalid external ID" id))))
  (define (handle-subset doctype subset)
    (define (set-it table e)
      (define key (node-node-name e))
      (if (hashtable-contains? table key)
	  (assertion-violation '!doctype "Duplicate definition" key e)
	  (hashtable-set! table key e)))
    (let ((e (dispatch-factory subset)))
      (cond ((entity? e) (set-it (document-type-entities doctype) e))
	    ((element-type? e) (set-it (document-type-elements doctype) e))
	    ;; TODO the rest
	    )))
  (let ((name (cadr !doctype))
	(id (caddr !doctype))
	(subsets (cadddr !doctype)))
    (let-values (((public-id system-id) (parse-id id)))
      (let ((doctype (document:create-document-type root-document
						    name public-id system-id)))
	(for-each (lambda (subset) (handle-subset doctype subset))
		  (cdr subsets))
	doctype))))

(define-factory (!entity root-document)
  (define (handle-parameter-entity entity)
    (assertion-violation '!entity "not supported yet"))
  (define (handle-general-entity entity)
    (let ((name (car entity))
	  (value (cadr entity)))
      (cond ((eq? (car value) 'entity-value)
	     (document:create-entity/value root-document name (cadr value)))
	    ((eq? (car value) 'public)
	     (let ((maybe-notation (cdddr value)))
	       (if (null? maybe-notation)
		   (apply document:create-entity/public-id root-document name
			  (cdr value))
		   (document:create-entity/public-id root-document name
						     (cadr value) (caddr value)
						     (cadar maybe-notation)))))
	    ((eq? (car value) 'system)
	     (let ((maybe-notation (cddr value)))
	       (if (null? maybe-notation)
		   (document:create-entity/system-id root-document name
						     (cadr value))
		   (document:create-entity/system-id root-document name
						     (cadr value)
						     (cadar maybe-notation)))))
	    (else
	     (assertion-violation 'general-entity "unknown entity" !entity)))))
  (if (eq? 'pe (cadr !entity))
      (handle-parameter-entity (cddr !entity))
      (handle-general-entity (cddr !entity))))

(define-factory (!element root-document)
  (let ((name (cadr !element))
	(spec (caddr !element)))
    (document:create-element-type root-document name spec)))
    
(define-factory (element root-document)
  (define (make-element name)
    (if (qname? name)
	(document:create-element-qname root-document
				       (qname-namespace name)
				       (qname-prefix name)
				       (qname-local-part name))
	(document:create-element root-document name)))
  (define (->attribute-node attribute)
    (define (set-value attr)
      (attr-value-set! attr (cadr attribute))
      attr)
    (let ((name (car attribute)))
      (cond ((qname? name)
	     (set-value
	      (document:create-attribute-qname root-document
					       (qname-namespace name)
					       (qname-prefix name)
					       (qname-local-part name))))
	    ((string? name)
	     (set-value (document:create-attribute root-document name)))
	    (else
	     ;; xmlns
	     ;; it seems like this (at least on Firefox)
	     (let ((local-name (cadr name))
		   (namespace "http://www.w3.org/2000/xmlns/"))
	       (set-value
		(if local-name
		    (document:create-attribute-qname root-document
						     namespace "xmlns"
						     local-name)
		    (document:create-attribute-qname root-document
						     namespace ""
						     "xmlns"))))))))
  (define (merge-text elements)
    (define (create-empty-text) (document:create-text-node root-document #f))
    (let-values (((out extract) (open-string-output-port)))
      (define (flush text)
	(when text (character-data-data-set! text (extract))))
      (let loop ((elements elements) (r '()) (text #f))
	(if (null? elements)
	    (begin (flush text) (reverse! r))
	    (let ((element (car elements)))
	      (cond ((and (eqv? (node-node-type element) +text-node+)
			  (not (char-ref-text? element)))
		     (let ((new (or text (create-empty-text))))
		       (put-string out (character-data-data element))
		       (loop (cdr elements)
			     (if text r (cons new r))
			     new)))
		    (else
		     (flush text)
		     (loop (cdr elements) (cons (car elements) r) #f))))))))
		    
  (let ((name (cadr element))
	(attributes (caddr element))
	(content (cdddr element)))
    (define elm (make-element name))
    (for-each (lambda (attr) (element:set-attribute-node-ns! elm attr))
	      (filter-map ->attribute-node (cdr attributes)))
    (for-each (lambda (node)
		(node-parent-node-set! node elm)
		(node-parent-element-set! node elm)
		(node:append-child! elm node))
	      (merge-text (map dispatch-factory content)))
    elm))

(define-factory (entity-ref root-document)
  (define name (cadr entity-ref))
  (if (%expand-entity?)
      (let ((entities (document-type-entities
		       (document-doctype root-document))))
	(cond ((hashtable-ref entities name) =>
	       (lambda (e)
		 (cond ((entity-entity-value e) =>
			(lambda (value)
			  (document:create-text-node root-document value)))
		       (else (assertion-violation
			      'entity-ref "External entity is not supported yet"
			      name)))))
	      (else (assertion-violation 'entity-ref "Unknown entity name"
					 name))))
      (document:create-entity-reference root-document name)))

(define-factory (char-ref root-document)
  (let ((s (string (integer->char (caddr char-ref)))))
    (if (%expand-char-ref?)
	(document:create-text-node root-document s)
	(document:create-char-ref-text root-document s char-ref))))

(define-factory (cdata root-document)
  (document:create-cdata-section root-document (cadr cdata)))
)
