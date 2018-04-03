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
	  ignore-comments?
	  coalescing?)
  (protocol (lambda (p)
	      ;; the options are taken from Java.
	      (lambda (:key (namespace-aware? #t)
			    (xinclude-aware? #f)
			    (validating? #f)
			    (whitespace? #f)
			    (expand-entity-reference? #t)
			    (ignore-comments? #f)
			    (coalescing? #f))
		(p namespace-aware? xinclude-aware?
		   validating? whitespace? expand-entity-reference?
		   ignore-comments? coalescing?)))))
(define-syntax %expand-entity?
  (syntax-rules ()
    ((_)
     (xml-document-factory-options-expand-entity-reference?
      (*xml:factory-option*)))))
(define +default-factory-option+ (make-xml-document-factory-options))

;; internal parameter
(define *factory-options* (make-parameter #f))
(define *root-document* (make-parameter #f))
(define *current-node* (make-parameter #f))

(define (input-port->dom-tree in :optional (option +default-factory-option+))
  (let ((parsed (parse-xml in))
	(document (make-root-document #f)))
    (parameterize ((*factory-options* option)
		   (*root-document* document)
		   (*current-node* document))
      (dispatch-factory parsed)
      document)))

(define (xml-file->dom-tree file . opt)
  (call-with-input-file file
    (lambda (in) (apply input-port->dom-tree in opt))))

(define *factory-table* (make-eq-hashtable))
(define (dispatch-factory tree)
  (if (pair? tree)
      (let ((name (car tree)))
	(cond ((hashtable-ref *factory-table* name #f) =>
	       (lambda (proc) (proc tree (*root-document*))))
	      (else tree)))
      (assertion-violation 'document-factory "TODO make text")))
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

(define-factory (document root-document)
  (let ((prolog (dispatch-factory (cadr document)))
	(element (dispatch-factory (caddr document)))
	(misc (map dispatch-factory (cdddr document))))
    ;; Add to root-document
    ))

(define-factory (prolog root-document)
  (cond ((cadr prolog) => dispatch-factory))
  (let ((misc1 (map dispatch-factory (cdaddr prolog)))
	(doctype (cond ((cadddr prolog) => dispatch-factory) (else #f)))
	(misc2 (map dispatch-factory (cdar (cddddr prolog)))))
    (for-each (lambda (node) (node:append-child! (*current-node*) node)) misc1)
    (when doctype
      (document-doctype-set! root-document doctype)
      (node:append-child! root-document doctype))
    (for-each (lambda (node) (node:append-child! (*current-node*) node)) misc2)
    ))

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
    (cond ((not id) (values "" ""))
	  ((eq? (car id) 'system) (values "" (cadr id)))
	  ((eq? (car id) 'public) (values (cadr id) (caddr id)))
	  (else (assertion-violation '!doctype "Invalid external ID" id))))
  (let ((name (cadr !doctype))
	(id (caddr !doctype))
	(subsets (cadddr !doctype)))
    (let-values (((public-id system-id) (parse-id id)))
      ;; TODO add all subsets as its child element
      (document:create-document-type root-document name public-id system-id))))

)
