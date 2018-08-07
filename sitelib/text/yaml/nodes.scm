;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/yaml/nodes.scm - YAML nodes
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

;; see http://yaml.org/spec/1.2/spec.html#id2763452

#!nounbound
(library (text yaml nodes)
    (export (rename (yaml-document <yaml-document>))
	    make-yaml-document yaml-document?
	    yaml-document-directives yaml-document-root-node

	    (rename (yaml-directive <yaml-directive>))
	    make-yaml-directive yaml-directive?
	    yaml-directive-name yaml-directive-parameters

	    (rename (yaml-yaml-directive <yaml-yaml-directive>))
	    make-yaml-yaml-directive yaml-yaml-directive?
	    yaml-yaml-directive-major-version yaml-yaml-directive-minor-version

	    (rename (yaml-tag-directive <yaml-tag-directive>))
	    make-yaml-tag-directive yaml-tag-directive?
	    yaml-tag-directive-handle yaml-tag-directive-prefix
	    
	    (rename (yaml-node <yaml-node>))
	    yaml-node?
	    yaml-node-tag yaml-node-value
	    yaml-node-start-mark yaml-node-end-mark

	    (rename (yaml-scalar-node <yaml-scalar-node>))
	    make-yaml-scalar-node yaml-scalar-node?
	    yaml-scalar-node-style

	    (rename (yaml-collection-node <yaml-collection-node>))
	    yaml-collection-node?
	    yaml-collection-node-flow-style?

	    (rename (yaml-sequence-node <yaml-sequence-node>))
	    make-yaml-sequence-node yaml-sequence-node?

	    (rename (yaml-mapping-node <yaml-mapping-node>))
	    make-yaml-mapping-node yaml-mapping-node?

	    yaml-document->canonical-sexp
	    yaml-directive->canonical-sexp
	    yaml-node->canonical-sexp
	    )
    (import (rnrs))

;; YAML document holds a YAML document
(define-record-type yaml-document
  (fields directives
	  root-node))

(define-record-type yaml-directive
  (fields name parameters))
(define-record-type yaml-yaml-directive
  (parent yaml-directive)
  (fields major-version minor-version)
  (protocol (lambda (p)
	      (lambda (major&minor)
		((p "YAML" (list major&minor))
		 (car major&minor) (cdr major&minor))))))
(define-record-type yaml-tag-directive
  (parent yaml-directive)
  (fields handle prefix)
  (protocol (lambda (p)
	      (lambda (handle&prefix)
		((p "TAG" (list handle&prefix))
		 (car handle&prefix) (cdr handle&prefix))))))

(define-record-type yaml-node
  (fields tag
	  value
	  ;; scanner mark, see (text yaml scanner)
	  ;; for debug information
	  start-mark
	  end-mark))

(define-record-type yaml-scalar-node
  (parent yaml-node)
  (fields style)
  (protocol (lambda (n)
	      (case-lambda
	       ((tag value start-mark end-mark)
		((n tag value start-mark end-mark) #f))
	       ((tag value style start-mark end-mark)
		((n tag value start-mark end-mark) style))))))

(define-record-type yaml-collection-node
  (parent yaml-node)
  (fields flow-style?)
  (protocol (lambda (n)
	      (case-lambda
	       ((tag value start-mark end-mark)
		((n tag value start-mark end-mark) #f))
	       ((tag value style start-mark end-mark)
		((n tag value start-mark end-mark) style))))))

(define-record-type yaml-sequence-node
  (parent yaml-collection-node)
  (protocol (lambda (n) (lambda args ((apply n args))))))

(define-record-type yaml-mapping-node
  (parent yaml-collection-node)
  (protocol (lambda (n) (lambda args ((apply n args))))))

(define (yaml-document->canonical-sexp yaml)
  (let ((d* (yaml-document-directives yaml))
	(node (yaml-document-root-node yaml)))
    `(*yaml*
      ,@(if (or (not d*) (null? d*))
	    '()
	    `((*directives* ,@(map yaml-directive->canonical-sexp d*))))
      ,@(if node
	    (list (yaml-node->canonical-sexp node))
	    '()))))
(define (yaml-directive->canonical-sexp directive)
  (cond ((yaml-yaml-directive? directive)
	 `(%YAML ,(yaml-yaml-directive-major-version directive)
		 ,(yaml-yaml-directive-minor-version directive)))
	((yaml-tag-directive? directive)
	 `(%TAG ,(yaml-tag-directive-handle directive)
		,(yaml-tag-directive-prefix directive)))
	(else
	 `(,(string->symbol (string-append "%" (yaml-directive-name directive)))
	   ,(yaml-directive-parameters directive)))))

;; YAML node to canonical YAML SEXP
;; node     ::= scalar | mapping | sequence
;; scalar   ::= (tag value)
;; mapping  ::= (tag (map-key map-value) ...)
;; sequence ::= (tag node ...)
;; NB: we drop flow style information here
(define (yaml-node->canonical-sexp node)
  (cond ((yaml-scalar-node? node)
	 ;; scalar value might be a non sexp value.
	 ;; so check
	 (let ((v (yaml-node-value node)))
	   `(,(yaml-node-tag node)
	     ,(cond ((pair? v) (map yaml-node->canonical-sexp v))
		    (else (yaml-node->canonical-sexp v))))))
	((yaml-sequence-node? node)
	 `(,(yaml-node-tag node)
	   ,@(map yaml-node->canonical-sexp (yaml-node-value node))))
	((yaml-mapping-node? node)
	 `(,(yaml-node-tag node)
	   ,@(map (lambda (k&v) `(,(yaml-node->canonical-sexp (car k&v))
				  ,(yaml-node->canonical-sexp (cdr k&v))))
		  (yaml-node-value node))))
	(else node)))
)
