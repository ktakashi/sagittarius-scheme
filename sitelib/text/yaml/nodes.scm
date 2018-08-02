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

;; we don't create tag node
;; see http://yaml.org/spec/1.2/spec.html#id2763452

(library (text yaml nodes)
    (export (rename (yaml-document <yaml-document>))
	    make-yaml-document yaml-document?
	    yaml-document-directives yaml-document-root-node

	    (rename (yaml-directive <yaml-directive>))
	    yaml-directive-name yaml-directive-parameters

	    (rename (yaml-node <yaml-node>))
	    yaml-node?
	    yaml-node-tag yaml-node-value
	    yaml-node-start-mark yaml-node-end-mark

	    (rename (yaml-scalar-node <yaml-scalar-node>))
	    make-yaml-scalar-node yaml-scalar-node?
	    yaml-scalar-node-style

	    (rename (yaml-collection-node <yaml-collection-node>))
	    yaml-collection-node?
	    yaml-collection-node-flow-style

	    (rename (yaml-sequence-node <yaml-sequence-node>))
	    make-yaml-sequence-node yaml-sequence-node?

	    (rename (yaml-mapping-node <yaml-mapping-node>))
	    make-yaml-mapping-node yaml-mapping-node?
	    )
    (import (rnrs))

;; YAML document holds a YAML document
(define-record-type yaml-document
  (fields directives
	  root-node))

(define-record-type yaml-directive
  (fields name parameters))

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
  (fields flow-style)
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
)
