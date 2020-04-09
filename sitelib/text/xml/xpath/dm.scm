;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/xpath/dm.scm - XPath datamodel
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

;; ref:
;;  XQuery and XPath Data Model 3.1
;;  https://www.w3.org/TR/xpath-datamodel-31/

(library (text xml xpath dm)
    (export xpath-dm:string-value)
    (import (rnrs)
	    (text xml dom nodes))
;; Namespace node
;; I'm not sure how it should be handled, but we just check
;; if it's a URI looks like string
(define namespace? string?) ;; for now


;;; 5 Accessors

;;;; 5.12 string-value Accessor
;;;; dm:string-value($n as node()) as xs:string
(define (xpath-dm:string-value n)
  (cond ((document? n) (xpath-dm:document-string-value n))
	((element? n)  (xpath-dm:element-string-value n))
;;	((attr? n)     (xpath-dm:attribute-string-value n))
;;	((processing-instruction? n) (xpath-dm:processing-instruction-content n))
;;	((comment? n)  (xpath-dm:comment-content n))
	((text? n)     (xpath-dm:text-content n))
;;	((namespace? n) (xpath-dm:namespace-uri n))
	(else (assertion-violation 'xpath-dm:string-value "Unknown node" n))))


;;; 6 Nodes
;;;; 6.1 Document Nodes
(define (xpath-dm:document-string-value d)
  (define itr (document:create-node-iterator d (document-document-element d)
					     +node-filter-show-text+))
  (text-node-iterator->string itr))

;;;; 6.2 Element Nodes
(define (xpath-dm:element-string-value e)
  (define itr (document:create-node-iterator (node-owner-document e) e
					     +node-filter-show-text+))
  (text-node-iterator->string itr))

;;;; 6.7 Text Nodes
(define (xpath-dm:text-content t) (text-whole-text t))


;;; Helpers
(define (text-node-iterator->string itr)
  (let-values (((out e) (open-string-output-port)))
    (do ((n (node-iterator:next-node itr) (node-iterator:next-node itr)))
	((not n) (e))
      (put-string out (xpath-dm:string-value n)))))
)
