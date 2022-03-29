;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dom/util.scm - DOM util
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
(library (text xml dom util)
    (export element:collect-parent-namespaces
	    element:skip-namespace?)
    (import (rnrs)
	    (srfi :13 strings)
	    (text xml dom nodes))

(define (element:collect-parent-namespaces e0 exclusive?)
  (define doc (node-owner-document e0))
  (define (clone-attr a)
    (let ((c (document:create-attribute-ns doc (attr-namespace-uri a)
					   (attr-name a))))
      (attr-value-set! c (attr-value a))
      c))
  
  (define (rec e r)
    (if (or (document? e) (not e))
	r
	(let ((r (named-node-map:fold (element-attributes e) r
		   (lambda (k v r)
		     (let ((name (attr-name k)))
		       ;; not sure why but seems
		       ;; we need to put xml:base as well
		       (if (or (and (string-prefix? "xmlns" name)
				    (not (element:has-attribute? e0 name)))
			       (and (not exclusive?)
				    (string-prefix? "xml:" name)))
			   (cons (clone-attr k) r)
			   r))))))
	  (rec (node-parent-node e) r))))
  (rec (node-parent-node e0) '()))

(define (element:skip-namespace? e attr root exclusive?)
  (define partial-element?
    (not (eq? root (document-document-element (node-owner-document root)))))
  (define (namespace-used-in-children? attr)
    (define ns (attr-value attr))
    (define (check e)
      (if (equal? (element-namespace-uri e) ns)
	  +node-filter-filter-accept+
	  +node-filter-filter-skip+))
    (define tw (document:create-tree-walker (node-owner-document e)
					    e +node-filter-show-element+
					    check))
    (tree-walker:next-node tw))
  
  (define (ancestor-of? root e) ;; check if e is ancestor of root or not
    (cond ((eq? root e) #f)
	  ((document? e) #t)
	  (else (ancestor-of? root (node-parent-node e)))))
  
  (define (check-element attr e2)
    (cond ((not e2) #f) ;; doesn't have parent yet
	  ((document? e2) (zero? (string-length (attr-value attr))))
	  ((ancestor-of? root e2) #f)
	  ((element? e2)
	   (let ((ns (named-node-map:get-named-item (element-attributes e2)
						    (attr-name attr))))
	     (cond (ns (equal? (attr-value attr) (attr-value ns)))
		   
		   (else (check-element attr (node-parent-node e2))))))
	  ;; ???
	  (else #f)))
  (and (string-prefix? "xmlns" (attr-name attr))
       ;; check
       (or (and exclusive?
		;; 3. Specification of Exclusive XML Canonicalization
		;; of https://www.w3.org/TR/xml-exc-c14n
		partial-element?
		(not (namespace-used-in-children? attr)))
	   (check-element attr (node-parent-node e)))))
)
