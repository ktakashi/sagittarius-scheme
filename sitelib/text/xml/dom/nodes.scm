;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dom/nodes.scm - DOM nodes
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

;; reference
;;  - https://dom.spec.whatwg.org/
;; mainly section 4
;; we don't implement shadow tree for now

(library (text xml dom nodes)
    (export)
    (import (rnrs)
	    (sagittarius) ;; for define-constant
	    (text xml dom events))

;;; NodeList
(define-record-type node-list
  (fields length
	  items))
(define (node-list:item nl index) )
  
  
;;; Node
(define-constant +element-node+                1)
(define-constant +attribute-node+              2)
(define-constant +text-node+                   3)
(define-constant +cdata-section-node+          4)
(define-constant +entity-reference-node+       5) ;; historical
(define-constant +entity-node+                 6) ;; historical
(define-constant +processing-instruction-node+ 7)
(define-constant +comment-node+                8)
(define-constant +document-node+               9)
(define-constant +document-type-node+          10)
(define-constant +document-fragment-node+      11)
(define-constant +notation-node+               12) ;; historical

(define-constant +document-position-disconnected+ #x01)
(define-constant +document-position-preceding+    #x02)
(define-constant +document-position-following+    #x04)
(define-constant +document-position-contains+     #x08)
(define-constant +document-position-contained-by+ #x10)
(define-constant +document-position-implementation-specifix+ #x20)

(define-record-type node
  (parent <event-target>)
  (fields node-type ;; unsigned short
	  node-name ;; DOMString
	  base-uri  ;; USVString
	  connected?	 ;; boolean
	  owner-document ;; Document?
	  parent-node	 ;; Node?
	  parent-element ;; Element?
	  child-nodes	 ;; NodeList
	  first-child	 ;; Node?
	  last-child	 ;; Node?
	  previous-sibling ;; Node?
	  next-sibling	   ;; Node?
	  node-value	   ;; DOMString?
	  text-content	   ;; DOMString?
	  )
  (protocol (lambda (n)
	      (lambda args
		(assertion-violation 'make-node "Not yet")))))
(define (node:get-root-node node :optional (options #f)) )
(define (node:normalize node))
(define (node:clone-node node :optional (deep #f)))
(define (node:equal-node? node1 node2) #f)
(define (node:same-node? node1 node2) (eq? node1 node2))
(define (node:compare-document-position node other) 0)
(define (node:contains node other))
(define (node:lookup-prefix node namespace))
(define (node:lookup-namespace-uri node prefix))
(define (node:default-namespace node namespace))
(define (node:insert-before! node node0 child) node)
(define (node:append-child! node child))
(define (node:replace-child! node node0 child))
(define (node:remove-child! node child))

;;; Element
(define-record-type element
  (parent node)
  (fields namespace-uri ;; DOMString?
	  prefix	;; DOMString?
	  local-name	;; DOMString
	  tag-name	;; DOMString
	  id		;; DOMString
	  class-name	;; DOMString
	  class-list	;; DOMTokenList
	  slot		;; DOMString
	  attributes	;; NamedNodeMap
	  shadow-root	;; ShadowRoot? (not supported)
	  )
  (protocol (lambda (n)
	      (lambda args
		(assertion-violation 'make-document "not yet")))))
(define (element:has-attributes? element) #f)
(define (element:get-attribute-names element) '())
(define (element:get-attribute element qualified-name) #f)
(define (element:get-attribute-ns element namespace local-name) #f)
(define (element:set-attribute! element qualified-name value) )
(define (element:set-attribute-ns! element namespace qualified-name value) )
(define (element:remove-attribute! element qualified-name) )
(define (element:remove-attribute-ns! element namespace local-name) )
(define (element:has-attribute? element qualified-name))
(define (element:has-attribute-ns? element namespace local-name))

(define (element:get-attribute-node element qualified-name) #f)
(define (element:get-attribute-node-ns element namespace local-name) #f)
(define (element:set-attribute-node! element attr) )
(define (element:set-attribute-node-ns! element attr) )
(define (element:remove-attribute-node! element attr) )
(define (element:attach-shadow! element init)
  (assertion-violation 'element:attach-shadow! "not supported"))

(define (element:closest element selector) #f)
(define (element:matches? element selector) #f)

(define (element:get-elements-by-tag-name element qualified-name) '())
(define (element:get-elements-by-tag-name-ns element namespace local-name) '())
(define (element:get-elements-by-class-name element class-name) '())

;;; Attr
(define-record-type attr
  (parent node)
  (fields namespace-uri ;; DOMString?
	  prefix	;; DOMString?
	  local-name	;; DOMString
	  name		;; DOMString
	  value		;; DOMString
	  owner-element ;; Element
	  specified?	;; boolean (useless always returns true)
	  )
  (protocol (lambda (n)
	      (lambda args
		(assertion-violation 'make-document "not yet")))))

;;; NamedNodeMap
(define-record-type named-node-map
  (fields length
	  element    ;; internal
	  attributes ;; internal
	  )
  (protocol (lambda (p)
	      (lambda (element attributes)
		(let ((v (list->vector attributes)))
		  (p (vector-length v) element v))))))
(define (named-node-map:item map index)
  (vector-ref (named-node-map-attributes map) index))
(define (named-node-map:get-named-item map qualified-name)
  (define len (named-node-map-length map))
  (define attributes (named-node-map-attributes map))
  (define (->qualified-name attr)
    ;; TODO 
    (string-append (or (attr-namespace-uri attr) "")
		   ":"
		   (or (attr-local-name attr) "")))
  (let loop ((i 0))
    (if (= i len)
	#f
	(let ((attr (vector-ref attributes i)))
	  (if (string=? qualified-name (->qualified-name attr))
	      attr
	      (loop (+ i 1)))))))

(define (named-node-map:get-named-item-ns map namespace local-name)
  (define len (named-node-map-length map))
  (define attributes (named-node-map-attributes map))
  (let loop ((i 0))
    (if (= i len)
	#f
	(let ((attr (vector-ref attributes i)))
	  (if (and (string=? namespace (attr-namespace-uri attr))
		   (string=? namespace (attr-local-name attr)))
	      attr
	      (loop (+ i 1)))))))

(define (named-node-map:set-named-item! map attr))
(define (named-node-map:set-named-item-ns! map attr))
(define (named-node-map:remove-named-item! map qualified-name))
(define (named-node-map:remove-named-item-ns! map namespace local-name))

;;; Document
(define-record-type document
  (parent node)
  (fields url	       ;; USVString
	  document-uri ;; USVString
	  origin       ;; USVString
	  compat-mode  ;; DOMString
	  character-set ;; DOMString
	  charset	;; DOMString historical
	  input-encoding ;; DOMString historical
	  content-type	 ;; DOMString
	  doctype	 ;; DocumentType?
	  document-element ;;Element?
	  )
  (protocol (lambda (n)
	      (lambda args
		(assertion-violation 'make-document "not yet")))))
(define (document:get-element-by-tag-name document qualified-name))
(define (document:get-element-by-tag-name-ns document namespace local-name))
(define (document:get-element-by-class-name document class-name))
(define (document:create-element document local-name :optional (option #f)))
(define (document:create-element-ns document namespace qualified-name
				    :optional (option #f)))
(define (document:create-document-fragment document))
(define (document:create-text-node document data))
(define (document:create-cdata-section document data))
(define (document:create-comment document data))
(define (document:create-processing-instruction document target data))

(define (document:import-node document node :optional (deep #f)))
(define (document:adopt-node document node))

(define (document:create-attribute document local-name))
(define (document:create-attribute-ns document namespace qualified-name))

(define (document:create-event document interface))
(define (document:create-range document))

(define (document:create-node-iterator document root
				       :optional (what-to-show #xFFFFFFFF)
					         (filter #f)))
(define (document:create-tree-walker document root
				     :optional (what-to-show #xFFFFFFFF)
					       (filter #f)))

)



