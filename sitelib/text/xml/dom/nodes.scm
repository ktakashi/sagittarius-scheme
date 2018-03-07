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
    (export node-list? node-list-length node-list:item

	    +element-node+ +attribute-node+ +text-node+
	    +cdata-section-node+ +entity-reference-node+
	    +entity-node+ +processing-instruction-node+
	    +comment-node+ +document-node+
	    +document-type-node+ +document-fragment-node+ +notation-node+

	    node? node-node-type node-node-name node-base-uri
	    node-connected? node-owner-document node-parent-node
	    node-parent-element node-child-nodes node-first-child
	    node-last-child node-previous-sibling node-next-sibling
	    node-node-value node-text-content
	    node:get-root-node node:normalize! node:clone-node
	    node:equal-node? node:same-node? node:compare-document-position
	    node:contains? node:lookup-prefix node:lookup-namespace-uri
	    node:default-namespace node:insert-before! node:append-child!
	    node:replace-child! node:remove-child!
	    
	    element? element-namespace-uri element-prefix
	    element-local-name element-tag-name element-id
	    element-class-name element-class-list element-slot
	    element-attributes
	    ;; element-shadow-root ;; not supported yet
	    element:has-attributes? element:get-attribute-names
	    element:get-attribute element:get-attribute-ns
	    element:set-attribute! element:set-attribute-ns!
	    element:remove-attribute! element:remove-attribute-ns!
	    element:has-attribute? element:has-attribute-ns?
	    element:get-attribute-node element:get-attribute-node-ns
	    element:set-attribute-node! element:set-attribute-node-ns!
	    element:remove-attribute-node!
	    ;; element:attach-shadow! ;; not supported yet
	    element:closest element:matches?
	    element:get-elements-by-tag-name element:get-elements-by-tag-name-ns
	    element:get-elements-by-class-name

	    attr? attr-namespace-uri attr-prefix attr-local-name
	    attr-name attr-value attr-owner-element attr-specified?

	    named-node-map? named-node-map-length
	    named-node-map:item named-node-map:get-named-item
	    named-node-map:get-named-item-ns
	    named-node-map:set-named-item! named-node-map:set-named-item-ns!
	    named-node-map:remove-named-item!
	    named-node-map:remove-named-item-ns!

	    document? document-uri document-document-uri document-origin
	    document-compat-mode document-character-set document-charset
	    document-input-encoding document-content-type
	    document-doctype document-document-element
	    document:get-element-by-tag-name document:get-element-by-tag-name-ns
	    document:get-element-by-class-name
	    document:create-element document:create-element-ns
	    document:create-document-fragment document:create-text-node
	    document:create-cdata-section document:create-comment
	    document:create-processing-instruction
	    document:import-node document:adopt-node
	    document:create-attribute document:create-attribute-ns
	    document:create-event document:create-range
	    document:create-node-iterator document:create-tree-walker
	    )
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
  (fields node-type                  ;; unsigned short
	  node-name                  ;; DOMString
	  base-uri                   ;; USVString
	  connected?                 ;; boolean
	  owner-document             ;; Document?
	  (mutable parent-node)	     ;; Node?
	  (mutable parent-element)    ;; Element?
	  (mutable child-nodes)	     ;; NodeList
	  (mutable first-child)	     ;; Node?
	  (mutable last-child)	     ;; Node?
	  (mutable previous-sibling) ;; Node?
	  (mutable next-sibling)     ;; Node?
	  node-value		     ;; DOMString?
	  text-content		     ;; DOMString?
	  )
  (protocol (lambda (n)
	      (lambda (node-type :key (node-name #f)
				      (base-uri #f)
				      (connected? #f)
				      (owner-document #f)
				      (parent-node #f)
				      (parent-element #f)
				      (child-nodes (make-node-list 0 '()))
				      (node-value #f)
				      (text-content #f))
		((n)
		 node-type
		 node-name
		 base-uri
		 connected?
		 owner-document
		 parent-node
		 parent-element
		 child-nodes
		 #f ;; TODO calculate from child-nodes
		 #f ;; ditto
		 #f ;; previous sibling will be set
		 #f ;; next sibling will be set later
		 node-value
		 text-content)))))
(define (node:get-root-node node :optional (options #f)) )
(define (node:normalize! node))
(define (node:clone-node node :optional (deep #f)))
(define (node:equal-node? node1 node2) #f)
(define (node:same-node? node1 node2) (eq? node1 node2))
(define (node:compare-document-position node other) 0)
(define (node:contains? node other))
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
	  (mutable id)	;; DOMString
	  (mutable class-name) ;; DOMString
	  class-list	;; DOMTokenList
	  slot		;; DOMString
	  attributes	;; NamedNodeMap
	  shadow-root	;; ShadowRoot? (not supported)
	  )
  (protocol (lambda (n)
	      (lambda (namespace-uri prefix local-name tag-name
		       :key (id #f) (class-name #f) (slot #f)
			    (attributes (make-named-node-map)))
		((n +element-node+ :node-name tag-name)
		 namespace-uri prefix local-name tag-name id class-name
		 #f ;; class-list later
		 slot attributes
		 #f ;; shadow-root later
		 )))))
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

;;; DocumentType
(define-record-type document-type
  (parent node)
  (fields name	    ;; DOMString 
	  public-id ;; DOMString 
	  system-id ;; DOMString
	  )
  (protocol (lambda (n)
	      (lambda (name public-id system-id)
		((n +document-type-node+) name public-id system-id)))))

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
