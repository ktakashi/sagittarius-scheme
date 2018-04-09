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
	    node-connected? node-owner-document
	    node-parent-node node-parent-node-set!
	    node-parent-element node-parent-element-set!
	    node-child-nodes node-first-child
	    node-last-child node-previous-sibling node-next-sibling
	    node-node-value node-text-content
	    node:get-root-node node:normalize! node:clone-node
	    node:equal-node? node:same-node? node:compare-document-position
	    node:contains? node:lookup-prefix node:lookup-namespace-uri
	    node:default-namespace node:insert-before! node:append-child!
	    node:replace-child! node:remove-child!

	    document-type-name document-type-public-id
	    document-type-system-id
	    
	    element? element-namespace-uri element-prefix
	    element-local-name element-tag-name element-id
	    element-class-name element-class-list
	    ;; element-slot
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
	    attr-value-set!

	    named-node-map? named-node-map-length
	    named-node-map:item named-node-map:get-named-item
	    named-node-map:get-named-item-ns
	    named-node-map:set-named-item! named-node-map:set-named-item-ns!
	    named-node-map:remove-named-item!
	    named-node-map:remove-named-item-ns!

	    document? document-uri document-document-uri document-origin
	    document-compat-mode document-character-set document-charset
	    document-input-encoding document-content-type
	    document-doctype document-document-element document-xml-standalone?
	    document-xml-version
	    document:get-element-by-tag-name document:get-element-by-tag-name-ns
	    document:get-element-by-class-name
	    document:create-element document:create-element-ns
	    document:create-document-fragment document:create-text-node
	    document:create-cdata-section document:create-comment
	    document:create-processing-instruction
	    document:create-document-type ;; non-dom
	    document:import-node document:adopt-node
	    document:create-attribute document:create-attribute-ns
	    document:create-event document:create-range
	    document:create-node-iterator document:create-tree-walker
	    document-character-set-set! document-content-type-set!
	    document-doctype-set! document-document-element-set!
	    document-xml-standalone?-set! document-xml-version-set!

	    ;; internal use only
	    (rename (make-document make-root-document))
	    make-document-type
	    document:create-element-qname
	    document:create-attribute-qname
	    )
    (import (rnrs)
	    (sagittarius) ;; for define-constant
	    (sagittarius treemap)
	    (srfi :13 strings)
	    (srfi :117 list-queues)
	    (text xml dom events))

(define-constant +undfined+ (undefined))

;;; NodeList
(define-record-type node-list
  (fields length items)
  (protocol
   (lambda (p)
     (lambda (queue)
       (p (list-queue-length queue) (list->vector (list-queue-list queue)))))))
(define (node-list:item nl index) (vector-ref (node-list-items nl) index))

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
	  (mutable owner-document)   ;; Document?
	  (mutable parent-node)	     ;; Node?
	  (mutable parent-element)   ;; Element?
	  ;; (mutable child-nodes)	     ;; NodeList
	  children
	  (mutable previous-sibling) ;; Node?
	  (mutable next-sibling)     ;; Node?
	  ;; TODO can we merge them?
	  (mutable node-value)	 ;; DOMString?
	  (mutable text-content) ;; DOMString?
	  )
  (protocol (lambda (n)
	      (lambda (node-type :key (node-name +undfined+)
				      (base-uri +undfined+)
				      (connected? +undfined+)
				      (owner-document +undfined+)
				      (parent-node +undfined+)
				      (parent-element +undfined+)
				      (child-nodes (list-queue))
				      (node-value +undfined+)
				      (text-content +undfined+))
		((n)
		 node-type
		 node-name
		 base-uri
		 connected?
		 owner-document
		 parent-node
		 parent-element
		 child-nodes
		 #f ;; previous sibling will be set
		 #f ;; next sibling will be set later
		 node-value
		 text-content)))))
(define (node-child-nodes node)
  (make-node-list (node-children node)))
(define (node-first-child node)
  (let ((children (node-children node)))
    (and (not (list-queue-empty? children))
	 (list-queue-front children))))
(define (node-last-child node)
  (let ((children (node-children node)))
    (and (not (list-queue-empty? children))
	 (list-queue-front children))))

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
(define (node:append-child! node child)
  (node-parent-node-set! node child)
  (list-queue-add-back! (node-children node) child))
(define (node:replace-child! node node0 child))
(define (node:remove-child! node child))

;;; Element
(define-record-type element
  (parent node)
  (fields namespace-uri ;; DOMString?
	  prefix	;; DOMString?
	  local-name	;; DOMString
	  class-list	;; DOMTokenList
	  attributes ;; NamedNodeMap (hashtable)
	  shadow-root	;; ShadowRoot? (not supported)
	  )
  (protocol
   (lambda (n)
     (define (->tagname prefix localname)
       (if (or (not prefix) (zero? (string-length prefix)))
	   localname
	   (string-append prefix ":" localname)))
     (define (attr-compare aa ab)
       (or (and (eq? (attr-owner-element aa) (attr-owner-element ab))
		(string=? (attr-namespace-uri aa) (attr-namespace-uri ab))
		(string=? (attr-name aa) (attr-name ab))
		0)
	   ;; well order doesn't matter anyway
	   -1))
     (lambda (namespace-uri prefix local-name)
       ((n +element-node+
	   :node-name (->tagname prefix local-name))
	namespace-uri prefix local-name
	'()
	(make-named-node-map attr-compare)
	#f ;; shadow-root later
	)))))
(define (element-tag-name element) (node-node-name element))
(define (element-id element)
  (cond ((element:get-attribute element "id") => attr-value)
	(else #f)))
(define (element-class-name element)
  (cond ((element:get-attribute element "class") => attr-value)
	(else #f)))
(define (element:has-attributes? element)
  (not (zero? (named-node-map-length (element-attributes element)))))
(define (element:get-attribute-names element)
  )
(define (element:get-attribute element qualified-name)
  (cond ((named-node-map:get-named-item (element-attributes element)
					qualified-name) => attr-value)
	(else #f)))
(define (element:get-attribute-ns element namespace local-name)
  (cond ((named-node-map:get-named-item-ns (element-attributes element)
					   namespace local-name) => attr-value)
	(else #f)))
(define (element:set-attribute! element qualified-name value) )
(define (element:set-attribute-ns! element namespace qualified-name value) )
(define (element:remove-attribute! element qualified-name) )
(define (element:remove-attribute-ns! element namespace local-name) )
(define (element:has-attribute? element qualified-name))
(define (element:has-attribute-ns? element namespace local-name))

(define (element:get-attribute-node element qualified-name) #f)
(define (element:get-attribute-node-ns element namespace local-name) #f)
(define (element:set-attribute-node! element attr)
  (named-node-map:set-named-item! (element-attributes element) attr)
  (attr-owner-element-set! attr element))
(define (element:set-attribute-node-ns! element attr)
  (named-node-map:set-named-item-ns! (element-attributes element) attr)
  (attr-owner-element-set! attr element))
  
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
	  (mutable owner-element) ;; Element
	  specified?	;; boolean (useless always returns true)
	  )
  (protocol (lambda (n)
	      (define (->qualified-name prefix local-name)
		(cond ((zero? (string-length prefix)) local-name)
		      (else (string-append prefix ":" local-name))))
	      (lambda (namespace-uri prefix local-name)
		((n +attribute-node+
		    :node-name (->qualified-name prefix local-name)
		    :node-value ""
		    :text-content "")
		 namespace-uri prefix local-name #f #t)))))
(define (attr-name attr) (node-node-name attr))
(define (attr-value attr) (node-node-value attr))
(define (attr-value-set! attr value)
  (node-node-value-set! attr value)
  (node-text-content-set! attr value))

;;; NamedNodeMap
;; Internally, it's a treemap using as a set. 
(define-record-type named-node-map
  (fields values)
  (protocol (lambda (p)
	      (lambda (compare)
		(p (make-rb-treemap compare))))))
(define (named-node-map-length map) (treemap-size (named-node-map-values map)))
(define (named-node-map:item map index)
  (treemap-find/index (lambda (i k) (= i index)) (named-node-map-values map)))

(define (named-node-map:get-named-item map qualified-name)
  (treemap-find (lambda (attr)
		  (string=? (attr-qualified-name attr) qualified-name))
		(named-node-map-values map)))

(define (named-node-map:get-named-item-ns map namespace local-name)
  (treemap-find (lambda (attr)
		  (and (equal? namespace (attr-namespace-uri attr))
		       (equal? local-name (attr-local-name attr))))
		(named-node-map-values map)))

(define (named-node-map:set-named-item! map attr)
  (treemap-set! (named-node-map-values map) attr attr))
  
(define (named-node-map:set-named-item-ns! map attr)
  ;; TODO maybe check namespace-uri?
  (treemap-set! (named-node-map-values map) attr attr))
(define (named-node-map:remove-named-item! map qualified-name)
  (cond ((named-node-map:get-named-item map qualified-name) =>
	 (lambda (attr)
	   (treemap-delete! (named-node-map-values map) attr)))))
(define (named-node-map:remove-named-item-ns! map namespace local-name)
  (cond ((named-node-map:get-named-item-ns map namespace local-name) =>
	 (lambda (attr)
	   (treemap-delete! (named-node-map-values map) attr)))))

;;; DocumentType
(define-record-type document-type
  (parent node)
  (fields public-id ;; DOMString 
	  system-id ;; DOMString
	  )
  (protocol (lambda (n)
	      (lambda (name public-id system-id)
		((n +document-type-node+ :node-name name) public-id system-id)))))
(define (document-type-name dt) (node-node-name dt))
;;; CharacterData
(define-record-type character-data
  (parent node)
  (protocol (lambda (n)
	      (lambda (type data . args)
		((apply n type :text-content data :node-value data args))))))
(define (character-data-data cd) (node-text-content cd))
(define (character-data-data-set! cd data) (node-text-content-set! cd data))
(define (character-data-length cd) (string-length (character-data-data cd)))
(define (character-data:substring-data cd offset count))
(define (character-data:append-data! cd data))
(define (character-data:insert-data! cd offset data))
(define (character-data:delete-data! cd offset count))
(define (character-data:replace-data! cd offset count))

;; Text
(define-record-type text
  (parent character-data)
  (protocol (lambda (n)
	      (lambda (:key (data "")
			    (type +text-node+) ;; for CDATA...
			    )
		((n type data))))))
(define (text-whole-text text) (character-data-data text))

(define (text:split-text text offset))

(define-record-type cdata-section
  (parent text)
  (protocol (lambda (n)
	      (lambda (:key (data ""))
		((n :data data :type +cdata-section-node+))))))

(define-record-type processing-instruction
  (parent character-data)
  (protocol (lambda (n)
	      (lambda (name data)
		((n +processing-instruction-node+ data :node-name name))))))
(define (processing-instruction-target pi) (node-node-name pi))

(define-record-type comment
  (parent character-data)
  (protocol (lambda (n)
	      (lambda (data)
		((n +comment-node+ data :node-name "#comment"))))))

;;; Document
(define-record-type document
  (parent node)
  (fields ;; url	  ;; USVString 
	  ;; document-uri ;; USVString
	  ;; we use above the same as baseURI of the node
	  ;; origin       ;; USVString (not used)
	  compat-mode  ;; DOMString (always BackCompat)
	  (mutable character-set) ;; DOMString
	  ;; merged into character-set
	  ;; charset	;; DOMString historical
	  ;; input-encoding ;; DOMString historical
	  (mutable content-type)	 ;; DOMString (default text/xml)
	  (mutable doctype)	 ;; DocumentType?
	  (mutable document-element) ;;Element?
	  (mutable xml-standalone?)
	  (mutable xml-version)
	  )
  (protocol
   (lambda (n)
     (lambda (url)
       ((n +document-type-node+ :node-name "#document" :base-uri url)
	"BackCompat" "UTF-8" "text/xml" +undfined+ +undfined+
	+undfined+ +undfined+)))))
(define document-url node-base-uri)
(define document-document-uri node-base-uri)
(define (document-origin document) +undfined+)
(define document-charset document-character-set)
(define document-input-encoding document-character-set)

(define (document:get-element-by-tag-name document qualified-name))
(define (document:get-element-by-tag-name-ns document namespace local-name))
(define (document:get-element-by-class-name document class-name))
(define (document:create-element document local-name :optional (option #f))
  (let ((node (make-element "" "" local-name)))
    (node-owner-document-set! node document)
    node))
(define (document:create-element-ns document namespace qualified-name
				    :optional (option #f)))
(define (document:create-document-fragment document))
(define (document:create-text-node document data)
  (let ((node (make-text :data data)))
    (node-owner-document-set! node document)
    node))
(define (document:create-cdata-section document data))

(define (document:create-comment document data)
  (let ((node (make-comment data)))
    (node-owner-document-set! node document)
    node))
(define (document:create-processing-instruction document target data)
  (let ((node (make-processing-instruction target data)))
    (node-owner-document-set! node document)
    node))
;; non-dom factory
(define (document:create-document-type document name public-id system-id)
  (let ((node (make-document-type name public-id system-id)))
    (node-owner-document-set! node document)
    node))
(define (document:create-element-qname document namespace prefix local-part
					    :optional (option #f))
  (let ((node (make-element namespace prefix local-part)))
    (node-owner-document-set! node document)
    node))

(define (document:import-node document node :optional (deep #f)))
(define (document:adopt-node document node))

(define (document:create-attribute document local-name)
  (let ((node (make-attr "" "" local-name)))
    (node-owner-document-set! node document)
    node))
(define (document:create-attribute-ns document namespace qualified-name)
  
  )

(define (document:create-attribute-qname document namespace prefix local-part)
  (let ((node (make-attr namespace prefix local-part)))
    (node-owner-document-set! node document)
    node))

(define (document:create-event document interface))
(define (document:create-range document))

(define (document:create-node-iterator document root
				       :optional (what-to-show #xFFFFFFFF)
					         (filter #f)))
(define (document:create-tree-walker document root
				     :optional (what-to-show #xFFFFFFFF)
					       (filter #f)))

)
