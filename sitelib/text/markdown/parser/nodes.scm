;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/nodes.scm - Node of markdown
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
(library (text markdown parser nodes)
    (export make-markdown-document
	    make-paragraph-node paragraph-node?
	    make-block-quote-node block-quote-node?
	    make-list-node list-node?
	    make-code-block-node code-block-node?
	    code-block-node-info code-block-node-info-set!
	    code-block-node:literal-set! code-block-node:literal
	    make-fenced-code-block-node fenced-code-block-node?
	    fenced-code-block-node-fence-char
	    fenced-code-block-node-fence-length
	    fenced-code-block-node-fence-indent
	    
	    make-heading-node heading-node?
	    heading-node-level heading-node-level-set!
	    
	    make-thematic-break-node thematic-break-node?
	    make-html-block-node html-block-node?
	    make-custom-block-node custom-block-node?

	    (rename (text-node make-text-node)) text-node?
	    text-node:content-set!

	    make-linebreak-node linebreak-node?
	    make-softbreak-node softbreak-node?
	    
	    *commonmark-namespace*

	    define-markdown-node ;; for custom node
	    markdown-node:append-child!
	    markdown-node:get-attribute
	    markdown-node:set-attribute!
	    markdown-node:remove-attribute!
	    markdown-node:unlink!
	    markdown-node:source-locations
	    markdown-node:add-source-location!
	    markdown-node:source-locations-set!

	    markdown-node:set-text!
	    markdown-node:get-text

	    (rename (markdown-node-element markdown-node->dom-tree))
	    )
    (import (rnrs)
	    (srfi :1 lists)
	    (srfi :117 list-queues)
	    (text xml dom))

(define *commonmark-namespace* "http://commonmark.org/xml/1.0")

;; The node structure will be kept on XML DOM
;; the wrapper node only managed necessary structure and meta data
(define-record-type markdown-node
  (fields element ;; xml element
	  source-locations
	  ;; below are for future convenience
	  ;; if we want to manipulate node programatically
	  (mutable parent)   ;; parent markdown node (#f = stray or root)
	  children	     ;; child markdown nodes
	  )
  (protocol (lambda (p)
	      (lambda (element)
		(p element (list-queue) #f (list-queue))))))

(define-syntax namespace (syntax-rules ()))
(define-syntax element (syntax-rules ()))
(define-syntax attribute (syntax-rules ()))
(define-syntax define-markdown-node
  (lambda (x)
    (define (parse-clause name clause*)
      (define (rec clause* ns e)
	(syntax-case clause* (namespace element)
	  (((namespace ns) rest ...)
	   (rec #'(rest ...) #'ns e))
	  (((element e) rest ...)
	   (rec #'(rest ...) ns #'e))
	  (() (list ns e))
	  (_ (syntax-violation 'define-markdown-node "malformed clause"
			       (syntax->datum x)
			       (syntax->datum clause*)))))
      (with-syntax ((e name))
	(rec clause* #'*commonmark-namespace*
	     #'(symbol->string 'e))))
    
    (define (identifier->string n) (symbol->string (syntax->datum n)))
    (define (string->identifier k n) (datum->syntax k (string->symbol n)))
    (define (make-record-type-name k name)
      (define n (identifier->string name))
      (string->identifier k (string-append n "-node")))

    (define (process-fields k type ns instance elm-name f*)
      (define type-name (identifier->string type))
      (define (make-getter&setter k f setter-prefix)
	(define n (identifier->string f))
	(map (lambda (v) (string->identifier k v))
	     (list (string-append type-name "-" n)
		   (string-append setter-prefix type-name "-" n "-set!"))))
      (define (make-mutator k f setter)
	(define n (identifier->string f))
	(string->identifier k (string-append type-name "-" n "-set!")))
      (let loop ((r '()) (f* f*))
	(syntax-case f* (attribute)
	  (() (reverse! r))
	  (((attribute f) rest ...)
	   (with-syntax (((get set) (make-getter&setter k #'f "%")))
	     (with-syntax ((mut (make-mutator k #'f #'set))
			   (ns ns)
			   (instance instance)
			   (elm-name elm-name))
	       (loop (cons #'(f get set
				(define (mut node v)
				  (set node v)
				  (if v
				      (markdown-node:set-attribute!
				       node ns elm-name v)
				      (markdown-node:remove-attribute!
				       node ns elm-name)))
				(mut instance f))
			   r) #'(rest ...)))))
	  ((f rest ...)
	   (with-syntax (((get set) (make-getter&setter k #'f "")))
	     (loop (cons #'(f get set #'(begin) #'(begin)) r) #'(rest ...)))))))
    
    (syntax-case x ()
      ((k name)
       (identifier? #'name)
       #'(k (name)))
      ((k (name fields ...))
       #'(k (name fields ...)
	    (namespace *commonmark-namespace*)
	    (element (symbol->string 'name))))
      ;; Full
      ((k name clause* ...)
       (identifier? #'name)
       #'(k (name) clause* ...))
      
      ((k (name f* ...) clause* ...)
       (with-syntax (((ns element) (parse-clause #'name #'(clause* ...)))
		     (type-name (make-record-type-name #'k #'name))
		     ((instance elm-name)
		      (generate-temporaries '(instance elm-name))))
	 (with-syntax ((((f get set mut init) ...)
			(process-fields #'k #'type-name #'ns
					#'instance #'elm-name
					#'(f* ...))))
	   #'(begin
	       (define elm-name element)
	       (define-record-type type-name
		 (parent markdown-node)
		 (fields (mutable f get set) ...)
		 (protocol
		  (lambda (n)
		    (lambda (doc f ...)
		      (define xml-doc
			(if (document? doc)
			    doc
			    (node-owner-document (markdown-node-element doc))))
		      (let ((e (document:create-element-ns xml-doc
							   ns elm-name)))
			(let ((instance ((n e) f ...)))
			  init ...
			  instance))))))
	       mut
	       ...)))))))

(define (markdown-node:append-child! node child)
  (markdown-node-parent-set! child node)
  (list-queue-add-back! (markdown-node-children node) child)
  (node:append-child! (markdown-node-element node)
		      (markdown-node-element child))
  node)

(define markdown-node:get-attribute
  (case-lambda
   ((node name)
    (markdown-node:get-attribute node *commonmark-namespace* name))
   ((node ns name)
    (element:get-attribute-ns (markdown-node-element node) ns name))))
(define markdown-node:set-attribute!
  (case-lambda
   ((node name value)
    (markdown-node:set-attribute! node *commonmark-namespace* name value))
   ((node ns name value)
    (element:set-attribute-ns! (markdown-node-element node) ns name value))))

(define markdown-node:remove-attribute!
  (case-lambda
   ((node name)
    (markdown-node:remove-attribute! node *commonmark-namespace* name))
   ((node ns name)
    (element:remove-attribute-ns! (markdown-node-element node) ns name))))

(define (markdown-node:set-text! node text)
  (define elm (markdown-node-element node))
  (define doc (node-owner-document elm))
  (let ((data (document:create-text-node doc text)))
    (node:append-child! elm data)
    node))

(define (markdown-node:get-text node)
  (define elm (markdown-node-element node))
  (node-text-content elm))

(define (markdown-node:unlink! node)
  (define elm (markdown-node-element node))
  (let ((p (node-parent-node elm)))
    (node:remove-child! p elm))
  (let* ((p (markdown-node-parent node))
	 (children (markdown-node-children p)))
    (list-queue-set-list! children
     (remove! (lambda (e) (eq? e node)) (list-queue-list children)))
    (markdown-node-parent-set! node #f)
    node))

(define (markdown-node:add-source-location! node loc)
  (list-queue-add-back! (markdown-node-source-locations node) loc)
  node)
(define (markdown-node:source-locations-set! node loc)
  (when (list-queue? loc)
    (list-queue-set-list! (markdown-node-source-locations node)
			  (list-queue-list loc)))
  node)
(define (markdown-node:source-locations node)
  (markdown-node-source-locations node))

(define-markdown-node document)
(define (make-markdown-document)
  (let* ((doc (make-xml-document))
	 (e (make-document-node doc)))
    (node:append-child! doc (markdown-node-element e))
    e))
(define-markdown-node paragraph)
(define-markdown-node block-quote (element "block_quote"))
(define-markdown-node list)
(define-markdown-node (code-block (attribute info)) (element "code_block"))
(define (code-block-node:literal-set! node literal)
  (markdown-node:set-text! node literal))
(define (code-block-node:literal node)
  (markdown-node:get-text node))
(define-record-type fenced-code-block-node
  (parent code-block-node)
  (fields fence-char
	  fence-length
	  fence-indent)
  (protocol (lambda (n)
	      (lambda (doc info fc fl fi)
		((n doc info) fc fl fi)))))

(define-markdown-node (heading (attribute level)))
(define-markdown-node thematic-break (element "thematic_break"))
(define-markdown-node html-block)
(define-markdown-node custom-block (element "custom_block"))

;; TODO inline block
(define-markdown-node (text content))
(define (text-node document content)
  (define text (make-text-node document content))
  (text-node:content-set! text content))

(define (text-node:content-set! text content)
  (markdown-node:set-text! text content))

(define-markdown-node linebreak)
(define-markdown-node softbreak)

)
