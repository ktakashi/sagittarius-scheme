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
	    document-node?
	    make-paragraph-node paragraph-node?
	    make-block-quote-node block-quote-node?
	    ;; make-list-node
	    list-node? list-node-tight list-node-tight-set!
	    list-node-type
	    make-ordered-list-node ordered-list-node?
	    ordered-list-node-start-number ordered-list-node-delimiter
	    make-bullet-list-node bullet-list-node?
	    bullet-list-node-bullet-marker
	    make-item-node item-node?
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
	    html-block-node:literal-set! html-block-node:literal
	    make-custom-block-node custom-block-node?

	    (rename (%text-node make-text-node)) text-node?
	    text-node:content text-node:content-set!

	    make-linebreak-node linebreak-node?
	    make-softbreak-node softbreak-node?
	    make-link-node link-node? link-node-destination link-node-title
	    make-image-node image-node? image-node-destination image-node-title
	    (rename (%code-node make-code-node))
	    code-node? code-node:literal-set! code-node:literal
	    (rename (%html-inline make-html-inline-node))
	    html-inline-node?
	    html-inline-node:literal-set! html-inline-node:literal

	    make-emphasis-node emphasis-node? emphasis-node-delimiter
	    make-strong-emphasis-node strong-emphasis-node?
	    strong-emphasis-node-delimiter

	    custom-inline-node?

	    (rename (custom-block-node <custom-block-node>)
		    (custom-inline-node <custom-inline-node>))
	    define-markdown-node
	    namespace element attribute
	    
	    *commonmark-namespace*

	    define-markdown-node ;; for custom node
	    markdown-node?
	    markdown-node-parent
	    markdown-node-prev
	    markdown-node-next
	    markdown-node:children
	    markdown-node:first-child
	    markdown-node:last-child
	    markdown-node:append-child!
	    markdown-node:insert-after!
	    markdown-node:get-attribute
	    markdown-node:set-attribute!
	    markdown-node:remove-attribute!
	    markdown-node:unlink!
	    markdown-node:source-locations
	    markdown-node:add-source-location!
	    markdown-node:source-locations-set!

	    markdown-node:set-text!
	    markdown-node:get-text

	    markdown-node-between

	    (rename (markdown-node-element markdown-node->dom-tree)
		    (document-node-xml-document markdown-document->xml-document)
		    (source-aware <source-aware>)
		    (markdown-node:source-locations source-aware:locations)
		    (markdown-node:add-source-location!
		     source-aware:add-location!)
		    (markdown-node:source-locations-set!
		     source-aware:locations-set!)))
    (import (rnrs)
	    (srfi :1 lists)
	    (srfi :117 list-queues)
	    (srfi :158 generators-and-accumulators)
	    (text xml dom))

(define *commonmark-namespace* "http://commonmark.org/xml/1.0")

;; The node structure will be kept on XML DOM
;; the wrapper node only managed necessary structure and meta data
(define-record-type source-aware
  (fields locations)
  (protocol (lambda (p)
	      (lambda ()
		(p (list-queue))))))
  
(define-record-type markdown-node
  (parent source-aware)
  (fields element
	  ;; below are for future convenience
	  ;; if we want to manipulate node programatically
	  (mutable parent)   ;; parent markdown node (#f = stray or root)
	  (mutable prev)
	  (mutable next)
	  children	     ;; child markdown nodes
	  )
  (protocol (lambda (n)
	      (lambda (element)
		((n) element #f #f #f (list-queue))))))
(define markdown-node-source-locations source-aware-locations)

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
	(rec clause*
	     #'*commonmark-namespace*
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
      (define (get-attribute-name field attr-name)
	(syntax-case attr-name ()
	  (() (identifier->string field))
	  ((name) #'name)))
      (let loop ((r '()) (f* f*))
	(syntax-case f* (attribute)
	  (() (reverse! r))
	  (((attribute f . attr-name) rest ...)
	   (with-syntax (((get set) (make-getter&setter k #'f "%"))
			 (attr (get-attribute-name #'f #'attr-name)))
	     (with-syntax ((mut (make-mutator k #'f #'set))
			   (ns ns)
			   (instance instance)
			   (elm-name elm-name))
	       (loop (cons #'(f get set
				(define (mut node v)
				  (set node v)
				  (if v
				      (markdown-node:set-attribute!
				       node ns attr v)
				      (markdown-node:remove-attribute!
				       node ns attr)))
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

(define (markdown-node:children node)
  (list-queue-list (markdown-node-children node)))
(define (markdown-node:first-child node)
  (define children (markdown-node-children node))
  (and (not (list-queue-empty? children))
       (list-queue-front children)))
(define (markdown-node:last-child node)
  (define children (markdown-node-children node))
  (and (not (list-queue-empty? children))
       (list-queue-back children)))

(define (markdown-node:append-child! node child)
  (define children (markdown-node-children node))
  (markdown-node:unlink! child)
  (markdown-node-parent-set! child node)
  (unless (list-queue-empty? children)
    (let ((last (list-queue-back children)))
      (markdown-node-next-set! last child)
      (markdown-node-prev-set! child last)))
  (list-queue-add-back! children child)
  (node:append-child! (markdown-node-element node)
		      (markdown-node-element child))
  node)

(define (markdown-node:insert-after! node sibling)
  (define parent (markdown-node-parent node))
  (markdown-node:unlink! sibling)
  (markdown-node-next-set! sibling (markdown-node-next node))
  (when (markdown-node-next sibling)
    (markdown-node-prev-set! (markdown-node-next sibling) sibling))
  (markdown-node-prev-set! sibling node)
  (markdown-node-next-set! node sibling)
  (markdown-node-parent-set! sibling parent)
  ;; maybe we should use flexible-vector
  (let ((children (markdown-node-children parent)))
    (let loop ((r '()) (c* (list-queue-list children)))
      (cond ((null? c*) (list-queue-set-list! children (reverse! r)))
	    ((eq? (car c*) node)
	     (loop (cons sibling (cons node r)) (cdr c*)))
	    (else (loop (cons (car c*) r) (cdr c*))))))
  (let ((n (markdown-node-element node)))
    ;; this works even `node-next-sibling` returns #f as `node:insert-before!`
    ;; searches the second argumemt from the children of first argument
    ;; and if it's not found, then it pushed to the last
    (node:insert-before! (node-parent-node n)
			 (node-next-sibling n)
			 (markdown-node-element sibling)))
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
  (cond ((node-first-child elm) =>
	 ;; must be text
	 (lambda (t) (character-data-data-set! t text)))
	(else
	 (let ((data (document:create-text-node doc text)))
	   (node:append-child! elm data))))
  node)

(define (markdown-node:get-text node)
  (define elm (markdown-node-element node))
  (let ((t (node-first-child elm)))
    (character-data-data t)))

(define (markdown-node:unlink! node)
  (define elm (markdown-node-element node))
  (cond ((node-parent-node elm) => (lambda (p) (node:remove-child! p elm))))
  (when (markdown-node-prev node)
    (markdown-node-next-set! (markdown-node-prev node)
			     (markdown-node-next node)))
  (when (markdown-node-next node)
    (markdown-node-prev-set! (markdown-node-next node)
			     (markdown-node-prev node)))
  
  (cond ((markdown-node-parent node) =>
	 (lambda (p)
	   (let ((children (markdown-node-children p)))
	     (list-queue-set-list! children
	      (remove! (lambda (e) (eq? e node)) (list-queue-list children)))
	     (markdown-node-parent-set! node #f)))))
  node)

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

(define (markdown-node-between start end)
  (make-unfold-generator
   (lambda (s) (or (not s) (eq? s end)))
   values
   (lambda (s) (markdown-node-next s))
   (markdown-node-next start)))


(define-markdown-node (document xml-document))
(define (make-markdown-document)
  (let* ((doc (make-xml-document))
	 (e (make-document-node doc doc)))
    (node:append-child! doc (markdown-node-element e))
    e))
(define-markdown-node paragraph)
(define-markdown-node block-quote (element "block_quote"))
(define-markdown-node (list (attribute type) (attribute tight)))
(define-record-type ordered-list-node
  (parent list-node)
  (fields start-number delimiter)
  (protocol (lambda (n)
	      (lambda (doc number delimiter)
		((n doc "ordered" "true") number delimiter)))))
(define-record-type bullet-list-node
  (parent list-node)
  (fields bullet-marker)
  (protocol (lambda (n)
	      (lambda (doc marker)
		((n doc "bullet" "true") marker)))))
(define-markdown-node item)
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
(define-markdown-node html-block (element "html_block"))
(define (html-block-node:literal-set! node literal)
  (markdown-node:set-text! node literal))
(define (html-block-node:literal node)
  (markdown-node:get-text node))
(define-markdown-node custom-block (element "custom_block"))

;; TODO inline block
(define-markdown-node (text content))
(define (%text-node document content)
  (define text (make-text-node document content))
  (text-node:content-set! text content))
(define (text-node:content-set! text content)
  (text-node-content-set! text content)
  (markdown-node:set-text! text content))
(define (text-node:content text) (text-node-content text))

(define-markdown-node linebreak)
(define-markdown-node softbreak)
(define-markdown-node (link (attribute destination) (attribute title)))
(define-markdown-node (image (attribute destination) (attribute title)))
(define-markdown-node (code literal))
(define (%code-node doc literal)
  (let ((node (make-code-node doc literal)))
    (code-node:literal-set! node literal)))
(define (code-node:literal-set! node literal)
  (code-node-literal-set! node literal)
  (markdown-node:set-text! node literal))
(define (code-node:literal node) (code-node-literal node))

(define-markdown-node html-inline (element "html_inline"))
(define (%html-inline doc literal)
  (let ((node (make-html-inline-node doc)))
    (html-inline-node:literal-set! node literal)))
(define (html-inline-node:literal-set! node literal)
  (markdown-node:set-text! node literal))
(define (html-inline-node:literal node) (markdown-node:get-text node))

(define-markdown-node (emphasis delimiter) (element "emph"))
(define-markdown-node (strong-emphasis delimiter) (element "strong"))
(define-markdown-node custom-inline (element "custom_inline"))
)
