;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/format/html.scm - HTML format
;;;  
;;;   Copyright (c) 2022 Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius document format html)
    (export document->html
	    html->document

	    (rename (html-output-options <html-output-options>))
	    html-output-options?
	    html-output-options-builder)
    (import (rnrs)
	    (record builder)
	    (sagittarius) ;; for gensym
	    (sagittarius conditions)
	    (sagittarius document output)
	    (sagittarius document tools)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :117 list-queues)
	    (text sxml serializer)
	    (text sxml sxpath)
	    (text sxml tools))

(define-record-type html-output-options
  (parent <document-output-options>)
  (fields default-title
	  header-populator
	  attribute-resolver))

(define-syntax html-output-options-builder
  (make-record-builder html-output-options
   ((default-title "no title"))))

;;; We don't suppport HTML parsing, it's too flexible
(define (html->document input . options)
  (implementation-restriction-violation 'html->document
   "HTML to document is not supported"))

(define (document->html doc options out)
  (define (populate-header options info)
    (define header-populator (html-output-options-header-populator options))
    (or (and header-populator (header-populator options info))
	'()))
  (let ((content (document:content doc))
	(info (document:info doc)))
    (unless content
      (assertion-violation 'document->html "Unknown document" doc))
    (srl:sxml->html
     `(html
       (head
	,@(populate-header options info)
	(title ,(search-title doc options)))
       (body
	,@(map (->html options) (sxml:content content))))
     out)))

(define heading1-path
  (if-car-sxpath "//content/header[@level='1']/text()"))
(define (search-title doc options)
  (cond ((heading1-path doc))
	((html-output-options? options)
	 (html-output-options-default-title options))
	(else "no title")))

(define ((->html options) element)
  (cond ((string? element) element)
	((pair? element) (element->html element options))
	(else (assertion-violation 'document->html "Unknown document element"
				   element))))

(define (element->html element options)
  (define tag (sxml:name element))
  (cond ((assq tag node-handlers) =>
	 (lambda (proc) ((cadr proc) element options)))
	(else (error 'element->html "Tag not supported" element))))

(define (id/tag element)
  (cond ((sxml:attr element 'tag))
	((sxml:attr element 'id))
	(else #f)))

(define (header-handler element options)
  (define level (sxml:attr element 'level))
  (define tag (string->symbol (string-append "h" level)))
  (define mark (string->symbol (string-append "header-" level)))
  `(,tag (@ (node-type ,(symbol->string mark))
	    ,@(options->attribute options mark))
	 (a (@ ,@(options->attribute options 'header-anchor)
	       ,@(let ((name (id/tag element)))
		   (if name `((name ,name)) '())))
	    ,@(map (->html options) (sxml:content element)))))

(define (paragraph-handler element options)
  `(p (@ (node-type "paragraph")
	 ,@(sxml:attr-list element)
	 ,@(options->attribute options 'paragraph))
      ,@(map (->html options) (sxml:content element))))

(define (eval-handler element options)
  (document-output:eval element (car (sxml:content element))))

(define (marker-handler element options)
  (cond ((document-output:resolve-marker (sxml:name element)) =>
	 (->html options))
	(else element)))

(define (list-handler element options)
  (define ((->item tag) item)
    (unless (eq? 'item (sxml:name item))
      (assertion-violation 'html-list-handler "Unknown item" element))
    `(li (@ ,@(options->attribute options 'list-item))
	 ,@(map (->html options) (sxml:content item))))
    
  (let* ((style (string->symbol (cond ((sxml:attr element 'style))
				      (else "bullet"))))
	 (start (and (eq? style 'number)
		     (cond ((sxml:attr element 'start))
			   (else #f))))
	 (tag (if (eq? style 'bullet) 'ul 'ol))
	 (marker (if (eq? style 'bullet) 'bullet-list 'ordered-list)))
    `(,tag (@ (node-type ,(symbol->string marker))
	      ,@(if start `((start ,start)) '())
	      ,@(options->attribute options marker))
	   ,@(map (->item tag) (sxml:content element)))))

(define default-callback (document-output:make-file-link-callback ".html"))
(define (get-callback options)
  (cond ((document-output-options-link-source-callback options))
	(else default-callback)))
(define (link-handler element options)
  (define channel (list-queue))
  (define (create content link)
    `(a (@ (href ,link)
	    ,@(options->attribute options 'link))
	 ,@(map (->html options) content)))
  (define (put-channel content link)
    (list-queue-add-front! channel (create content link)))
  (let ((source (sxml:attr element 'source))
	(format (sxml:attr element 'format))
	(href (sxml:attr element 'href))
	(anchor (sxml:attr element 'anchor)))
    (cond (source ((get-callback options) source format put-channel)
		  (list-queue-remove-front! channel))
	  (href (create (sxml:content element) href))
	  (anchor (create (sxml:content element) (string-append "#" anchor)))
	  (else (document-output-error 'write-link
				       "Link doesn't have source or href"
				       element)))))

(define (section-handler element options)
  (let ((level (sxml:attr element 'level)))
    (if level
	(let ((marker (string->symbol (string-append "section-" level))))
	  `(section (@ (node-type "section")
		       ,@(options->attribute options marker))
		    ,@(map (->html options) (sxml:content element))))
	;; section without level = div for now
	`(div (@ (node-type "section")
		 ,@(options->attribute options 'anon-section))
	      ,@(map (->html options) (sxml:content element))))))

(define (symbol-append . s*)
  (string->symbol (string-join (map symbol->string s*) "")))
(define (table-handler element options)
  (define (->cell cell)
    (let ((tag (sxml:name cell)))
      `(,(if (eq? tag 'header) 'th 'td)
	(@ ,@(options->attribute options (symbol-append 'table- tag)))
	,@(map (->html options) (sxml:content cell)))))
  (define (->row row)
    `(tr (@ ,@(options->attribute options 'table-row))
	 ,@(map ->cell (sxml:content row))))
  (let ((title (sxml:attr element 'title))
	(rows (map ->row (sxml:content element))))
    `(table (@ (node-type "table")
	       ,@(options->attribute options 'table))
	    ,@(if title
		  `((caption (@ ,@(options->attribute options 'table-title))
			     ,title))
		  '())
	    ,@rows)))

(define ((emphasis-handler orig tag) element options) ;; SRFI-219 style :)
  `(,tag (@ (node-type ,(symbol->string tag))
	    ,@(options->attribute options orig))
	 ,@(map (->html options) (sxml:content element))))

(define (raw-string-handler element options)
  `(*RAW-HTML* ,@(sxml:content element)))

(define (code-handler element options)
  `(code (@ (node-type "code")
	    ,@(options->attribute options 'code))
	 ,@(map (->html options) (sxml:content element))))

(define (define-handler element options)
  (define conv (->html options))
  (let ((category (or (sxml:attr element 'category) "Unknown"))
	(tag (or (sxml:attr element 'tag) (symbol->string (gensym))))
	(content (sxml:content element)))
    `(div (@ (node-type "define")
	     ,@(options->attribute options 'define))
	  (span ,@(options->attribute options 'define-catetory) ,category)
	  (a (@ (name ,tag))
	     (span (@ ,@(options->attribute options 'define-name))
		   ,(conv (car content)))
	     ,@(map (lambda (e)
		      `(span (@ ,@(options->attribute options 'define-arg))
			     ,(conv e)))
		    (cdr content))))))

(define (codeblock-handler element options)
  (define (output? e) (document-element-of? e 'output))
  (define (codeblock lang code style)
    `(pre (@ ,@(if lang `((lang ,lang)) '())
	     (node-type ,(symbol->string style))
	     ,@(options->attribute options 'codeblock))
	  (code ,@(map (->html options) code))))
  (let-values (((output code) (partition output? (sxml:content element))))
    (let ((lang (sxml:attr element 'lang))
	  (style (cond ((sxml:attr element 'style) => string->symbol)
		       (else 'block))))
      (if (null? output)
	  (codeblock lang code style)
	  `(div (@ (node-type ,(symbol->string style))
		   ,@(options->attribute options
					 (if (eq? style 'block)
					     'block-container
					     'snipet-container)))
		,(codeblock lang code style)
		(span (@ ,@(options->attribute options 'codeblock-arrow)))
		(span (@ ,@(options->attribute options 'codeblock-result))
		      ,@(append-map (lambda (o)
				      (map (->html options) (sxml:content o)))
				    output)))))))

(define (blockquote-handler element options)
  `(blockquote (@ (node-type "blockquote")
		,@(options->attribute options 'blockquote))
	       ,@(map (->html options) (sxml:content element))))

(define (dlist-handler element options)
  (define e->html (->html options))
  (define (->ditem e)
    (define (title? e) (document-element-of? e 'title))
    (define (->dt title)
      `(dt (@ ,@(options->attribute options 'dlist-title))
	   ,@(map e->html (sxml:content title))))
    (let-values (((title* desc) (partition title? (sxml:content e))))
      `(,@(map ->dt title*)
	(dd (@ ,@(options->attribute options 'dlist-description))
	    ,@(map e->html desc)))))
  `(dl (@ (node-type "dlist")
	  ,@(options->attribute options 'dlist))
       ,@(append-map ->ditem (sxml:content element))))

(define ((simple-handler tag) element options) tag)

(define (ref-handler element options)
  `(a (@ (node-type "footnote-ref")
	 (href ,(string-append "#" (sxml:attr element 'name)))
	 (name ,(string-append (sxml:attr element 'name) "-ref")))
      (sup (@ ,@(options->attribute options 'ref))
	   ,((->html options) (sxml:attr element 'label)))))

(define (footnote-handler element options)
  `(section (@ (node-type "footnote")
	       (label ,(sxml:attr element 'label))
	       ,@(options->attribute options 'footnote))
	    (p (@ (node-type "footnote-item")
		  ,@(options->attribute options 'footnote-item))
	       (a (@ (name ,(sxml:attr element 'name))
		     (href ,(string-append "#"
					   (sxml:attr element 'name) "-ref")))
		  ,((->html options) (sxml:attr element 'label)))
	       (div (@ (node-type "footnote-description"))
		    ,@(map (->html options) (sxml:content element))))))

(define (annotation-handler element options)
  (let ((name (string->symbol (sxml:attr element 'name)))
	(value (sxml:attr element 'value)))
    (case name
      ((since)
       `(since (@ (node-type "since"))
	       (span (@ (node-type "since-version")
			(version ,value))
		     ,value)))
      ((deprecated) `(deprecated (@ (node-type "deprecated"))))
      (else
       (if (string-null? value)
	   `(,name (@ (node-type "annotation")))
	   `(,name (@ (node-type "annotation"))
		   (span (@ (node-type "annotation-value")
			    (value ,value))
			 ,value)))))))

(define (expansion-handler element options)
  `(details (@ (node-type "expansion")
	       (source ,(sxml:attr element 'source)))
    (summary ,(sxml:attr element 'title))
    ,@(map (->html options) (sxml:content element))))

(define (since-handler element options)
  `(since (@ (node-type "since"))
	  (span (@ (node-type "since-version")
		   (version ,(sxml:attr element 'version)))
		,(sxml:attr element 'version))))

(define (deprecated-handler element options)
  `(deprecated (@ (node-type "deprecated"))))

(define (options->attribute options marker)
  (let ((resolver (html-output-options-attribute-resolver options)))
    (or (and resolver (resolver options marker))
	'())))

(define node-handlers
  `((header ,header-handler)
    (paragraph ,paragraph-handler)
    (list ,list-handler)
    (link ,link-handler)
    (section ,section-handler)
    (table ,table-handler)
    (italic ,(emphasis-handler 'italic 'i))
    (strong ,(emphasis-handler 'strong 'strong))
    (raw-string ,raw-string-handler)
    (code ,code-handler)
    (define ,define-handler)
    (codeblock ,codeblock-handler)
    (blockquote ,blockquote-handler)
    (dlist ,dlist-handler)
    ;; (include ,include-handler)
    (linebreak ,(simple-handler '(br)))
    (thematic-break ,(simple-handler '(hr)))

    (index-table ,marker-handler)
    (author ,marker-handler)
    (table-of-contents ,marker-handler)
    (eval ,eval-handler)
    (ref ,ref-handler)
    (footnote ,footnote-handler)
    (annotation ,annotation-handler)
    (expansion ,expansion-handler)
    ;;(since ,since-handler)
    ;;(deprecated ,deprecated-handler)
    ))

)
