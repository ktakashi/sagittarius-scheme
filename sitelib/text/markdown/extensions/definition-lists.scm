;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/extensions/definition-lists.scm - Definition lists extension
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
(library (text markdown extensions definition-lists)
    (export definition-lists-extension
	    definition-lists-converter

	    ;; For custom converter
	    definition-list-block-node?
	    definition-item-node?
	    definition-term-node?
	    definition-description-node?)
    (import (rnrs)
	    (srfi :117 list-queues)
	    (srfi :197 pipeline)
	    (text markdown extensions api)
	    (text markdown parser blocks)
	    (text markdown parser factories)
	    (text markdown parser inlines)
	    (text markdown parser nodes)
	    (text markdown parser parsing)
	    (text markdown parser source)
	    (text markdown converter html))

(define *definition-list-namespace* 
  "https://markdown.sagittarius-scheme.io/definition-list")

(define-markdown-node (definition-list-block)
  (namespace *definition-list-namespace*)
  (element "dl:definition-list"))
(define-markdown-node (definition-item terms descriptions)
  (namespace *definition-list-namespace*)
  (element "dl:item"))
(define-markdown-node (definition-term term)
  (namespace *definition-list-namespace*)
  (element "dl:term"))
(define-markdown-node (definition-description description)
  (namespace *definition-list-namespace*)
  (element "dl:description"))

(define (definition-item document terms)
  (let ((item (make-definition-item-node document terms '())))
    (source-lines:for-each
     (lambda (term)
       (definition-item:add-term! item (source-line-content term))) terms)
    item))
(define (definition-item:add-term! item term)
  (let ((term-node (make-definition-term-node item term)))
    ;;(markdown-node:set-text! term-node term)
    (markdown-node:append-child! item term-node)
    item))

(define-record-type definition-list-block-parser
  (parent <block-parser>)
  (protocol
   (lambda (n)
     (lambda (dlist-block-node)
       ((n dlist-block-node #t #f
	   (lambda (self block) (definition-item-node? block))
	   (lambda (self ps) (block-continue:at-index (parser-state-index ps)))
	   block-parser-default-add-line!
	   block-parser-default-add-location!
	   (lambda (self)
	     (define block (block-parser-block self))
	     (define prev (markdown-node-prev block))
	     ;; Merge two definition-list-block if they are direct siblings
	     (when (definition-list-block-node? prev)
	       (for-each (lambda (item)
			   (markdown-node:append-child! prev item))
			 (markdown-node:children block))
	       (markdown-node:unlink! block)))
	   block-parser-default-parse-inlines!))))))

(define-record-type definition-item-parser
  (parent <block-parser>)
  (protocol
   (lambda (n)
     (lambda (document terms)
       ((n (definition-item document terms) #t #f
	   (lambda (self block)
	     (or (definition-term-node? block)
		 (definition-description-node? block)))
	   (lambda (self ps) (block-continue:at-index (parser-state-index ps)))
	   block-parser-default-add-line!
	   block-parser-default-add-location!
	   block-parser-default-close-block!
	   (lambda (self inline-parser)
	     (define terms
	       (filter definition-term-node?
		       (markdown-node:children (block-parser-block self))))
	     (for-each (lambda (term)
			 (let ((line (definition-term-node-term term)))
			   (inline-parser:parse! inline-parser
			    (source-lines:of (source-line:of line #f))
			    term))) terms))))))))

(define-record-type definition-description-parser
  (parent <block-parser>)
  (fields content-indent
	  source-lines)
  (protocol
   (lambda (n)
     (lambda (document content-indent)
       ((n (make-definition-description-node document (list-queue)) #t #f
	   (lambda (self block) #t)
	   (lambda (self ps)
	     (define line (parser-state-line ps))
	     (define nns (parser-state-next-non-space-index ps))
	     (cond ((parser-state-blank? ps)
		    (if (>= (source-line:length line)
			    (definition-description-parser-content-indent self))
			(block-continue:at-column
			 (definition-description-parser-content-indent self))
			(block-continue:none)))
		   ((and (eqv? #\: (source-line:char-at line nns))
			 ;; Check if the active parser is paragraph parser or
			 ;; not to avoid closing blocks unnecessarily
			 ;; e.g. inside of a code block
			 ;; foo
			 ;; : bar
			 ;;   ```
			 ;;   : bla
			 ;;   ```
			 (paragraph-parser?
			  (parser-state:active-block-parser ps)))
		    (block-continue:none))
		   (else (block-continue:at-column
			  (parser-state-column ps)))))
	   block-parser-default-add-line!
	   block-parser-default-add-location!
	   block-parser-default-close-block!
	   block-parser-default-parse-inlines!)
	content-indent
	(list-queue))))))
  
(define (try-start-definition-list ps mbp)
  (define (search-non-space line start)
    (do ((len (source-line:length line))
	 (i start (+ i 1)))
	((or (= i len) (not (source-line:whitespace? line i))) i)))
  (if (>= (parser-state-index ps) +parsing-code-block-indent+)
      (block-start:none)
      (let ((line (parser-state-line ps))
	    (nns (parser-state-next-non-space-index ps)))
	(if (eqv? #\: (source-line:char-at line nns))
	    (let ((lines (matched-block-parser:paragraph-lines mbp))
		  (doc (parser-state-document ps))
		  (matched (matched-block-parser:get mbp)))
	      (if (source-lines:empty? lines)
		  (if (definition-item-parser? matched)
		      (let* ((nns2 (search-non-space line (+ nns 1)))
			     (ddp (make-definition-description-parser doc
				   (- nns2 nns))))
			(chain (block-start:of ddp)
			       (block-start:at-column _
				(+ (parser-state-column ps) nns2))))
		      (block-start:none))
		  (let* ((dlb (make-definition-list-block-node doc))
			 (dlbp (make-definition-list-block-parser dlb))
			 (dip (make-definition-item-parser doc lines))
			 (nns2 (search-non-space line (+ nns 1)))
			 (ddp (make-definition-description-parser doc
			       (- nns2 nns))))
		    (markdown-node:append-child! dlb (block-parser-block dip))
		    (chain (block-start:of dlbp dip ddp)
			   (block-start:at-column _
			    (+ (parser-state-column ps) nns2))
			   (block-start:replace-active-block-parser _)))))
	    (block-start:none)))))

(define definition-lists-extension
  (markdown-extension-builder
   (block-factories `(,(lambda () try-start-definition-list)))))

(define (convert-definition-list-block node data next)
  `("\n"
    (dl (@ ,@(html-attribute node data 'dl))
	,@(convert-nodes next (markdown-node:children node)))
    "\n"))

(define (convert-definition-item node data next)
  ;; This doesn't have HTML tag, so just proceed to children
  (convert-nodes next (markdown-node:children node)))

(define (convert-definition-term node data next)
  `("\n"
    (dt (@ ,@(html-attribute node data 'dt))
	,@(convert-nodes next (markdown-node:children node)))
    "\n"))
(define (convert-definition-description node data next)
  `("\n"
    (dd (@ ,@(html-attribute node data 'dd))
	,@(convert-nodes next (markdown-node:children node)))
    "\n"))

(define-markdown-converter definition-lists-converter html
  (definition-list-block-node? convert-definition-list-block)
  (definition-item-node? convert-definition-item)
  (definition-term-node? convert-definition-term)
  (definition-description-node? convert-definition-description))
)
