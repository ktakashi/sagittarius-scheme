;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/format/markdown/reader.scm - Markdown reader
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
(library (sagittarius document format markdown reader)
    (export read-markdown
	    )
    (import (rnrs)
	    (sagittarius document input)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :115 regexp)
	    (srfi :158 generators-and-accumulators)
	    (text markdown parser)
	    (text markdown parser inlines)
	    (text markdown parser nodes)
	    (text markdown parser post-processor)
	    (text markdown converter api)
	    (text markdown extensions)
	    (text markdown extensions gfm)
	    (text markdown extensions definition-lists)
	    (text sxml tools))

(define read-markdown 
  (case-lambda
   ((input) (read-markdown input (document-input-options-builder)))
   ((input options)
    (define sagittarius-document-parser
      (markdown-parser-builder:build
       (markdown-parser-builder
	(extensions (list gfm-extensions
			  definition-lists-extension
			  document-extension)))))
    (let ((e (parse-markdown sagittarius-document-parser
			     (document-input-port input))))
      (for-each (lambda (processor) (processor e))
		adjust-section-processors)
      (markdown-node:set-attribute! e "filename"
				    (document-input-filename input))
      (markdown-converter:convert markdown->document-converter 'document e)))))

(define (section-level-of level)
  (define l (number->string level))
  (lambda (node)
    (and (section-node? node)
	 (equal? (section-node-level node) l))))

(define (adjust-section node visit-children)
  (define l (section-node-level node))
  (define n (string->number l))
  (define same? (section-level-of (string->number l)))
  (define (higher? node) (> (string->number (section-node-level node)) n))
  (define (continue? node) (and (same? node) (not (higher? node))))
  (let loop ((sib (markdown-node-next node)))
    (unless (or (not sib) (continue? sib))
      (let ((next (markdown-node-next sib)))
	(markdown-node:append-child! node sib)
	(loop next)))))

(define adjust-section-processors
  ;; order is important (top to bottom)
  (list (make-post-processor
	 (make-post-processor-spec (section-level-of 1) adjust-section))
	(make-post-processor
	 (make-post-processor-spec (section-level-of 2) adjust-section))
	(make-post-processor
	 (make-post-processor-spec (section-level-of 3) adjust-section))
	(make-post-processor
	 (make-post-processor-spec (section-level-of 4) adjust-section))))

(define *document-namespace* "https://markdown.sagittarius-scheme.io/document")
;; post processors
(define-markdown-node (output-code code output block?)
  (namespace *document-namespace*)
  (element "doc:output-code"))
(define *output-marker* (rx (* space) "=>" (* space)))
(define (code-output-processor node visit-children)
  (define (check-code node)
    (let ((next (markdown-node-next node)))
      (and (code-node? next)
	   (cond ((code-node? (markdown-node-prev node))
		  (cons (markdown-node-prev node) next))
		 ((and (paragraph-node? (markdown-node-parent node))
		       (code-block-node? (markdown-node-prev
					  (markdown-node-parent node))))
		  (cons (markdown-node-prev
			 (markdown-node-parent node)) next))
		 (else #f)))))
  (when (regexp-matches *output-marker* (text-node:content node))
    ;; <code> => <code>: snipet
    ;; <codeblock> => <code>: block
    (cond ((check-code node) =>
	   (lambda (code&output)
	     (let* ((code (car code&output))
		    (out  (cdr code&output))
		    (parent (markdown-node-parent code))
		    (output-code (make-output-code-node parent code out
				  (code-block-node? code))))
	       (markdown-node:insert-before! code output-code)
	       (markdown-node:append-child! output-code code)
	       (markdown-node:append-child! output-code out)
	       (markdown-node:unlink! node)))))))
  
(define code-output-post-processor
  (make-post-processor
   (make-post-processor-spec text-node? code-output-processor)))

(define-markdown-node (include (attribute file "doc:file"))
  (namespace *document-namespace*)
  (element "doc:include"))
(define (include-processor node visit-children)
  (define child (markdown-node:first-child node))
  (and-let* (( (paragraph-node? child) )
	     (text (markdown-node:first-child child))
	     ( (text-node? text) )
	     ( (string=? "@[" (text-node:content text)) )
	     (link (markdown-node-next text))
	     ( (link-node? link) )
	     (close (markdown-node-next link))
	     ( (text-node? close) )
	     ( (string=? "]" (text-node:content close)) )
	     (parent (markdown-node-parent node)))
    (let ((include-node (make-include-node (markdown-node-parent parent)
					   (link-node-destination link))))
      (markdown-node:unlink! node)
      (markdown-node:insert-before! parent include-node)
      (unless (markdown-node:first-child parent)
	(markdown-node:unlink! parent)))))

(define include-post-processor
  (make-post-processor
   (make-post-processor-spec item-node? include-processor)))

(define-markdown-node (section (attribute level "doc:level"))
  (namespace *document-namespace*)
  (element "doc:section"))

(define *section* (rx "[§" ($ (/ "14") ) "]" (+ space) ($ (+ any))))
(define (section-processor node visit-children)
  (define child (markdown-node:first-child node))
  (and-let* (( (text-node? child) )
	     (text (text-node:content child))
	     (m (regexp-matches *section* text))
	     (level (m 1))
	     (new-text (m 2))
	     (section (make-section-node (markdown-node-parent node) level)))
    (text-node:content-set! child new-text)
    (markdown-node:insert-before! node section)))

(define section-post-processor
  (make-post-processor
   (make-post-processor-spec heading-node? section-processor)))

(define-markdown-node eval (namespace *document-namespace*)
  (element "doc:eval"))
(define-record-type eval-delimiter-processor
  (parent <delimiter-processor>)
  (protocol (lambda (n)
	      (lambda ()
		((n #\@ #\@ 2
		    (make-delimiter-processor:process make-eval-node)))))))
(define-markdown-node marker (namespace *document-namespace*)
  (element "doc:marker"))
(define-record-type marker-delimiter-processor
  (parent <delimiter-processor>)
  (protocol (lambda (n)
	      (lambda ()
		((n #\{ #\} 2
		    (make-delimiter-processor:process make-marker-node)))))))
(define (make-delimiter-processor:process make-node)
  (lambda (dp opening closing)
    (if (and (>= (delimiter:length opening) 2)
	     (>= (delimiter:length closing) 2))
	(let* ((opener (delimiter:opener opening))
	       (closer (delimiter:closer closing))
	       (enode (make-node opener)))
	  (generator-for-each (lambda (n)
				(let ((loc (markdown-node:source-locations n)))
				  (markdown-node:append-child! enode n)))
			      (markdown-node-between opener closer))
	  (markdown-node:insert-after! opener enode)
	  2)
	0)))


(define document-extension
  (markdown-extension-builder
   (delimiter-processors (list make-eval-delimiter-processor
			       make-marker-delimiter-processor))
   (post-processors (list code-output-post-processor
			  include-post-processor
			  section-post-processor))))

;; Converters
(define (convert-document document data next)
  `(document
    (info (source ,(markdown-node:get-attribute document "filename")))
    (content ,@(append-map next (markdown-node:children document)))))

(define (convert-paragraph paragraph data next)
  (convert-container 'paragraph paragraph data next))

(define *category* (rx "[!" ($ (+ (~ #\]))) "]" (* space)))
(define (convert-heading node data next)
  (define (check-define node)
    (define fc (markdown-node:first-child node))
    ;; (write (text-node:content fc)) (newline)
    (cond ((and (string=? "6" (heading-node-level node))
		(text-node? fc)
		(regexp-matches *category* (text-node:content fc))) =>
	   (lambda (m)
	     (let ((category (regexp-match-submatch m 1)))
	       (and (code-node? (markdown-node-next fc))
		    (cons* category
			   (code-node:literal (markdown-node-next fc))
			   (cddr (markdown-node:children node)))))))
	  (else #f)))
  (define (nonempty-text-node? n)
    (define (empty/space? s)
      (or (string-null? s)
	  (string-every char-whitespace? s)))
    (or (not (text-node? n))
	(not (empty/space? (text-node:content n)))))
  (cond ((check-define node) =>
	 (lambda (category&name&args)
	   `((define (@ (category ,(car category&name&args)))
	       ,(cadr category&name&args)
	       ,@(append-map next (filter nonempty-text-node?
					  (cddr category&name&args)))))))
	(else
	 `((header (@ (level ,(heading-node-level node)))
		   ,@(append-map next (markdown-node:children node)))))))

(define (convert-block-quote node data next)
  (convert-container 'blockquote node data next))
(define (convert-bullet-list node data next)
  `((list (@ (style "bullet"))
	  ,@(append-map next (markdown-node:children node)))))
(define (convert-ordered-list node data next)
  `((list (@ (style "number")
	     (start ,(number->string (ordered-list-node-start-number node))))
	  ,@(append-map next (markdown-node:children node)))))

(define (convert-item node data next)
  `((item (@) ,@(append-map next (markdown-node:children node)))))

(define (convert-container tag node data next)
  `((,tag (@) ,@(append-map next (markdown-node:children node)))))

(define (convert-link node data next)
  `((link (@ (href ,(link-node-destination node)))
	  ,@(append-map next (markdown-node:children node)))))

(define (convert-image node data next)
  (let-values (((out e) (open-string-output-port)))
    (define (alt-text-converter node)
      (cond ((or (linebreak-node? node) (softbreak-node? node))
	     (put-string out "\n"))
	    ((text-node? node) (put-string out (text-node:content node)))
	    (else (for-each alt-text-converter
			    (markdown-node:children node)))))
    (alt-text-converter node)
    (let ((alt-text (e)))
      `((image (@ (src ,(image-node-destination node))
		  (alt ,alt-text)
		  ,@(cond ((image-node-title node) =>
			   (lambda (title) `((title ,title))))
			  (else '()))))))))

(define (convert-thematic-break thematic-break data next)
  '((thematic-break (@))))

(define (trim-info info)
  (let ((lang (cond ((string-index info #\space) =>
		     (lambda (i) (substring info 0 i)))
		    (else info))))
    lang))
(define (convert-code-block code-block data next)
  (let ((style (cond ((code-block-node-info code-block) => trim-info)
		     (else ""))))
    `((codeblock (@ (lang ,style)
		    (style "block"))
		 ,(code-block-node:literal code-block)))))

(define (convert-output-code node data next)
  (define code (output-code-node-code node))
  (define output (code-node:literal (output-code-node-output node)))
  (let ((style (if (output-code-node-block? node) "block" "snipet"))
	(lang (cond ((and (code-block-node? code) (code-block-node-info code))
		     => trim-info)
		    (else ""))))
    `((codeblock (@ (lang ,lang)
		    (style ,style))
		 (output ,output)
		 ,(if (code-block-node? code)
		      (code-block-node:literal code)
		      (code-node:literal code))))))

(define (convert-code code data next)
  `((code (@) ,(code-node:literal code))))

(define (convert-softbreak node data next) '("\n"))

(define (convert-html-block node data next)
  `((raw-string ,(html-block-node:literal node))))

(define (convert-html-inline node data next)
  `((raw-string ,(html-inline-node:literal node))))

(define (convert-linebreak node data next) '((linebreak)))

(define (convert-emphasis node data next)
  (convert-inline 'italic node data next))
(define (convert-strong-emphasis node data next)
  (convert-inline 'strong node data next))
(define (convert-inline tag node data next)
  `((,tag (@) ,@(append-map next (markdown-node:children node)))))

(define (convert-text text data next) (list (text-node:content text)))

(define (convert-definition-list-block node data next)
  `((dlist (@) ,@(append-map next (markdown-node:children node)))))
(define (convert-definition-item node data next)
  `((ditem (@) ,@(append-map next (markdown-node:children node)))))
(define (convert-definition-term node data next)
  `((title (@) ,@(append-map next (markdown-node:children node)))))
(define (convert-definition-description node data next)
  (append-map next (markdown-node:children node)))

(define (convert-include node data next)
  (define file (include-node-file node))
  `((include (link (@ (source ,file) (format "markdown")) ,file))))

(define (convert-eval node data next)
  `((eval ,@(append-map next (markdown-node:children node)))))

(define (convert-marker node data next)
  (let ((marker (append-map next (markdown-node:children node))))
    `((,(string->symbol (string-join marker))))))

(define (convert-section node data next)
  (let ((level (section-node-level node)))
    ;; (write `(section (@ (level ,level))
    ;; 		     ,@(append-map next (markdown-node:children node))))
    ;; (newline)
    `((section (@ (level ,level))
	       ,@(append-map next (markdown-node:children node))))))

(define (convert-table-block node data next)
  `((table (@) ,@(append-map next (markdown-node:children node)))))
(define (convert-table-head node data next)
  (append-map next (markdown-node:children node)))
(define (convert-table-body node data next)
  (append-map next (markdown-node:children node)))
(define (convert-table-row node data next)
  `((row ,@(append-map next (markdown-node:children node)))))
(define (convert-table-cell node data next)
  (define tag (if (gfm:table-cell-node-header? node) 'header 'cell))
  ;; TODO alignment
  `((,tag (@) ,@(append-map next (markdown-node:children node)))))


(define-markdown-converter markdown->document-converter document
  (document-node? convert-document)
  (paragraph-node? convert-paragraph)
  (code-block-node? convert-code-block)
  (thematic-break-node? convert-thematic-break)
  (bullet-list-node? convert-bullet-list)
  (ordered-list-node? convert-ordered-list)
  (item-node? convert-item)
  (block-quote-node? convert-block-quote)
  (heading-node? convert-heading)
  (link-node? convert-link)
  (image-node? convert-image)
  (text-node? convert-text)
  (code-node? convert-code)
  (html-block-node? convert-html-block)
  (html-inline-node? convert-html-inline)
  (softbreak-node? convert-softbreak)
  (linebreak-node? convert-linebreak)
  (emphasis-node? convert-emphasis)
  (strong-emphasis-node? convert-strong-emphasis)
  (definition-list-block-node? convert-definition-list-block)
  (definition-item-node? convert-definition-item)
  (definition-term-node? convert-definition-term)
  (definition-description-node? convert-definition-description)
  (output-code-node? convert-output-code)
  (include-node? convert-include)
  (eval-node? convert-eval)
  (marker-node? convert-marker)
  (section-node? convert-section)
  (gfm:table-block-node? convert-table-block)
  (gfm:table-head-node? convert-table-head)
  (gfm:table-body-node? convert-table-body)
  (gfm:table-row-node? convert-table-row)
  (gfm:table-cell-node? convert-table-cell))

)
