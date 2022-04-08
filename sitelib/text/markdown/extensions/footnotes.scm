;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/extensions/footnotes.scm - Footnotes
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

;; Pandoc or PHP Markdown Extra style foot notes

#!nounbound
(library (text markdown extensions footnotes)
    (export footnotes-extension
	    footnotes-markdown-converter)
    (import (rnrs)
	    (srfi :13 strings)
	    (srfi :115 regexp)
	    (srfi :117 list-queues)
	    (srfi :158 generators-and-accumulators)
	    (srfi :197 pipeline)
	    (text markdown extensions api)
	    (text markdown parser blocks)
	    (text markdown parser factories) ;; FIXME
	    (text markdown parser inlines)
	    (text markdown parser link-reference)
	    (text markdown parser nodes)
	    (text markdown parser parsing)
	    (text markdown parser scanner)
	    (text markdown parser source))

(define *footnotes-namespace* 
  "https://markdown.sagittarius-scheme.io/footnotes")

(define-markdown-node (footnote-block (attribute label "notes:label"))
  (namespace *footnotes-namespace*)
  (element "notes:footnote"))
(define-markdown-node (footnote (attribute label "notes:label"))
  (namespace *footnotes-namespace*)
  (element "notes:footnote-ref"))

(define-record-type footnote-reference-definition
  (parent <reference-definition>)
  (fields content)
  (protocol (lambda (n)
	      (lambda (label content)
		((n label) content)))))
  

(define-record-type footnote-block-parser
  (parent <definition-parser>)
  (fields content)
  (protocol
   (lambda (n)
     (lambda (document label)
       ((n (make-footnote-block-node document label) #t #f
	   (lambda ignore #t)
	   (lambda (self ps)
	     (define nnsi (parser-state-next-non-space-index ps))
	     (cond ((parser-state-blank? ps)
		    (if (markdown-node:first-child (block-parser-block self))
			(block-continue:at-index nnsi)
			;; blank line after empty list item
			(block-continue:none)))
		   ((>= (parser-state-indent ps) +parsing-code-block-indent+)
		    (let ((ci (+ (parser-state-index ps)
				 +parsing-code-block-indent+)))
		      (block-continue:at-index ci)))
		   (else (block-continue:none))))
	   (lambda (self line)
	     (let ((content (footnote-block-parser-content self)))
	       (list-queue-add-back! content (source-line-content line))))
	   block-parser-default-add-location!
	   block-parser-default-close-block!
	   block-parser-default-parse-inlines!)
	footnote-block-parser:definitions
	(list-queue))))))
(define (footnote-block-parser:definitions fbp)
  (define block (block-parser-block fbp))
  (list
   (make-footnote-reference-definition
    (string-append "^" (footnote-block-node-label block))
    (footnote-block-parser-content fbp))))

(define footnote-define-pattern
  (rx "[^" (* space) ($ (* any)) (* space) "]:"))
(define (try-start-footnote parser-state matched-block-parser)
  (if (>= (parser-state-indent parser-state) +parsing-code-block-indent+)
      (block-start:none)
      (let ((line (parser-state-line parser-state))
	    (nns (parser-state-next-non-space-index parser-state)))
	(cond ((source-line:regexp-search line footnote-define-pattern nns) =>
	       (lambda (m)
		 (let ((text (regexp-match-submatch m 1))
		       (end (regexp-match-submatch-end m 0)))
		   (chain (block-start:of (make-footnote-block-parser
					   (parser-state-document parser-state)
					   text))
			  (block-start:at-index _ end)))))
	      (else (block-start:none))))))

(define (footnote-reference-processor ref text? image?)
  (define label (reference-definition-label ref))
  (and (footnote-reference-definition? ref)
       (eqv? (string-ref label 0) #\^)
       (not text?) (not image?)
       (lambda (parent)
	  (make-footnote-node parent
			      (substring label 1 (string-length label))))))

(define-record-type inline-footnote-parser
  (parent <inline-content-parser>)
  (fields (mutable label))
  (protocol (lambda (n)
	      (lambda ()
		((n #\^ inline-footnote-parser:try-parse) 1)))))

(define (inline-footnote-parser:try-parse ifp inline-parser c)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (scanner:next! scanner) ;; skip ^
  (and (scanner:next-char? scanner #\[)
       (let ((s (scanner:position scanner)))
	 (and (scanner:find-char scanner #\])
	      (let* ((e (scanner:position scanner))
		     (source (scanner:source scanner s e))
		     (l (inline-footnote-parser-label ifp))
		     ;; current block
		     (cb (inline-parser-state-block state))
		     (fn (make-footnote-block-node cb (number->string l)))
		     (pp (make-paragraph-parser fn)))
		(scanner:next! scanner) ;; discards #\]
		(source-lines:for-each (lambda (sl)
					 (block-parser:add-line! pp sl)) source)
		(block-parser:close-block! pp)
		(block-parser:parse-inlines! pp inline-parser)
		(markdown-node:append-child! fn (block-parser-block pp))
		(inline-footnote-parser-label-set! ifp (+ l 1))
		(markdown-node:insert-after! cb fn)
		(list (make-footnote-node cb (number->string l))))))))

(define footnotes-extension
  (markdown-extension-builder
   (block-factories `(,try-start-footnote))
   (inline-content-factories `(,make-inline-footnote-parser))
   (reference-processors `(,footnote-reference-processor))))

(define (convert-html-footnote-block node data next)
  ;; TODO
  '())
(define-markdown-converter fn->html-converter html
  (footnote-block-node? convert-html-footnote-block))

(define (convert-sexp-footnote-block node data next)
  `(:note (:ref ,(footnote-block-node-label node))
	  ,@(map next (markdown-node:children node))))
(define (convert-sexp-footnote node data next)
  `(:note ,(footnote-node-label node)))
(define-markdown-converter fn->sexp-converter sexp
  (footnote-block-node? convert-sexp-footnote-block)
  (footnote-node? convert-sexp-footnote)
  )

(define footnotes-markdown-converter
  (markdown-converter:merge fn->html-converter fn->sexp-converter))


)
