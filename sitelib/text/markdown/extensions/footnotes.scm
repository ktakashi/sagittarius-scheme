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
    (export footnotes-extension)
    (import (rnrs)
	    (srfi :115 regexp)
	    (srfi :117 list-queues)
	    ;; (srfi :158 generators-and-accumulators)
	    (srfi :197 pipeline)
	    (text markdown extensions api)
	    (text markdown parser blocks)
	    (text markdown parser factories) ;; FIXME
	    (text markdown parser inlines)	    
	    (text markdown parser nodes)
	    (text markdown parser parsing)
	    (text markdown parser source))

(define *footnotes-namespace* "urn:markdown.sagittarius/footnotes")

(define-markdown-node (footnote-block (attribute label "notes:label"))
  (namespace *footnotes-namespace*)
  (element "notes:footnote-block"))
(define-markdown-node footnote (namespace *footnotes-namespace*)
  (element "notes:footnote"))

(define-record-type footnote-block-parser
  (parent <block-parser>)
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
	   (lambda (self loc)
	     (markdown-node:add-source-location! (block-parser-block self) loc))
	   (lambda (self)
	     ;; TODO
	     )
	   (lambda (self inline-parser)
	     ;; TODO
	     ))
	(list-queue))))))

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
(define footnotes-extension
  (markdown-extension-builder
   (custom-block-factories `(,try-start-footnote))))

)
