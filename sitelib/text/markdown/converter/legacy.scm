;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/converter/legacy.scm - Markdown -> Legacy SEXP conveter
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

;; This library is only for backward compatibility
#!nounbound
(library (text markdown converter legacy)
    (export markdown->sexp-converter)
    (import (rnrs)
	    (sagittarius)
	    (srfi :1 lists)
	    (text markdown converter api)
	    (text markdown parser nodes))

(define (convert-document node data next)
  `(:doc ,@(map next (markdown-node:children node))))
(define (convert-paragraph node data next)
  `(:plain ,@(map next (markdown-node:children node))))
(define (convert-code-block node data next)
  `(:verbatim ,(code-block-node:literal node)))
(define (convert-thematic-break node data next) '(:line))

(define (convert-bullet-list node data next)
  `(:bullet-list ,@(map next (markdown-node:children node))))
(define (convert-ordered-list node data next)
  `(:ordered-list ,@(map next (markdown-node:children node))))
(define (convert-item node data next)
  `(:item ,@(map next (markdown-node:children node))))
(define (convert-block-quote node data next)
  `(:blockquote ,@(map next (markdown-node:children node))))

(define (convert-heading node data next)
  `(:header ,(string->keyword (string-append "h" (heading-node-level node)))
	    ,@(map next (markdown-node:children node))))

(define (convert-link node data next)
  `(:link (:label ,@(map next (markdown-node:children node)))
	  ,(link-node-destination node)
	  ,(cond ((link-node-title node))
		 (else ""))))
(define (convert-image node data next)
  `(:image (:link (:label ,@(map next (markdown-node:children node)))
		  ,(image-node-destination node)
		  ,(cond ((image-node-title node))
			 (else "")))))

(define (convert-text node data next) (text-node:content node))
(define (convert-code node data next) `(:code ,(code-node:literal node)))

(define (convert-html-block node data next)
  `(:html-block ,(html-block-node:literal node)))

(define (convert-html-inline node data next)
  ;; ???
  `(:html-block ,(html-inline-node:literal node)))

(define (convert-softbreak node data next) :eol)
(define (convert-linebreak node data next) :eol)

(define (convert-emphasis node data next)
  `(:emph ,@(map next (markdown-node:children node))))
(define (convert-strong-emphasis node data next)
  `(:strong ,@(map next (markdown-node:children node))))
  
(define-markdown-converter markdown->sexp-converter sexp
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
  (strong-emphasis-node? convert-strong-emphasis))


)
