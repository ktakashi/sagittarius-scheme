;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser.scm - Parser for Markdown text
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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


;; For now only needed part is implemented.
#!nounbound
(library (text markdown parser)
    (export markdown-parser?
	    markdown-node?
	    

	    commonmark-parser ;; For testing
	    
	    parse-markdown

	    (rename (%markdown-parser-builder markdown-parser-builder))
	    markdown-parser-builder?
	    markdown-parser-builder:build

	    make-post-processor
	    make-post-processor-spec
	    post-processor-spec?
	    
	    ;; Below procedures are only for backward compatibility
	    markdown-parser-error?
	    markdown-parser-position
	    markdown-parser-expected

	    markdown-document->xml-document
	    )
    (import (rnrs)
	    (record builder)
	    (text markdown parser nodes)
	    (text markdown parser factories)
	    (text markdown parser inlines)
	    (text markdown parser document)
	    (text markdown parser post-processor)
	    ;; a bit ugly
	    (text markdown extensions))

;; For backward compatibility, this will never be raised
(define-condition-type &markdown-parser-error &error
  make-parser-error markdown-parser-error?
  (position markdown-parser-position)
  (expected markdown-parser-expected))

(define-record-type markdown-parser
  (fields document-parser-producer
	  post-processors))

(define-record-type markdown-parser-builder
  (fields block-parsers
	  inline-parser-producer
	  post-processors
	  extensions))
(define default-block-parsers
  `(
    ,try-start-block-quote-block
    ,try-start-heading
    ,try-start-fenced-code-block
    ,try-start-html-block
    ,try-start-thematic-break
    ,try-start-list-block
    ,try-start-indented-code-block
    ))

(define-syntax %markdown-parser-builder
  (make-record-builder markdown-parser-builder
   ((block-parsers default-block-parsers)
    (inline-parser-producer make-inline-parser)
    (post-processors '())
    (extensions '()))))
(define (markdown-parser-builder:build builder)
  (define (run-factories f*) (map (lambda (f) (f)) f*))
  (define extensions
    (apply combine-markdown-extensions
	   (markdown-parser-builder-extensions builder)))
  (make-markdown-parser
   (lambda ()
     (make-document-parser
      `(,@(run-factories (markdown-extension-block-factories extensions))
	,@(markdown-parser-builder-block-parsers builder))
      (markdown-parser-builder-inline-parser-producer builder)
      (run-factories (markdown-extension-inline-content-factories extensions))
      (run-factories (markdown-extension-delimiter-processors extensions))
      (markdown-extension-reference-processors extensions)))
   `(,@(markdown-extension-post-processors extensions)
     ,@(markdown-parser-builder-post-processors builder))))

(define commonmark-parser
  (markdown-parser-builder:build (%markdown-parser-builder)))

(define (parse-markdown parser input-port)
  (define document-parser ((markdown-parser-document-parser-producer parser)))
  (post-process parser (document-parser:parse document-parser input-port)))

(define (post-process parser node)
  (fold-left (lambda (node processor) (processor node))
	     node (markdown-parser-post-processors parser)))
)
