;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/blocks.scm - Block parsers
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
(library (text markdown parser blocks)
    (export make-parser-state parser-state?
	    parser-state-document
	    parser-state-line parser-state-line-set!
	    parser-state-line-index parser-state-line-index-set!
	    parser-state-index parser-state-index-set!
	    parser-state-next-non-space-index
	    parser-state-next-non-space-index-set!
	    parser-state-column parser-state-column-set!
	    parser-state-indent parser-state-indent-set!
	    parser-state-blank? parser-state-blank?-set!

	    block-continue?
	    block-continue-index
	    block-continue-column
	    block-continue-finalize?
	    
	    (rename (block-parser <block-parser>))
	    block-parser?
	    block-parser-block
	    block-parser-container?
	    block-parser-allow-lazy-continuation-line?
	    block-parser:can-contain?
	    block-parser:try-continue
	    block-parser:add-line!
	    block-parser:add-source-location!
	    block-parser:close-block!
	    block-parser:parse-inlines!

	    make-matched-block-parser matched-block-parser?
	    matched-block-parser:get
	    matched-block-parser:paragraph-lines

	    make-document-block-parser document-block-parser?
	    
	    make-paragraph-parser paragraph-parser?
	    paragraph-parser-paragraph-lines
	    paragraph-parser-definitions

	    make-heading-parser heading-parser?
	    )
    (import (rnrs)
	    (core misc)
	    (text markdown parser inlines)
	    (text markdown parser nodes)
	    (text markdown parser source)
	    (text markdown parser link-reference))
;; parser state
;; mutaable object passed from document-parser
(define-vector-type parser-state 
  (make-parser-state document line line-index
		     index column
		     next-non-space-index
		     indent blank?)
  parser-state?
  (document parser-state-document) ;; Markdown document
  (line parser-state-line parser-state-line-set!)
  (line-index parser-state-line-index parser-state-line-index-set!)
  (index parser-state-index parser-state-index-set!)
  (column parser-state-column parser-state-column-set!)
  (next-non-space-index parser-state-next-non-space-index
			parser-state-next-non-space-index-set!)
  (indent parser-state-indent parser-state-indent-set!)
  (blank? parser-state-blank? parser-state-blank?-set!))

(define-vector-type block-continue (make-block-continue index column finished?)
  block-continue?
  (index block-continue-index)
  (column block-continue-column)
  (finished? block-continue-finalize?))
(define (block-continue:none)             #f)
(define (block-continue:at-index index)   (make-block-continue index -1 #f))
(define (block-continue:at-column column) (make-block-continue -1 column #f))
(define (block-continue:finished)         (make-block-continue -1 -1 #t))

;; parsers are stateful, so each time we create one
(define-record-type block-parser
  (fields block
	  container?
	  allow-lazy-continuation-line?
	  can-contain?
	  try-continue
	  add-line
	  add-source-location
	  close-block
	  parse-inlines))
(define (block-parser:can-contain? bp child)
  ((block-parser-can-contain? bp) bp child))
(define (block-parser:try-continue bp parser-state)
  ((block-parser-try-continue bp) bp parser-state))
(define (block-parser:add-line! bp line)
  ((block-parser-add-line bp) bp line))
(define (block-parser:add-source-location! bp source-location)
  ((block-parser-add-source-location bp) bp source-location))
(define (block-parser:close-block! bp) ((block-parser-close-block bp) bp))
(define (block-parser:parse-inlines! bp inline-parser)
  ((block-parser-parse-inlines bp) bp inline-parser))

(define-record-type matched-block-parser
  (fields block-parser))
(define (matched-block-parser:get mbs)
  (matched-block-parser-block-parser mbs))
(define (matched-block-parser:paragraph-lines mbs)
  (let ((bs (matched-block-parser:get mbs)))
    (if (paragraph-parser? bs)
	(paragraph-parser-paragraph-lines bs)
	(source-lines:empty))))

(define (false . ignore) #f) ;; for convenience
(define (true . ignore) #t) ;; for convenience
(define (default-add-line! self line) )
(define (default-add-location! self loc)
  (markdown-node:add-source-location! (block-parser-block self) loc))
(define (default-close-block! self) )
(define (default-parse-inlines! self inline-parser) )

;;; document block parser
(define-record-type document-block-parser
  (parent block-parser)
  (protocol
   (lambda (n)
     (lambda ()
       ((n (make-markdown-document) #t #f true
	   (lambda (self ps) (block-continue:at-index (parser-state-index ps)))
	   default-add-line!
	   default-add-location!
	   default-close-block!
	   default-parse-inlines!))))))

;;; paragraph parser
(define-record-type paragraph-parser
  (parent block-parser)
  (fields link-reference-definition-parser)
  (protocol
   (lambda (n)
     (lambda (document)
       ((n (make-paragraph-node document) #f #t false
	   (lambda (self ps)
	     (if (parser-state-blank? ps)
		 (block-continue:none)
		 (block-continue:at-index (parser-state-index ps))))
	   (lambda (self line)
	     (let ((p (paragraph-parser-link-reference-definition-parser self)))
	       (link-reference-definition-parser:parse! p line)))
	   (lambda (self loc)
	     (let ((p (paragraph-parser-link-reference-definition-parser self)))
	       (link-reference-definition-parser:add-source-location! p loc)))
	   (lambda (self)
	     (let* ((p (paragraph-parser-link-reference-definition-parser self))
		    (l* (link-reference-definition-parser:paragraph-lines p))
		    (b (block-parser-block self)))
	       (if (source-lines:empty? l*)
		   (markdown-node:unlink! b)
		   (markdown-node:source-locations-set! b
		    (link-reference-definition-parser-source-locations p)))))
	   (lambda (bp inline-parser)
	     (let* ((p (paragraph-parser-link-reference-definition-parser bp))
		    (l* (link-reference-definition-parser:paragraph-lines p)))
	       (unless (source-lines:empty? l*)
		 (inline-parser:parse! inline-parser l*
				       (block-parser-block bp))))))
	(make-link-reference-definition-parser))))))
(define (paragraph-parser-paragraph-lines pl)
  (define lrdp (paragraph-parser-link-reference-definition-parser pl))
  (link-reference-definition-parser:paragraph-lines lrdp))
(define (paragraph-parser-definitions pl)
  (define lrdp (paragraph-parser-link-reference-definition-parser pl))
  (link-reference-definition-parser:definitions lrdp))

;;; heading parser
(define-record-type heading-parser
  (parent block-parser)
  (fields content)
  (protocol
   (lambda (n)
     (lambda (document level content)
       ((n (make-heading-node document (number->string level)) #f #f false
	   (lambda (self line) (block-continue:none))
	   default-add-line!
	   default-add-location!
	   default-close-block!
	   (lambda (self inline-parser)
	     (inline-parser:parse! inline-parser
	      (heading-parser-content self) (block-parser-block self))))
	content)))))

)
