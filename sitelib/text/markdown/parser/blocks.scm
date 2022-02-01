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
	    parser-state-index parser-state-index-set!
	    parser-state-next-non-space-index
	    parser-state-next-non-space-index-set!
	    parser-state-column parser-state-column-set!
	    parser-state-indent parser-state-indent-set!
	    parser-state-blank? parser-state-blank?-set!

	    (rename (block-parser <block-parser>))
	    block-parser?
	    block-parser-block
	    block-parser-container?
	    block-parser-allow-lazy-continuation-line?
	    block-parser:can-contain?
	    block-parser:try-continue
	    block-parser:add-line
	    block-parser:add-source-span
	    block-parser:close-block
	    block-parser:parse-inlines

	    make-matched-block-parser matched-block-parser?
	    matched-block-parser:get
	    matched-block-parser:paragraph-lines

	    make-paragraph-parser paragraph-parser?
	    paragraph-parser-paragraph-lines
	    paragraph-parser-definitions

	    make-heading-parser heading-parser?
	    )
    (import (rnrs)
	    (core misc)
	    (text markdown parser nodes)
	    (text markdown parser source)
	    (text markdown parser link-reference))
;; parser state
;; mutaable object passed from document-parser
(define-vector-type parser-state 
  (make-parser-state document line index next-non-space-index column indent blank?)
  parser-state?
  (document parser-state-document) ;; Markdown document
  (line parser-state-line parser-state-line-set!)
  (index parser-state-index parser-state-index-set!)
  (next-non-space-index parser-state-next-non-space-index
			parser-state-next-non-space-index-set!)
  (column parser-state-column parser-state-column-set!)
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
	  add-source-span
	  close-block
	  parse-inlines))
(define (block-parser:can-contain? bp child)
  ((block-parser-can-contain? bp) bp child))
(define (block-parser:try-continue bp parser-state)
  ((block-parser-try-continue bp) bp parser-state))
(define (block-parser:add-line bp line)
  ((block-parser-add-line bp) bp line))
(define (block-parser:add-source-span bp source-span)
  ((block-parser-add-source-span bp) bp source-span))
(define (block-parser:close-block bp) ((block-parser:close-block bp) bp))
(define (block-parser:parse-inlines bp inline-parser)
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

(define (false _) #f) ;; for convenience


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
		 (block-continue:at-index (parser-state-indent ps))
		 (block-continue:none)))
	   (lambda (self line)
	     (let ((p (paragraph-parser-link-reference-definition-parser self)))
	       (link-reference-definition-parser:parse! p line)))
	   (lambda (self sp)
	     (let ((p (paragraph-parser-link-reference-definition-parser self)))
	       (link-reference-definition-parser:add-source-span! p sp)))
	   (lambda (self)
	     ;; unlink or adding source span from
	     ;; link-reference-definition-parser
	     )
	   (lambda (bp inline-parser)
	     ;; TODO parse lines
	     ))
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
	   (lambda (self span) )
	   (lambda (self) )
	   (lambda (self inline-parser)
	     ;; TODO
	     #;(inline-parser:parse-inlines inline-parser
	     (heading-parser-content self) (block-parser-block self))))
	content)))))

)
