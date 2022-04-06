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
	    parser-state:active-block-parser

	    block-continue?
	    block-continue-index
	    block-continue-column
	    block-continue-finalize?
	    block-continue:none
	    block-continue:at-index
	    block-continue:at-column
	    block-continue:finished
	    
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

	    (rename (default-add-location! block-parser-default-add-location!)
		    (default-parse-inlines! block-parser-default-parse-inlines!)
		    (default-add-line! block-parser-default-add-line!)
		    (default-close-block! block-parser-default-close-block!))

	    (rename (definition-parser <definition-parser>))
	    definition-parser?
	    definition-parser:definitions

	    make-matched-block-parser matched-block-parser?
	    matched-block-parser:get
	    matched-block-parser:paragraph-lines

	    make-document-block-parser document-block-parser?
	    
	    make-paragraph-parser paragraph-parser?
	    paragraph-parser-paragraph-lines
	    ;; paragraph-parser-definitions
	    
	    make-heading-parser heading-parser?
	    make-thematic-break-parser thematic-break-parser?
	    make-indented-code-block-parser indented-code-block-parser?
	    make-fenced-code-block-parser fenced-code-block-parser?
	    make-html-block-parser html-block-parser?
	    make-block-quote-parser block-quote-parser?
	    block-quote-parser:marker?

	    make-list-block-parser list-block-parser?
	    make-list-item-parser list-item-parser?
	    )
    (import (rnrs)
	    (core misc)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :115 regexp)
	    (srfi :117 list-queues)
	    (text markdown parser escaping)
	    (text markdown parser inlines)
	    (text markdown parser nodes)
	    (text markdown parser parsing)
	    (text markdown parser source)
	    (text markdown parser link-reference))
;; parser state
;; mutaable object passed from document-parser
(define-vector-type parser-state 
  (make-parser-state document line line-index
		     index column
		     next-non-space-index
		     indent blank?
		     active-block-parser)
  parser-state?
  (document parser-state-document) ;; Markdown document
  (line parser-state-line parser-state-line-set!)
  (line-index parser-state-line-index parser-state-line-index-set!)
  (index parser-state-index parser-state-index-set!)
  (column parser-state-column parser-state-column-set!)
  (next-non-space-index parser-state-next-non-space-index
			parser-state-next-non-space-index-set!)
  (indent parser-state-indent parser-state-indent-set!)
  (blank? parser-state-blank? parser-state-blank?-set!)
  (active-block-parser parser-state-active-block-parser))
(define (parser-state:active-block-parser ps)
  ((parser-state-active-block-parser ps)))

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
(define-record-type definition-parser
  (parent block-parser)
  (fields get-definitions))
(define (definition-parser:definitions p)
  ((definition-parser-get-definitions p) p))

(define-record-type paragraph-parser
  (parent definition-parser)
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
	paragraph-parser-definitions
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
	   (lambda (self ps) (block-continue:none))
	   default-add-line!
	   default-add-location!
	   default-close-block!
	   (lambda (self inline-parser)
	     (inline-parser:parse! inline-parser
	      (heading-parser-content self) (block-parser-block self))))
	content)))))

;;; thematic break parser
(define-record-type thematic-break-parser
  (parent block-parser)
  (protocol
   (lambda (n)
     (lambda (document)
       ((n (make-thematic-break-node document) #f #f false
	(lambda (self ps) (block-continue:none))
	default-add-line!
	default-add-location!
	default-close-block!
	default-parse-inlines!))))))

;;; indented code block parser
(define-record-type indented-code-block-parser
  (parent block-parser)
  (fields lines)
  (protocol
   (lambda (n)
     (lambda (document)
       ((n (make-code-block-node document #f) #f #f false
	(lambda (self ps)
	  (cond ((>= (parser-state-indent ps) +parsing-code-block-indent+)
		 (block-continue:at-column (+ (parser-state-column ps)
					      +parsing-code-block-indent+)))
		((parser-state-blank? ps)
		 (block-continue:at-index
		  (parser-state-next-non-space-index ps)))
		(else (block-continue:none))))
	(lambda (self line)
	  (let ((lines (indented-code-block-parser-lines self)))
	    (list-queue-add-back! lines (source-line-content line))))
	default-add-location!
	(lambda (self)
	  (define (strip-last-blanks lines)
	    (let loop ((lines (reverse! (list-queue-list lines))))
	      (cond ((null? lines) '())
		    ((string-index (car lines)
				   (lambda (c) (not (char-whitespace? c))))
		     (reverse! lines))
		    (else (loop (cdr lines))))))
	      
	  (let ((lines (strip-last-blanks
			 (indented-code-block-parser-lines self))))
	    (code-block-node:literal-set! (block-parser-block self)
					  (string-append
					   (string-join lines "\n")
					   "\n"))))
	default-parse-inlines!)
	(list-queue))))))

;;; fenced code block parser
(define-record-type fenced-code-block-parser
  (parent block-parser)
  (fields (mutable first-line)
	  other-lines)
  (protocol
   (lambda (n)
     (lambda (document fence-char fence-length fence-indent)
       (define (closing? block line index)
	 (define fc (fenced-code-block-node-fence-char block))
	 (define fl (fenced-code-block-node-fence-length block))
	 (define content (source-line-content line))
	 (define e (string-length content))
	 (let ((fences (- (or (string-skip content fc index) e) index)))
	   (and (>= fences fl)
		(let ((after (or (string-skip content parsing:space/tab?
					      (+ index fences))
				 e)))
		  (= after e)))))
       (define (adjust-index block line new-index)
	 (define fi (fenced-code-block-node-fence-indent block))
	 (do ((i fi (- i 1)) (ni new-index (+ ni 1))
	      (len (source-line:length line)))
	     ((or (<= i 0) (<= len ni)
		  (not (eqv? (source-line:char-at line ni) #\space)))
	      ni)))
	     
       ((n (make-fenced-code-block-node document #f
					fence-char fence-length fence-indent)
	   #f #f false
	   (lambda (self ps)
	     (let ((nns (parser-state-next-non-space-index ps))
		   (ni (parser-state-index ps))
		   (line (parser-state-line ps))
		   (block (block-parser-block self)))
	       (if (and (< (parser-state-indent ps) +parsing-code-block-indent+)
			(< nns (source-line:length line))
			(eqv? (source-line:char-at line nns)
			      (fenced-code-block-node-fence-char block))
			(closing? block line nns))
		   (block-continue:finished)
		   (block-continue:at-index (adjust-index block line ni)))))
	   (lambda (self line)
	     (let ((c (source-line-content line)))
	       (if (fenced-code-block-parser-first-line self)
		   (list-queue-add-back!
		    (fenced-code-block-parser-other-lines self) c)
		   (fenced-code-block-parser-first-line-set! self c))))
	   default-add-location!
	   (lambda (self)
	     (define (get-info first)
	       (let ((escaped (escaping:unescape 
			       (or (and (not (string-null? first)) first) ""))))
		 (and (> (string-length escaped) 0)
		      escaped)))
	     (define (get-literal others)
	       (define s* (list-queue-list others))
	       (if (null? s*)
		   ""
		   (string-append (string-join s* "\n") "\n")))
	     (let ((block (block-parser-block self))
		   (first
		    (string-trim-both
		     (fenced-code-block-parser-first-line self)))
		   (others (fenced-code-block-parser-other-lines self)))
	       (code-block-node-info-set! block (get-info first))
	       (code-block-node:literal-set! block (get-literal others))))
	   default-parse-inlines!)
	#f (list-queue))))))

;;; HTML block parser
(define-record-type html-block-parser
  (parent block-parser)
  (fields closing-pattern
	  (mutable finished?)
	  content)
  (protocol
   (lambda (n)
     (lambda (document closing-pattern)
       ((n (make-html-block-node document) #f #f false
	   (lambda (self ps)
	     (if (or (html-block-parser-finished? self)
		     (and (parser-state-blank? ps)
			  (not (html-block-parser-closing-pattern self))))
		 (block-continue:none)
		 (block-continue:at-index (parser-state-index ps))))
	   (lambda (self line)
	     (define str (source-line-content line))
	     (list-queue-add-back! (html-block-parser-content self) str)
	     (let ((re (html-block-parser-closing-pattern self)))
	       (when (and (regexp? re) (regexp-search re str))
		 (html-block-parser-finished?-set! self #t))))
	   default-add-location!
	   (lambda (self)
	     (let ((block (block-parser-block self))
		   (content (html-block-parser-content self)))
	       (html-block-node:literal-set! block
		(string-join (list-queue-list content) "\n"))))
	   default-parse-inlines!)
	closing-pattern #f (list-queue))))))
		    

;;; Container blocks
;;; Block quote parser
(define (block-quote-parser:marker? ps index)
  (let ((line (parser-state-line ps))
	(index (parser-state-index ps)))
    (and (< (parser-state-indent ps) +parsing-code-block-indent+)
	 (< index (source-line:length line))
	 (eqv? (source-line:char-at line index) #\>))))
(define-record-type block-quote-parser
  (parent block-parser)
  (protocol
   (lambda (n)
     (lambda (document)
       ((n (make-block-quote-node document) #t #f true
	   (lambda (self ps)
	     (let ((nns (parser-state-next-non-space-index ps)))
	       (if (block-quote-parser:marker? ps nns)
		   (let ((col (+ (parser-state-column ps)
				 (parser-state-indent ps)
				 1))
			 (line (parser-state-line ps)))
		     (block-continue:at-column
		      (if (parsing:space/tab?
			   (source-line:char-at line (+ nns 1)))
			  (+ col 1)
			  col)))
		   (block-continue:none))))
	   default-add-line!
	   default-add-location!
	   default-close-block!
	   default-parse-inlines!))))))

;;; List block parser
(define-record-type list-block-parser
  (parent block-parser)
  (fields (mutable had-blank-line?)
	  (mutable lines-after-blank))
  (protocol
   (lambda (n)
     (lambda (list-block)
       ((n list-block #t #f
	   (lambda (self block)
	     (define (update! self block)
	       (when (and (list-block-parser-had-blank-line? self)
			  (= (list-block-parser-lines-after-blank self) 1))
		 (list-node-tight-set! block "false")
		 (list-block-parser-had-blank-line?-set! self #f))
	       #t)
	     (and (item-node? block)
		  (update! self (block-parser-block self))))
	   (lambda (self ps)
	     (cond ((parser-state-blank? ps)
		    (list-block-parser-had-blank-line?-set! self #t)
		    (list-block-parser-lines-after-blank-set! self 0))
		   ((list-block-parser-had-blank-line? self)
		    (let ((n (list-block-parser-lines-after-blank self)))
		      (list-block-parser-lines-after-blank-set! self (+ n 1)))))
	     (block-continue:at-index (parser-state-index ps)))
	   default-add-line!
	   default-add-location!
	   default-close-block!
	   default-parse-inlines!)
	#f -1)))))

(define-record-type list-item-parser
  (parent block-parser)
  (fields content-indent
	  (mutable had-blank-line?))
  (protocol
   (lambda (n)
     (lambda (document content-indent)
       ((n (make-item-node document) #t #f
	   (lambda (self block)
	     (when (list-item-parser-had-blank-line? self)
	       (let ((parent (markdown-node-parent (block-parser-block self))))
		 (when (list-node? parent)
		   (list-node-tight-set! parent "false"))))
	     #t)
	   (lambda (self ps)
	     (cond ((parser-state-blank? ps)
		    (let ((block (block-parser-block self)))
		      (if (null? (markdown-node:children block))
			  (block-continue:none)
			  (let ((active-block
				 (block-parser-block
				  (parser-state:active-block-parser ps))))
			    (list-item-parser-had-blank-line?-set! self
			     (or (paragraph-node? active-block)
				 (item-node? active-block)))
			    (block-continue:at-index
			     (parser-state-next-non-space-index ps))))))
		   ((>= (parser-state-indent ps)
			(list-item-parser-content-indent self))
		    (block-continue:at-column
		     (+ (parser-state-column ps)
			(list-item-parser-content-indent self))))
		   (else (block-continue:none))))
	   default-add-line!
	   default-add-location!
	   default-close-block!
	   default-parse-inlines!)
	content-indent #f)))))
				 
)
