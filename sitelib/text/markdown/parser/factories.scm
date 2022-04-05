;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/factories.scm - Parser factories
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
(library (text markdown parser factories)
    (export block-start?
	    block-start:of block-start:none
	    block-start:at-index
	    block-start:at-column
	    block-start:replace-active-block-parser
	    block-start-parsers
	    block-start-new-index
	    block-start-new-column
	    block-start-replace-active-block-parser?

	    try-start-heading
	    try-start-thematic-break
	    try-start-indented-code-block
	    try-start-fenced-code-block
	    try-start-html-block
	    try-start-block-quote-block
	    try-start-list-block)
    (import (rnrs)
	    (core misc)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :115 regexp)
	    (srfi :197 pipeline)
	    (text markdown parser blocks)
	    (text markdown parser nodes)
	    (text markdown parser parsing)
	    (text markdown parser source)
	    (text markdown parser scanner))
(define-vector-type block-start 
  (make-block-start parsers new-index new-column replace-active-block-parser?)
  block-start?
  (parsers    block-start-parsers)
  (new-index  block-start-new-index block-start-new-index-set!)
  (new-column block-start-new-column block-start-new-column-set!)
  (replace-active-block-parser? block-start-replace-active-block-parser?
				block-start-replace-active-block-parser?-set!))

(define (block-start:none) #f)
(define (block-start:of . parsers)
  (make-block-start parsers #f #f #f))
(define (block-start:at-index bs new-index)
  (block-start-new-index-set! bs new-index)
  bs)
(define (block-start:at-column bs new-column)
  (block-start-new-column-set! bs new-column)
  bs)
(define (block-start:replace-active-block-parser bs)
  (block-start-replace-active-block-parser?-set! bs #t)
  bs)

(define (try-start-heading parser-state matched-block-parser)
  (if (>= (parser-state-index parser-state) +parsing-code-block-indent+)
      (block-start:none)
      (let* ((line (parser-state-line parser-state))
	     (nns (parser-state-next-non-space-index parser-state)))
	(cond ((and (eqv? (source-line:char-at line nns) #\#)
		    (atx-heading parser-state (source-line:substring line nns)))
	       => (lambda (p)
		    (chain (block-start:of p)
			   (block-start:at-index _ (source-line:length line)))))
	      ((setex-heading-level parser-state line nns) =>
	       (lambda (level)
		 (let ((lines (matched-block-parser:paragraph-lines
			       matched-block-parser)))
		   (if (source-lines:empty? lines)
		       (block-start:none)
		       (chain (block-start:of
			       (make-heading-parser
				(parser-state-document parser-state)
				level lines))
			      (block-start:at-index _ (source-line:length line))
			      (block-start:replace-active-block-parser _))))))
	      (else (block-start:none))))))
(define (atx-heading parser-state line)
  (define scanner (scanner:of (source-lines:of line)))
  (let ((level (scanner:match-char scanner #\#)))
    (cond ((or (zero? level) (> level 6)) #f)
	  ((not (scanner:has-next? scanner))
	   (make-heading-parser (parser-state-document parser-state)
				level (source-lines:empty)))
	  ((not (memv (scanner:peek scanner) '(#\space #\tab))) #f)
	  (else
	   (scanner:whitespace scanner) ;; skip
	   (let ((start (scanner:position scanner)))
	     (let loop ((end start) (hash-can-end? #t))
	       (if (scanner:has-next? scanner)
		   (let ((c (scanner:peek scanner)))
		     (case c
		       ((#\#)
			(cond (hash-can-end?
			       (scanner:match-char scanner #\#)
			       (let ((ws (scanner:whitespace scanner)))
				 (loop (if (scanner:has-next? scanner)
					   (scanner:position scanner)
					   end)
				       (positive? ws))))
			      (else
			       (scanner:next! scanner)
			       (loop (scanner:position scanner)
				     hash-can-end?))))
		       ((#\space #\tab)
			(scanner:next! scanner)
			(loop end #t))
		       (else
			(scanner:next! scanner)
			(loop (scanner:position scanner) #f))))
		   (let* ((source (scanner:source scanner start end))
			  (content (source-lines:content source)))
		     (if (string-null? content)
			 (make-heading-parser
			  (parser-state-document parser-state)
			  level (source-lines:empty))
			 (make-heading-parser
			  (parser-state-document parser-state)
			  level source))))))))))
(define (setex-heading-level parser-state sl index)
  (define (setex-heading-rest? sl index marker)
    (let* ((content (source-line-content sl))
	   (last (string-length content))
	   (after-marker (or (string-skip content marker index) last))
	   (after-space (or (string-skip content parsing:space/tab?
					 after-marker)
			    last)))
      (>= after-space last)))
  (case (source-line:char-at sl index)
    ((#\=) (and (setex-heading-rest? sl (+ index 1) #\=) 1))
    ((#\-) (and (setex-heading-rest? sl (+ index 1) #\-) 2))
    (else #f)))

(define (try-start-thematic-break parser-state matched-block-parser)
  (define (thematic-break? line index)
    (define scanner (scanner:of (source-lines:of line)))
    (define pos (scanner:position scanner))
    (define (try-scan scanner pos char)
      (scanner:position! scanner pos)
      (let loop ((count 0))
	(let ((c (scanner:next! scanner)))
	  (cond ((eqv? c char) (loop (+ count 1)))
		((memv c '(#\space #\tab)) (loop count))
		(else count)))))
    
    (or (>= (try-scan scanner pos #\-) 3)
	(>= (try-scan scanner pos #\_) 3)
	(>= (try-scan scanner pos #\*) 3)))
  
  (if (>= (parser-state-indent parser-state) +parsing-code-block-indent+)
      (block-start:none)
      (let ((next-non-space (parser-state-next-non-space-index parser-state))
	    (line (parser-state-line parser-state)))
	(if (thematic-break? line next-non-space)
	    (chain (block-start:of (make-thematic-break-parser
				    (parser-state-document parser-state)))
		   (block-start:at-index _ (source-line:length line)))
	    (block-start:none)))))
			    

(define (try-start-indented-code-block parser-state matched-block-parser)
  (if (and (>= (parser-state-indent parser-state) +parsing-code-block-indent+)
	   (not (parser-state-blank? parser-state))
	   (not (paragraph-node?
		 (block-parser-block
		  (parser-state:active-block-parser parser-state)))))
      (chain (block-start:of (make-indented-code-block-parser
			      (parser-state-document parser-state)))
	     (block-start:at-column _ (+ (parser-state-column parser-state)
					 +parsing-code-block-indent+)))
      (block-start:none)))

(define (try-start-fenced-code-block parser-state matched-block-parser)
  (define (check-opener line index indent)
    (define scanner (scanner:of (source-lines:of line)))
    (define (try-scan scanner pos c)
      (scanner:position! scanner pos)
      (let ((n (scanner:match-char scanner c)))
	(and n (>= n 3) n)))
    (define (make fc fl indent)
      (cons (make-fenced-code-block-parser (parser-state-document parser-state)
					   fc fl indent)
	    fl))
    (do ((i 0 (+ i 1)))
	((= i index))
      (scanner:next! scanner))
    (let ((pos (scanner:position scanner)))
      (cond ((try-scan scanner pos #\`) => (lambda (n) (make #\` n indent)))
	    ((try-scan scanner pos #\~) => (lambda (n) (make #\~ n indent)))
	    (else #f))))
  (let ((indent (parser-state-indent parser-state))
	(nns (parser-state-next-non-space-index parser-state)))
    (cond ((>= indent +parsing-code-block-indent+) (block-start:none))
	  ((check-opener (parser-state-line parser-state) nns indent) =>
	   (lambda (bp&c)
	     (chain (block-start:of (car bp&c))
		    (block-start:at-index _ (+ nns (cdr bp&c))))))
	  (else (block-start:none)))))

(define html-block-patterns
  `#(
     (#f #f) ;; block type 0, not used
     (,(rx "<" (w/nocase (or "script" "pre" "style" "textarea"))
	   (or space ">" eol))
      ,(rx "</" (w/nocase (or "script" "pre" "style" "textarea")) ">"))
     (,*parsing:html-comment-open-pattern*
      ,*parsing:html-comment-close-pattern*) ;; comment
     (,*parsing:html-pi-open-pattern*
      ,*parsing:html-pi-close-pattern*)	   ;; PI
     (,*parsing:html-declaration-open-pattern*
      ,*parsing:html-declaration-close-pattern*) ;; <!ATTR ... > or so
     (,*parsing:html-cdata-open-pattern*
      ,*parsing:html-cdata-close-pattern*) ;; <![CDATA[ ... ]]>
     (,(rx "<" (? "/")
	   (w/nocase
	    (or
	     "address" "article" "aside"
	     "base" "basefont" "blockquote" "body"
	     "caption" "center" "col" "colgroup"
	     "dd" "details" "dialog" "dir" "div" "dl" "dt"
	     "fieldset" "figcaption" "figure" "footer" "from" "frame" "frameset"
	     "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hr" "html"
	     "iframe"
	     "legend" "li" "link"
	     "main" "menu" "menuitem"
	     "nav" "noframes"
	     "ol" "optgroup" "option"
	     "p" "param"
	     "section" "source" "summary"
	     "table" "tbody" "td" "tfoot" "thead" "title" "tr" "track"
	     "ul"))
	   (or space (: (? "/") ">") eol))
      #t)
     (,(rx bol
	   (or ,*parsing:html-open-tag-pattern*
	       ,*parsing:html-close-tag-pattern*)
	   (* space) eol)
      #t))
  )
(define (try-start-html-block parser-state matched-block-parser)
  (define (check-lazy)
    (define bp (matched-block-parser:get matched-block-parser))
    (or (paragraph-node? (block-parser-block bp))
	(block-parser-allow-lazy-continuation-line?
	 (parser-state:active-block-parser parser-state))))
  (define (check-match i line)
    (let ((o&c (vector-ref html-block-patterns i)))
      (and (regexp-search (car o&c) line)
	   (cadr o&c))))
  (let* ((nns (parser-state-next-non-space-index parser-state))
	 (indent (parser-state-indent parser-state))
	 (source (parser-state-line parser-state))
	 (line (source-line-content (source-line:substring source nns)))
	 (size (vector-length html-block-patterns)))

    (if (and (< indent +parsing-code-block-indent+)
	     (eqv? (source-line:char-at source nns) #\<))
	(let loop ((i 1))
	  (cond ((and (= i 7) (check-lazy)) (block-start:none))
		((and (< i size) (check-match i line)) =>
		 (lambda (closer?)
		   (let ((doc (parser-state-document parser-state))
			 (index (parser-state-index parser-state))
			 (closer (and (regexp? closer?) closer?)))
		     (chain (block-start:of (make-html-block-parser doc closer))
			    (block-start:at-index _ index)))))
		((< i size) (loop (+ i 1)))
		(else (block-start:none))))
	(block-start:none))))

(define (try-start-block-quote-block parser-state matched-block-parser)
  (let ((nns (parser-state-next-non-space-index parser-state)))
    (if (block-quote-parser:marker? parser-state nns)
	(let ((col (+ (parser-state-column parser-state)
		      (parser-state-indent parser-state)
		      1))
	      (line (parser-state-line parser-state)))
	  (chain (block-start:of (make-block-quote-parser
				  (parser-state-document parser-state)))
		 (block-start:at-column _ (if (parsing:space/tab?
					       (source-line:char-at line
								    (+ nns 1)))
					      (+ col 1)
					      col))))
	(block-start:none))))

(define (try-start-list-block ps matched-block-parser)
  (define matched (matched-block-parser:get matched-block-parser))
  (define (in-paragraph? matched-block-parser)
    (not (source-lines:empty?
	  (matched-block-parser:paragraph-lines matched-block-parser))))
  (define (list-match? a b)
    (or (and (bullet-list-node? a) (bullet-list-node? b)
	     (eqv? (bullet-list-node-bullet-marker a)
		   (bullet-list-node-bullet-marker b)))
	(and (ordered-list-node? a) (ordered-list-node? b)
	     (eqv? (ordered-list-node-delimiter a)
		   (ordered-list-node-delimiter b)))))
  (cond ((>= (parser-state-indent ps) +parsing-code-block-indent+)
	 (block-start:none))
	((parse-list (parser-state-document ps)
		     (parser-state-line ps)
		     (parser-state-next-non-space-index ps)
		     (+ (parser-state-column ps) (parser-state-indent ps))
		     (in-paragraph? matched-block-parser)) =>
	 (lambda (list-data)
	   (define col (car list-data))
	   (define lip (make-list-item-parser
			(parser-state-document ps)
			(- col (parser-state-column ps))))
	   (define block (cdr list-data))
	   (if (or (not (list-block-parser? matched))
		   (not (list-match? (block-parser-block matched) block)))
	       (let ((lbp (make-list-block-parser block)))
		 (list-node-tight-set! block "true")
		 (chain (block-start:of lbp lip)
			(block-start:at-column _ col)))
	       (chain (block-start:of lip)
		      (block-start:at-column _ col)))))
	(else (block-start:none))))

(define (parse-list doc line marker-index marker-column in-paragraph?)
  (define (space-tab-end? line index)
    (or (< index (source-line:length line))
	(parsing:space/tab? (source-line:char-at line index))))
  (define (parse-ordered-list doc line index)
    (define scanner (scanner:of (source-lines:of line)))
    (do ((i 0 (+ i 1))) ((= i index)) (scanner:next! scanner))
    (let ((digits (scanner:match-charset scanner char-set:digit)))
      (and (not (> digits 9))
	   (let ((c (scanner:peek scanner)))
	     (case c
	       ((#\. #\))
		(and (>= digits 1)
		     (space-tab-end? line (+ digits index 1))
		     (let ((n (source-line:substring line index
						     (+ digits index))))
		       (cons (make-ordered-list-node doc
			      (string->number (source-line-content n)) c)
			     (+ digits index 1)))))
	       (else #f))))))
  (define (parse-list-marker doc line index)
    (let ((c (source-line:char-at line index)))
      (case c
	((#\- #\+ #\*)
	 (and (space-tab-end? line (+ index 1))
	      (cons (make-bullet-list-node doc c) (+ index 1))))
	(else (parse-ordered-list doc line index)))))

  (cond ((parse-list-marker doc line marker-index) =>
	 (lambda (list-marker)
	   (define block (car list-marker))
	   (define index-after-marker (cdr list-marker))
	   (define marker-length (- index-after-marker marker-index))
	   (define column-after-marker (+ marker-column marker-length))
	   (define (check-content line content-column index-after-marker)
	     (define len (source-line:length line))
	     (let loop ((i index-after-marker) (cc content-column))
	       (if (= i len)
		   (values cc #f)
		   (case (source-line:char-at line i)
		     ((#\tab)
		      (loop (+ i 1) (+ cc (parsing:columns->next-tab-stop cc))))
		     ((#\space) (loop (+ i 1) (+ cc 1)))
		     (else (values cc #t))))))
	   (let-values (((content-column has-content?)
			 (check-content line
					column-after-marker
					index-after-marker)))
	     (if (and in-paragraph?
		      (or (and (ordered-list-node? block)
			       (not (= (ordered-list-node-start-number block)
				       1)))
			  (not has-content?)))
		 #f
		 (let ((cc (if (or (not has-content?)
				   (> (- content-column column-after-marker)
				      +parsing-code-block-indent+))
			       (+ column-after-marker 1)
			       content-column)))
		   (cons cc block))))))
	(else #f)))
		   
)
