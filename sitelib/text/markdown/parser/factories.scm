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
	    try-start-block-quote-block)
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
      (>= after-space after-marker)))
  (case (source-line:char-at sl index)
    ((#\=) (and (setex-heading-rest? sl (+ index 1) #\=) 1))
    ((#\-) (and (setex-heading-rest? sl (+ index 1) #\-) 2))
    (else #f)))

(define *dash&space-set* (char-set #\- #\space #\tab))
(define *underscore&space-set* (char-set #\_ #\space #\tab))
(define *asterisk&space-set* (char-set #\* #\space #\tab))
(define (try-start-thematic-break parser-state matched-block-parser)
  (define (thematic-break? line index)
    (define scanner (scanner:of (source-lines:of line)))
    (define pos (scanner:position scanner))
    (define (try-scan scanner pos set)
      (scanner:position! scanner pos)
      (scanner:match-charset scanner set))
    (or (>= (try-scan scanner pos *dash&space-set*) 3)
	(>= (try-scan scanner pos *underscore&space-set*) 3)
	(>= (try-scan scanner pos *asterisk&space-set*) 3)))
  
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
     (,(rx "<" (w/nocase (|\|| "script" "pre" "style" "textarea"))
	   (|\|| space ">" eol))
      ,(rx "</" (w/nocase (|\|| "script" "pre" "style" "textarea")) ">"))
     (,(rx bol "<!--")        ,(rx "-->")) ;; comment
     (,(rx bol "<?")          ,(rx "?>"))  ;; PI
     (,(rx bol "<!" (/ "AZ")) ,(rx ">"))	  ;; <!ATTR ... > or so
     (,(rx bol "<![CDATA[")   ,(rx "]]>")) ;; <![CDATA[ ... ]]>
     (,(rx "<" (? "/")
	   (w/nocase
	    (|\||
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
	   (|\|| space (: (? "/") ">") eol))
      #t)
     (,(rx bol (|\||
		(w/nocase "<" (/ "AZaz") (* (/ "AZaz09"))
			  ;; attribute
			  (: (+ space) (/ "AZaz_:") (* (/ "AZaz09:._-")) ;; name
			     (* space) "=" (* space)
			     (|\|| (+ (~ ("\"'=<>`") (/ #\x0 #\x20)))
				   (: #\' (~ #\') #\')
				   (: #\' (~ #\") #\')))
			  (* space) (? #\/) ">")
		(w/nocase "</" (/ "AZaz") (* (/ "AZaz09") (* space) ">")))
	   (* space) eol)
      #t)))
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
)
