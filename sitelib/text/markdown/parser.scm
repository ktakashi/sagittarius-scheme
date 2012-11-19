;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser.scm - Parser for Markdown text
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

(library (text markdown parser)
    (export markdown-parser
	    parse-markdown
	    markdown-parser-error?
	    markdown-parser-position
	    markdown-parser-expected)
    (import (rnrs)
	    (packrat)
	    (sagittarius)
	    (srfi :14 char-sets)
	    (srfi :26 cut)
	    (srfi :39 parameters))

  ;; lexical generator
  (define (generator p)
    (let ((ateof #f)
	  (pos (top-parse-position (cond ((car (port-info p)))
					 (else "<?>")))))
      (lambda ()	
	(if ateof
	    (values pos #f)
	    (let ((x (read-char p)))
	      (if (eof-object? x)
		  (begin
		    (set! ateof #t)
		    (values pos #f))
		  (let ((old-pos pos))
		    (set! pos (update-parse-position pos x))
		    (values old-pos (cons x x)))))))))

  ;; helper for tokens
  (define (token str)
    (lambda (starting-results)
      (let loop ((pos 0) (results starting-results))
	(if (= pos (string-length str))
	    (make-result str results)
	    (let ((ch (parse-results-token-value results)))
	      (if (and ch (char=? ch (string-ref str pos)))
		  (loop (+ pos 1) (parse-results-next results))
		  (make-expected-result
		   (parse-results-position starting-results) str)))))))

  (define (min-max char/pred/charset :optional (min 0) (max #f))
    (let ((pred (cond ((char? char/pred/charset)
		       (lambda (c) (char=? char/pred/charset c)))
		      ((procedure? char/pred/charset) char/pred/charset)
		      ((char-set? char/pred/charset)
		       (lambda (c) (char-set-contains? char/pred/charset c)))
		      (else
		       (assertion-violation 
			'min-max
			(format "character or procedure required but got ~s"
				char/pred) char/pred))))
	  (required (format "~a{~a,~a}" char/pred/charset min (if max max ""))))
      (lambda (starting-results)
	(let loop ((count 0) (results starting-results) (acc '()))
	  (let ((c (parse-results-token-value results)))
	    (if (and c (pred c))
		(loop (+ count 1) (parse-results-next results) (cons c acc))
		(cond ((or (and max (<= min count max)) ;; when max is given
			   (and (not max) (<= min count)))
		       (make-result (list->string (reverse! acc)) results))
		      (else
		       (make-expected-result
			(parse-results-position starting-results)
			required)))))))))

  (define (one-of-token tokens)
    (lambda (starting-results)
      (let outer ((tokens tokens))
	(if (null? tokens)
	    (make-expected-result
	     (parse-results-position starting-results) tokens)
	    (let ((str (symbol->string (car tokens))))
	      (let loop ((pos 0) (results starting-results))
		(if (= pos (string-length str))
		    (make-result str results)
		    (let ((ch (parse-results-token-value results)))
		      (if (and ch (char=? ch (string-ref str pos)))
			  (loop (+ pos 1) (parse-results-next results))
			  (outer (cdr tokens)))))))))))

  (define (combine-pred . preds)
    (lambda (ch)
      (exists (cut <> ch) preds)))

  ;; should not contain linefeed
  (define *any-sets* (char-set-delete char-set:full #\linefeed #\return))
  ;; id should not contain #\[ and #\]
  (define *id-sets* (char-set-delete *any-sets* #\" #\[ #\]))
  ;; RFC 2396 (#\( and #\) are not in it)
  (define *url-sets* (char-set-union
		      char-set:letter+digit
		      (string->char-set "-_.!~*'%;/?:@&=+$,")))
  (define *code-span-sets* 
    (char-set-delete char-set:full #\` #\linefeed #\return))
  (define *plain-sets*
    (char-set-delete char-set:full #\linefeed #\return #\`
		     #\_ #\- #\* #\[ #\] #\: #\< #\= #\" #\! #\#))
  (define *no-linefeed-ws-sets*
    (char-set-delete char-set:whitespace #\return #\linefeed))

  (define *html-char-sets*
    (char-set-delete char-set:full #\<))

  (define *header-char-sets*
    (char-set-delete char-set:full #\linefeed #\return #\#))

  (define *attribute-value-sets*
    (char-set-delete char-set:full #\"))
  
  ;; parameters
  (define inline-tags (make-parameter '()))
  (define block-tags (make-parameter '()))

  (define *known-inline-tags* 
    ;; TODO add
    ;; we need not a literal list
    '(a img span code b strong strike i abbr address br hr sub sup))

  (define *known-block-tags* 
    ;; TODO add
    '(div p table blockquote pre dl))

#|
BNF of Markdown,

Doc ::= Element*
Element ::= Paragraph | Header | List | Blockquote | Line
         |  CodeBlock | InlineHtml | BlockHtml | Reference
Paragraph ::= Inline* Linefeed{2,}
Inline ::= (Text | Emphasis | Link | Image | CoseSpan | InlineHtml)
Header ::= Setext-Style | Atx-Style
Setext-Style ::= Inline+ Linefeed ('=' | '-')+
Atx-Style ::= '#'{1,6} Inline+ '#'*
List ::= (Number '.' SP+ Inline+) (Linefeed List)*
      |  ('*' SP+ Inline+) (Linefeed List)*
Blockquote ::= '>' SP* Paragraph
            |  '>' SP* Linefeed (Blockquote)*
Line ::= ( (SP* '-') | (SP* '*') ){3,} Linefeed
CodeBlock ::= SP{4,} Inline+ Linefeed
BlockHtml ::= '<' (div|p|table|pre) (SP+ HtmlAttribute)* '>'
              (Text | BlockHtml | InlineHtml)+ 
              '<' '/' (div|p|table|pre) '>'
InlineHtml ::= '<' ({not block html}) (SP+ HtmlAttribute)* '>'
               (Text | InlineHtml)+
               '<' '/' ({not block html}) '>'
Text ::= ANY
Emphasis ::= '*' Text '*' | '**' Text '**'
          |  '_' Text '_' | '__' Text '__'
Link ::= '[' ID ']' '(' URL ('"' Text '"')? ')'
ID ::= Text
Image ::= '!' '[' ID ']' '(' URL ('"' Text '"')? ')'
CodeSpan ::= '`' ANY '`'
Reference ::= SP{0,3} '[' ID ']' ':' URL ('"' Text '"')?
|#
  ;; (packrat) does not support *, +, and ?. so we need to write the definition
  ;; kinda awkward way.
  ;; for now does not accept tab
  (define markdown-parser
    (packrat-parser
     (begin
       (define (token-with-charset sets results)
	 (let loop ((acc '()) (results results))
	   (let ((ch (parse-results-token-value results)))
	     (cond ((and ch (char-set-contains? sets ch))
		    (loop (cons ch acc) (parse-results-next results)))
		   ((zero? (length acc))
		    (make-expected-result
		     (parse-results-position results) "<?>"))
		   (else
		    (make-result (list->string (reverse! acc)) results))))))
       ;; for block html
       (define (html-chars results)
	 (token-with-charset *html-char-sets* results))
       (define (any-chars results)
	 (token-with-charset *any-sets* results))
       (define (id results)
	 (token-with-charset *id-sets* results))
       (define (url results)
	 (token-with-charset *url-sets* results))
       (define (code-span-chars results)
	 (token-with-charset *code-span-sets* results))
       (define (eof? results)
	 (let ((ch (parse-results-token-value results)))
	   (if (eof-object? ch)
	       (make-result ch results)
	       (make-expected-result
		(parse-results-position results) "#<eof>"))))
       (define (number results)
	 (token-with-charset char-set:digit results))
       (define (plain-chars results)
	 (token-with-charset *plain-sets* results))
       ;; TODO I think except #\" but I'm not sure so for now the same as URL.
       (define (title results)
	 (token-with-charset *attribute-value-sets* results))
       (define (h-chars results)
	 (token-with-charset *header-char-sets* results))
       (define (attr-chars results)
	 (token-with-charset *attribute-value-sets* results))
       ;; for line
       (define (line-def char)
	 (lambda (starting-results)
	   (let loop ((results starting-results) (appear 0))
	     (let ((ch (parse-results-token-value results)))
	       (cond ((and ch
			   (or (char=? ch char)
			       (char-set-contains? *no-linefeed-ws-sets* ch)))
		      (loop (parse-results-next results)
			    (if (char=? ch char) (+ appear 1) appear)))
		     ((and (positive? appear) ch (char=? ch #\linefeed))
		      (make-result "---" results))
		     (else
		      (make-expected-result
		       (parse-results-position starting-results) "---")))))))
       doc)
     (doc ((es <- elements) (cons :doc es))
	  (() (list :doc))) ;; empty document
     (elements ((e <- entry es <- elements) (cons e es))
	       (() '()))
     (entry ((h <- header)      h)
	    ((b <- block-quote) b)
	    ((l <- line)        l)
	    ((s <- separator)   s)
	    ((i <- inline-html) i)
	    ((r <- reference)   r)
	    ;; blocks must be lower order
	    ((c <- code-block)  c)
	    ((b <- block-html)  b)
	    ((l <- mlist)       l)
	    ((p <- paragraph)   p))
     ;; TODO Should we store this to somewhere or into the AST?
     ;; first the most toplevel elements
     (paragraph ((i <- inlines separator)
		 (cons* :paragraph i)))
     (p-entry ((i <- inlines linefeed) i))
     (separator (((min-max (lambda (c)
			     (char-set-contains? *no-linefeed-ws-sets* c)) 0)
		  linefeed) :separator)
		((eof?) :separator))
     ;; inline elements
     (inlines ((i <- inline is <- inlines) (cons i is))
	      (() '()))
     (inline ((e <- emphasis) e)
	     ((i <- image) i) ;; image must be higher
	     ((l <- link) l)
	     ((c <- code-span) c)
	     ((i <- inline-html) i)
	     ;; ah, i want a smart solution
	     ((p <- plain) p)
	     ((t <- text) t))
     ;; headers
     (header ((h <- atx-style linefeed)    (cons :header h))
	     ((h <- setext-style linefeed) (cons :header h)))
     (setext-style ((t <- h-chars linefeed (min-max #\= 1)) (list :h1 t))
		   ((t <- h-chars linefeed (min-max #\- 1)) (list :h2 t)))
     (atx-style (((token "######") t <- h-chars (min-max #\#)) (list :h6 t))
		(((token "#####") t <- h-chars (min-max #\#))  (list :h5 t))
		(((token "####") t <- h-chars (min-max #\#))   (list :h4 t))
		(((token "###") t <- h-chars (min-max #\#))    (list :h3 t))
		(((token "##") t <- h-chars (min-max #\#))     (list :h2 t))
		(((token "#") t <- h-chars (min-max #\#))      (list :h1 t)))
     ;; block quote
     (block-quote (('#\> (min-max char-whitespace?) p <- inlines linefeed)
		   (cons :block-quote p))
		  (('#\> (min-max char-whitespace?) linefeed
		    b <- block-quote)
		   b))
     ;; lists
     (mlist ((l <- star-lists linefeed) (cons :star-list l))
	    ((l <- number-lists linefeed) (cons :number-list l)))
     (star-lists ((l <- star-list ls <- star-lists) (cons l ls))
		 (() '()))
     (star-list ((space* (/ ('#\*) ('#\-)) space+ i <- inlines linefeed) i))
     (number-lists ((l <- number-list ls <- number-lists) (cons l ls))
		   (() '()))
     (number-list ((space* number '#\. space+ i <- inlines linefeed) i))
     ;; line
     (line (((line-def #\-) linefeed) (list :line))
	   (((line-def #\*) linefeed) (list :line)))
     ;; code block
     ;; for now we only supports space
     (code-block (((token "    ") c <- inlines linefeed)
		  (cons :code-block c)))
     ;; block html
     (block-html (('#\< s <- block-tag attr <- html-attributes space* '#\>
		   h <- block-contents
		   (token "</") e <- block-tag '#\> linefeed)
		  ;; TODO check tag
		  (cons* :html (cons :tag s) (cons :attr attr) h)))
     (block-tag ((t <- (one-of-token (block-tags))) t))
     (block-contents ((b <- block-html bs <- block-contents) (cons b bs))
		     ((i <- inline-html bs <- block-contents) (cons i bs))
		     ((t <- html-chars bs <- block-contents)
		      (cons t bs))
		     (() '()))
     ;; inline html
     (inline-html ((space* '#\< s <- inline-tag
			   attr <- html-attributes space* '#\>
		   h <- inline-contents
		   (token "</") e <- inline-tag '#\>)
		  ;; TODO check tag
		  (cons* :html (cons :tag s) (cons :attr attr) h)))
     (inline-tag ((t <- (one-of-token (inline-tags))) t))
     (inline-contents ((t <- html-chars is <- inline-contents) (cons t is))
		      ((i <- inline-html is <- inline-contents) (cons i is))
		      (() '()))
     ;; for now attribute value is the same as plain
     (html-attributes ((attr <- html-attribute attrs <- html-attributes)
		       (cons attr attrs))
		      (() '()))
     (html-attribute ((space+ name <- plain '#\= '#\" value <- attr-chars '#\")
		      (cons (cadr name) value)))
     ;; basic inline elements
     (text ((t <- any-chars) (list :text t)))
     (plain ((p <- plain-chars) (list :plain p)))
     (emphasis (('#\* t <- plain '#\*) (cons :italic (cadr t)))
	       (((token "**") t <- plain (token "**"))
		(cons :bold (cadr t)))
	       (('#\_ t <- plain '#\_) (cons :italic (cadr t)))
	       (((token "__") t <- plain (token "__"))
		(cons :bold (cadr t))))
     (link (( '#\[ i <- id '#\] '#\( u <- url space* '#\" t <- title '#\" '#\))
	    (list :link (cons :url u) (cons :title t) i))
	   (('#\[ i <- id '#\] '#\( u <- url '#\))
	    (list :link (cons :url u) i))
	   (('#\[ i <- id '#\] '#\[ ref <- id '#\])
	    (list :link (cons :id ref) i))
	   (('#\[ i <- id '#\] '#\[ '#\])
	    (list :link (cons :id i) i)))
     (image (('#\! '#\[ i <- id '#\] '#\( u <- url space* 
	      '#\" t <- title '#\" '#\))
	     (list :image (cons :url u) (cons :title t) i))
	    (('#\! '#\[ i <- id '#\] '#\( u <- url '#\))
	     (list :image (cons :url u) i))	    
	    (('#\! '#\[ i <- id '#\] '#\[ ref <- id '#\])
	     (list :image (cons :id ref) i))
	    (('#\! '#\[ i <- id '#\] '#\[ '#\])
	     (list :image (cons :id i) i)))
     (code-span (('#\` t <- code-span-chars '#\`) (cons :code-span t)))
     ;; reference (tag (:url . url) id)
     (reference (((min-max #\space 0 3) '#\[ i <- id '#\]
		  '#\: space+ u <- url space+ '#\" t <- title '#\")
		 (cons* :reference i u t))
		(((min-max #\space 0 3) '#\[ i <- id '#\] '#\: space+ u <- url)
		 (cons* :reference i u #f)))
     ;; linefeed can be \n, \r or \r\n
     (linefeed (('#\linefeed) "\n")
	       (('#\return) "\n")
	       (((token "\r\n")) "\n"))
     (space+ (((min-max *no-linefeed-ws-sets* 1)) '()))
     (space* (((min-max *no-linefeed-ws-sets*)) '()))
     ))

  ;; condition
  (define-condition-type &markdown-parser-error &error
    make-parser-error markdown-parser-error?
    (position markdown-parser-position)
    (expected markdown-parser-expected))

  (define (raise-markdown-perser-error who msg position expected)
    (raise
     (condition (make-parser-error position expected)
		(make-who-condition who)
		(make-message-condition msg))))

  #;(define (remove-separator lst)
    ;; :separator must be only toplevel
    (let loop ((acc '())
	       (lst lst))
      (cond ((null? lst) (reverse! acc))
	    ((eq? (car lst) :separator)
	     (loop acc (cdr lst)))
	    (else (loop (cons (car lst) acc) (cdr lst))))))

  ;; entry point
  (define (parse-markdown p :key (parser markdown-parser)
			         ;; additional tags
			         (inline-tag '())
				 (block-tag '())
			    ;; we don't use but need this
			    :allow-other-keys)
    (parameterize ((inline-tags *known-inline-tags*)
		   (block-tags *known-block-tags*))
      (unless (or (null? inline-tag)
		  (list? inline-tag))
	(assertion-violation 'parse-markdown
			     (format "list required but got ~s" inline-tag)
			     inline-tag))
      (unless (or (null? block-tag)
		  (list? block-tag))
	(assertion-violation 'parse-markdown
			     (format "list required but got ~s" block-tag)
			     block-tag))
      (inline-tags (append (inline-tags) inline-tag))
      (block-tags (append (block-tags) block-tag))
      (let ((result (parser (base-generator->results (generator p)))))
	(if (parse-result-successful? result)
	    (parse-result-semantic-value result)
	    (let ((e (parse-result-error result)))
	      (raise-markdown-perser-error 'parse-markdown
					   (parse-error-messages e)
					   (parse-error-position e)
					   (parse-error-expected e)))))))
	     
)