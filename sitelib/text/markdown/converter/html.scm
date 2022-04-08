;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/converter/html.scm - Markdown->html converter
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
(library (text markdown converter html)
    (export markdown->html-converter
	    markdown-html-conversion-context-builder
	    commonmark-url-encoder
	    (rename (append-convert convert-nodes)
		    (get-attribute html-attribute))
	    
	    )
    (import (rnrs)
	    (record builder)
	    (rfc uri)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (text markdown parser nodes)
	    (text markdown converter api)
	    (text sxml tools))

(define-record-type markdown-html-conversion-context
  (fields user-attributes
	  url-encoder
	  url-sanitizer))

;; This is basically only for passing tests
(define (commonmark-url-encoder url)
  (let-values (((out e) (open-string-output-port)))
    (string-for-each (lambda (c)
		       (case c
			 ((#\") (put-string out "%22"))
			 ((#\\) (put-string out "%5C"))
			 ((#\`) (put-string out "%60"))
			 ((#\[) (put-string out "%5B"))
			 ((#\]) (put-string out "%5D"))
			 ((#\space) (put-string out "%20"))
			 (else
			  (if (char-set-contains? char-set:ascii c)
			      (put-char out c)
			      ;; TODO inefficient
			      (put-string out
					  (uri-encode-string (string c)))))))
		     url)
    (e)))

(define-syntax markdown-html-conversion-context-builder
  (make-record-builder markdown-html-conversion-context))

(define (convert-document document data next)
  (let ((r (append-convert next (markdown-node:children document))))
    (cond ((null? r) '(*TOP*))
	  ((equal? "\n" (car r)) `(*TOP* ,@(cdr r)))
	  (else `(*TOP* ,@r)))))

(define (convert-paragraph paragraph data next)
  (convert-container 'p paragraph data next))
(define (convert-heading node data next)
  (let ((tag (string->symbol (string-append "h" (heading-node-level node)))))
    (convert-container tag node data next)))

(define (convert-block-quote node data next)
  (convert-container/tag-line 'blockquote node data next))
(define (convert-bullet-list bullet-list data next)
  (convert-container/tag-line 'ul bullet-list data next))
(define (convert-ordered-list ordered-list data next)
  (define (wrap node tag)
    (let ((prev (get-attribute data node tag))
	  (start (ordered-list-node-start-number node)))
      (if (= start 1)
	  prev
	  (cons `(start ,(number->string start)) prev))))
      
  (convert-container/tag-line 'ol ordered-list wrap next))

(define (convert-item node data next)
  (define (in-tight-list? node)
    (cond ((markdown-node-parent node) =>
	   (lambda (parent)
	     (and (list-node? parent) ;; must be
		  (string=? "true" (list-node-tight parent)))))
	  (else #f)))
  (let ((tight? (in-tight-list? node)))
    (define (next-paragraph-strip node)
      (let ((r (next node)))
	;; to strip paragraph, it looks like this
	;; ("\n" (p ...) "\n")
	(if (and tight?
		 (string? (car r))
		 (string=? (car r) "\n")
		 (pair? (cadr r))
		 (eq? (caadr r) 'p))
	    (sxml:content (cadr r))
	    r)))
    `((li (@ ,@(get-attribute data node 'li))
	  ,@(append-convert next-paragraph-strip
			    (markdown-node:children node)))
      "\n")))

(define (convert-container/tag-line tag node data next)
  (let* ((content (append-convert next (markdown-node:children node)))
	 (elm `(,tag "\n"
		  (@ ,@(get-attribute data node tag))
		  ,@(cond ((null? content) content)
			  ((equal? (car content) "\n") (cdr content))
			  (else content))
		  ,@(if (or (null? content) (equal? "\n" (last content)))
			'()
			'("\n")))))
    (list "\n" elm "\n")))

(define (convert-container tag node data next)
  (let ((r `(,tag (@ ,@(get-attribute data node tag))
		  ,@(append-convert next (markdown-node:children node)))))
    (list "\n" r "\n")))

(define (convert-link node data next)
  (let-values (((url sanitized?)
		(sanitize-url data (link-node-destination node))))
    `((a (@ ,@(if sanitized?
		  '((rel "nofollow"))
		  '())
	    (href ,(encode-url data url))
	    ,@(cond ((link-node-title node) =>
		     (lambda (title) `((title ,title))))
		    (else '()))
	    ,@(get-attribute data node 'a))
	 ,@(append-convert next (markdown-node:children node))))))

(define (convert-image node data next)
  (let-values (((out e) (open-string-output-port)))
    (define (alt-text-converter node)
      (cond ((or (linebreak-node? node) (softbreak-node? node))
	     (put-string out "\n"))
	    ((text-node? node) (put-string out (text-node:content node)))
	    (else (for-each alt-text-converter
			    (markdown-node:children node)))))
    (alt-text-converter node)
    (let ((alt-text (e)))
      (let-values (((url _) (sanitize-url data (image-node-destination node))))
	`((img (@ (src ,(encode-url data url))
		  (alt ,alt-text)
		  ,@(cond ((image-node-title node) =>
			   (lambda (title) `((title ,title))))
			  (else '()))
		  ,@(get-attribute data node 'img))))))))

(define (convert-thematic-break thematic-break data next)
  `("\n" (hr (@ ,@(get-attribute data thematic-break 'hr))) "\n"))

(define (convert-code-block code-block data next)
  (let ((attrs (cond ((code-block-node-info code-block) =>
		      (lambda (info)
			(let ((lang (cond ((string-index info #\space) =>
					   (lambda (i) (substring info 0 i)))
					  (else info))))
			  (if (zero? (string-length lang))
			      '()
			      `((class ,(string-append "language-" lang)))))))
		     (else '()))))
    `("\n"
      (pre (@ ,@(get-attribute data code-block 'pre))
	   (code (@ ,@(append attrs (get-attribute data code-block 'code)))
		 ,(code-block-node:literal code-block)))
      "\n")))

(define (convert-code code data next)
  `((code (@ ,@(get-attribute data code 'code)) ,(code-node:literal code))))

(define (convert-softbreak node data next) '("\n"))

(define (convert-html-block node data next)
  `("\n"
    (*RAW-HTML* ,(html-block-node:literal node))
    "\n"))

(define (convert-html-inline node data next)
  `((*RAW-HTML* ,(html-inline-node:literal node))))

(define (convert-linebreak node data next)
  `((br (@ ,@(get-attribute data node 'br)))
    "\n"))

(define (convert-emphasis node data next)
  (convert-inline 'em node data next))
(define (convert-strong-emphasis node data next)
  (convert-inline 'strong node data next))

(define (convert-inline tag node data next)
  `((,tag (@ ,@(get-attribute data node tag))
	  ,@(append-convert next (markdown-node:children node)))))

(define (convert-text text data next) (list (text-node:content text)))

(define (get-attribute data node tag)
  (if (procedure? data)
      (or (data node tag) '())
      (let ((attribute
	     (and (markdown-html-conversion-context? data)
		  (markdown-html-conversion-context-user-attributes data))))
	(or (and (hashtable? data) (hashtable-ref data tag '()))
	    '()))))

(define (encode-url data url)
  (define encoder (and (markdown-html-conversion-context? data)
		       (markdown-html-conversion-context-url-encoder data)))
  (if (procedure? encoder)
      (encoder url)
      url))

(define (sanitize-url data url)
  (define sanitizer (and (markdown-html-conversion-context? data)
			 (markdown-html-conversion-context-url-sanitizer data)))
  (if (procedure? sanitizer)
      (values (sanitizer url) #t)
      (values url #f)))

(define (append-convert next node*)
  (define (strip-linefeed r prev)
    ;; FIXME this doesn't handle <softbreak /><softbreak /> case
    ;; though that shouldn't be done, as far as I know, by parser
    (if (null? r)
	r
	(let ((first (car r)))
	  (cond ((and (string? prev) (> (string-length prev) 0)
		      (string? first))
		 (let ((lc (string-ref prev (- (string-length prev) 1))))
		   (if (and (eqv? lc #\newline) (string=? first "\n"))
		       (values (cdr r) (last r))
		       (values r (last r)))))
		(else (values r (last r)))))))
	
  (let loop ((acc '()) (prev #f) (node* node*))
    (if (null? node*)
	(concatenate (reverse! acc))
	(let-values (((r prev) (strip-linefeed (next (car node*)) prev)))
	  (loop (cons r acc) prev (cdr node*))))))

(define-markdown-converter markdown->html-converter html
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
