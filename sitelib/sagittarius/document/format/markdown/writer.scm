;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/format/markdown/writer.scm - Markdown writer
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
(library (sagittarius document format markdown writer)
    (export write-markdown
	    (rename (make-markdown-writer output-port->markdown-writer)))
    (import (rename (rnrs)
		    (put-string r6:put-string)
		    (put-char r6:put-char)
		    (put-datum r6:put-datum)
		    (newline r6:newline))
	    (match)
	    (rename (sagittarius) (format sg:format)) ;; for format
	    (sagittarius document output)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (util file)
	    (util port))

(define-record-type markdown-writer
  (fields output-port
	  (mutable last-char))
  (protocol (lambda (p) (lambda (port) (p port #f)))))
(define (put-string writer s)
  (define port (markdown-writer-output-port writer))
  (if (and (= (string-length s) 1) (eqv? (string-ref s 0) #\newline))
      (markdown-writer-last-char-set! writer #\newline)
      (markdown-writer-last-char-set! writer #\A))
  (r6:put-string port s))
(define (put-char writer c) (put-string writer (string c)))
(define (newline writer)
  (define last-char (markdown-writer-last-char writer))
  (unless (or (not last-char) (eqv? #\newline last-char))
    (put-string writer (string #\newline))))
(define (format writer fmt . args)
  (let ((s (apply sg:format fmt args)))
    (put-string writer s)))
(define (put-datum writer d)
  (r6:put-datum (markdown-writer-output-port writer) d))

(define (put-escaped-string out e)
  (string-for-each (lambda (c)
		     (case c
		       ((#\` #\_ #\* #\\ #\<)
			(put-char out #\\) (put-char out c))
		       (else (put-char out c)))) e))

(define (write-markdown e options out)
  (cond ((string? e) (put-escaped-string out e))
	((pair? e)
	 (let-values (((name attr content) (document-decompose e)))
	   (case name
	     ((section) (write-section attr content options out))
	     ((define) (write-define attr content options out))
	     ((paragraph) (write-description attr content options out))
	     ((code) (write-wrap "`" attr content options out))
	     ((strong) (write-wrap "**" attr content options out))
	     ((italic) (write-wrap "_" attr content options out))
	     ((list) (write-list attr content options out))
	     ((dlist) (write-dlist attr content options out))
	     ((codeblock) (write-codeblock attr content options out))
	     ((link) (write-link attr content options out))
	     ((table) (write-table attr content options out))
	     ((header) (write-header attr content options out))
	     ((eval) (write-eval attr content options out))
	     ((include) (write-include attr content options out))
	     ((thematic-break) (put-string out "---"))
	     ((linebreak) (put-string out "  \n"))
	     ((table-of-contents)
	      (write-marker 'table-of-contents attr content options out))
	     ((index-table)
	      (write-marker 'index-table attr content options out))
	     ((author)
	      (write-marker 'author attr content options out))
	     ((blockquote) (write-blockquote attr content options out))
	     ((html) (for-each (lambda (c) (put-string out c)) content))
	     (else
	      (document-output-error 'write-markdown "Unknown element" e)))))
	(else (put-datum out e))))

(define (write-blockquote attr content options out)
  (define (write-it c)
    (let-values (((o e) (open-string-output-port)))
      (let ((writer (make-markdown-writer o)))
	(write-markdown c options writer)
	(let ((in (open-string-input-port (e))))
	  (do ((l (get-line in) (get-line in)))
	      ((eof-object? l))
	    (unless (string-null? l)
	      (put-string out "> ")
	      (put-string out l)
	      (put-char out #\newline)))))))
  (newline out)
  (for-each write-it content)
  #;(put-string out "> ")
  #;(for-each (lambda (e)
	      (write-markdown e options out)
	      (when (equal? "\n" e)
		(put-string out "> ")))
	    content)
  (put-char out #\newline)
  (put-char out #\newline))


(define (write-marker type attr content options out)
  (newline out)
  (put-string out "{{")
  (put-string out (symbol->string type))
  (put-string out "}}")
  (put-char out #\newline)
  (put-char out #\newline))

(define (write-eval attr content options out)
  (put-string out "@@")
  (for-each (lambda (e) (write-markdown e options out)) content)
  (put-string out "@@"))

(define (write-include attr content options out)
  (put-string out "- @[")
  (for-each (lambda (e) (write-markdown e options out)) content)
  ;; bah...
  (put-string out "]\n"))

(define (write-table attr content options out)
  (define (->cell cell)
    (define (replace-linefeed s)
      (let ((in (open-string-input-port s)))
	(let-values (((out e) (open-string-output-port)))
	  (do ((c (get-char in) (get-char in)))
	      ((eof-object? c) (e))
	    (case c
	      ((#\newline) (r6:put-string out "<br>"))
	      ((#\|) (r6:put-string out "\\|"))
	      (else (r6:put-char out c)))))))
    (let-values (((n attr content) (document-decompose cell))
		 ((out e) (open-string-output-port)))
      (let ((writer (make-markdown-writer out)))
	(for-each (lambda (e) (write-markdown e options writer)) content))
      (let ((colspan (- (cond ((assq 'colspan attr) => cadr) (else 1)) 1)))
	(do ((i 0 (+ i 1)) (r (list (list n (replace-linefeed (e))))
			      (cons (list 'colspan "<") r)))
	    ((= i colspan) (reverse! r))))))
  (define (->row row)
    (let-values (((n attr cells) (document-decompose row)))
      ;; TODO handle attr
      (append-map ->cell cells)))
  (define (compute-cell-size* l rows)
    (define v (make-vector l 0))
    (define (compute row)
      (do ((i 0 (+ i 1))
	   (c* row (cdr c*)))
	  ((= i l))
	;; (write c*) (newline)
	(let ((n (string-length (cadar c*)))
	      (l (vector-ref v i)))
	  (vector-set! v i (max n l)))))
    (for-each compute rows)
    v)
  (define (write-row out row cell-size* header-written?)
    (define (write-cell out cell size psuedo-header?)
      (put-string out "| ")
      (let* ((v (cadr cell))
	     (s (if psuedo-header? (string-append "**" v "**") v)))
	(format out (string-append "~" (number->string size) "a") s)))
    (define (write-line l)
      (put-string out "| ")
      (do ((i 0 (+ i 1))) ((= i l)) (put-char out #\-))
      (put-char out #\space))
      
    (do ((i 0 (+ i 1)) (c* row (cdr c*))
	 (header? #f (or header? (eq? (caar c*) 'header))))
	((null? c*)
	 (put-string out " |")
	 (when (and header? (not header-written?))
	   (put-char out #\newline)
	   (vector-for-each write-line cell-size*)
	   (put-char out #\|))
	 header?)
      (let ((c (car c*))
	    (l (vector-ref cell-size* i)))
	(unless (zero? i) (put-char out #\space))
	(write-cell out c l (and (eq? (caar c*) 'header) header-written?)))))
  (newline out)
  (let* ((title (cond ((assq 'title attr) => cadr) (else #f)))
	 (rows (map ->row content))
	 (row-length (length (car rows)))
	 (cell-size* (compute-cell-size* row-length rows)))
    (put-char out #\newline)
    (fold-left (lambda (header-written? row)
		 (let ((r (write-row out row cell-size* header-written?)))
		   (put-char out #\newline)
		   (or header-written? r)))
	       #f rows)
    (put-char out #\newline)))

(define default-callback (document-output:make-file-link-callback ".md"))
(define (get-callback options)
  (cond ((document-output-options-link-source-callback options))
	(else default-callback)))
(define (write-link attr content options out)
  (define (write-it content link)
    (put-string out "[")
    (for-each (lambda (e) (write-markdown e options out)) content)
    (put-string out "](")
    (put-string out link)
    (put-char out #\)))
  (let ((source (cond ((assq 'source attr) => cadr) (else #f)))
	(format (cond ((assq 'format attr) => cadr) (else #f)))
	(href (cond ((assq 'href attr) => cadr) (else #f)))
	(anchor (cond ((assq 'anchor attr) => cadr) (else #f))))
    (cond (source ((get-callback options) source format write-it))
	  (href (write-it content href))
	  (anchor (write-it content (string-append "#" anchor)))
	  (else (document-output-error 'write-link
				       "Link doesn't have source or href"
				       `(link (@ ,@attr) ,@content))))))
	   
	    
(define (write-codeblock attr content options out)
  (define (output? e) (document-element-of? e 'output))
  (define (write-output output options out)
    (define (write-it e)
      (cond ((pair? e)
	     (put-char out #\()
	     (write-markdown (car e) options out)
	     (for-each (lambda (e)
			 (put-char out #\space)
			 (write-markdown e options out)) (cdr e))
	     (put-char out #\)))
	    (else (write-markdown e options out))))
    ;; output may contain sexp so sxml:content my filter them out
    (match output
      (('output ('@) e) (write-it e))
      (('output e) (write-it e))
      (else (document-output-error 'write-codeblock "Unknown output format"
				   output))))
  (let ((style (string->symbol (cond ((assq 'style attr) => cadr)
				     (else "block"))))
	(lang (cond ((assq 'lang attr) => cadr)
		    (else (document-output-options-default-codeblock-language
			   options)))))
    (let-values (((output code) (partition output? content)))
      (case style
	((block)
	 (newline out)
	 (put-string out "``````````")
	 (when lang (put-string out lang))
	 (put-char out #\newline)
	 (for-each (lambda (e) (write-markdown e options out)) code)
	 (put-string out "``````````"))
	(else
	 (put-string out "``")
	 (for-each (lambda (e) (write-markdown e options out)) code)
	 (put-string out "``")))
      (unless (null? output)
	(if (eq? style 'block)
	    (put-char out #\newline)
	    (put-char out #\space))
	(put-string out "=> ``")
	(for-each (lambda (e) (write-output e options out)) output)
	(put-string out "``"))
      (put-char out #\newline)
      (put-char out #\newline))))

(define (write-dlist attr content options out)
  ;; using definition lists
  ;; https://www.markdownguide.org/extended-syntax/#definition-lists
  ;; NOTE: Seems GitHub doesn't support this
  (define (put-ditem i options out)
    (define (title? e) (document-element-of? e 'title))
    (let-values (((n a c) (document-decompose i)))
      (unless (eq? n 'ditem)
	(document-output-error 'write-dlist "Unknown ditem" i))
      (let-values (((title* desc) (partition title? c)))
	(for-each (lambda (title)
		    (let-values (((tn ta tc) (document-decompose title)))
		      (for-each (lambda (e) (write-markdown e options out)) tc))
		    (put-char out #\newline)) title*)
	(put-string out ": ")
	(unless (null? desc)
	  (write-trimmed-string out (lambda (out)
				      (write-markdown (car desc) options out)))
	  (for-each (lambda (e)
		      (let ((s (trim-markdown
				(lambda (out) (write-markdown e options out)))))
			(unless (or (zero? (string-length s))
				    (string-every char-whitespace? s))
			  (put-char out #\newline)
			  (put-string out "  ")
			  (put-string out s))))
		    (cdr desc)))))
    (put-char out #\newline)
    (put-char out #\newline))
  (newline out)
  (for-each (lambda (i) (put-ditem i options out)) content)
  (newline out))

(define (write-list attr content options out)
  (define (bullet-render out)
    (put-string out "- "))
  (define number-render
    (let ((n (cond ((assq 'start attr) =>
		    (lambda (s) (string->number (cadr s))))
		   (else 1))))
      (lambda (out)
	(put-string out (number->string n))
	(put-string out ". ")
	(set! n (+ n 1)))))
  (define (put-item i options out renderer)
    (renderer out)
    (let-values (((n a c) (document-decompose i)))
      (unless (eq? n 'item)
	(document-output-error 'write-list "Unknown item" i))
      (let-values (((o e) (open-string-output-port)))
	(let ((writer (make-markdown-writer o)))
	  (for-each (lambda (e) (write-markdown e options writer)) c)
	  ;; The first letter and the last letters are #\newline
	  (put-string out (string-trim-both (e))))))
    (put-char out #\newline))
  (let* ((style (string->symbol
		 (cond ((assq 'style attr) => cadr) (else "bullet"))))
	 (style-render (if (eq? style 'bullet)
			   bullet-render
			   number-render)))
    (put-char out #\newline)
    (for-each (lambda (i) (put-item i options out style-render)) content)
    (put-char out #\newline)))
    

(define (write-wrap s attr content options out)
  (put-string out s)
  (for-each (lambda (e) (write-markdown e options out)) content)
  (put-string out s))

(define (write-description attr content options out)
  (newline out)
  (write-trimmed-string out
   (lambda (out)
     (for-each (lambda (e) (write-markdown e options out)) content)))
  (put-char out #\newline)
  (put-char out #\newline)
  )

(define (write-define attr content options out)
  (define (write-it category name args)
    (put-string out "###### [!")
    (put-string out category)
    (put-string out "] `")
    (write-markdown name options out)
    (put-string out "`")
    (when args
      (put-char out #\space)
      (for-each (lambda (e)
		  (cond ((pair? e) (write-markdown e options out))
			(else
			 (put-string out " _")
			 (write-markdown e options out)
			 (put-char out #\_)))) args))
    (put-char out #\newline))
  (let ((category (cond ((assq 'category attr) => cadr) (else #f))))
    (write-it category (car content) (cdr content))))

(define (write-section attr content options out)
  (match content
    ((('header h ...) content ...)
     (let ((tag (cond ((assq 'tag attr) => cadr)
		      (else #f))))
       (newline out)
       (let-values (((n a c)
		     (document-decompose
		      (cons 'header
			    (if tag
				(append h (list (string-append " {#" tag "}")))
				h)))))
	 (write-header a c options out))
       (newline out)
       (for-each (lambda (e) (write-markdown e options out)) content)))
    (else (document-output-error 'write-section "Unknown element" content))))

(define (write-header attr content options out)
  (define (write-it level content)
    (define l (string->number level))
    (cond ((memv l '(1 2))
	   (for-each (lambda (e) (write-markdown e options out)) content)
	   (put-char out #\newline)
	   (if (eqv? l 1)
	       (put-string out "=============")
	       (put-string out "-------------"))
	   (put-char out #\newline)
	   (put-char out #\newline))
	  (else
	   (do ((i 0 (+ i 1)))
	       ((= i l))
	     (put-char out #\#))
	   (put-char out #\space)
	   (for-each (lambda (e) (write-markdown e options out)) content)
	   (put-char out #\newline)
	   (put-char out #\newline))))
  (newline out)
  (let ((level (cond ((assq 'level attr) => cadr) (else "1"))))
    (write-it level content)))

(define (write-trimmed-string out proc)
  (put-string out (trim-markdown proc)))

(define (trim-markdown proc)
  (let-values (((o e) (open-string-output-port)))
    (proc (make-markdown-writer o))
    (trim-string* (e))))

(define (trim-string* s)
  (define (check-space s) (cond ((string-skip s char-whitespace?)) (else 0)))
  (define (string-drop-while s pred max)
    ;; we use string-ref as Sagittarius does O(1) access :)
    ;; good boys shouldn't do it
    (do ((i 0 (+ i 1)))
	((or (= i max) (not (pred (string-ref s i))))
	 (substring s i (string-length s)))))
  (let ((lines (port->string-list (open-string-input-port s))))
    (if (null? lines)
	""
	;; max length to remove space
	(let ((space (check-space (car lines))))
	  (string-join
	   (map (lambda (s) (string-drop-while s char-whitespace? space))
		lines) "\n")))))
)
