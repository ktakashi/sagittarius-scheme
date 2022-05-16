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
    (export output-port->markdown-writer)
    (import (rename (rnrs)
		    (put-string r6:put-string)
		    (put-char r6:put-char)
		    (newline r6:newline))
	    (match)
	    (rename (sagittarius) (format sg:format)) ;; for format
	    (sagittarius document output)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (util file)
	    (util port))

(define-record-type markdown-writer
  (parent <document-writer>)
  (fields (mutable last-string))
  (protocol (lambda (p)
	      (lambda args
		((apply p args) "")))))

(define (output-port->markdown-writer output-port options)
  (make-markdown-writer output-port options
			node-handlers string-handler datum-handler))
(define (wrote-newline? writer)
  (let ((s (markdown-writer-last-string writer)))
    (and (not (string-null? s))
	 (eqv? #\newline (string-ref s (- (string-length s) 1))))))

(define (put-string writer s)
  (define port (document-writer-output-port writer))
  (define last (markdown-writer-last-string writer))
  
  (if (and (= (string-length s) 1) (eqv? (string-ref s 0) #\newline))
      (if (or (string-null? last)
	      (eqv? (string-ref last (- (string-length last) 1)) #\newline))
	  (markdown-writer-last-string-set! writer s) ;; ok
	  ;; make it a line
	  (markdown-writer-last-string-set! writer (string-append last s)))
      (markdown-writer-last-string-set! writer s))
  
  (r6:put-string port s))
(define (put-char writer c) (put-string writer (string c)))
(define (newline writer)
  (define last (markdown-writer-last-string writer))
  (unless (or (string-null? last) (string=? last "\n"))
    (put-string writer (string #\newline))))
(define (format writer fmt . args)
  (let ((s (apply sg:format fmt args)))
    (put-string writer s)))

(define (put-escaped-string out e)
  (string-for-each (lambda (c)
		     (case c
		       ((#\` #\* #\\ #\<)
			(put-char out #\\) (put-char out c))
		       (else (put-char out c)))) e))

(define (string-handler writer s next) (put-escaped-string writer s))
(define (datum-handler writer e next)
  (put-datum (document-writer-output-port writer) e))
  
(define (linebreak-handler out n attr content next)
  (put-string out "  \n"))

(define (blockquote-handler out n attr content next)
  (newline out)
  (for-each (lambda (s)
	      (unless (string-null? s)
		(put-string out "> ") (put-string out s)
		(put-char out #\newline)))
	    (->markdown-lines out content))
  (put-char out #\newline))

(define (raw-string-handler writer n attr content next)
  (for-each (lambda (c) (put-string writer c)) content))

(define (marker-handler writer n attr content next)
  (newline writer)
  (put-string writer "{{")
  (put-string writer (symbol->string n))
  (put-string writer "}}")
  (put-char writer #\newline)
  (put-char writer #\newline))

(define (eval-handler writer n attr content next)
  (put-string writer "@@")
  (for-each next content)
  (put-string writer "@@"))

(define (include-handler out n attr content next)
  (put-string out "* @[")
  (for-each next content)
  ;; bah...
  (put-string out "]\n"))

(define (table-handler out n attr content next)
  (define options (document-writer-options out))
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
      (let ((writer (output-port->markdown-writer out options)))
	(for-each (lambda (e) (traverse-document writer e)) content))
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
(define (link-handler out n attr content next)
  (define (write-it content link)
    (put-string out "[")
    (for-each next content)
    (put-string out "](")
    (put-string out link)
    (put-char out #\)))
  (define options (document-writer-options out))
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
	   
	    
(define (codeblock-handler out n attr content next)
  (define options (document-writer-options out))
  (define (output? e) (document-element-of? e 'output))
  (define (write-output output out)
    (define (write-it e)
      (cond ((pair? e)
	     (put-char out #\()
	     (next (car e))
	     (for-each (lambda (e)
			 (put-char out #\space)
			 (next e)) (cdr e))
	     (put-char out #\)))
	    (else (next e))))
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
	 ;; (for-each next code)
	 (for-each (lambda (c)
		     (if (string? c)
			 (put-string out c)
			 ;; damn...
			 (next c))) code)
	 (unless (wrote-newline? out) (newline out))
	 (put-string out "``````````"))
	(else
	 (put-string out "``")
	 (for-each (lambda (c)
		     (if (string? c)
			 (put-string out c)
			 ;; damn...
			 (next c))) code)
	 ;; (for-each next code)
	 (put-string out "``")))
      (unless (null? output)
	(if (eq? style 'block)
	    (put-char out #\newline)
	    (put-char out #\space))
	(put-string out "=> ``")
	(for-each (lambda (e) (write-output e out)) output)
	(put-string out "``"))
      (put-char out #\newline)
      (put-char out #\newline))))

(define (dlist-handler out n attr content next)
  ;; using definition lists
  ;; https://www.markdownguide.org/extended-syntax/#definition-lists
  ;; NOTE: Seems GitHub doesn't support this
  (define (put-ditem i out)
    (define (title? e) (document-element-of? e 'title))
    (let-values (((n a c) (document-decompose i)))
      (unless (eq? n 'ditem)
	(document-output-error 'write-dlist "Unknown ditem" i))
      (let-values (((title* desc) (partition title? c)))
	(for-each (lambda (title)
		    (let-values (((tn ta tc) (document-decompose title)))
		      (for-each next tc))
		    (put-char out #\newline)) title*)
	(put-string out ": ")
	(unless (null? desc)
	  (let ((lines (->markdown-lines out desc)))
	    (put-string out (car lines))
	    (put-char out #\newline)
	    (for-each (lambda (s)
			(unless (string-null? s)
			  (put-string out "  ")
			  (put-string out s)
			  (put-char out #\newline))) (cdr lines))))))
    (put-char out #\newline))
  (newline out)
  (for-each (lambda (i) (put-ditem i out)) content)
  (newline out))

(define (list-handler out n attr content next)
  (define (bullet-render out)
    (put-string out "- ")
    2)
  (define number-render
    (let ((n (cond ((assq 'start attr) =>
		    (lambda (s) (string->number (cadr s))))
		   (else 1))))
      (lambda (out)
	(let ((s (number->string n)))
	  (put-string out s)
	  (put-string out ". ")
	  (set! n (+ n 1))
	  (+ (string-length s) 2)))))
  (define options (document-writer-options out))
  (define (put-item i out renderer)
    
    (let-values (((n a c) (document-decompose i)))
      (unless (eq? n 'item)
	(document-output-error 'write-list "Unknown item" i))
      (let ((lines (->markdown-lines out c))
	    (indent (renderer out)))
	(put-string out (car lines))
	(for-each (lambda (s)
		    (unless (string-null? s)
		      (put-char out #\newline)
		      (do ((i 0 (+ i 1))) ((= i indent)) (put-char out #\space))
		      (put-string out s)))
		  (cdr lines)))
      (put-char out #\newline)))
  (let* ((style (string->symbol
		 (cond ((assq 'style attr) => cadr) (else "bullet"))))
	 (style-render (if (eq? style 'bullet)
			   bullet-render
			   number-render)))
    (put-char out #\newline)
    (for-each (lambda (i) (put-item i out style-render)) content)
    (put-char out #\newline)))
    
(define (code-handler out n attr content next)
  (put-char out #\`)
  (for-each (lambda (e)
	      ;; Damn, we can't assume
	      (if (string? e)
		  (put-string out e)
		  (next e))) content)
  (put-char out #\`))

(define (make-emph-handler s)
  (lambda (out n attr content next)
    (put-string out s)
    (for-each next content)
    (put-string out s)))
(define italic-handler (make-emph-handler "_"))
(define strong-handler (make-emph-handler "**"))

(define (paragraph-handler writer n attr content next)
  (newline writer)
  (let ((lines (->markdown-lines writer content)))
    (unless (null? lines)
      (put-string writer (car lines))
      (put-char writer #\newline)
      (for-each (lambda (s)
		  (put-string writer s)
		  (put-char writer #\newline)) (cdr lines))))
  (put-char writer #\newline))

(define (define-handler out n attr content next)
  (define (write-it category name args)
    (put-string out "###### [!")
    (put-string out category)
    (put-string out "] `")
    (if (string? name)
	(put-string out name)
	(next name))
    (put-string out "`")
    (when args
      (put-char out #\space)
      (for-each (lambda (e)
		  (cond ((string? e)
			 (put-string out " _")
			 (put-string out e)
			 (put-char out #\_))
			(else
			 (put-string out " ")
			 (next e)))) args))
    (put-char out #\newline))
  (let ((category (cond ((assq 'category attr) => cadr) (else #f))))
    (write-it category (car content) (cdr content))))

(define (section-handler out n attr content next)
  (match content
    ((('header h ...) content ...)
     (let ((tag (cond ((assq 'tag attr) => cadr)
		      (else #f)))
	   (level (cond ((assq 'level attr) => cadr)
		      (else "1"))))
       (newline out)
       (let-values (((n a c)
		     (document-decompose
		      (cons 'header h))))
	 (header-handler out n
			 (cons `(tag ,tag) a)
			 (cons (string-append "[§" level "] ") c) next)
	 (newline out))
       (for-each next content)))
    (else (document-output-error 'write-section "Unknown element" content))))

(define (header-handler out n attr content next)
  (define (write-it level tag content)
    (define l (string->number level))
    (define anchor (if tag (string-append " {#" tag "}") ""))
    (define (write-title)
      (define lines (->markdown-lines out content))
      (put-string out (car lines))
      (for-each (lambda (line)
		  (put-string out " ")
		  (put-string out line)) (cdr lines)))
      
    (cond ((memv l '(1 2))
	   (write-title)
	   (when anchor (put-string out anchor))
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
	   (write-title)
	   (when anchor (put-string out anchor))
	   (put-char out #\newline)
	   (put-char out #\newline))))
  (newline out)
  (let ((level (cond ((assq 'level attr) => cadr) (else "1")))
	(tag (cond ((assq 'tag attr) => cadr) (else #f))))
    (write-it level tag content)))

(define (thematic-break-handler out n attr content next)
  (newline out)
  (put-string out "---")
  (put-char out #\newline)
  (put-char out #\newline))

(define node-handlers
  `((header ,header-handler)
    (section ,section-handler)
    (paragraph ,paragraph-handler)
    (eval ,eval-handler)
    (table-of-contents ,marker-handler)
    (dlist ,dlist-handler)
    (code ,code-handler)
    (link ,link-handler)
    (define ,define-handler)
    (italic ,italic-handler)
    (strong ,strong-handler)
    (codeblock ,codeblock-handler)
    (list ,list-handler)
    (include ,include-handler)
    (table ,table-handler)
    (raw-string ,raw-string-handler)
    (blockquote ,blockquote-handler)
    (linebreak ,linebreak-handler)
    (index-table ,marker-handler)
    (author ,marker-handler)
    (thematic-break ,thematic-break-handler)))

(define (->markdown-lines out content)
  (let-values (((o e) (open-string-output-port)))
    (let ((out (output-port->markdown-writer o (document-writer-options out))))
      (for-each (lambda (e) (traverse-document out e)) content)
      (drop-while string-null? (trim-string* (e))))))

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
	'()
	;; max length to remove space
	(let ((space (check-space (car lines))))
	  (map (lambda (s) (string-drop-while s char-whitespace? space))
	       lines)))))
)
