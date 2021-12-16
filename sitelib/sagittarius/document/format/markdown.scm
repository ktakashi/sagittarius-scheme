;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/format/markdown.scm - Markdown format
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius document format markdown)
    (export document->markdown)
    (import (rnrs)
	    (match)
	    (util file)
	    (sagittarius document output))

(define (document->markdown doc options out)
  (match doc
    (('document ('info info) ('content elm ...))
     (for-each (lambda (e) (write-markdown e options out)) elm))
    (('document ('@ attr) ('info info) ('content elm ...))
     (for-each (lambda (e) (write-markdown e options out)) elm))
    (else
     (assertion-violation 'document->markdown "Unknown document" doc))))

(define (write-markdown e options out)
  (cond ((string? e) (put-string out e))
	((pair? e)
	 (let-values (((name attr content) (document-decompose e)))
	   (case name
	     ((section) (write-section attr content options out))
	     ((define) (write-define attr content options out))
	     ((description) (write-description attr content options out))
	     ((code) (write-wrap "`" attr content options out))
	     ((var) (write-wrap "_" attr content options out))
	     ((strong) (write-wrap "**" attr content options out))
	     ((list) (write-list attr content options out))
	     ((dlist) (write-dlist attr content options out))
	     ((codeblock) (write-codeblock attr content options out))
	     ((link) (write-link attr content options out))
	     (else
	      (assertion-violation 'write-markdown "Unknown element" e)))))
	(else (put-datum out e))))

(define (write-link attr content options out)
  (define (write-it content link)
    (put-string out "[")
    (for-each (lambda (e) (write-markdown e options out)) content)
    (put-string out "](")
    (put-string out link)
    (put-char out #\)))
  (let ((source (cond ((assq 'source attr) => cadr) (else #f)))
	(format (cond ((assq 'format attr) => cadr) (else #f)))
	(href (cond ((assq 'href attr) => cadr) (else #f))))
    (cond (source
	   ;; TODO handle included document
	   (let* ((file (path-sans-extension source))
		  (md (string-append file ".md")))
	     (write-it (list md) md)))
	  (href (write-it content href))
	  (else (assertion-violation 'write-link
				     "Link doesn't have source or href"
				     `(link (@ ,@attr) ,@content))))))
	   
	    
(define (write-codeblock attr content options out)
  (define (output? e) (document-element-of? e 'output))
  (define (write-output output options out)
    (define (write-it e)
      (if (pair? e)
	  (begin
	    (put-char out #\()
	    (write-markdown (car e) options out)
	    (for-each (lambda (e)
			(put-char out #\space)
			(write-markdown e options out)) (cdr e))
	    (put-char out #\)))
	  (write-markdown e options out)))
    ;; output may contain sexp so sxml:content my filter them out
    (match output
      (('output ('@) e) (write-it e))
      (('output e) (write-it e))
      (else (assertion-violation 'write-codeblock "Unknown output format"
				 output))))
  (let ((style (string->symbol (cond ((assq 'style attr) => cadr)
				     (else "block"))))
	(lang (cond ((assq 'lang attr) => cadr)
		    (else (document-output-options-default-codeblock-language
			   options)))))
    (let-values (((output code) (partition output? content)))
      (case style
	((block)
	 (put-string out "```")
	 (when lang (put-string out lang))
	 (put-char out #\newline)
	 (for-each (lambda (e) (write-markdown e options out)) code)
	 (put-string out "```")
	 (put-char out #\newline))
	(else
	 (put-string out "``")
	 (for-each (lambda (e) (write-markdown e options out)) code)
	 (put-string out "``")))
      (unless (null? output)
	(unless (eq? style 'block) (put-char out #\space))
	(put-string out "=> ``")
	(for-each (lambda (e) (write-output e options out)) output)
	(put-string out "``")
	(when (eq? style 'block) (put-char out #\newline))))))

(define (write-dlist attr content options out)
  ;; using definition lists
  ;; https://www.markdownguide.org/extended-syntax/#definition-lists
  ;; NOTE: Seems GitHub doesn't support this
  (define (put-ditem i options out)
    (define (title? e) (document-element-of? e 'title))
    (let-values (((n a c) (document-decompose i)))
      (unless (eq? n 'ditem)
	(assertion-violation 'write-list "Unknown ditem" i))
      (let-values (((title* desc) (partition title? c)))
	(for-each (lambda (title)
		    (let-values (((tn ta tc) (document-decompose title)))
		      (for-each (lambda (e) (write-markdown e options out)) tc))
		    (put-char out #\newline)) title*)
	(put-string out ": ")
	(unless (null? desc)
	  (write-markdown (car desc) options out)
	  (for-each (lambda (e)
		      (put-string out "  ")
		      (write-markdown e options out)) (cdr desc)))))
    (put-char out #\newline)
    (put-char out #\newline))
  (put-char out #\newline)
  (for-each (lambda (i) (put-ditem i options out)) content))

(define (write-list attr content options out)
  (define (bullet-render out)
    (put-string out "- "))
  (define number-render
    (let ((n 1))
      (lambda (out)
	(put-string out (number->string n))
	(put-string out ". ")
	(set! n (+ n 1)))))
  (define (put-item i options out renderer)
    (renderer out)
    (let-values (((n a c) (document-decompose i)))
      (unless (eq? n 'item)
	(assertion-violation 'write-list "Unknown item" i))
      (for-each (lambda (e) (write-markdown e options out)) c))
    (put-char out #\newline))
  (let* ((style (string->symbol
		 (cond ((assq 'style attr) => cadr) (else "bullet"))))
	 (style-render (if (eq? style 'bullet)
			   bullet-render
			   number-render)))
    (put-char out #\newline)
    (for-each (lambda (i) (put-item i options out style-render)) content)))
    

(define (write-wrap s attr content options out)
  (put-string out s)
  (for-each (lambda (e) (write-markdown e options out)) content)
  (put-string out s))

(define (write-description attr content options out)
  (for-each (lambda (e) (write-markdown e options out)) content))

(define (write-define attr content options out)
  (define (write-it category name args)
    (put-string out "###### _")
    (put-string out category)
    (put-string out "_ `**")
    (write-markdown name options out)
    (put-string out "**")
    (when args
      (for-each (lambda (e)
		  (put-char out #\space)
		  (put-char out #\_)
		  (write-markdown e options out)
		  (put-char out #\_)) args))
    (put-string out "` {#")
    (write-markdown name options out)
    (put-char out #\})    
    (put-char out #\newline))
  (let ((category (cond ((assq 'category attr) => cadr) (else #f))))
    (write-it category (car content) (cdr content))))

(define (write-section attr content options out)
  (match content
    ((('header h ...) content ...)
     (let-values (((n a c) (document-decompose (cons 'header h))))
       (write-header a c options out))
     (for-each (lambda (e) (write-markdown e options out)) content))
    (else (assertion-violation 'write-section "Unknown element" content))))

(define (write-header attr content options out)
  (define (write-it level content)
    (define l (string->number level))
    (cond ((memv l '(1 2))
	   (for-each (lambda (e) (write-markdown e options out)) content)
	   (put-char out #\newline)
	   (if (eqv? l 1)
	       (put-string out "======")
	       (put-string out "------"))
	   (put-char out #\newline))
	  (else
	   (do ((i 0 (+ i 1)))
	       ((= i l))
	     (put-char out #\#))
	   (put-char out #\space)
	   (for-each (lambda (e) (write-markdown e options out)) content)
	   (put-char out #\newline))))
  (let ((level (cond ((assq 'level attr) => cadr) (else "1"))))
    (write-it level content)))

)
