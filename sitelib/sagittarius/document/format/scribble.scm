;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/format/scribble.scm - Scribble to document parser
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
(library (sagittarius document format scribble)
    (export scribble->document)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius document input)
	    (match)
	    ;; We use this for simplicity...
	    (scribble parser)
	    (scribble convert)
	    (srfi :1 lists)
	    (srfi :13 strings))

(define (scribble->document input)
  `(document
    (info
     (source ,(document-input-filename input)))
    (content
     ,@(scribble-token*->content
	(scribble-parse (document-input-port input))))))

(define (scribble-token*->content token*)
  (let loop ((token* token*) (acc '()))
      (if (null? token*)
	  (reverse! acc)
	  (let-values (((next* next-acc)
			(consume-token* (car token*) (cdr token*) acc)))
	    (loop next* next-acc)))))

;; We need to do sort of the same as scribble->html unfortunately
(define (consume-token* token next* acc)
  ;; (write token) (newline)
  (cond ((pair? token)
	 (case (car token)
	   ((section subsection subsubsection sub*section)
	    (handle-section token next* acc))
	   ((code var) (simple-handler token next* acc))
	   ((dots) (values next* (cons "..." acc)))
	   ((atmark) (values next* (cons "..." acc)))
	   ((itemlist) (handle-itemlist token next* acc))
	   ((define) (handle-define token next* acc))
	   ((desc) (handle-desc token next* acc))
	   ((codeblock) (handle-codeblock "block" token next* acc))
	   ((snipet) (handle-codeblock "inline" token next* acc))
	   ((include-section) (handle-include-section token next* acc))
	   ((secref) (handle-secref token next* acc))
	   ((dl-list) (handle-dl-list token next* acc))
	   ((string) (handle-string token next* acc))
	   ((b) (handle-emphasize 'strong token next* acc))
	   (else (assertion-violation 'consume-token* "Unknown token" token))))
	(else (values next* (cons token acc)))))

(define (handle-emphasize tag token next* acc)
  (values next*
	  (cons `(,tag (@) ,@(scribble-token*->content (cdr token))) acc)))
;; no idea what I was thinking...
(define (handle-string token next* acc)
  (values next* `(,@(scribble-token*->content (cdr token)) . ,acc)))

(define (handle-dl-list token next* acc)
  (define (->title item)
    `(title (@) ,@(scribble-token*->content (list item))))
  (define (handle-item item)
    (cond ((pair? item)
	   (case (car item)
	     ((dl-item)
	      `(item (@)
		     ,(->title (cadr item))
		     ,@(scribble-token*->content (cddr item))))
	     ((dl-itemx)
	      (let ((n (cadr item)))
		(let-values (((title* body) (split-at (cddr item) n)))
		  `(item (@)
			 ,@(map ->title title*)
			 ,@(scribble-token*->content body)))))
	     (else
	      (assertion-violation 'handle-dl-list
				   "Invalid @dl-list format" item))))
	  ((string? item)
	   (unless (string-every char-whitespace? item)
	     (assertion-violation 'handle-dl-list
				  "@dl-list contains non space string"
				  item))
	   #f)
	  (else #f)))
  (values next* (cons
		 `(dlist (@)
		   ,@(filter-map handle-item (cdr token))) 
		 acc)))

(define (handle-secref token next* acc)
  (values next* (cons `(link (@ (anchar ,(cadr token)))
			     ,@(scribble-token*->content (cddr token)))
		      acc)))

(define (handle-include-section token next* acc)
  (define file (cadr token))
  (values next*
	  (cons `(link (@ (source ,file) (format scribble)) ,file) acc))) 

(define (handle-codeblock style token next* acc)
  (define args (cdr token))
  (let-values (((e body)
		(if (eq? (car args) '=>)
		    (values (cadr args) (cddr args))
		    (values #f args))))
    (values next*
	    (cons `(codeblock (@ (style ,style))
		    ,@(if e `((output (@) ,e)) '())
		    ,@(scribble-token*->content body))
		  acc))))

(define (handle-desc token next* acc)
  ;; TODO I think 'description' should be psuedo...
  (values next*
	  (cons `(description (@)
		  ,@(scribble-token*->content (cdr token)))
		acc)))

(define (handle-define token next* acc)
  (define (strip-string body) (remove string? body))
  (match token
    (('define category body ...)
     (let ((cmd* (strip-string body)))
       (match cmd*
	 ((('name name ...))
	  (values next*
		  (cons `(define (@ (category ,(format "~a" category)))
			   ,@(scribble-token*->content name))
			acc)))
	 ((('name name ...) ('args args ...))
	  (values next*
		  (cons `(define (@ (category ,(format "~a" category)))
			   ,@(scribble-token*->content name)
			   ,@(scribble-token*->content args))
			acc)))
	 (else (assertion-violation 'handle-define
				    "Unknotn body format" token)))))
    (else (assertion-violation 'handle-define
			       "Unknotn format" token))))


(define (simple-handler token next* acc)
  (values next*
	  (cons `(,(car token) (@)
		  ,@(scribble-token*->content (cdr token)))
		acc)))

(define (handle-itemlist token next* acc)
  (define (handle-item item)
    (or (and (pair? item)
	     (or (eq? 'item (car item))
		 (assertion-violation 'handle-itemlist
				      "Invalid @itemlist format"
				      token))
	     `(item (@) ,@(scribble-token*->content (cdr item))))
	(and (string? item)
	     (not (string-every char-whitespace? item))
	     (assertion-violation 'handle-itemlist
				  "@itemlist contains non space string"
				  item))))
  (values next* (cons `(list (@ (style "bullet"))
			     ,@(filter-map handle-item (cdr token))) acc)))

(define (handle-section token next* acc0)
  (define section (car token))
  (define (section->level section)
    (case section
      ((section)       1)
      ((subsection)    2)
      ((subsubsection) 3)
      ((sub*section)   4)
      (else #f)))
  (define section-level (section->level section))
  (define (consume cur next*)
    (let loop ((acc '()) (next* next*))
      (if (null? next*)
	  (values next* (reverse! acc))
	  (let ((token (car next*)) (next* (cdr next*)))
	    (cond ((and (pair? token) (section->level (car token))) =>
		   (lambda (level)
		     (if (< cur level)
			 ;; okey sub section can be in this section
			 (let-values (((next* next-acc)
				       (consume-token* token next* acc)))
			   (loop next-acc next*))
			 ;; put it back :)
			 (values (cons token next*) (reverse! acc)))))
		  (else
		   (let-values (((next* next-acc)
				 (consume-token* token next* acc)))
		     (loop next-acc next*))))))))

  (unless section-level
    (assertion-violation 'handle-section "Unknown section" section token))

  (let-values (((attr content) (scribble-parse-attribute (cdr token)))
	       ((next* acc) (consume section-level next*)))
    (values next*
	    (cons `(section (@ ,@attr)
			    (header (@ (level ,(number->string section-level)))
				    ,@(scribble-token*->content content))
			    ,@acc)
		  acc0))))

)
