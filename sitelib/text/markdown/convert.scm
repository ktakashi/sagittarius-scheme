;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/convert.scm - Converter for parsed markdown
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

(library (text markdown convert)
    (export markdown-sexp->sxml
	    markdown-sexp->string)
    (import (rnrs)
	    (match)
	    (sagittarius)
	    (srfi :1 lists) ;; reverse!
	    (srfi :26 cut)
	    (text sxml serializer)
	    (text markdown parser))

  ;; if Sagittarius has eql specializer, it will be mutch easier,
  ;; or should we make <ast> from the beginning?
  ;;
  ;; structure of input sexp
  ;; :doc
  ;;  - :header
  ;;  - :block-quote
  ;;  - :line
  ;;  - :html
  ;;  - :reference ;; needs to be collected first
  ;;  - :code-block
  ;;  - :star-list
  ;;  - :number-list
  ;;  - :paragraph
  ;; First we need to collect :reference to make link work properly and check
  ;; the validity.
  ;; Second convert to sxml (shtml)
  ;; style keyword arguments must be alist with these structure keyword and
  ;; detail keywords (ex :inline) to specify its style.
  (define (markdown-sexp->sxml sexp :key (style #f) ;; for the top most
			                 (class #f)
			            :allow-other-keys attributes)
    ;; under the :doc
    (define (collect-reference sexp)
      (let loop ((sexp sexp) (acc '()) (ref '()))
	(cond ((null? sexp) (values (reverse! acc) ref))
	      ((and (pair? (car sexp)) (keyword? (caar sexp)))
	       (case (caar sexp)
		 ((:header :block-quote :line :html :code-block
		   :star-list :number-list :paragraph)
		  (loop (cdr sexp) (cons (car sexp) acc) ref))
		 ((:reference)
		  (loop (cdr sexp) acc (cons (cdar sexp) ref)))
		 (else
		  (assertion-violation 'markdown-sexp->sxml
				       "invalid keyword in the given list"
				       (caar sexp) sexp))))
	      ((eq? (car sexp) :separator) (loop (cdr sexp)
						 (cons :separator acc) ref))
	      (else
	       ;; invalid list
	       (assertion-violation 'markdown-sexp->sxml
				    "invalid markdown list"  sexp)))))
    (define (alist->attr alist)
      (let loop ((alist alist) (r '()))
	(cond ((null? alist) (reverse! r))
	      ((and (string? (caar alist)) (string? (cdar alist)))
	       (loop (cdr alist) (cons (list (string->symbol (caar alist))
					     (cdar alist)) r)))
	      (else
	       (assertion-violation 'markdown-sexp->sxml
				    "invalid html attribute, must be alist of strings"
				    alist)))))

    (unless (and (pair? sexp) (eq? (car sexp) :doc))
      (assertion-violation 'markdown-sexp->sxml
			   "invalid keyword in the given list"
			   (car sexp) sexp))
    (let-values (((sexp refs) (collect-reference (cdr sexp))))
      (define (lookup-reference ref)
	(cond ((assoc ref refs) => cdr)
	      (else
	       (assertion-violation 'markdown-sexp->sxml
				    "invalid reference"
				    ref sexp))))

      (define (get-attribute key)
	(cond ((get-keyword key attributes #f)
	       => (cut append '(@) <>))
	      (else '(@))))

      (define (detail->sxml sexp)
	(define (gen-link param attrs)
	  (let loop ((param param) (attr '()))
	    (match param
	      (((:url . url) . rest)
	       (loop (cdr param)
		     (cons `(href ,url) attr)))
	      (((:title . t) . rest)
	       (loop (cdr param) (cons `(title ,t) attr)))
	      (((:id . ref) . rest)
	       (let ((r (lookup-reference ref)))
		 (loop (cdr param) (cons* `(href ,(car r))
					  (if (cdr r) `(title ,(cdr r)) '())
					  attr))))
	      (((? string? title))
	       `(a ,(append attrs attr) ,title)))))
	(define (gen-img-link param attrs)
	  (let loop ((param param) (attr '()))
	    (match param
	      (((:url . url) . rest)
	       (loop (cdr param)
		     (cons `(src ,url) attr)))
	      (((:title . t) . rest)
	       (loop (cdr param) (cons `(title ,t) attr)))
	      (((:id . ref) . rest)
	       (let ((r (lookup-reference ref)))
		 (loop (cdr param) (cons* `(src ,(car r))
					  (if (cdr r) `(title ,(cdr r)) '())
					  attr))))
	      (((? string? title))
	       `(img ,(append attrs attr `((alt ,title))))))))

	(let loop ((sexp sexp) (acc '()))
	  (match sexp
	    (() (reverse! acc))
	    (((:plain content) . rest)
	     (loop (cdr sexp) (cons content acc)))
	    (((:text content) . rest)
	     (loop (cdr sexp) (cons content acc)))
	    (((:bold . content) . rest)
	     (loop (cdr sexp) (cons `(strong ,(get-attribute :bold)
					     ,content) acc)))
	    (((:italic . content) . rest)
	     (loop (cdr sexp) (cons `(em ,(get-attribute :italic)
					 ,content) acc)))
	    (((:link . param) . rest)
	     (loop (cdr sexp) (cons (gen-link param (get-attribute :link))
				    acc)))
	    (((:image . param) . rest)
	     (loop (cdr sexp) (cons (gen-img-link param (get-attribute :image))
				    acc)))
	    (((:code-span . content) . rest)
	     (loop (cdr sexp) (cons `(code ,(get-attribute :code-span) ,content)
				    acc)))
	    ;; inline html can be here
	    (((:html (:tag . name) (:attr . attr) content) . rest)
	     (loop (cdr sexp) (cons `(,(string->symbol name)
				      (@ ,@(alist->attr attr)) ,content) acc)))
	    ;; block-quote can have paragraph
	    (((:paragraph . content) . rest)
	     (loop (cdr sexp) (cons `(p ,(get-attribute :paragraph)
					,@(detail->sxml content)) acc)))
	    (_ (assertion-violation 'markdown-sexp->sxml
				    "invalid inline sexp form" sexp))
	    )))

      (define (merge-by-tag tag first rest)
	(let loop ((lst rest) (acc (detail->sxml first)))
	  (if (and (pair? lst) (pair? (car lst))
		   (eq? (caar lst) tag))
	      (loop (cdr lst)
		    (append acc '("\n") (detail->sxml (cdar lst))))
	      (values acc lst))))

      (define (collect-codes first rest)
	(merge-by-tag :code-block first rest))
      (define (collect-paragraph first rest)
	(merge-by-tag :paragraph first rest))
      (define (collect-blockquote first rest)
	(merge-by-tag :block-quote first rest))
      ;; TODO refactor
      (define (rec sexp in-html?)
	(let loop ((sexp sexp) (acc '()))
	  ;; TODO add style
	  (match sexp
	    (() 
	     (if in-html?
		 (reverse! acc)
		 (append `(div (@ ,@(let ((s style) (c class))
				      (cond ((and s c)
					     `((style ,s) (class ,c)))
					    (s `((style ,s)))
					    (c `((class ,c)))
					    (else '())))))
			 (reverse! acc))))
	    (((:header (? keyword? type) content) . rest)
	     (loop (cdr sexp) (cons `(,(keyword->symbol type)
				      ,(let ((try (get-attribute type)))
					 (if (null? try)
					     (get-attribute :header)
					     try))
				      ,content) acc)))
	    (((:block-quote . content) . rest)
	     (let-values (((bq next) (collect-blockquote content rest)))
	       (loop next (cons `(blockquote ,(get-attribute :block-quote)
					     ,@bq) acc))))
	    (((:line) . rest)
	     (loop (cdr sexp) (cons `(hr ,(get-attribute :line)) acc)))
	    (((:html (:tag . name) (:attr . attr) . content) . rest)
	     (loop (cdr sexp) (cons `(,(string->symbol name)
				      (@ ,@(alist->attr attr))
				      ,@(rec content #t)) acc)))
	    (((:paragraph . content) . rest)
	     ;; the same as code-block
	     (let-values (((para next) (collect-paragraph content rest)))
	       (loop next (cons `(p ,(get-attribute :paragraph) ,@para) acc))))
	    (((:star-list . items) . rest)
	     (loop (cdr sexp) (cons `(ul ,(get-attribute :star-list)
					 ,@(map (lambda (item)
						  `(li ,(get-attribute :list-item)
						       ,@(detail->sxml item)))
						items))
				    acc)))
	    (((:number-list . items) . rest)
	     (loop (cdr sexp) (cons `(ol ,(get-attribute :number-list)
					 ,@(map (lambda (item)
						  `(li ,(get-attribute :list-item)
						       ,@(detail->sxml item)))
						items))
				    acc)))
	    (((:code-block . code) . rest)
	     ;; we need special treat for this
	     (let-values (((codes next) (collect-codes code rest)))
	       (loop next (cons `(pre ,(get-attribute :code-block) ,@codes)
				acc))))
	    ((:separator . rest)
	     ;; just a seperator ignore
	     (loop (cdr sexp) acc))
	    ;; for block html's content
	    (((? string? text) . rest)
	     (loop (cdr sexp) (cons text acc)))
	    (_ (assertion-violation 'markdown-sexp->sxml
				    "invalid block sexp form" sexp))
	    )))
      (rec sexp #f)
      ))

  (define (markdown-sexp->string sexp . opts)
    (let ((sxml (apply markdown-sexp->sxml sexp opts)))
      (srl:sxml->html sxml)))

  )
