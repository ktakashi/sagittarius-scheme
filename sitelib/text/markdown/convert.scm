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
  (define (markdown-sexp->sxml sexp :key (style '())
			            :allow-other-keys)
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

      (define (detail->sxml sexp)
	(define (gen-link name param)
	  (let loop ((param param) (attr '()))
	    (match param
	      (((:url . url) . rest)
	       (loop (cdr param)
		     (cons `(,(case name
				((a) 'href)
				((img) 'src)) ,url) attr)))
	      (((:title . t) . rest)
	       (loop (cdr param) (cons `(title ,t) attr)))
	      (((:id . ref) . rest)
	       ;; TODO check consistancy with :url
	       (loop (cdr param) (cons `(,(case name ((a) 'href) ((img) 'src))
					 ,(lookup-reference ref)) attr)))
	      (((? string? title))
	       `(,name (@ ,@attr) ,title)))))

	(let loop ((sexp sexp) (acc '()))
	  (match sexp
	    (() (reverse! acc))
	    (((:plain content) . rest)
	     (loop (cdr sexp) (cons content acc)))
	    (((:text content) . rest)
	     (loop (cdr sexp) (cons content acc)))
	    (((:bold content) . rest)
	     (loop (cdr sexp) (cons `(strong ,content) acc)))
	    (((:italic content) . rest)
	     (loop (cdr sexp) (cons `(i ,content) acc)))
	    (((:link . param) . rest)
	     (loop (cdr sexp) (cons (gen-link 'a param) acc)))
	    (((:image . param) . rest)
	     (loop (cdr sexp) (cons (gen-link 'img param) acc)))
	    )))

      (define (collect-codes first rest)
	(define (rec code)
	  (detail->sxml code))
	(let loop ((codes rest) (acc (rec first)))
	  (match codes
	    (((:code-block . code) . rest)
	     ;; ((:code-block ...) (:code-block ...) ...)
	     ;; must be one <pre> so we need to add linefeed
	     (loop (cdr codes) (append acc '("\n") (rec code))))
	    (_ (values acc codes))
	  ))
	)

      (let loop ((sexp sexp) (acc '()))
	;; TODO add style
	(match sexp
	  (() (append '(div) (reverse! acc)))
	  (((:header (? keyword? type) content) . rest)
	   (loop (cdr sexp) (cons `(,(keyword->symbol type) ,content) acc)))
	  (((:block-quote . content) . rest)
	   (loop (cdr sexp) (cons `(blockquote ,@(detail->sxml content)) acc)))
	  (((:line) . rest)
	   (loop (cdr sexp) (cons `(hr) acc)))
	  (((:html (:tag . name) (:attr . attr) content) . rest)
	   (loop (cdr sexp) (cons `(,(string->symbol name)
				    (@ ,@(alist->attr attr)) ,content) acc)))
	  (((:paragraph . content) . rest)
	   (loop (cdr sexp) (cons `(p ,@(detail->sxml content)) acc)))
	  (((:star-list . items) . rest)
	   (loop (cdr sexp) (cons `(ul ,@(map (lambda (item)
						`(li ,@(detail->sxml item)))
					      items))
				  acc)))
	  (((:number-list . items) . rest)
	   (loop (cdr sexp) (cons `(ol ,@(map (lambda (item)
						`(li ,@(detail->sxml item)))
					      items))
				  acc)))
	  (((:code-block . code) . rest)
	   ;; we need special treat for this
	   (let-values (((codes next) (collect-codes code rest)))
	     (loop next (cons `(pre ,@codes) acc))))

	  ))
      ))

  (define (markdown-sexp->string sexp . opts)
    (let ((sxml (apply markdown-sexp->sxml sexp opts)))
      (srl:sxml->html sxml)))

  )
