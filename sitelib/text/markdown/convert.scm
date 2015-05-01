;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/convert.scm - Converter for parsed markdown
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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
	    (sagittarius control) ;; for push
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :26 cut)
	    (text sxml serializer)
	    (text markdown parser)
	    (pp))

  ;; or should we make <ast> from the beginning?
  ;;
  ;; structure of input sexp
  ;; :doc
  ;;  - :blockquote
  ;;  - :verbatim
  ;;  - :note
  ;;  - :reference
  ;;  - :line
  ;;  - :header
  ;;  - :ordered-list
  ;;  - :bullet-list
  ;;  - :paragraph
  ;;  - :plain
  ;;  ;; - :html-block  - not yet
  ;;  ;; - :style-block - not yet
  ;; First we need to collect :reference to make link work properly and check
  ;; the validity.
  ;; Second convert to sxml (shtml)
  ;; style keyword arguments must be alist with these structure keyword and
  ;; detail keywords (ex :inline) to specify its style.
  (define (markdown-sexp->sxml sexp :key (style #f) ;; for the top most
			                 (class #f)
					 (no-reference #f)
					 (no-notes #f)
			            :allow-other-keys attributes)
    ;; under the :doc
    (define (collect-reference&notes sexp)
      (let loop ((sexp sexp) (acc '()) (ref '()) (notes '()))
	(cond ((null? sexp) (values (reverse! acc) ref notes))
	      ((and (pair? (car sexp)) (keyword? (caar sexp)))
	       (case (caar sexp)
		 ((:header :blockquote :line :html-block :verbatim
		   :ordered-list :bullet-list :paragraph :plain)
		  (loop (cdr sexp) (cons (car sexp) acc) ref notes))
		 ((:reference)
		  (loop (cdr sexp) acc (cons (car sexp) ref) notes))
		 ((:note)
		  (loop (cdr sexp) acc ref (cons (car sexp) notes)))
		 (else
		  (assertion-violation 'markdown-sexp->sxml
				       "invalid keyword in the given list"
				       (caar sexp) sexp))))
	      (else
	       ;; invalid list
	       (assertion-violation 'markdown-sexp->sxml
				    "invalid markdown list" sexp)))))
    (define (alist->attr alist)
      (let loop ((alist alist) (r '()))
	(cond ((null? alist) (reverse! r))
	      ((and (string? (caar alist)) (string? (cdar alist)))
	       (loop (cdr alist) (cons (list (string->symbol (caar alist))
					     (cdar alist)) r)))
	      (else
	       (assertion-violation
		'markdown-sexp->sxml
		"invalid html attribute, must be alist of strings" alist)))))

    (unless (and (pair? sexp) (eq? (car sexp) :doc))
      (assertion-violation 'markdown-sexp->sxml
			   "invalid keyword in the given list"
			   (car sexp) sexp))
    (let-values (((sexp refs notes) (collect-reference&notes (cdr sexp))))
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
      (define note-prefix "fnref_")
      (define sup-prefix "fn_")
      (define note-refs '())
      (define (find-note ref)
	(let loop ((notes notes))
	  (if (null? notes)
	      '("")
	      (let ((note (car notes)))
		(if (string=? (cadr (cadr note)) ref)
		    (cddr note)
		    (loop (cdr notes)))))))

      (define (detail->sxml sexp)
	;; inlines
	;;  :eol
	;;  :link
	;;  :emph
	;;  :strong
	;;  :image
	;;  :link
	;;  :note-ref
	;; TODO attribute
	(let loop ((sexp sexp) (acc '()))
	  (define (gen-sup count ref note)
	    (define (strip note)
	      (let loop ((note note) (r '()))
		(if (null? note)
		    (string-concatenate (reverse! r))
		    (let ((e (car note)))
		      (cond ((string? e)
			     (loop (cdr note) (cons e r)))
			    ((eq? e :eol)
			     (loop (cdr note) (cons "\n" r)))
			    (else
			     (case (car e)
			       ((:label :code)
				(loop (cdr note) (cons (cadr e) r)))
			       ((:link) 
				(loop (cdr note) (cons (cadr (cadr e)) r)))
			       ((:emph :strong)
				(loop (cdr note) (cons (strip (cdr e)) r)))
			       ;; ignore
			       (else (loop (cdr note) r)))))))))

	    `(sub (@ (id ,(string-append note-prefix ref))) 
		  (a (@ (href ,(string-append "#" sup-prefix ref))
			(title ,(strip note)))
		     ,(number->string (+ count 1)))))
	  (match sexp
	    (() (reverse! acc))
	    ;; :label is sort of special we just need its content
	    (((:label str ...) . rest) 
	     (loop rest (cons (string-concatenate str) acc)))
	    ;; :image can be 2 pattern, one is with link
	    ;; otherone is just a label.
	    (((:image (:link (:label label ...) source title)) . rest)
	     (loop rest (cons `(img (@ (src ,(string-trim-both source))
				       (alt ,(string-concatenate label))
				       (title ,title)))
			      acc)))
	    (((:image (:label label ...)) . rest) 
	     (loop rest (cons (apply string-append "!" label) acc)))
	    (((:link (:label label ...) source title) . rest)
	     (loop rest (cons `(a (@ (href ,(string-trim-both source))
				     (title ,(if (string-null? title)
						 (string-concatenate label)
						 title)))
				  ,@label)
			      acc)))
	    (((:code code) . rest) (loop rest (cons `(code ,code) acc)))
	    (((:emph code maybe ...) . rest) 
	     (loop rest (cons `(em ,code ,@(detail->sxml maybe)) acc)))
	    (((:strong code maybe ...) . rest) 
	     (loop rest (cons `(strong ,code ,@(detail->sxml maybe)) acc)))
	    ((:eol . rest) (loop rest (cons "\n" acc)))
	    (((:item item ...) . rest) 
	     (loop rest (cons `(li ,(get-attribute :item) ,@(detail->sxml item))
			      acc)))
	    (((:note-ref ref) . rest)
	     (let ((count (length note-refs))
		   (note (find-note ref)))
	       (push! note-refs (cons ref count))
	       (loop rest (cons (gen-sup count ref note) acc))))
	    (((:note note ...) . rest)
	     (let ((count (length note-refs))
		   (ref (symbol->string (gensym))))
	       (push! note-refs (cons ref count))
	       (push! notes (cons ref note))
	       (loop rest (cons (gen-sup count ref note) acc))))
	    (((? string? s) . rest) 
	     ;; explicitly add newline
	     (loop rest (cons s acc)))
	    ;; HTML entity
	    ((('& entity) . rest) (loop rest (cons (car sexp) acc)))
	    (_ (assertion-violation 'markdown-sexp->sxml
				    "invalid inline sexp form" sexp)))))
      (define (gen-notes notes)
	(define (format-notes notes)
	  (map (lambda (note)
		 (cons (cadr (cadr note)) (cddr note))) notes))
	(define (order-notes notes)
	  (list-sort (lambda (note-a note-b) 
		       (let ((a (assoc (car note-a) note-refs))
			     (b (assoc (car note-b) note-refs)))
			 (< (cdr b) (cdr a))))
		     notes))
	(define (gen notes)
	  (fold (lambda (note acc)
		  (let ((ref (car note))
			(content (cdr note)))
		    (cons
		     `(li ,(append (get-attribute :note)
				   `((id ,(string-append sup-prefix ref))))
			  ,@(detail->sxml content)
			  (a (@ (href ,(string-append "#" note-prefix ref)))
			     "."))
		     acc)))
		'() notes))
	(if no-notes
	    '()
	    (let ((notes (gen (order-notes (format-notes notes)))))
	      (if (null? notes)
		  notes
		  `((ol (@ (id "notes"))
			,(get-attribute :notes) ,@notes))))))

      (define (gen-reference refs)
	(define (gen refs)
	  (fold (lambda (ref acc)
		  (match ref
		    ((:reference (:label label) source title)
		     (cons 
		      `(div ,(get-attribute :reference)
			    ,(string-append "[" label "]: " 
					    source " '" title "'"))
		      acc))
		    (_ acc)))
		'() refs))
	(if no-reference
	    '()
	    (let ((refs (gen refs)))
	      (if (null? refs)
		  '()
		  `((div ,(append (get-attribute :references)
				  '((id "references")))
			 ,@refs))))))

      ;; TODO refactor
      (define (rec sexp in-html?)
	(let loop ((sexp sexp) (acc '()))
	  ;; TODO add style
	  (match sexp
	    (() 
	     (if in-html?
		 (reverse! acc)
		 `(div (@ ,@(let ((s style) (c class))
				      (cond ((and s c)
					     `((style ,s) (class ,c)))
					    (s `((style ,s)))
					    (c `((class ,c)))
					    (else '()))))
		       ,@(reverse! acc)
		       ,@(gen-reference refs)
		       ,@(gen-notes notes))))
	    (((:header (? keyword? type) . content) . rest)
	     (loop (cdr sexp) (cons `(,(keyword->symbol type)
				      ,(let ((try (get-attribute type)))
					 (if (null? try)
					     (get-attribute :header)
					     try))
				      ,@content) acc)))
	    (((:blockquote . content) . rest)
	     (loop rest (cons `(blockquote ,(get-attribute :blockquote)
					   ,@(detail->sxml content))
			 acc)))
	    (((:line) . rest)
	     (loop (cdr sexp) (cons `(hr ,(get-attribute :line)) acc)))
	    (((:html-block content) . rest)
	    ;; assume it's SXML style
	     (loop rest (cons content acc)))
	    (((:paragraph . content) . rest)
	     (loop rest (cons `(p ,(get-attribute :paragraph)
				  ,@(detail->sxml content))
			      acc)))
	    ;; should we handle plain the same as paragraph?
	    (((:plain . content) . rest)
	     (loop rest (cons `(p ,(get-attribute :plain)
				  ,@(detail->sxml content))
			      acc)))
	    (((:bullet-list . items) . rest)
	     (loop rest 
		   (cons `(ul ,(get-attribute :bullet-list)
			      ,@(detail->sxml items))
			 acc)))
	    (((:ordered-list . items) . rest)
	     (loop rest (cons `(ol ,(get-attribute :ordered-list)
					 ,@(detail->sxml items))
				    acc)))
	    (((:verbatim . code) . rest)
	     ;; we need special treat for this
	     (loop rest (cons `(pre ,(get-attribute :verbatim) 
				    ,@(detail->sxml code))
				acc)))
	    ;; for block html's content
	    (((? string? text) . rest)
	     (loop rest (cons text acc)))
	    (_ (assertion-violation 'markdown-sexp->sxml
				    "invalid block sexp form" sexp))
	    )))
      (rec sexp #f)
      ))

  (define (markdown-sexp->string sexp . opts)
    (let ((sxml (apply markdown-sexp->sxml sexp opts)))
      (srl:sxml->html sxml)))

  )
