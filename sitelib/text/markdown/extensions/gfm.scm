;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/extensions/gfm.scm - Github flavoured markdown
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
(library (text markdown extensions gfm)
    (export gfm-extensions
	    (rename (strikethrough-extension gfm-strikethrough-extension)
		    (table-extensions gfm-table-extensions))
	    gfm-markdown-converter)
    (import (rnrs)
	    (srfi :1 lists)
	    (srfi :117 list-queues)
	    (srfi :158 generators-and-accumulators)
	    (srfi :197 pipeline)
	    (text markdown extensions api)
	    (text markdown parser blocks)
	    (text markdown parser factories)
	    (text markdown parser inlines)
	    (text markdown parser nodes)
	    (text markdown parser parsing)
	    (text markdown parser source)
	    (util flexible-vector))

(define *gfm-namespace* "https://github.github.com/gfm/")

(define-markdown-node strikethrough (namespace *gfm-namespace*)
  (element "gfm:strikethrough"))

(define-record-type strikethrough-delimiter-processor
  (parent <delimiter-processor>)
  (protocol (lambda (n)
	      (lambda ()
		((n #\~ #\~ 2
		    strikethrough-delimiter-processor:process))))))
(define (strikethrough-delimiter-processor:process dp opening closing)
  (if (and (>= (delimiter:length opening) 2) (>= (delimiter:length closing) 2))
      (let* ((opener (delimiter:opener opening))
	     (closer (delimiter:closer closing))
	     (strike (make-strikethrough-node opener))
	     (loc*   (source-locations:empty)))
	(source-locations:add-all! loc*
	 (map markdown-node:source-locations (delimiter:opener* opening 2)))
	(generator-for-each (lambda (n)
			      (let ((loc (markdown-node:source-locations n)))
				(markdown-node:append-child! strike n)
				(source-locations:add! loc* loc)))
			    (markdown-node-between opener closer))
	(source-locations:add-all! loc*
	 (map markdown-node:source-locations (delimiter:closer* closing 2)))
	(markdown-node:source-locations-set! strike
	 (source-locations:locations loc*))
	(markdown-node:insert-after! opener strike)
	2)
      0))

(define strikethrough-extension
  (markdown-extension-builder
   (custom-delimiter-processors `(,make-strikethrough-delimiter-processor))))

;; Table
(define-markdown-node table-block (namespace *gfm-namespace*)
  (element "gfm:table"))
(define-markdown-node table-head (namespace *gfm-namespace*)
  (element "gfm:thead"))
(define-markdown-node table-body (namespace *gfm-namespace*)
  (element "gfm:tbody"))
(define-markdown-node table-row (namespace *gfm-namespace*)
  (element "gfm:trow"))
(define-markdown-node (table-cell header? (attribute alignment "gfm:alignment"))
  (namespace *gfm-namespace*)
  (element "gfm:tcell"))

(define-record-type table-block-parser
  (parent <block-parser>)
  (fields row-lines
	  columns)
  (protocol
   (lambda (n)
     (lambda (document columns header-line)
       ((n (make-table-block-node document) #f #t (lambda ignore #f)
	   (lambda (self ps)
	     (if (source-line:index (parser-state-line ps) #\|)
		 (block-continue:at-index (parser-state-index ps))
		 (block-continue:none)))
	   (lambda (self line)
	     (define row-lines (table-block-parser-row-lines self))
	     (flexible-vector-insert-back! row-lines line))
	   block-parser-default-add-location!
	   block-parser-default-close-block!
	   table-block-parse-inlines!)
	(flexible-vector header-line)
	columns)))))

(define (table-block-parse-inlines! parser inline-parser)
  (define block (block-parser-block parser))
  (define row-lines (table-block-parser-row-lines parser))
  (define columns (table-block-parser-columns parser))
  (define locs (list->vector
		(list-queue-list (markdown-node:source-locations block))))
  (define locs-len (vector-length locs))
  (define (parse-cell parent cell column header? inline-parser)
    (define alignment (cond ((and (< column (vector-length columns))
				  (vector-ref columns column))
			     => symbol->string)
			    (else #f)))
    (define table-cell (make-table-cell-node parent header? alignment))
    (define start (source-line:index cell non-space/tab?))
    (define end (source-line:index-right cell non-space/tab? start))
    (inline-parser:parse! inline-parser
			  (source-lines:of
			   (source-line:substring cell start (+ end 1)))
			  table-cell)
    table-cell)
  (define (add-header!)
    (define head (make-table-head-node block))
    (unless (zero? locs-len)
      (markdown-node:add-source-location! head (vector-ref locs 0)))
    (markdown-node:append-child! block head)
    (let ((row  (make-table-row-node head))
	  (header-cells (split-source-line (flexible-vector-ref row-lines 0))))
      (markdown-node:source-locations-set! row
       (markdown-node:source-locations head))
      (markdown-node:append-child! head row)
      (do ((len (length header-cells))
	   (i 0 (+ i 1))
	   (cells header-cells (cdr cells)))
	  ((null? cells) len)
	(let ((cell (parse-cell row (car cells) i #t inline-parser)))
	  (markdown-node:append-child! row cell)))))
  (let ((header-columns (add-header!)))
    (when (>= (flexible-vector-size row-lines) 2)
      (do ((body (make-table-body-node block))
	   (index 2 (+ index 1)))
	  ((= index (flexible-vector-size row-lines))
	   (markdown-node:append-child! block body))
	(let* ((row-line (flexible-vector-ref row-lines index))
	       (loc (and (< index locs-len) (vector-ref locs index)))
	       (row (make-table-row-node body)))
	  (markdown-node:append-child! body row)
	  (when loc
	    (markdown-node:add-source-location! row loc)
	    (markdown-node:add-source-location! body loc))
	  (do ((i 0 (+ i 1))
	       (cells (split-source-line row-line) (cdr cells)))
	      ((= i header-columns))
	    (let* ((cell (if (null? cells)
			     (source-line:of "" #f)
			     (car cells)))
		   (tcell (parse-cell row cell i #f inline-parser)))
	      (markdown-node:append-child! row tcell))))))))

(define (split-source-line line)  
  (define nns (source-line:index line non-space/tab?))
  (define (get-last-nns line start end)
    (cond ((source-line:index-right line non-space/tab? end start))	     
	  (else (- end 1))))
  (let-values (((cell-start cell-end)
		(if (eqv? (source-line:char-at line nns) #\|)
		    (values (+ nns 1)
			    (+ (get-last-nns line 
					     (source-line:length line)
					     (+ nns 2)) 1)))))
    (let loop ((r '()) (s cell-start) (i cell-start))
      (cond ((= i cell-end)
	     (if (= i s)
		 (reverse! r)
		 (reverse! (cons (source-line:substring line s s) r))))
	    (else
	     (case (source-line:char-at line i)
	       ((#\\)
		(if (and (< (+ i 1) cell-end)
			 (eqv? #\| (source-line:char-at line (+ i 1))))
		    (loop r s (+ i 2))
		    (loop r s (+ i 1))))
	       ((#\|)
		(loop (cons (source-line:substring line s i) r)
		      (+ i 1) (+ i 1)))
	       (else (loop r s (+ i 1)))))))))

(define (non-space/tab? c) (not (parsing:space/tab? c)))

(define (try-start-gfm-table-block ps mbp)
  (define paragraph-lines (matched-block-parser:paragraph-lines mbp))
  (define (parse-separator line)
    (define len (source-line:length line))
    
    (define (check-dash i line len)
      (let loop ((i i) (have-dash? #f))
	(cond ((= i len) (values i have-dash?))
	      ((eqv? (source-line:char-at line i) #\-) (loop (+ i 1) #t))
	      (else (values i have-dash?)))))
    (define (check-right i line len)
      (if (and (< i len) (eqv? (source-line:char-at line i) #\:))
	  (values (+ i 1) #t)
	  (values i #f)))
    (define (get-alignment left? right?)
      (cond ((and left? right?) 'center)
	    (left? 'left)
	    (right? 'right)
	    (else #f)))
    (let loop ((i 0) (valid? #f) (pipes 0) (columns '()))
      (if (= i len)
	  (and valid? (not (null? columns)) (list->vector (reverse! columns)))
	  (let ((c (source-line:char-at line i)))
	    (case c
	      ((#\|)
	       (let ((new-pipe (+ pipes 1)))
		 (and (<= new-pipe 1) ;; More than one adjacent pipe not allowed
		      (loop (+ i 1) #t new-pipe columns))))
	      ((#\- #\:)
	       (and (or (> pipes 0) (null? columns))
		    (let*-values (((i left?) (if (eqv? c #\:)
						 (values (+ i 1) #t)
						 (values i #f)))
				  ((i have-dash?) (check-dash i line len)))
		      (and have-dash?
			   (let-values (((i right?) (check-right i line len)))
			     (let ((align (get-alignment left? right?)))
			       (loop i valid? 0 (cons align columns))))))))
	      ((#\space #\tab) (loop (+ i 1) valid? pipes columns))
	      (else #f))))))
  (define (check ps mbp)
    (let* ((line (parser-state-line ps))
	   (separator (source-line:substring line (parser-state-index ps))))
      (cond ((parse-separator separator) =>
	     (lambda (columns)
	       (let* ((paragraph (source-lines:first paragraph-lines))
		      (header-cells (split-source-line paragraph))
		      (doc (parser-state-document ps)))
		 (and (>= (vector-length columns) (length header-cells))
		      (chain (block-start:of
			      (make-table-block-parser doc columns paragraph))
			     (block-start:at-index _ (parser-state-index ps))
			     (block-start:replace-active-block-parser _))))))
	    (else #f))))
  (if (and (= (source-lines:size paragraph-lines) 1)
	   (source-line:index (source-lines:first paragraph-lines) #\|))
      (cond ((check ps mbp))
	    (else (block-start:none)))
      (block-start:none)))

(define table-extensions
  (markdown-extension-builder
   (custom-block-factories `(,try-start-gfm-table-block))))

(define gfm-extensions
  (combine-markdown-extensions strikethrough-extension table-extensions))

;; Converters
(define (convert-html-strikethrough node data next)
  ;; TODO
  '())
(define-markdown-converter gfm->html-converter html
  (strikethrough-node? convert-html-strikethrough))

(define (convert-sexp-strikethrough node data next)
  `(:strike ,@(map next (markdown-node:children node))))
(define-markdown-converter gfm->sexp-converter sexp
  (strikethrough-node? convert-sexp-strikethrough)
  )

(define gfm-markdown-converter
  (markdown-converter:merge gfm->html-converter gfm->sexp-converter))

)
