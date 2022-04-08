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
		    (table-extension gfm-table-extension)
		    (task-list-extension gfm-task-list-extension))
	    gfm-markdown-converter)
    (import (rnrs)
	    (srfi :1 lists)
	    (srfi :115 regexp)
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
	    (text markdown parser post-processor)
	    (text markdown converter html)
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
   (delimiter-processors `(,make-strikethrough-delimiter-processor))))

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
    (define start (or (source-line:index cell non-space/tab?)
		      (source-line:length cell)))
    (define end (or (source-line:index-right cell non-space/tab? start)
		    (- start 1)))
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
    (when (> (flexible-vector-size row-lines) 2)
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
	       (cells (split-source-line row-line)
		      (if (null? cells)
			  cells
			  (cdr cells))))
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
					     (+ nns 2)) 1))
		    (values nns (source-line:length line))))
	       ((out e) (open-string-output-port)))
    (define (->sl s i)
      (source-line:of s (source-line-location
			 (source-line:substring line cell-start i))))
    (let loop ((r '()) (s cell-start) (i cell-start))
      (cond ((= i cell-end)
	     (if (= i s)
		 (reverse! r)
		 (reverse! (cons (->sl (e) i) r))))
	    (else
	     (case (source-line:char-at line i)
	       ((#\\)
		(if (and (< (+ i 1) cell-end)
			 (eqv? #\| (source-line:char-at line (+ i 1))))
		    (begin (put-char out #\|) (loop r s (+ i 2)))
		    (begin (put-char out #\\) (loop r s (+ i 1)))))
	       ((#\|)
		(loop (cons (->sl (e) i) r) (+ i 1) (+ i 1)))
	       (else => (lambda (c)
			  (put-char out c)
			  (loop r s (+ i 1))))))))))

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

(define table-extension
  (markdown-extension-builder
   (block-factories `(,try-start-gfm-table-block))))

;;; Task list
(define-markdown-node (task-list-item-marker (attribute checked "gfm:checked"))
  (namespace *gfm-namespace*)
  (element "gfm:task-marker"))
(define *task-list-item*
  (rx bol #\[ ($ (or #\x #\X space)) #\] (+ space) ($ (* any))))
(define (task-list-processor node visit-children)
  ;; it's item-node
  (define child (markdown-node:first-child node))
  (when (paragraph-node? child)
    (let ((n (markdown-node:first-child child)))
      (cond ((and (text-node? n)
		  (regexp-matches *task-list-item* (text-node:content n)))
	     => (lambda (m)
		  (let ((marker
			 (make-task-list-item-marker-node node
			  (if (member (regexp-match-submatch m 1) '("x" "X"))
			      "true"
			      "false")))
			(new-content (regexp-match-submatch m 2)))
		    (markdown-node:prepend-child! node marker)
		    (text-node:content-set! n new-content)))))))
  (visit-children node))
(define task-list-post-processor
  (make-post-processor
   (make-post-processor-spec item-node? task-list-processor)))

(define task-list-extension
  (markdown-extension-builder
   (post-processors (list task-list-post-processor))))
		     

(define gfm-extensions
  (combine-markdown-extensions strikethrough-extension
			       table-extension
			       task-list-extension))

;; Converters
(define (convert-html-strikethrough node data next)
  `((del (@ ,@(html-attribute data node 'del))
	 ,@(convert-nodes next (markdown-node:children node)))))
(define (convert-task-marker node data next)
  `((input (@ ,@(if (equal? "true" (task-list-item-marker-node-checked node))
		    '((checked ""))
		    '())
	      (disabled "")
	      (type "checkbox")
	      ,@(html-attribute data node 'input)))
    " "))

(define (convert-table-block node data next)
  `("\n"
    (table (@ ,@(html-attribute data node 'table))
	   ,@(convert-nodes next (markdown-node:children node)))
    "\n"))
(define (convert-table-head node data next)
  `("\n"
    (thead (@ ,@(html-attribute data node 'thead))
	   ,@(convert-nodes next (markdown-node:children node)))
    "\n"))
(define (convert-table-body node data next)
  `("\n"
    (tbody (@ ,@(html-attribute data node 'tbody))
	   ,@(convert-nodes next (markdown-node:children node)))
    "\n"))
(define (convert-table-row node data next)
  `("\n"
    (tr (@ ,@(html-attribute data node 'tr))
	,@(convert-nodes next (markdown-node:children node)))
    "\n"))
(define (convert-table-cell node data next)
  (define tag (if (table-cell-node-header? node) 'th 'td))
  (define align (cond ((table-cell-node-alignment node) =>
		       (lambda (v) `((align ,v))))
		      (else '())))
  `("\n"
    (,tag (@ ,@align
	     ,@(html-attribute data node tag))
	  ,@(convert-nodes next (markdown-node:children node)))
    "\n"))


(define-markdown-converter gfm->html-converter html
  (strikethrough-node? convert-html-strikethrough)
  (task-list-item-marker-node? convert-task-marker)
  (table-block-node? convert-table-block)
  (table-head-node? convert-table-head)
  (table-body-node? convert-table-body)
  (table-row-node? convert-table-row)
  (table-cell-node? convert-table-cell))

(define (convert-sexp-strikethrough node data next)
  `(:strike ,@(map next (markdown-node:children node))))
(define-markdown-converter gfm->sexp-converter sexp
  (strikethrough-node? convert-sexp-strikethrough)
  )

(define gfm-markdown-converter
  (markdown-converter:merge gfm->html-converter gfm->sexp-converter))

)
