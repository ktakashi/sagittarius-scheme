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
	    strikethrough-extension
	    )
    (import (rnrs)
	    (srfi :158 generators-and-accumulators)
	    (srfi :197 pipeline)
	    (text markdown extensions api)
	    (text markdown parser blocks)
	    (text markdown parser factories)
	    (text markdown parser inlines)
	    (text markdown parser nodes)
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
	   (lambda (self inline)
	     ;; TODO
	     ))
	(flexible-vector header-line)
	columns)))))

(define (split-source-line paragraph) '())

(define (try-start-gfm-table-block ps mbp)
  (define paragraph-lines (matched-block-parser:paragraph-lines mbp))
  (define (parse-separator line)
    (define len (source-line:length line))
    (define columns (flexible-vector))
    
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
    (let loop ((i 0) (valid? #f) (pipes 0))
      (if (= i len)
	  (and valid? (not (flexible-vector-empty? columns)) columns)
	  (let ((c (source-line:char-at line i)))
	    (case c
	      ((#\|)
	       (let ((new-pipe (+ pipes 1)))
		 (and (<= new-pipe 1) ;; More than one adjacent pipe not allowed
		      (loop (+ i 1) #t new-pipe))))
	      ((#\- #\:)
	       (and (or (> pipes 0) (flexible-vector-empty? columns))
		    (let*-values (((i left?) (if (eqv? c #\:)
						 (values (+ i 1) #t)
						 (values i #f)))
				  ((i have-dash?) (check-dash i line len)))
		      (and have-dash?
			   (let-values (((i right?) (check-right i line len)))
			     (let ((align (get-alignment left? right?)))
			       (flexible-vector-insert-back! columns align)
			       (loop i valid? 0)))))))
	      ((#\space #\tab) (loop (+ i 1) valid? pipes))
	      (else #f))))))
  (define (check ps mbp)
    (let* ((line (parser-state-line ps))
	   (separator (source-line:substring line (parser-state-index ps))))
      (cond ((parse-separator separator) =>
	     (lambda (columns)
	       (let* ((paragraph (source-lines:first paragraph-lines))
		      (header-cells (split-source-line paragraph))
		      (doc (parser-state-document ps)))
		 (display (flexible-vector->list columns)) (newline)
		 (and (>= (flexible-vector-size columns) (length header-cells))
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

)
