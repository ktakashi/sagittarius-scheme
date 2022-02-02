;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/factories.scm - Parser factories
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
(library (text markdown parser factories)
    (export block-start?
	    block-start:of block-start:none
	    block-start:at-index
	    block-start:at-column
	    block-start:replace-active-block-parser
	    block-start-new-index
	    block-start-new-column
	    block-start-replace-active-block-parser?

	    try-start-heading)
    (import (rnrs)
	    (core misc)
	    (srfi :13 strings)
	    (srfi :197 pipeline)
	    (text markdown parser parsing)
	    (text markdown parser source)
	    (text markdown parser scanner)
	    (text markdown parser blocks))
(define-vector-type block-start 
  (make-block-start parsers new-index new-column replace-active-block-parser?)
  block-start?
  (parsers    block-start-parsers)
  (new-index  block-start-new-index block-start-new-index-set!)
  (new-column block-start-new-column block-start-new-column-set!)
  (replace-active-block-parser? block-start-replace-active-block-parser?
				block-start-replace-active-block-parser?-set!))

(define (block-start:none) #f)
(define (block-start:of parsers ...)
  (make-block-start parsers -1 -1 #f))
(define (block-start:at-index bs new-index)
  (block-start-new-index-set! bs new-index)
  bs)
(define (block-start:at-column bs new-column)
  (block-start-new-column-set! bs new-column)
  bs)
(define (block-start:replace-active-block-parser bs)
  (block-start-replace-active-block-parser?-set! bs #t)
  bs)

(define (try-start-heading parser-state matched-block-parser)
  (if (>= (parser-state-index parser-state) +parsing-code-block-indent+)
      (block-start:none)
      (let* ((line (parser-state-line parser-state))
	     (nns (parser-state-next-non-space-index parser-state)))
	(cond ((and (eqv? (source-line:char-at line nns) #\#)
		    (atx-heading parser-state (source-line:substring line nns)))
	       => (lambda (p)
		    (chain (block-start:of p)
			   (block-start:at-index _ (source-line:length line)))))
	      ((setex-heading-level parser-state line nns) =>
	       (lambda (level)
		 (let ((lines (matched-block-parser:paragraph-lines
			       matched-block-parser)))
		   (if (source-lines:empty? lines)
		       (block-start:none)
		       (chain (block-start:of
			       (make-heading-parser
				(parser-state-document parser-state)
				level lines))
			      (block-start:at-index _ (source-line:length line))
			      (block-start:replace-active-block-parser _))))))
	      (else (block-start:none))))))
(define (atx-heading parser-state line)
  (define scanner (scanner:of (source-lines:of line)))
  (let ((level (scanner:match-char scanner #\#)))
    (cond ((or (zero? level) (> level 6)) #f)
	  ((not (scanner:has-next? scanner))
	   (make-heading-parser (parser-state-document parser-state)
				level (source-lines:empty)))
	  ((not (memv (scanner:peek scanner) '(#\space #\tab))) #f)
	  (else
	   (scanner:whitespace scanner) ;; skip
	   (let ((start (scanner:position scanner)))
	     (let loop ((end start) (hash-can-end? #t))
	       (if (scanner:has-next? scanner)
		   (let ((c (scanner:peek scanner)))
		     (case c
		       ((#\#)
			(cond (hash-can-end?
			       (scanner:match-char scanner #\#)
			       (let ((ws (scanner:whitespace scanner)))
				 (loop (if (scanner:has-next? scanner)
					   (scanner:position scanner)
					   end)
				       (positive? ws))))
			      (else
			       (scanner:next! scanner)
			       (loop (scanner:position scanner)
				     hash-can-end?))))
		       ((#\space #\tab)
			(scanner:next! scanner)
			(loop end #t))
		       (else
			(scanner:next! scanner)
			(loop (scanner:position scanner) #f))))
		   (let* ((source (scanner:source scanner start end))
			  (content (source-lines:content source)))
		     (if (string-null? content)
			 (make-heading-parser
			  (parser-state-document parser-state)
			  level (source-lines:empty))
			 (make-heading-parser
			  (parser-state-document parser-state)
			  level source))))))))))
(define (setex-heading-level parser-state sl next-non-space-index) )

)
