;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/inlines.scm - Inline parsers
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
(library (text markdown parser inlines)
    (export inline-parser:parse!

	    make-inline-parser-context inline-parser-context?
	    
	    make-inline-parser inline-parser?

	    inline-parser-state?
	    inline-parser-state-scanner
	    inline-parser-state-block)
    (import (rnrs)
	    (core misc)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :117 list-queues)
	    (text markdown parser inlines contents)
	    (text markdown parser escaping)
	    (text markdown parser link-reference)
	    (text markdown parser nodes)
	    (text markdown parser parsing)
	    (text markdown parser scanner)
	    (text markdown parser source))

(define-record-type parsing-state
  (parent <inline-parser-state>)
  (fields include-source-locations?
	  (mutable trailing-spaces)
	  (mutable last-delimiter)
	  (mutable last-bracket)))

(define-vector-type inline-parser-context
  (make-inline-parser-context delimiter-processors definitions)
  inline-parser-context?
  (delimiter-processors inline-parser-context-processors)
  (definitions inline-parser-context-definitions))

(define (inline-parser-context:get-deffinition context label)
  (link-reference-definitions:get (inline-parser-context-definitions context)
				  label))

(define-record-type inline-parser
  (fields (mutable parsing-state)
	  context
	  processors ;; delimiter processors
	  parsers    ;; inline parsers
	  )
  (protocol (lambda (p)
	      (lambda (context)
		(let ((parsers (make-eqv-hashtable)))
		  ;; TODO setup inline parsers
		  (hashtable-set! parsers #\\ (list try-parse-backslash))
		  (hashtable-set! parsers #\` (list try-parse-backticks))
		  (hashtable-set! parsers #\& (list try-parse-entity))
		  (p #f
		     context
		     (calculate-delimiter-processors
		      (inline-parser-context-processors context))
		     parsers))))))

(define (calculate-delimiter-processors processors)
  (let ((processors (make-eqv-hashtable (length processors))))
    ;; TODO
    processors))

(define (inline-parser:parse! inline-parser source-lines block)
  (define scanner (scanner:of source-lines))
  (define locs (source-lines:source-loactions source-lines))
  (define state (make-parsing-state block scanner (not (null? locs))
				    0 #f #f))
  (inline-parser-parsing-state-set! inline-parser state)
  (let loop ()
    (cond ((inline-parser:parse-inline! inline-parser) =>
	   (lambda (nodes)
	     (for-each (lambda (node)
			 (markdown-node:append-child! block node))
		       nodes)
	     (loop)))))
  (inline-parser:process-delimiters! inline-parser #f)
  (inline-parser:merge-text-nodes! inline-parser block))

(define (inline-parser:parse-inline! inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define parsers (inline-parser-parsers inline-parser))
  (define processors (inline-parser-processors inline-parser))

  (define (parser-match parsers)
    (define pos (scanner:position scanner))
    (let loop ((p* parsers))
      (cond ((null? p*) #f)
	    (((car p*) state) =>
	     (lambda (parsed-inline)
	       (let ((node (parsed-inline-node parsed-inline)))
		 (scanner:position! scanner
				    (parsed-inline-position parsed-inline))
		 (let ((source (markdown-node:source-locations node)))
		   (unless (list-queue-empty? source)
		     (markdown-node:source-locations-set! node source)))
		 (list node))))
	    (else
	     (scanner:position! scanner pos)
	     (loop (cdr p*))))))
  (define (processor-match processors)
    #f)
  (let ((c (scanner:peek scanner)))
    (case c
      ((#\[) (list (inline-parser:parse-open-blacket inline-parser)))
      ((#\!) (list (inline-parser:parse-bang inline-parser)))
      ((#\]) (list (inline-parser:parse-close-blacket inline-parser)))
      ((#\newline) (list (inline-parser:parse-line-break inline-parser)))
      (else
       (and c
	    (or (cond ((hashtable-ref parsers c #f) => parser-match)
		      (else #f))
		(cond ((hashtable-ref processors c #f) => processor-match)
		      (else #f))
		(list (inline-parser:parse-text inline-parser))))))))

(define (inline-parser:text inline-parser source-lines)
  (define state (inline-parser-parsing-state inline-parser))
  (define block (inline-parser-state-block state))
  (let ((t (make-text-node block (source-lines:content source-lines))))
    (markdown-node:source-locations-set! t
     (source-lines:source-loactions source-lines))
    t))
(define (inline-parser:add-bracket! inline-parser braket)
  (define state (inline-parser-parsing-state inline-parser))
  (define last-braket (parsing-state-last-bracket state))
  (when last-braket
    (bracket-bracket-after?-set! last-braket #t))
  (parsing-state-last-bracket-set! state braket))

(define (inline-parser:remove-last-bracket! inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define last-braket (parsing-state-last-bracket state))
  (parsing-state-last-bracket-set! state (bracket-previous last-braket)))

(define (inline-parser:parse-open-blacket inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define start (scanner:position scanner))
  (scanner:next! scanner)
  (let* ((p (scanner:position scanner))
	 (source (scanner:source scanner start p))
	 (t (inline-parser:text inline-parser source)))
    (inline-parser:add-bracket! inline-parser
     (bracket:link t start p
		   (parsing-state-last-bracket state)
		   (parsing-state-last-delimiter state)))
    t))

(define (inline-parser:parse-bang inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define start (scanner:position scanner))
  (scanner:next! scanner)
  (if (scanner:next-char? scanner  #\[)
      (let* ((p (scanner:position scanner))
	     (source (scanner:source scanner start p))
	     (t (inline-parser:text inline-parser source)))
	(inline-parser:add-bracket! inline-parser
	 (bracket:link t start p
		       (parsing-state-last-bracket state)
		       (parsing-state-last-delimiter state)))
	t)
      (let* ((p (scanner:position scanner))
	     (source (scanner:source scanner start p)))
	(inline-parser:text inline-parser source))))

(define (inline-parser:parse-close-blacket inline-parser)
  (define (check-inline-link scanner after-close)
    (define (parse-link-destination scanner)
      (define delim (scanner:peek scanner))
      (define start (scanner:position scanner))
      (define (parse-destination scanner delim start)
	(let ((dest (source-lines:content
		     (scanner:source scanner start
				     (scanner:position scanner)))))
	  (if (eqv? delim #\<)
	      (substring dest 1 (- (string-length dest) 1))
	      dest)))
      (and (link-scanner:scan-link-destination! scanner)
	   (let ((dest (parse-destination scanner delim start)))
	     (escaping:unescape dest))))
    (define (parse-link-title scanner)
      (define start (scanner:position scanner))
      (and (link-scanner:scan-link-title! scanner)
	   (let ((title (source-lines:content
			 (scanner:source scanner start
					 (scanner:position scanner)))))
	     (escaping:unescape (substring title 1
					   (- (string-length title) 1))))))
    (define (finish scanner dest title)
      (cond ((not (scanner:next-char? scanner #\)))
	     (scanner:position! scanner after-close)
	     (values #f #f))
	    (else (values dest title))))
    (cond ((scanner:next-char? scanner #\()
	   (scanner:whitespace scanner)
	   (cond ((parse-link-destination scanner) =>
		  (lambda (dest)
		    (if (>= (scanner:whitespace scanner) 1)
			(let ((title (parse-link-title scanner)))
			  (scanner:whitespace scanner)
			  (finish scanner dest title))
			(finish scanner dest #f))))
		 (else
		  (scanner:position! scanner after-close)
		  (values #f #f))))
	  (else (values #f #f))))

  (define (check-label scanner opener after-close dest title)
    (define context (inline-parser-context inline-parser))
    (define (parse-link-label scanner)
      (and-let* (( (scanner:next-char? scanner #\[) )
		 (start (scanner:position scanner))
		 ( (link-scanner:scan-link-label-content! scanner) )
		 (end (scanner:position scanner))
		 ( (scanner:next-char? scanner #\]) )
		 (content (source-lines:content
			   (scanner:source scanner start end)))
		 ;; at most 999 characters, so less than 1000
		 ( (< (string-length content) 1000) ))
	content))
    (if (not dest)
	(let ((ref (parse-link-label scanner)))
	  (unless ref (scanner:position! scanner after-close))
	  (let ((ref (if (and (or (not ref) (zero? (string-length ref)))
			      (not (bracket-bracket-after? opener)))
			 (source-lines:content
			  (scanner:source scanner
					  (bracket-content-position opener)
					  before-close))
			 ref)))
	    (cond ((and ref
			(inline-parser-context:get-deffinition context ref)) =>
		   (lambda (destination)
		     (values (link-reference-definition-destination destination)
			     (link-reference-definition-title destination))))
		  (else (values dest title)))))
	(values dest title)))
  
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define before-close (scanner:position scanner))

  (scanner:next! scanner)
  (let ((after-close (scanner:position scanner))
	(opener (parsing-state-last-bracket state)))
    (cond ((not opener)
	   (inline-parser:text inline-parser
	     (scanner:source scanner before-close after-close)))
	  ((not (bracket-allowed? opener))
	   (inline-parser:remove-last-bracket! inline-parser)
	   (inline-parser:text inline-parser
	     (scanner:source scanner before-close after-close)))
	  (else
	   (let*-values (((dest title) (check-inline-link scanner after-close))
			 ((dest title)
			  (check-label scanner opener after-close dest title)))
	     (if dest
		 (let ((link/image ((if (bracket-image? opener)
					make-image-node
					make-link-node)
				    (inline-parser-state-block state)
				    dest title)))
		   (do ((node (markdown-node-next (bracket-node opener))
			      (markdown-node-next node)))
		       ((not node))
		     (markdown-node:append-child! link/image node))
		   (markdown-node:source-locations-set! link/image
		    (source-lines:source-loactions
		     (scanner:source scanner 
				     (bracket-mark-position opener)
				     (scanner:position scanner))))
		   (inline-parser:process-delimiters! inline-parser
		    (bracket-previous-delimiter opener))
		   (inline-parser:merge-text-nodes! inline-parser link/image)
		   (markdown-node:unlink! (bracket-node opener))
		   (inline-parser:remove-last-bracket! inline-parser)
		   (unless (bracket-image? opener)
		     (do ((bracket (parsing-state-last-bracket state)
				   (bracket-previous bracket)))
			 ((not bracket))
		       (unless (bracket-image? bracket)
			 (bracket-allowed?-set! bracket #f))))
		   link/image)
		 (begin
		   (inline-parser:remove-last-bracket! inline-parser)
		   (scanner:position! scanner after-close)
		   (inline-parser:text inline-parser
		    (scanner:source scanner before-close after-close)))))))))

(define (inline-parser:parse-line-break inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (scanner:next! scanner)
  (if (>= (parsing-state-trailing-spaces state) 2)
      (make-linebreak-node (inline-parser-state-block state))
      (make-softbreak-node (inline-parser-state-block state))))

(define (inline-parser:parse-text inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define start (scanner:position scanner))
  (define (scan-until-special-char inline-parser scanner)
    (let loop ((c (scanner:peek scanner)))
      (if (or (not c) (inline-parser:special-char? inline-parser c))
	  c
	  (begin
	    (scanner:next! scanner)
	    (loop (scanner:peek scanner))))))
  (define (get-content c source)
    (let ((content (source-lines:content source)))
      (cond ((eqv? c #\newline)
	     ;; only space
	     (let ((r (string-trim-right content #\space)))
	       (parsing-state-trailing-spaces-set! state
						   (- (string-length content)
						      (string-length r)))
	       r))
	    ((not c)
	     ;; for the last line, both tabs and spaces are trimmed
	     (string-trim-right content parsing:space/tab?))
	    (else content))))
  (scanner:next! scanner)
  (let* ((c (scan-until-special-char inline-parser scanner))
	 (source (scanner:source scanner start (scanner:position scanner)))
	 (text (make-text-node (inline-parser-state-block state)
			       (get-content c source))))
    (markdown-node:source-locations-set! text
     (source-lines:source-loactions source))))

(define (inline-parser:process-delimiters! inline-parser stack-bottom)
  )

(define (inline-parser:merge-text-nodes! inline-parser block)
  )

(define (inline-parser:special-char? inline-parser c)
  (define parsers (inline-parser-parsers inline-parser))
  (define processors (inline-parser-processors inline-parser))
  (or (memv c '(#\[ #\] #\! #\newline))
      (hashtable-ref parsers c #f)
      (hashtable-ref processors c #f)))

;; bracket
(define-vector-type bracket
  (make-bracket node mark-position content-position previous
		previous-delimiter image? allowed? barcket-after?)
  bracket?
  (node bracket-node)
  (mark-position bracket-mark-position)
  (content-position bracket-content-position)
  (previous bracket-previous)
  (previous-delimiter bracket-previous-delimiter)
  (image? bracket-image?)
  (allowed? bracket-allowed? bracket-allowed?-set!)
  (barcket-after? bracket-bracket-after? bracket-bracket-after?-set!))

(define (bracket:link node mark-position content-position previous
		      previous-delimiter)
  (make-bracket node mark-position content-position
		previous previous-delimiter #f #t #f))
(define (bracket:image node mark-position content-position previous
		       previous-delimiter)
  (make-bracket node mark-position content-position
		previous previous-delimiter #t #t #f))

)
