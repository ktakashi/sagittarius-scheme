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
	    inline-parser-state-scanner
	    make-inline-parser inline-parser?)
    (import (rnrs)
	    (core misc)
	    (srfi :13 strings)
	    (text markdown parser nodes)
	    (text markdown parser scanner)
	    (text markdown parser source))

(define-record-type inline-parser-state
  (fields block
	  (mutable scanner)))
(define-record-type parsing-state
  (parent inline-parser-state)
  (fields include-source-locations?
	  (mutable trailing-spaces)
	  (mutable last-delimiter)
	  (mutable last-braket)))

(define-vector-type inline-parser-context
  (make-inline-parser-context delimiter-processors definitions)
  inline-parser-context?
  (delimiter-processors inline-parser-context-processors)
  (definitions inline-parser-context-definitions))

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
  (inline-parser:process-delimiters inline-parser #f)
  (inline-parser:merge-text-nodes! inline-parser block))

(define (inline-parser:parse-inline! inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define parsers (inline-parser-parsers inline-parser))
  (define processors (inline-parser-processors inline-parser))
  (let ((c (scanner:peek scanner)))
    (case c
      ((#\[) (list (inline-parser:parse-open-blacket inline-parser)))
      ((#\!) (list (inline-parser:parse-bang inline-parser)))
      ((#\]) (list (inline-parser:parse-close-blacket inline-parser)))
      ((#\newline) (list (inline-parser:parse-line-break inline-parser)))
      (else
       (cond ((not c) #f)
	     ((hashtable-ref parsers c #f) =>
	      (lambda (parser)
		;; DO IT
		))
	     ((hashtable-ref processors c #f) =>
	      (lambda (processor)
		;; DO IT
		))
	     (else (list (inline-parser:parse-text inline-parser))))))))

(define (inline-parser:parse-open-blacket inline-parser) )
(define (inline-parser:parse-bang inline-parser))
(define (inline-parser:parse-close-blacket inline-parser))
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
	     (string-trim-right content (lambda (c) (memv c '(#\space #\tab)))))
	    (else content))))
  (let* ((start (scanner:position scanner))
	 (c (scan-until-special-char inline-parser scanner))
	 (source (scanner:source scanner start (scanner:position scanner)))
	 (text (text-node (inline-parser-state-block state)
			  (get-content c source))))
    (markdown-node:source-locations-set! text
     (source-lines:source-loactions source))))

(define (inline-parser:process-delimiters inline-parser stack-bottom)
  )

(define (inline-parser:merge-text-nodes! inline-parser block)
  )

(define (inline-parser:special-char? inline-parser c)
  (define parsers (inline-parser-parsers inline-parser))
  (define processors (inline-parser-processors inline-parser))
  (or (memv c '(#\[ #\] #\! #\newline))
      (hashtable-ref parsers c #f)
      (hashtable-ref processors c #f)))

)
