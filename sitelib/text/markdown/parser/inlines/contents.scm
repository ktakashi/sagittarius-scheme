;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/inlines/contents.scm - Inline content parsers
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
(library (text markdown parser inlines contents)
    (export (rename inline-parser-state <inline-parser-state>)
	    inline-parser-state?
	    inline-parser-state-block
	    inline-parser-state-scanner

	    parsed-inline:none parsed-inline:of
	    parsed-inline? parsed-inline-node parsed-inline-position

	    try-parse-backslash
	    try-parse-backticks
	    try-parse-entity
	    )
    (import (rnrs)
	    (core misc)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (text markdown parser nodes)
	    (text markdown parser parsing)
	    (text markdown parser scanner)
	    (text markdown parser source)
	    (text xml entities))

(define-record-type inline-parser-state
  (fields block scanner))

(define-vector-type parsed-inline (parsed-inline:of node position)
  parsed-inline?
  (node parsed-inline-node)
  (position parsed-inline-position))

(define (parsed-inline:none) #f)

(define (try-parse-backslash state)
  (define scanner (inline-parser-state-scanner state))
  (define block (inline-parser-state-block state))
  (scanner:next! scanner) ;; #\\
  (let ((c (scanner:peek scanner)))
    (cond ((eqv? c #\newline)
	   (scanner:next! scanner)
	   (parsed-inline:of (make-linebreak-node block)
			     (scanner:position scanner)))
	  ((or (eqv? c #\-) (parsing:escapable? c))
	   (scanner:next! scanner)
	   (parsed-inline:of (make-text-node block (string c))
			     (scanner:position scanner)))
	  (else
	   (parsed-inline:of (make-text-node block "\\")
			     (scanner:position scanner))))))

(define (try-parse-backticks state)
  (define scanner (inline-parser-state-scanner state))
  (define block (inline-parser-state-block state))
  (define start (scanner:position scanner))
  (define open-ticks (scanner:match-char scanner #\`))

  (define (strip content)
    (define len (string-length content))
    (if (and (>= len 3)
	     (eqv? (string-ref content 0) #\space)
	     (eqv? (string-ref content (- len 1)) #\space)
	     (string-any (lambda (c) (not (eqv? c #\space))) content))
	(substring content 1 (- len 1))
	content))
  (let ((after-opening (scanner:position scanner)))
    (let loop ()
      (cond ((positive? (scanner:find-char scanner #\`))
	     (let* ((before-closing (scanner:position scanner))
		    (count (scanner:match-char scanner #\`)))
	       (if (= open-ticks count)
		   (let* ((source (scanner:source scanner
						  after-opening
						  before-closing))
			  (content (string-map
				    (lambda (c) (if (eqv? c #\newline)
						    #\space
						    c))
				    (source-lines:content source))))
		     (parsed-inline:of
		      (make-code-node block (strip content))
		      (scanner:position scanner)))
		   (loop))))
	    (else
	     (let ((source (scanner:source scanner start after-opening)))
	       (parsed-inline:of
		(make-text-node block (source-line-content source))
		after-opening)))))))

(define ascii:hex-digit
  (char-set-intersection char-set:hex-digit char-set:ascii))
(define ascii:digit (char-set-intersection char-set:digit char-set:ascii))
(define ascii:letter (char-set-intersection char-set:letter char-set:ascii))
(define ascii:letter+digit
  (char-set-intersection char-set:letter+digit char-set:ascii))
(define (try-parse-entity state)
  (define scanner (inline-parser-state-scanner state))
  (define start (scanner:position scanner))

  (define (entity scanner start)
    (define block (inline-parser-state-block state))
    (define position (scanner:position scanner))
    (define source (scanner:source scanner start position))
    (define (resolve-entity text)
      (if (and (string-prefix? "&" text) (string-suffix? ";" text))
	  (let ((v (substring text 1 (- (string-length text) 1))))
	    (cond ((string-prefix? "#" v)
		   (let ((n (if (memv (string-ref v 1) '(#\x #\X))
				(string->number v)
				(let ((v (substring v 1 (string-length v))))
				  (string->number v)))))
		     (or (and n (string (integer->char n)))
			 "\xFFFD;")))
		  ((xml-entity-name->char v) => string)
		  (else text)))
	  text))
    (let ((text (source-lines:content source)))
      (parsed-inline:of (make-text-node block (resolve-entity text)) position)))
    
  (scanner:next! scanner) ;; skip #\&
  (let ((c (scanner:peek scanner)))
    (cond ((eqv? c #\#)
	   (scanner:next! scanner)
	   (let-values (((cset bound)
			 (if (or (scanner:next-char? scanner #\x)
				 (scanner:next-char? scanner #\X))
			     (values char-set:hex-digit 6)
			     (values char-set:digit 7))))
	     (or (and-let* ((n (scanner:match-charset scanner cset))
			    ( (<= 1 n bound) )
			    ( (scanner:next-char? scanner #\;) ))
		   (entity scanner start))
		 (parsed-inline:none))))
	  ((char-set-contains? ascii:letter c)
	   (scanner:match-charset scanner ascii:letter+digit)
	   (if (scanner:next-char? scanner #\;)
	       (entity scanner start)
	       (parsed-inline:none)))
	  (else (parsed-inline:none)))))
	  
)
