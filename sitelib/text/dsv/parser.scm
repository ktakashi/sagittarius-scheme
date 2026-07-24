;;; -*- Scheme -*-
;;;
;;; Delimiter Separated Value parser 
;;;  
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
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

;; ref
;; - https://www.rfc-editor.org/info/rfc4180/
#!nounbound
(library (text dsv parser)
    (export dsv-parser-options-builder
	    dsv-parser-options?
	    make-dsv-parser

	    ;; low level API
	    make-item-reader
	    make-line-reader)
    (import (rnrs)
	    (srfi :1 lists) ;; for reverse!
	    (srfi :14 char-sets)
	    (record builder))

(define-record-type dsv-parser-options
  (fields separator
	  dquote
	  comment
	  item-accumulator
	  item-seed
	  line-accumulator
	  line-seed))

(define-syntax dsv-parser-options-builder
  (make-record-builder dsv-parser-options
		       ((separator #\,)
			(dquote #\")
			(comment #\#)
			(item-seed '())
			(line-seed '()))))

(define (make-dsv-parser (options dsv-parser-options?))
  (define item-accumulator (dsv-parser-options-item-accumulator options))
  (define item-seed (dsv-parser-options-item-seed options))
  (define line-accumulator (dsv-parser-options-line-accumulator options))
  (define line-seed (dsv-parser-options-line-seed options))

  (define item-reader 
    (make-item-reader (dsv-parser-options-separator options)
		      (dsv-parser-options-dquote options)))
  
  (define line-reader
    (make-line-reader item-reader (dsv-parser-options-comment options)
		      item-accumulator item-seed))
  (lambda (p)
    (let loop ((r line-seed) (i 0))
      (let ((l (line-reader p)))
	(if (eof-object? l)
	    (line-accumulator r l #f)
	    (loop (line-accumulator r l i) (+ i 1)))))))

(define (char/char-set? o) (or (char? o) (char-set? o)))
(define ((make-item-reader (separator char/char-set?) (dquote char/char-set?))
	 (p (and input-port? textual-port?)))
  (define (crlf? c p)
    (case c
      ((#\return)
       (when (eqv? (lookahead-char p) #\newline)
         (get-char p))
       #t)
      ((#\newline) #t)
      (else #f)))
  (define (finish cs eol?) (values (list->string (reverse! cs)) eol?))

  (let loop ((cs '()) (q? #f))
    (let ((c (get-char p)))
      (cond ((eof-object? c) (finish cs #t))
	    ((and (not q?) (=? c separator)) (finish cs #f))
	    ((=? c dquote)
	     (if q?
		 (let ((c (lookahead-char p)))
		   (cond ((=? c dquote)
			  ;; 2DQUOTE case
			  (get-char p) (loop (cons c cs) q?))
			 (else
			  (loop cs #f))))
		 (loop cs #t)))
	    ((and (not q?) (crlf? c p)) (finish cs #t))
	    (else (loop (cons c cs) q?))))))
		   
(define ((make-line-reader item-reader comment item-accumulator item-seed)
	 (p (and input-port? textual-port?)))
  (let loop ((r item-seed) (i 0))
    (let ((c (lookahead-char p)))
      (cond ((eof-object? c) c)
	    ((=? c comment) (get-line p) (loop r i))
	    (else 
	     (let-values (((item eol?) (item-reader p)))
	       (if eol?
		   (item-accumulator r item #f)
		   (loop (item-accumulator r item i) (+ i 1)))))))))
  

;; helper
(define (=? c v)
  (cond ((char? v) (char=? c v))
	((char-set-contains? v c))
	;; `v` can be #f
	(else #t)))
)
