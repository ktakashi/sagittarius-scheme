;;; -*- Scheme -*-
;;;
;;; SRFI-49: Indentation-sensitive syntax
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

(library (srfi :49)
    (export group 
	    srfi-49-read
	    srfi-49-load
	    :export-reader
	    )
    (import (rnrs)
	    (rnrs eval)
	    (core errors)
	    (sagittarius)
	    (sagittarius reader)
	    (srfi :1))

  (define group 'group)

  (define (read-quote level port qt)
    (read-char port)
    (let ((char (peek-char port)))
      (if (or (eqv? char #\space)
	      (eqv? char #\newline)
	      (eqv? char #\tab))
	  (list qt)
	  (list qt (read port)))))

  (define (read-item level port)
    (let ((char (peek-char port)))
      (cond ((eqv? char #\`) (read-quote level port 'quasiquote))
	    ((eqv? char #\') (read-quote level port 'quote))
	    ((eqv? char #\,) (read-quote level port 'unquote))
	    (else (read port)))))

  (define (indentation>? indentation1 indentation2)
    (let ((len1 (string-length indentation1))
	  (len2 (string-length indentation2)))
      (and (> len1 len2)
	   (string=? indentation2 (substring indentation1 0 len2)))))

  (define (indentation-level port)
    (define (indentationlevel)
      (if (or (eqv? (peek-char port) #\space)
	      (eqv? (peek-char port) #\tab))
	  (cons (read-char port) (indentationlevel))
	  '()))
    (list->string (indentationlevel)))

  (define (clean line)
    (cond ((not (pair? line)) line)
	  ((null? line) line)
	  ((eq? (car line) 'group) (cdr line))
	  ((null? (car line)) (cdr line))
	  ((list? (car line))
	   (if (memq (caar line) '(quote quasiquote unquote))
	       (if (and (list? (cdr line))
			(null? (cddr line)))
		   (cons (car line) (cdr line))
		   (list (car line) (cdr line)))
	       (cons (clean (car line)) (cdr line))))
	  (else line)))

  (define (read-blocks level port)
    (let* ((read (read-block-clean level port))
	   (next-level (car read))
	   (block (cdr read)))
      (cond ((eqv? next-level -1)
	     ;; FIXME the last line is not empty but with some expression
	     ;; this case should not raise error just return expression
	     (raise-i/o-read-error 'read-blocks
				   "unexpected EOF"
				   (let ((info (port-info port)))
				     `((port ,port) 
				       (file ,(car info))
				       (line ,(cadr info))))))
	    ((string=? next-level level)
	     (let* ((reads (read-blocks level port))
		    (next-next-level (car reads))
		    (next-blocks (cdr reads)))
	       (if (eq? block '|.|)
		   (if (pair? next-blocks)
		       (cons next-next-level (car next-blocks))
		       (cons next-next-level next-blocks))
		   (cons next-next-level (cons block next-blocks)))))
	    (else (cons next-level (list block))))))

  (define (read-block level port)
    (let ((char (peek-char port)))
      (cond ((eof-object? char) (cons -1 char))
	    ((eqv? char #\newline)
	     (read-char port)
	     (let ((next-level (indentation-level port)))
	       (if (indentation>? next-level level)
		   (read-blocks next-level port)
		   (cons next-level '()))))
	    ((or (eqv? char #\space) (eqv? char #\tab))
	     (read-char port)
	     (read-block level port))
	    (else
	     (let* ((first (read-item level port))
		    (rest (read-block level port))
		    (level (car rest))
		    (block (cdr rest)))
	       (if (eq? first '|.|)
		   (if (pair? block)
		       (cons level (car block))
		       rest)
		   (cons level (cons first block))))))))

  (define (read-block-clean level port)
    (let* ((read (read-block level port))
	   (next-level (car read))
	   (block (cdr read)))
      (cond ((or (not (pair? block))
		 (and (pair? block)
		      (not (null? (cdr block)))))
	     (cons next-level (clean block)))
	    ((null? block) (cons next-level '|.|))
	    (else (cons next-level (car block))))))

  (define-reader (srfi-49-read p)
    (let* ((block (read-block-clean "" p))
	     (level (car block))
	     (block (cdr block)))
	(if (eq? block '|.|)
	    '()
	    block)))

  (define (srfi-49-load filename)
    (call-with-input-file filename
      (lambda (p)
	(do ((expr (srfi-49-read p) (srfi-49-read p)))
	    ((eof-object? expr) #t)
	  (eval expr (current-library))))))
  )