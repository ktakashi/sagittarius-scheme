;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sql/scanner.scm - SQL scanner
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=char-set
(library (text sql scanner)
    (export make-sql-scanner)
    (import (rnrs) 
	    (srfi :14)
	    (packrat)
	    (sagittarius))

(define (make-sql-scanner p)
  (let ((eof #f)
	(pos (top-parse-position (cond ((car (port-info p)))
				       (else "<?>")))))
    (lambda ()
      (if eof
	  (values pos #f)
	  (let ((c (get-char p)))
	    (if (eof-object? c)
		(begin
		  (set! eof #t)
		  (values pos #f))
		(let ((old-pos pos))
		  (let-values (((new-pos token) (scanner-dispatch c p pos)))
		    (set! pos new-pos)
		    (values old-pos token)))))))))

(define self     #[,()\[\].\;:+-*%^<>=/])
(define op-chars #[~!@#^&\|`?+-*/%<>=])

(define (read-comment port pos)
  (let loop ((ch (get-char port)) (pos pos))
    (cond ((eof-object? ch) (error 'sql-scanner "unexpected EOF"))
	  ((char=? #\* ch)
	   (let ((nc (get-char port)))
	     (case nc
	       ((#\/) (scanner-dispatch (get-char port) port pos))
	       (else (loop nc pos)))))
	  ((char=? #\newline ch)
	   (loop (get-char port) (update-parse-position pos #\newline)))
	  (else (loop (get-char port) pos)))))

;; b'' | B''
(define (read-bit-string port pos) (error 'read-bit-string "not yet"))

;; n'' | N''
(define (read-national-character port pos) 
  (error 'read-national-character "not yet"))

;; normal identifier
(define (read-identifier ch port pos) (error 'read-identifier "not yet"))
;; delimited identifier
(define (read-delimited-identifier port pos unicode?)
  (error 'read-delimited-identifier "not yet"))

(define (read-string port pos unicode?) (error 'read-string "not yet"))

(define (scanner-dispatch ch port pos)
  ;; for may convenience we use local case which handles EOF
  (define-syntax case
    (syntax-rules (else)
      ((_ check ((value) expr ...) ...)
       (let ((ch check))
	 (cond ((eof-object? ch) (error 'sql-scanner "unexpected EOF"))
	       ((char=? ch value) expr ...)
	       ...)))
      ((_ check ((value) expr ...) ... (else last))
       (let ((ch check))
	 (cond ((eof-object? ch) (error 'sql-scanner "unexpected EOF"))
	       ((char=? ch value) expr ...)
	       ...
	       (else last))))))

  (define (next ch port pos)
    (cond ((eof-object? ch) (values pos #f))
	  ;; handle comment first
	  ;; NB: / and - are self standing chars
	  ((char=? #\/ ch)
	   (case (lookahead-char port)
	     ((#\*) (get-char port) (read-comment port pos))
	     (else (values pos (cons ch ch)))))
	  ((char=? #\- ch)
	   (case (lookahead-char port)
	     ((#\-) 
	      (get-line port) 
	      (scanner-dispatch (get-char port) port 
				(update-parse-position pos #\newline)))
	     (else (values pos (cons ch ch)))))
	  ;; bit string?
	  ((char-ci=? #\b ch)
	   (case (lookahead-char port)
	     ((#\') (get-char port) (read-bit-string port pos))
	     (else (read-identifier ch port pos))))
	  ;; National character
	  ((char-ci=? #\n ch)
	   (case (lookahead-char port)
	     ((#\') (get-char port) (read-national-character port pos))
	     (else (read-identifier ch port pos))))
	  ;; quote
	  ((char=? #\' ch) (read-string port pos #f))
	  ;; unicode escape
	  ((char-ci=? #\u ch)
	   ;; FIXME this doesn't allow U&foo
	   ;; not sure if that's actually allowed by SQL though
	   (case (lookahead-char port)
	     ((#\&) (get-char port)
	      (case (get-char port)
		((#\') (read-string port pos #t))
		((#\") (read-delimited-identifier port pos #t))
		(else (error 'sql-scanner "invalid unicode escpape"))))
	     (else (read-identifier ch port pos))))
	  ;; TODO more
	  ))
  ;; skip continuous white spaces.
  (define (skip-whitespace ch port pos)
    (let loop ((ch ch) (pos pos))
      (if (and (char? ch) (char-whitespace? ch))
	  (let ((pos (case ch 
		       ((#\newline) (update-parse-position pos ch))
		       (else pos))))
	    (loop (get-char ch) pos))
	  (values ch pos))))
  (let-values (((ch pos) (skip-whitespace ch port pos)))
    (next ch port pos)))
	
)
