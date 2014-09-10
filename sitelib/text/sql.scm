;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sql.scm - SQL utilities
;;;  
;;;   Copyright (c) 2014  Takashi Kato  <ktakashi@ymail.com>
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

;; SQL utility
;;  for now, we only have reading SQL string from port.

(library (text sql)
    (export read-sql)
    (import (rnrs)
	    (text parse))

  ;; reads one SQL. it's separated by #\; so reads until there
  ;; comments are stripped out.
  ;; NOTE: this doesn't check grammar of SQL so "foo" is valid
  ;; and will be returned.
  (define (read-sql in)
    (define (white? c) (and (char? c) (char-whitespace? c)))
    (let-values (((out extract) (open-string-output-port)))
      ;; ignore white space until it gets something part of SQL
      (skip-while white? in)
      (let loop ((first #t))
	(define (handle-comment in first)
	  (get-char in)
	  (let lp ()
	    (case (get-char in)
	      ((#\*) 
	       (case (get-char in)
		 ((#\/) (loop first))
		 (else (lp))))
	      (else (lp)))))
	(define (handle-quote in)
	  (let lp ()
	    (let ((c (get-char in)))
	      (when (eof-object? c) (error 'read-sql "unexpected EOF"))
	      (put-char out c)
	      (case c
		((#\')
		 (let ((nc (peek-char in)))
		   (case nc
		     ((#\') ;; escapse
		      (put-char out (get-char in))
		      (lp))
		     (else))))
		(else (lp))))))
	(let ((c (get-char in)))
	  (case c
	    ((#\;)
	     ;; ignore empty SQL
	     (if first (loop first) (extract)))
	    ((#\-)
	     ;; could be comment
	     (case (peek-char in)
	       ((#\-) (get-line in)    (loop first))
	       (else  (put-char out c) (loop #f))))
	    ((#\/)
	     (case (peek-char in)
	       ((#\*) (handle-comment in first) (loop first))
	       (else  (put-char out c)          (loop #f))))
	    ((#\')
	     (put-char out c)
	     (handle-quote in)
	     (loop #f))
	    (else
	     (cond ((eof-object? c) 
		    (if first c (extract)))
		   (else (put-char out c) (loop #f)))))))))


)
