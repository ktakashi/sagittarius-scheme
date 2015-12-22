;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sql.scm - SQL utilities
;;;  
;;;   Copyright (c) 2014-2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (text sql)
    (export read-sql
	    sql->ssql
	    ssql->sql
	    ssql->sql-indent

	    ;; parameters
	    *preserve-case*
	    *non-unicode-charset*
	    *unicode-escape*
	    *identifier-case*
	    *character-converter*
	    )
    (import (rnrs)
	    (srfi :1 lists)
	    (text parse)
	    (text sql parser)
	    (text sql simplifier)
	    (text sql serializer))

  ;; reads one SQL. it's separated by #\; so reads until there
  ;; comments are stripped out.
  ;; NOTE: this doesn't check grammar of SQL so "foo" is valid
  ;; and will be returned.
  ;; 
  ;; key:
  ;;  :comment : skip - skips all comment (default)
  ;;             top  - returns comment if it's top level
  (define (read-sql in :key (comment 'skip))
    (define (white? c) (and (char? c) (char-whitespace? c)))

    (let-values (((out extract) (open-string-output-port)))
      ;; ignore white space until it gets something part of SQL
      (skip-while white? in)

      (let loop ((first #t))

	(define (handle-comment in first comment? k)
	  (get-char in)
	  (when comment? (put-string out "/*"))
	  (let lp ()
	    (case (get-char in)
	      ((#\*) 
	       (case (get-char in)
		 ((#\/) (when comment? (put-string out "*/")) (k))
		 (else => (lambda (c) 
			    (when comment? (put-char out #\*) (put-char out c))
			    (lp)))))
	      (else => (lambda (c) (when comment? (put-char out c)) (lp))))))

	(define (handle-quote in e)
	  (let lp ()
	    (let ((c (get-char in)))
	      (when (eof-object? c) (error 'read-sql "unexpected EOF"))
	      (put-char out c)
	      (cond ((char=? c e)
		     (let ((nc (peek-char in)))
		       (cond ((eqv? nc e)
			      ;; escape
			      (put-char out (get-char in))
			      (lp))
			     ;; end
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
	       ((#\-) 
		(if (and first (eq? comment 'top))
		    (begin (put-char out #\-)
			   (put-string out (get-line in))
			   (extract))
		    (begin (get-line in) (loop first))))
	       (else  (put-char out c) (loop #f))))
	    ((#\/)
	     (case (peek-char in)
	       ((#\*)
		(let ((preserve? (and first (eq? comment 'top))))
		(handle-comment in first preserve?
				(if preserve?
				    (lambda () (extract))
				    (lambda () (loop first))))))
	       (else  (put-char out c)          (loop #f))))
	    ;; ';' or ";" is a valid SQL so make it special
	    ((#\")
	     (put-char out c)
	     (handle-quote in #\")
	     (loop #f))
	    ((#\')
	     (put-char out c)
	     (handle-quote in #\')
	     (loop #f))
	    (else
	     (cond ((eof-object? c) 
		    (if first c (extract)))
		   (else (put-char out c) (loop #f)))))))))

  (define (sql->ssql in :key (comment 'top) (strict? #t) (simplify #f))
    (let loop ((r '()))
      (let ((sql (read-sql in :comment comment)))
	(if (eof-object? sql)
	    ;; kind of SSAX style
	    (cons '*TOP* (reverse! r))
	    (let* ((sin  (open-string-input-port sql))
		   (ssql (parse-sql sin (not (eq? comment 'top)) strict?)))
	      (loop (cons (if simplify (simplify-ssql ssql) ssql) r)))))))

)
