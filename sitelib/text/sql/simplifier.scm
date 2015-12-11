;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sql/simplifier - Simplify SQL
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

(library (text sql simplifier)
    (export simplify-ssql)
    (import (rnrs)
	    (srfi :1 lists) ;; for reverse!
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (match))

;; what we do here is basically 2 things (may increase later)
;;  - concatenate identifier with dot instead of (~ ...) form
;;  - unescape unicode character and normal character (like condision)
(define (simplify-ssql ssql)
  (cond ((pair? ssql)
	 (case (car ssql)
	   ((~) (simplify-identifier ssql))
	   ((unicode)
	    (let ((r (unescape-unicode ssql)))
	      (if (and (pair? r) (eq? (car r) '!))
		  (simplify-identifier (list '~ r))
		  r)))
	   ((escape) (unescape-string ssql))
	   (else => (lambda (e) (cons (simplify-ssql e)
				      (simplify-ssql (cdr ssql)))))))
	;; we don't care this
	(else ssql)))

(define (simplify-identifier ssql)
  (define (handle-identifier ssql)
    (define (has-dot? s) (string-any (lambda (c) (char=? c #\.)) s))
    (cond ((symbol? ssql) (symbol->string ssql))
	  ((pair? ssql)
	   (case (car ssql)
	     ((!)
	      (let ((s (cadr ssql)))
		;; if the value contains '.' then we can't simply
		;; make it symbol so keep it delimited
		(if (has-dot? s) ssql s)))
	     ((unicode) (handle-identifier (unescape-unicode ssql)))
	     ;; we can't simplify method invocation or some weird case
	     ;; e.g. 'select (select *).*' or so
	     (else ssql)))
	  ;; most likely invalid case but this process shouldn't
	  ;; raise an error
	  (else ssql)))
  (define (concat ids) (string->symbol (string-join (reverse! ids) ".")))
  (define (check-identifier id acc r)
    (if (string? id)
	(values (cons id acc) r)
	(values '() (if (null? acc) (cons id r) (cons* id (concat acc) r)))))
  (let loop ((converted (map handle-identifier (cdr ssql)))
	     (acc '()) ;; list of strings
	     (r '()))
    (if (null? converted)
	(if (null? r)
	     (concat acc)
	     (let ((i `(,@(reverse! r)
			,@(if (null? acc) '() (list (concat acc))))))
	       ;; (~ id) -> id
	       (if (null? (cdr i))
		   (car i)
		   (cons '~ i))))
	(let-values (((nacc nr) (check-identifier (car converted) acc r)))
	  (loop (cdr converted) nacc nr)))))

(define hexit (string->char-set "1234567890abcdefABCDEF"))

(define (unescape-unicode ssql)
  ;; unescaping may raise an error because of invalid format of
  ;; either unicode escape or escape character
  (define (handle-escape in e)
    (define (read-it in n)
      (let loop ((i 0) (r '()))
	(if (= i n)
	    (reverse! r)
	    (let ((ch (get-char in)))
	      (cond ((eof-object? ch)
		     (error
		      'simplify-ssql
		      "unexpected EOF during reading unicode escape character"
		      ssql))
		    ((char-set-contains? hexit ch)
		     (loop (+ i 1) (cons ch r)))
		    (else
		     (assertion-violation 'simplify-ssql "invalid hexit value"
					  ch ssql)))))))
    (define (convert chars)
      ;; TODO make it better
      (integer->char (string->number (list->string chars) 16)))
    (let ((nc (get-char in)))
      (cond ((eof-object? nc)
	     (error 'simplify-ssql
		    "unexpected EOF during reading unicode escape character"
		    ssql))
	    ((char=? nc e) e) ;; escaped escape
	    ((char=? nc #\+) (convert (read-it in 6)))
	    (else (convert (cons nc (read-it in 3)))))))

  (define (unescape str escape)
    (unless (= (string-length escape) 1)
      (assertion-violation 'simplify-ssql
       "unicode escape character must be one length string" escape ssql))
    (let-values (((out extract) (open-string-output-port))
		 ((in)          (open-string-input-port str))
		 ((e)           (string-ref escape 0)))
      (when (or (char=? e #\+)
		(char-set-contains? hexit e) (char-whitespace? e))
	(assertion-violation 'simplify-ssql
			     "invalid uescape character" e ssql))
      (let loop ()
	(let ((ch (get-char in)))
	  (cond ((eof-object? ch) (extract))
		((and (char=? ch #\\) (not (char=? e #\\)))
		 ;; we need to escape this one with '\'
		 (put-string out "\\\\")
		 (loop))
		((char=? ch e) (put-char out (handle-escape in e)) (loop))
		(else (put-char out ch) (loop)))))))
  (match ssql
    (('unicode ('! id) 'uescape s) (list '! (unescape id s)))
    (('unicode ('! id))            (list '! (unescape id "\\")))
    (('unicode s 'uescape e)       (unescape s e))
    (('unicode s)                  (unescape s "\\"))
    ;; sorry but we can't handle this one
    (else ssql)))

(define (unescape-string ssql)
  (define (unescape str escape)
    (unless (= (string-length escape) 1)
      (assertion-violation 'simplify-ssql
       "escape character must be one length string" escape ssql))
    (let-values (((out extract) (open-string-output-port))
		 ((in)          (open-string-input-port str))
		 ((e)           (string-ref escape 0)))
      (let loop ()
	(let ((ch (get-char in)))
	  (cond ((eof-object? ch) (extract))
		;; if user specifies '%' as escape. very unlikely but
		;; may happen
		((and (char=? ch #\\) (not (char=? e #\\)))
		 (put-string out "\\\\") (loop))
		((char=? ch e)
		 (let ((nc (get-char in)))
		   (cond ((eof-object? nc) (extract)) ;; how should we handle?
			 (else
			  (put-char out #\\)
			  (put-char out nc) (loop)))))
		(else (put-char out ch) (loop)))))))
  (match ssql
    (('escape (? string? s) (? string? e))
     (unescape s e))
    ;; incorrect but intensional?
    (else ssql)))

)
