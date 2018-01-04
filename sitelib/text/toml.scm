;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/toml.scm - TOML
;;;  
;;;   Copyright (c) 2017-2018  Takashi Kato  <ktakashi@ymail.com>
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
(library (text toml)
    (export toml-write
	    toml-read
	    *toml-version*
	    +toml-version-0.4.0+
	    &toml-parse-error make-toml-parse-error toml-parse-error?

	    *toml-use-inline-table*
	    )
    (import (rnrs)
	    (peg)
	    (text toml parser)
	    (sagittarius generators)
	    (srfi :14 char-sets)
	    (srfi :19 time)
	    (srfi :39 parameters)
	    (srfi :127 lseqs))

;;; API
(define (toml-read in)
  (let-values (((s v nl)
		(toml-parser (generator->lseq (port->char-generator in)))))
    (if (and (parse-success? s) (null? nl))
	v
	(raise
	 (condition
	  (make-toml-parse-error)
	  (make-who-condition 'toml-read)
	  (make-message-condition "Failed to read TOML")
	  (make-irritants-condition v))))))

;;; API
(define *toml-use-inline-table* (make-parameter #f))

;;; API
(define toml-write
  (case-lambda
   ((toml) (toml-write toml (current-output-port)))
   ((toml out)
    (if (*toml-use-inline-table*)
	(toml-write-inline toml out)
	(toml-write-readable toml out)))))

;;; Helpers
(define (write-key key out)
  (define (need-escape? key)
    (not (for-all (lambda (c) (char-set-contains? toml-key-char-set c))
		  (string->list key))))
  (define (write-key-escaped key out)
    (put-char out #\")
    (for-each (lambda (c)
		(cond ((char-set-contains? toml-quoted-char-set c)
		       (put-char out c))
		      (else
		       (put-char out #\\)
		       (case c
			 ((#\backspace) #\b)
			 ((#\page) #\f)
			 ((#\newline) #\n)
			 ((#\return) #\r)
			 ((#\tab) #\t)
			 (else c)))))
	      (string->list key))
    (put-char out #\"))
  (if (need-escape? key)
      (write-key-escaped key out)
      (put-string out key)))
(define (write-keys keys out)
  (do ((first? #t #f) (keys (reverse keys) (cdr keys)))
      ((null? keys))
    (unless first? (put-char out #\.))
    (write-key (car keys) out)))
(define (write-table-keys keys out)
  (put-char out #\[)
  (write-keys keys out)
  (put-char out #\]))
(define (write-array-table-keys keys out)
  (put-string out "[[")
  (write-keys keys out)
  (put-string out "]]"))

(define (write-value value out)
  (define (write-string value out)
    ;; TODO multi string and so
    (put-datum out value))
  (define (write-number value out)
    (cond ((infinite? value)
	   (if (negative? value)
	       (put-string out "-inf")
	       (put-string out "inf")))
	  ((nan? value)
	   ;; we don't have anyway of checking negative nan
	   (put-string out "nan"))
	  (else (put-datum out value))))
  (define (write-date value out)
    (put-string out (date->string value "~6")))
  (define (write-boolean value out)
    (if value
	(put-string out "true")
	(put-string out "false")))
  (define (write-array value out)
    (put-char out #\[)
    (do ((first? #t #f) (v* value (cdr v*)))
	((null? v*))
      (unless first? (put-string out ", "))
      (write-value (car v*) out))
    (put-char out #\]))
  (define (write-inline-table value out)
    (put-char out #\{)
    (do ((len (vector-length value)) (i 0 (+ i 1)))
	((= i len))
      (unless (zero? i) (put-string out ", "))
      (write-key-value (vector-ref value i) out))
    (put-char out #\}))
  (cond ((string? value)  (write-string value out))
	((number? value)  (write-number value out))
	((date? value)    (write-date value out))
	((boolean? value) (write-boolean value out))
	((or (pair? value) (null? value)) (write-array value out))
	((vector? value)  (write-inline-table value out))
	(else (assertion-violation 'toml-write "unknown value" value))))

(define (write-key-value kv out)
  (write-key (car kv) out)
  (put-string out " = ")
  (write-value (cdr kv) out))
    
(define (table-array? v*)
  (and (pair? v*) (for-all vector? v*)))

(define (toml-write-readable toml out)
  (toml-write-readable/with-parent toml '() out))

(define (toml-write-readable/with-parent toml parents out)
  (define (put-indent out parents)
    (do ((len (length parents)) (i 0 (+ i 1)))
	((= i len))
      (put-string out "  ")))
  (define (write-element kv)
    (put-indent out parents)
    (cond ((vector? (cdr kv))
	   (let ((keys (cons (car kv) parents)))
	     (write-table-keys keys out)
	     (newline out)
	     (toml-write-readable/with-parent (cdr kv) keys out)
	     (newline out)))
	  ((table-array? (cdr kv))
	   (do ((keys (cons (car kv) parents))
		(first? #t #f)
		(v* (cdr kv) (cdr v*)))
	       ((null? v*))
	     (unless first? (newline out) (put-indent out parents))
	     (write-array-table-keys keys out)
	     (newline out)
	     (toml-write-readable/with-parent (car v*) keys out)))
	  (else (write-key-value kv out) (newline out))))
  (vector-for-each write-element toml))

(define (toml-write-inline toml out)
  (vector-for-each (lambda (kv) (write-key-value kv out) (newline out)) toml))
)
