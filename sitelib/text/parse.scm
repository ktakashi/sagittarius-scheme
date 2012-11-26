;;; -*- Scheme -*-
;;;
;;; parse.scm - utilities to parse input
;;;  
;;;   Copyright (c) 2009-2011  Takashi Kato  <ktakashi@ymail.com>
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

;; This library implements the input parsing utilities described in Oleg's site
;; http://okmij.org/ftp/README.html
;; (follow the link of "Scheme -> Input parsing"

(library (text parse)
    (export find-string-from-port?
	    assert-curr-char
	    skip-until
	    skip-while
	    peek-next-char
	    next-token
	    next-token-of
	    read-string)
    (import (core)
	    (core base)
	    (core errors)
	    (sagittarius)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :6 basic-string-ports)
	    (srfi :26 cut))

  (define (char-list-predicate char-list)
    (cond ((char-set? char-list) char-list-contains?/char-set)
	  ((not (list? char-list))
	   (assertion-violation 'char-list-predicate
				"CHAR-LIST must be a list of characters and/or symbol '*eof*"
				char-list))
	  ((and (pair? char-list)
		(char-set? (car char-list))
		(pair? (cdr char-list))
		(null? (cddr char-list))
		(eq? '*eof* (cadr char-list)))
	   char-list-contains?/char-set/eof)
	  ((memq '*eof* char-list)
	   (if (for-all character-or-eof? char-list)
	       char-list/contains?/char/eof
	       char-list-contains?/eof))
	  ((for-all char? char-list) char-list-contains?/chars)
	  (else
	   char-list-contains?)))

  (define (character-or-eof? char)
    (or (char? char)
	(eof-object? char)))

  (define (char-list-contains?/char-set char-list char)
    (and (char? char) (char-set-contains? char-list char)))

  (define (char-list-contains?/char-set/eof char-list char)
    (or (eof-object? char)
	(and (char? char) (char-set-contains? char-list char))))

  (define (char-list-contains?/chars char-list char)
    (memv char char-list))

  (define (char-list-contains?/chars/eof char-list char)
    (or (eof-object? char)
	(memv char char-list)))

  (define (char-list-contains?/eof char-list char)
    (or (eof-object? char)
	(memv char char-list)))

  (define (char-list-contains? char-list char) ;generic version
    (let loop ((cs char-list))
      (if (null? cs)
	  #f
	  (or (eqv? (car cs) char)
	      (and (char-set? (car cs))
		   (char-list-contains?/char-set (car cs) char))
	      (loop (cdr cs))))))


  ;; from Gauche
  (define (find-string-from-port? str in-port . max-no-char)
    (set! max-no-char (if (null? max-no-char) #f (car max-no-char)))
    (if (string-null? str)
	0
	(let ((restart (make-kmp-restart-vector str))
	      (pattern (list->vector (string->list str)))
	      (patlen  (string-length str)))
	  (define (scan patpos count char)
	    (cond ((eof-object? char) #f)
		  ((char=? char (vector-ref pattern patpos))
		   (if (= patpos (- patlen 1))
		       count
		       (scan (+ patpos 1) (+ count 1) (read-char in-port))))
		  ((and max-no-char (>= count max-no-char)) #f)
		  ((= patpos 0)
		   (scan 0 (+ count 1) (read-char in-port)))
		  (else
		   (let ((pi (vector-ref restart patpos)))
		     (if (= pi -1)
			 (scan 0 0 (read-char in-port))
			 (scan pi count char))))))
	  (scan 0 1 (read-char in-port))
	  )))

  (define (assert-curr-char expected-chars comment . maybe-port)
    (let ((port (if (null? maybe-port)
		    (current-input-port)
		    (car maybe-port))))
      (let ((c (read-char port)))
	(if (memv c expected-chars) c
	    (error 'assert-curr-char
		   (format "Wrong character ~a (0x~a) ~a. ~a expexted"
			   c
			   (if (eof-object? c) "*eof*"
			       (number->string (char->integer c) 16))
			   comment 
			   expected-chars))))))

  (define (skip-until arg . maybe-port)
    (let ((port (if (null? maybe-port)
		    (current-input-port)
		    (car maybe-port))))
      (define (skip-until/common pred port)
	(let loop ((c (read-char port)))
	  (cond ((pred c) c)
		((eof-object? c)
		 (assertion-violation 'skip-until
				      "Unexpected EOF while skipping characters"))
		(else
		 (loop (read-char port))))))

      (cond ((number? arg)
	     (and (<= 1 arg)
		  (let loop ((i 1) (c (read-char port)))
		    (cond ((eof-object? c)
			   (assertion-violation 'skip-until
						(format "Unexpected EOF while skipping ~s characters" arg)))
			  ((>= i arg) #f)
			  (else (loop (+ i 1) (read-char port)))))))
	    ((procedure? arg)
	     (skip-until/common arg port))
	    (else			; skip until break-chars (=arg)
	     (skip-until/common (cut (char-list-predicate arg)
				     arg <>) port)))))

  (define (skip-while skip-chars . maybe-port)
    (define (skip-while/common pred port)
      (let loop ((c (peek-char port)))
	(cond ((pred c) (loop (peek-next-char port)))
	      (else c))))

    (let ((port (if (null? maybe-port)
		    (current-input-port)
		    (car maybe-port))))
      (cond ((procedure? skip-chars)
	     (skip-while/common skip-chars port))
	    (else
	     (skip-while/common (cut (char-list-predicate skip-chars)
				     skip-chars <>) port)))))

  (define (peek-next-char . p)
    (if (null? p)
	(begin
	  (read-char)
	  (peek-char))
	(begin
	  (or (textual-port? (car p))
	      (assertion-violation 'peek-next-char
				   "textual-port required"
				   p))
	  (read-char (car p))
	  (peek-char (car p)))))

  (define (next-token prefix-skipped-chars break-chars . options)
    (let ((comment (if (null? options)
		       "unexpexted EOF"
		       (car options)))
	  (port    (if (or (null? options)
			   (null? (cdr options)))
		       (current-input-port)
		       (cadr options))))
      (define (next-token/common pred char port)
	(define o (open-output-string))
	(let loop ((c char))
	  (cond ((pred c) (get-output-string o))
		((eof-object? c) (assertion-violation 'next-token
						      comment))
		(else 
		 (write-char c o)(loop (peek-next-char port))))))

      (let ((c (skip-while prefix-skipped-chars port)))
	(if (procedure? break-chars)
	    (next-token/common break-chars c port)
	    (next-token/common (cut (char-list-predicate break-chars)
				    break-chars <>) c port)))))

  (define (next-token-of incl-list/pred . maybe-port)
    (let ((port (if (null? maybe-port)
		    (current-input-port)
		    (car maybe-port))))
      (define (next-token-of/common pred port)
	(define o (open-output-string))
	(let loop ((c (peek-char port)))
	  (cond ((or (eof-object? c)
		     (not (pred c)))
		 (get-output-string o))
		(else
		 (write-char c o)
		 (loop (peek-next-char port))))))
      (cond ((procedure? incl-list/pred)
	     (next-token-of/common incl-list/pred port))
	    (else
	     (next-token-of/common (cut (char-list-predicate incl-list/pred)
					incl-list/pred <>) port)))
      ))

  (define (read-string num p)
    (get-string-n p num))
)
