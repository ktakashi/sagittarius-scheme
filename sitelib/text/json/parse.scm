;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/json/parse.scm - JSON parsing
;;;  
;;;   Copyright (c) 2010-2017  Takashi Kato  <ktakashi@ymail.com>
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

(library (text json parse)
    (export json-write
	    json-read
	    
	    *json-map-type*)
    (import (rnrs)
	    (util hashtables)
	    (text parse)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    ;; for digit-value, reverse!
	    (sagittarius))

  (define *json-map-type* 
    (make-parameter 'vector
		    (lambda (x)
		      (if (or (eq? x 'vector) (eq? x 'alist))
			  x
			  (error '*json-map-type* 
				 "type must be 'vector or 'alist" x)))))

  ;; write
  (define json-write
    (case-lambda
     ((x) (json-write x (current-output-port)))
     ((x port)
      (define type (*json-map-type*))
      (define (hashtable->vector ht)
	(list->vector (hashtable->alist ht)))
      (define (write-ht vec port)
	(define len (vector-length vec))
	(display "{" port)
	(let loop ((i 0))
	  (unless (= i len)
	    (unless (zero? i) (display ", " port))
	    (let* ((e (vector-ref vec i))
		   (k (car e))
		   (v (cdr e)))
	      (cond ((symbol? k) (write (symbol->string k) port))
		    ((string? k) (write k port))
		    (else (error 'json-write 
				 "Invalid JSON table key in json-write" k)))
	      (display ": " port)
	      (write-any v port))
	    (loop (+ i 1))))
	(display "}" port))
      (define (write-array arr port)
	(display "[" port)
	(let loop ((need-comma? #f) (arr arr))
	  (unless (null? arr)
	    (when need-comma? (display ", " port))
	    (write-any (car arr) port)
	    (loop #t (cdr arr))))
	(display "]" port))
      (define (write-any x port)
	(cond 
	 ((hashtable? x) (write-ht (hashtable->vector x) port))
	 ((vector? x) 
	  (if (eq? type 'vector)
	      (write-ht x port)
	      (write-array (vector->list x) port)))
	 ((list? x) 
	  (if (eq? type 'vector)
	      (write-array x port)
	      (write-ht (list->vector x) port)))
	 ((eq? x 'null) (display "null" port))
	 ((symbol? x) (write (symbol->string x) port))
	 ((or (string? x) (number? x)) (write x port))
	 ((boolean? x) (display (if x "true" "false") port))
	 (else (error 'json-write "Invalid JSON object in json-write" x))))
      (write-any x port))))

  (define (skip-white in)
    ;; TODO skip comment as well
    (define (skip-comment in)
      (define (read-block-comment in)
	;; discards #\*
	(get-char in)
	(let loop ()
	  (let ((c (get-char in)))
	    (case c
	      ((#\*) (let ((nc (get-char in)))
		       (case c
			 ((#\/) (skip-white in))
			 (else (loop)))))
	      (else (loop))))))
      ;; there is no token starts with #\/ so if this is given
      ;; it must be a comment
      (case (peek-next-char in)
	((#\*) (read-block-comment in))
	((#\\) (get-line in) (skip-white in))
	(else =>
	  (lambda (nc)
	    (error 'json-read "Invalid JSON token" 
		   (list->string (list #\/ nc)))))))
      
    (let ((c (skip-while (lambda (c) (and (not (eof-object? c))
					  (char-whitespace? c)))
			 in)))
      (case c
	((#\/) (skip-comment in))
	(else c))))

  ;; TODO maybe better to track a bit for debug?
  (define (read-token token in)
    (let loop ((token token))
      (if (null? token)
	  token
	  (let ((c (get-char in)))
	    (if (eqv? (car token) c)
		(loop (cdr token))
		(error 'json-read "Unexpected token character" c))))))

  ;; string
  (define (read-jstring in)
    (define named-escape-chars '((#\b . #\backspace)
				 (#\n . #\newline)
				 (#\f . #\page)
				 (#\r . #\return)
				 (#\t . #\tab)))

    (define (read-code-point in)
      (define (char->number c)
	(or (digit-value c)
	    (- (char->integer (char-downcase c)) #x57)))
      (let loop ((i 3) (cp 0))
	(let ((c (get-char in)))
	  (cond ((zero? i)
		 (unless (char-set-contains? char-set:hex-digit c)
		   (error 'json-read "Invalid unicode code point character" c))
		 (+ (char->number c) cp))
		((char-set-contains? char-set:hex-digit c)
		 (let ((n (char->number c)))
		   (loop (- i 1)
			 (+ cp (bitwise-arithmetic-shift-left n (* i 4))))))
		(else
		 (error 'json-read 
			"Invalid unicode code point character" c))))))
    (define (read-unicode-low-sarrogate in high)
      ;; just check
      (read-token '(#\\ #\u) in)
      (let ((cp (read-code-point in)))
	(if (<= #xdc00 cp #xdfff)
	    (integer->char
	     (+ #x10000
		(* (- high #xd800) #x400)
		(- cp #xdc00)))
	    (error 'json-read "Invalid unicode code point"
		   `((high ,high) (low ,cp))))))
    (define (read-unicode in)
      (let ((cp (read-code-point in)))
	(if (<= #xd800 cp #xdbff)
	    (read-unicode-low-sarrogate in cp)
	    (integer->char cp))))
    
    (define (read-escape in)
      (let ((c (get-char in)))
	(cond ((assv c named-escape-chars) => cdr)
	      ((eqv? c #\u) (read-unicode in))
	      (else c))))
    ;; discards first #\"
    (get-char in)
    (let loop ((acc '()))
      (let ((c (get-char in)))
	(when (eof-object? c) (error 'json-read "Unexpected EOF"))
	(case c
	  ((#\\) (loop (cons (read-escape in) acc)))
	  ((#\") (list->string (reverse! acc)))
	  (else (loop (cons c acc)))))))

  (define (read-jnumber in)
    (let loop ((c (lookahead-char in)) (acc '()))
      (case c
	((#\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E)
	 (get-char in)
	 (loop (lookahead-char in) (cons c acc)))
	(else
	 (let* ((s (list->string (reverse! acc)))
		(n (string->number s)))
	   (or n
	       (error 'json-read "Unexpected JSON number" s)))))))

  (define (read-map map-type in)
    (define (finish map-type result)
      (case map-type
	;; compatible one
	((vector) (list->vector result))
	;; Gauche compatible one
	((alist) result)))
    ;; discard #\{
    (get-char in)
    (let ((nc (skip-white in)))
      (if (eqv? nc #\})
	  (begin (get-char in) (finish map-type '()))
	  (let loop ((c nc) (r '()))
	    (unless (eqv? c #\") 
	      (error 'json-read "JSON map contains non string key" c))
	    (or (and-let* ((k (read-jstring in))
			   ( (eqv? #\: (skip-white in)) )
			   ;; discards #\:
			   ( (get-char in) ))
		  ;; v can be #f so must be outside of and-let*
		  (let* ((v (read-any map-type in))
			 (nc (skip-white in)))
		    (case (get-char in)
		      ((#\,) (loop (skip-white in) (acons k v r)))
		      ((#\}) (finish map-type (reverse! (acons k v r))))
		      (else (error 'json-read "Invalid format of JSON map"
				   nc)))))
		(error 'json-read "Invalid format of JSON map" r))))))

  (define (read-array map-type in)
    (define (finish map-type result)
      (case map-type
	;; original
	((vector) result)
	;; Gauche compatible
	((alist) (list->vector result))))
    ;; discards #\[
    (get-char in)
    ;; check empty
    (let ((nc (skip-white in)))
      (if (eqv? nc #\])
	  (begin (get-char in) (finish map-type '()))
	  (let loop ((e (read-any map-type in)) (r '()))
	    (let ((nc (skip-white in)))
	      (case (get-char in)
		((#\,) (loop (read-any map-type in) (cons e r)))
		((#\]) (finish map-type (reverse! (cons e r))))
		(else (error 'json-read "Invalid format of JSON array"
			     nc))))))))

  (define (read-any map-type in)
    (let ((c (skip-white in)))
      (if (eof-object? c)
	  c
	  (case c
	    ((#\{) (read-map map-type in))
	    ((#\[) (read-array map-type in))
	    ((#\") (read-jstring in))
	    ((#\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E)
	     (read-jnumber in))
	    ;; these 3 are the only symbols, others are either string or number.
	    ((#\t) (read-token '(#\t #\r #\u #\e) in) #t)
	    ((#\f) (read-token '(#\f #\a #\l #\s #\e) in) #f)
	    ((#\n) (read-token '(#\n #\u #\l #\l) in) 'null)
	    (else (error 'json-read "Invalid JSON character" c))))))
  (define json-read
    (case-lambda
     (() (json-read (current-input-port)))
     ((port) (read-any (*json-map-type*) port))))
)
