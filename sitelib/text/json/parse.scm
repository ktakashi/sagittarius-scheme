;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/json/parse.scm - JSON parsing
;;;  
;;;   Copyright (c) 2010-2026  Takashi Kato  <ktakashi@ymail.com>
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
(library (text json parse)
    (export json-write
	    json-write/normalized
	    json-write/pretty
	    json-read

	    json-read-error?
	    json-write-error?

	    make-json-reader

	    ;; for convenience
	    *json:array-handler*
	    *json:object-handler*
	    *json:null-handler*
	    *json:boolean-handler*
	    *json:number-handler*
	    *json:string-handler*

	    *json-map-type*)
    (import (rnrs)
	    (util hashtables)
	    (text parse)
	    (text json config)
	    (text json errors)
	    (text json normalize)
	    (text json parser-parameters)
	    (srfi :2 and-let*)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    ;; for digit-value, reverse!
	    (sagittarius))

(define-condition-type &json-read &json-i/o
  make-json-read-error json-read-error?)
(define-condition-type &json-write &json-i/o
  make-json-write-error json-write-error?)

(define (json-read-error message . irr)
  (define c (condition (make-json-read-error)
		       (make-who-condition 'json-read)
		       (make-message-condition message)))
  (if (null? irr)
      (raise c)
      (raise (condition c (make-irritants-condition irr)))))
(define (json-write-error message . irr)
  (define c (condition (make-json-write-error)
		       (make-who-condition 'json-read)
		       (make-message-condition message)))
  (if (null? irr)
      (raise c)
      (raise (condition c (make-irritants-condition irr)))))
  
(define *json-map-type* 
  (make-parameter #f
    (lambda (x)
      (if (or (not x) (eq? x 'vector) (eq? x 'alist))
	  x
	  (assertion-violation '*json-map-type* 
			       "type must be 'vector or 'alist" x)))))

;; write
(define (%json-write x port comma colon indent)
  (define (write-indent n out)
    (when n
      (do ((i 0 (+ i 1)))
	  ((= i (* n 2)))
	(display #\space out))))
  (define type (*json-map-type*))
  (define (hashtable->vector ht) (list->vector (hashtable->alist ht)))
  (define (write-ht vec port indent)
    (display "{" port) (when indent (newline port))
    (do ((len (vector-length vec)) (n (and indent (+ indent 1))) (i 0 (+ i 1)))
	((= i len))
      (write-indent n port)
      (let* ((e (vector-ref vec i))
	     (k (car e))
	     (v (cdr e)))
	(cond ((symbol? k) (write (symbol->string k) port))
	      ((string? k) (write k port))
	      (else (json-write-error
		     "Invalid JSON table key in json-write" k)))
	(display colon port)
	(write-any v port n))
      (unless (= i (- len 1)) (display comma port))
      (when indent (newline port)))
    (write-indent indent port) (display "}" port))
  (define (write-array arr port indent)
    (define new-indent (and indent (+ indent 1)))
    (display "[" port)
    (let loop ((need-comma? #f) (arr arr))
      (unless (null? arr)
	(when need-comma? (display comma port))
	(when new-indent
	  (newline port)
	  (write-indent new-indent port))
	(write-any (car arr) port new-indent)
	(loop #t (cdr arr))))
    (when indent (newline port))
    (write-indent indent port) (display "]" port))
  (define (write-any x port indent)
    (cond 
     ((hashtable? x) (write-ht (hashtable->vector x) port indent))
     ((vector? x) 
      (if (eq? type 'alist)
	  (write-array (vector->list x) port indent)
	  (write-ht x port indent)))
     ((list? x) 
      (if (eq? type 'alist)
	  (write-ht (list->vector x) port indent)
	  (write-array x port indent)))
     ((eq? x 'null) (display "null" port))
     ((symbol? x) (write (symbol->string x) port))
     ((or (string? x) (number? x)) (write x port))
     ((boolean? x) (display (if x "true" "false") port))
     (else (json-write-error "Invalid JSON object in json-write" x))))
  (write-any x port indent))

(define json-write/pretty
  (case-lambda
   ((x) (json-write/pretty x (current-output-port)))
   ((x port) (%json-write x port "," ": " 0))))

(define json-write/normalized
  (case-lambda
   ((x) (json-write/normalized x (current-output-port)))
   ((x port) (%json-write x port "," ":" #f))))

(define json-write
  (case-lambda
   ((x) (json-write x (current-output-port)))
   ((x port) (%json-write x port ", " ": " #f))))

;; TODO maybe better to track a bit for debug?
(define (read-token token in)
  (let loop ((token token))
    (if (null? token)
	token
	(let ((c (get-char in)))
	  (if (eqv? (car token) c)
	      (loop (cdr token))
	      (json-read-error "Unexpected token character" c))))))

;; string
(define (read-jstring in config in->char)
  (define strict? (json-parser-config-strict? config))
  (define named-escape-chars '((#\b . #\backspace)
			       (#\/ . #\/)
			       (#\\ . #\\)
			       (#\" . #\")
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
		 (json-read-error "Invalid unicode code point character" c))
	       (+ (char->number c) cp))
	      ((char-set-contains? char-set:hex-digit c)
	       (let ((n (char->number c)))
		 (loop (- i 1)
		       (+ cp (bitwise-arithmetic-shift-left n (* i 4))))))
	      (else
	       (json-read-error
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
	  (json-read-error "Invalid unicode code point"
			   `((high ,high) (low ,cp))))))
  (define (read-unicode in)
    (let ((cp (read-code-point in)))
      (cond ((<= #xd800 cp #xdbff)
	     (if strict?
		 (in->char #xfffd)
		 (read-unicode-low-sarrogate in cp)))
	    ((<= #xdc00 cp #xdfff)
	     (in->char #xfffd))
	    (else
	     (integer->char cp)))))
  
  (define (read-escape in)
    (let ((c (get-char in)))
      (cond ((assv c named-escape-chars) => cdr)
	    ((eqv? c #\u) (read-unicode in))
	    ((not strict?) c)
	    (else
	     (json-read-error "Invalid JSON escape character" c)))))
  ;; discards first #\"
  (get-char in)
  (let loop ((acc '()))
    (let ((c (get-char in)))
      (when (eof-object? c) (json-read-error "Unexpected EOF"))
      (case c
	((#\\) (loop (cons (read-escape in) acc)))
	((#\") (list->string (reverse! acc)))
	(else
	 (when (and strict? (< (char->integer c) #x20))
	   (json-read-error "Invalid control character in JSON string" c))
	 (loop (cons c acc)))))))

(define (read-jnumber in config)
  (define strict? (json-parser-config-strict? config))
  (define (json-number-string? s)
    (define len (string-length s))
    (define (digit? c) (char<=? #\0 c #\9))
    (define (digit1-9? c) (char<=? #\1 c #\9))
    (define (scan-digits i)
      (let loop ((i i))
	(if (and (< i len) (digit? (string-ref s i)))
	    (loop (+ i 1))
	    i)))
    (and-let* ((i0 (and (not (zero? len))
			(if (eqv? (string-ref s 0) #\-) 1 0)))
	       ( (< i0 len) )
	       (i1 (cond ((eqv? (string-ref s i0) #\0) (+ i0 1))
			 ((digit1-9? (string-ref s i0))
			  (scan-digits (+ i0 1)))
			 (else #f)))
	       (i2 (if (and (< i1 len) (eqv? (string-ref s i1) #\.))
		       (let ((i (+ i1 1)))
			 (and (< i len)
			      (digit? (string-ref s i))
			      (scan-digits (+ i 1))))
		       i1))
	       (i3 (if (and i2 (< i2 len) (memv (string-ref s i2) '(#\e #\E)))
		       (let* ((i (+ i2 1))
			      (i (if (and (< i len)
					  (memv (string-ref s i) '(#\+ #\-)))
				     (+ i 1)
				     i)))
			 (and (< i len)
			      (digit? (string-ref s i))
			      (scan-digits (+ i 1))))
		       i2)))
      (and i3 (= i3 len))))
  (let loop ((c (lookahead-char in)) (acc '()))
    (case c
      ((#\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E)
       (get-char in)
       (loop (lookahead-char in) (cons c acc)))
      (else
       (let* ((s (list->string (reverse! acc)))
	      (n (string->number s)))
	 (or (and n (or (not strict?) (json-number-string? s)) n)
	     (json-read-error "Unexpected JSON number" s)))))))

(define (skip-white in config)
  (define strict? (json-parser-config-strict? config))
  (define allow-comment? (json-parser-config-allow-comment? config))
  (define (json-ws-char? c)
    (if strict?
	(memv c '(#\space #\tab #\newline #\return))
	(char-whitespace? c)))
  (define (skip-comment in)
    (define (read-block-comment in)
      (get-char in)
      (let loop ()
	(let ((c (get-char in)))
	  (when (eof-object? c) (json-read-error "Unexpected EOF"))
	  (case c
	    ((#\*) (let ((nc (get-char in)))
		     (case nc
		       ((#\/) (skip-white in config))
		       (else (loop)))))
	    (else (loop))))))
    (unless allow-comment?
      (json-read-error "Invalid JSON token" "/"))
    (case (peek-next-char in)
      ((#\*) (read-block-comment in))
      ((#\/) (get-line in) (skip-white in config))
      (else =>
	    (lambda (nc)
	      (json-read-error "Invalid JSON token"
			       (list->string (list #\/ nc)))))))
  (let ((c (skip-while (lambda (c) (and (not (eof-object? c))
					(json-ws-char? c)))
		       in)))
    (case c
      ((#\/) (skip-comment in))
      (else c))))

(define (int->char cp)
  (guard (e (else #\xFFFD))
    (integer->char cp)))

(define (check-nesting-depth depth config)
  (define max-nesting-depth (json-parser-config-max-nesting-depth config))
  (when (and max-nesting-depth (> depth max-nesting-depth))
    (json-read-error "Too deeply nested JSON structure")))

(define (read-map config in depth)
  (check-nesting-depth depth config)
  (let ((finish (*json:object-handler*)))
    (get-char in)
    (let ((nc (skip-white in config)))
      (if (eqv? nc #\})
	  (begin (get-char in) (finish '()))
	  (let loop ((c nc) (r '()))
	    (unless (eqv? c #\")
	      (json-read-error "JSON map contains non string key" c))
	    (let* ((k ((*json:string-handler*) (read-jstring in config int->char)))
		   (sep (skip-white in config)))
	      (unless (eqv? sep #\:)
		(json-read-error "Invalid format of JSON map" sep))
	      (get-char in)
	      (let* ((v (read-any config in (+ depth 1)))
		     (next (skip-white in config)))
		(case (get-char in)
		  ((#\,) (loop (skip-white in config) (acons k v r)))
		  ((#\}) (finish (reverse! (acons k v r))))
		  (else
		   (json-read-error "Invalid format of JSON map"
				    next))))))))))

(define (read-array config in depth)
  (check-nesting-depth depth config)
  (let ((finish (*json:array-handler*)))
    (get-char in)
    (let ((nc (skip-white in config)))
      (if (eqv? nc #\])
	  (begin (get-char in) (finish '()))
	  (let loop ((e (read-any config in (+ depth 1))) (r '()))
	    (let ((next (skip-white in config)))
	      (case (get-char in)
		((#\,) (loop (read-any config in (+ depth 1))
			     (cons e r)))
		((#\]) (finish (reverse! (cons e r))))
		(else
		 (json-read-error "Invalid format of JSON array"
				  next)))))))))

(define (read-any config in depth)
  (define strict? (json-parser-config-strict? config))
  (define number-handler (*json:number-handler*))
  (define string-handler (*json:string-handler*))
  (define boolean-handler (*json:boolean-handler*))
  (define null-handler (*json:null-handler*))
  (let ((c (skip-white in config)))
    (if (eof-object? c)
	c
	(case c
	  ((#\{) (read-map config in depth))
	  ((#\[) (read-array config in depth))
	  ((#\") (string-handler (read-jstring in config int->char)))
	  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-)
	   (let* ((n (read-jnumber in config))
		  (v (number-handler n)))
	     (or v (json-read-error "Invalid JSON number" n))))
	  ((#\+ #\. #\e #\E)
	   (if strict?
	       (json-read-error "Invalid JSON character" c)
	       (let* ((n (read-jnumber in config))
		      (v (number-handler n)))
		 (or v (json-read-error "Invalid JSON number" n)))))
	  ((#\t) (read-token '(#\t #\r #\u #\e) in) (boolean-handler #t))
	  ((#\f) (read-token '(#\f #\a #\l #\s #\e) in) (boolean-handler #f))
	  ((#\n) (read-token '(#\n #\u #\l #\l) in) (null-handler))
	  (else (json-read-error "Invalid JSON character" c))))))

(define (make-json-reader config)
  (define strict? (json-parser-config-strict? config))
  (define array-handler (json-parser-config-array-handler config))
  (define object-handler (json-parser-config-object-handler config))
  (define numbert-handler (json-parser-config-number-handler config))
  (define string-handler (json-parser-config-string-handler config))
  (define null-handler (json-parser-config-null-handler config))
  (define boolean-handler (json-parser-config-boolean-handler config))

  (define (d v p) (or v (p)))
  (define (nhandler) 'null)
  (define (check-map-type)
    (case (*json-map-type*)
      ((vector) (values values list->vector values nhandler))
      ((alist)  (values list->vector values values nhandler))
      ;; default use config or parameter
      (else (values (d array-handler *json:array-handler*)
		    (d object-handler *json:object-handler*)
		    (d boolean-handler *json:boolean-handler*)
		    (d null-handler *json:null-handler*)))))

  (case-lambda
   (() ((make-json-reader config) (current-input-port)))
   ((port)
    (let-values (((ahandler ohandler bhandler nhandler) (check-map-type)))
      (parameterize
	  ((*json:array-handler* ahandler)
	   (*json:object-handler* ohandler)
	   (*json:number-handler* (d numbert-handler *json:number-handler*))
	   (*json:string-handler* (d string-handler *json:string-handler*))
	   (*json:boolean-handler* bhandler)
	   (*json:null-handler* nhandler))
	(let* ((in (if strict? (make-json-normalizing-input-port port) port))
	       (v (read-any config in 0)))
	  (if (eof-object? v)
	      (if strict?
		  (json-read-error "Unexpected EOF")
		  v)
	      (let ((c (skip-white in config)))
		(unless (eof-object? c)
		  (json-read-error "Unexpected trailing token" c))
		v))))))))

;; default
(define json-read (make-json-reader (json-parser-config)))
)
