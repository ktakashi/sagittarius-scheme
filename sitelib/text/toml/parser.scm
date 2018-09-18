;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/toml/parser.scm - TOML parser
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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
(library (text toml parser)
    (export *toml-version*
	    +toml-version-0.4.0+
	    +toml-version-0.5.0+
	    &toml-parse-error make-toml-parse-error toml-parse-error?
	    toml-parser

	    (rename (key-set toml-key-char-set)
		    (basic-unescaped-set toml-quoted-char-set))
	    )
    (import (rename (rnrs)
		    (newline r6:newline)
		    (string r6:string))
	    (rnrs mutable-pairs)
	    (peg)
	    (peg chars)
	    (sagittarius)
	    (sagittarius timezone)
	    (only (sagittarius time)
		  make-date
		  make-local-time
		  make-local-date
		  local-time-nanosecond
		  local-time-second
		  local-time-minute
		  local-time-hour
		  local-date-day
		  local-date-month
		  local-date-year)
	    (srfi :1 lists)
	    (only (srfi :13 strings) string-concatenate)
	    (srfi :14 char-sets)
	    (srfi :39 parameters))
(define-condition-type &toml-parse-error &error
  make-toml-parse-error toml-parse-error?)

(define *toml-version* (make-parameter +inf.0))
(define-constant +toml-version-0.4.0+ 0.4)
(define-constant +toml-version-0.5.0+ 0.5)

;; internal parameters
(define *current-table* (make-parameter #f))
(define *current-root-table* (make-parameter #f))
(define *tables* (make-parameter #f))
(define *array-tables* (make-parameter #f))

(define non-eol-set (ucs-range->char-set #x20 #x110000))
(define key-set
  (char-set-union
   (char-set-intersection char-set:ascii char-set:letter+digit)
   (string->char-set "-_")))

(define basic-unescaped-set
  (char-set-union
   (list->char-set '(#\space #\!)) ;; #x20-#x21
   (ucs-range->char-set #x23 #x5C)
   (ucs-range->char-set #x5D #x7F)
   (ucs-range->char-set #x80 #x110000)))

(define ml-basic-unescaped-set
  (char-set-union
   (ucs-range->char-set #x20 #x5C)
   (ucs-range->char-set #x5D #x110000)))

(define literal-char-set
  (char-set-union
   (list->char-set '(#\tab))
   (ucs-range->char-set #x20 #x27)
   (ucs-range->char-set #x28 #x110000)))

(define ml-literal-char-set
  (char-set-union
   (list->char-set '(#\tab))
   (ucs-range->char-set #x20 #x110000)))

(define hex-digit (string->char-set "0123456789abcdefABCDEF"))
(define dec-digit (string->char-set "0123456789"))
(define oct-digit (string->char-set "01234567"))
(define bin-digit (string->char-set "01"))

(define $in-set $char-set-contains?)

(define newline
  ($or ($eqv? #\newline)
       ($seq ($eqv? #\return) ($eqv? #\newline))))
(define wschar ($or ($eqv? #\space) ($eqv? #\tab)))
(define ws ($many wschar))

(define comment-start-symbol ($eqv? #\#)) ;; #\x23
(define non-eol
  ($or ($eqv? #\tab)
       ($in-set non-eol-set)))
(define comment ($seq comment-start-symbol ($many non-eol)))

(define escape ($eqv? #\\))
(define-syntax with-escape
  (syntax-rules ()
    ((_ char)   (with-escape char char))
    ((_ char r) ($seq ($eqv? char) ($return r)))))
(define escape-seq-char
  ($or (with-escape #\")
       (with-escape #\\)
       (with-escape #\/)
       (with-escape #\b #\backspace)
       (with-escape #\f #\page)
       (with-escape #\n #\newline)
       (with-escape #\r #\return)
       (with-escape #\t #\tab)
       ($do escape (($eqv? #\u)) (c* ($repeat ($in-set hex-digit) 4))
	    ($return (integer->char (string->number (list->string c*)))))
       ($do escape (($eqv? #\U)) (c* ($repeat ($in-set hex-digit) 8))
	    ($return (integer->char (string->number (list->string c*)))))))
(define escaped ($do escape (c escape-seq-char) ($return c)))
(define basic-unescaped ($in-set basic-unescaped-set))
(define basic-char ($or basic-unescaped escaped))
(define basic-string
  ($do (($eqv? #\"))
       (c* ($many basic-char))
       (($eqv? #\"))
       ($return (list->string c*))))

;; multiline basic string
(define ml-basic-string-delim ($repeat ($eqv? #\") 3))
(define ml-basic-unescaped ($in-set ml-basic-unescaped-set))
(define ml-basic-char ($or ml-basic-unescaped escaped))
(define ml-basic-string-body
  ($do (c* ($many ($seq ($not ml-basic-string-delim)
			($or ml-basic-char newline
			       ($do escape ws newline ($return #f))))))
       ($return (list->string (filter values c*)))))
(define ml-basic-string
  ($do ml-basic-string-delim
       (s ml-basic-string-body)
       ml-basic-string-delim
       ($return s)))

;; literal string
(define literal-char ($in-set literal-char-set))
(define literal-string
  ($do (($eqv? #\')) (c* ($many literal-char)) (($eqv? #\'))
       ($return (list->string c*))))

(define ml-literal-string-delim ($repeat ($eqv? #\') 3))
(define ml-literal-char ($in-set ml-literal-char-set))
(define ml-literal-body
  ($do (c* ($many ($seq ($not ml-literal-string-delim)
			($or ml-literal-char newline))))
       ($return (list->string c*))))
(define ml-literal-string
  ($do ml-literal-string-delim (s ml-literal-body) ml-literal-string-delim
       ($return s)))

(define string
  ($or ml-basic-string
       basic-string
       ml-literal-string
       literal-string))

(define unquoted-key
  ($do (c* ($many ($in-set key-set) 1))
       ($return (list->string c*))))
(define quoted-key ($or basic-string literal-string))
(define keyval-sep ($seq ws ($eqv? #\=) ws))
(define simple-key ($or unquoted-key quoted-key))
(define dot-sep ($seq ws ($eqv? #\.)  ws))
(define (dotted-key type)
  ($do (k simple-key)
       (k* ($many ($seq dot-sep simple-key) 1))
       ($return (keys->table type k k*))))
(define (key type) ($or (dotted-key type) simple-key))

(define boolean
  ($or ($do (($token "true")) ($return #t))
       ($do (($token "false")) ($return #f))))

;; Integer
(define (digit1-9? c) (and (char-set-contains? dec-digit c) (not (eqv? c #\0))))
(define unsigned-dec-int
  ($or ($do (d ($satisfy digit1-9?))
	    (d* ($many ($or ($in-set dec-digit)
			    ($do (($eqv? #\_))
				 (c ($in-set dec-digit))
				 ($return c)))))
	    ($return (string->number (list->string (cons d d*)))))
       ($do (d ($satisfy char-numeric?))
	    ($return (string->number (r6:string d))))))
(define dec-int
  ($do (s ($optional ($or ($eqv? #\-) ($eqv? #\+))))
       (n unsigned-dec-int)
       ($return (if (and s (eqv? s #\-)) (- n) n))))

(define (radius-int prefix char-set radius)
  ($do (($token prefix))
       (h ($in-set char-set))
       (h* ($many ($or ($in-set char-set)
		       ($do (($eqv? #\_))
			    (c ($in-set char-set))
			    ($return c)))))
       ($return (string->number (list->string (cons h h*)) radius))))
(define hex-int (radius-int "0x" hex-digit 16))
(define oct-int (radius-int "0o" oct-digit 8))
(define bin-int (radius-int "0b" bin-digit 2))
(define non-dec-int ($or hex-int oct-int bin-int))

(define integer
  ($or ($when (< +toml-version-0.4.0+ (*toml-version*)) non-dec-int)
       dec-int))

;; Float
;; silly...
(define float-int-part ($do (i dec-int) ($return (number->string i))))
(define zero-prefixable-int
  ($do (d ($in-set dec-digit))
       (d* ($many ($or ($in-set dec-digit)
		       ($do (($eqv? #\_))
			    (c ($in-set dec-digit))
			    ($return c)))))
       ($return (list->string (cons d d*)))))
(define frac
  ($do (($eqv? #\.))
       (v zero-prefixable-int)
       ($return (string-append "." v))))
(define exponent
  ($do (($eqv? #\e))
       (e float-int-part)
       ($return (string-append "e" e))))
(define float-inf
  ($do (s ($optional ($or ($eqv? #\+) ($eqv? #\-)) #\+))
       (($token "inf"))
       ($return (if (eqv? s #\-) -inf.0 +inf.0))))
(define float-nan
  ($do (s ($optional ($or ($eqv? #\+) ($eqv? #\-)) #\+))
       (($token "nan"))
       ($return (if (eqv? s #\-) -nan.0 +nan.0))))
(define float-special ($or float-inf float-nan))
(define float
  ($or ($do (i float-int-part)
	    (f ($or exponent
		    ($do (f frac) (e ($optional exponent))
			 ($return (if e (string-append f e) f)))))
	    ($return (string->number (string-append i f))))
       ($when (< +toml-version-0.4.0+ (*toml-version*)) float-special)))

;; Date and Time
;; hmmm
(define (local-tz-offset) (timezone-offset (local-timezone)))
(define (->date d t o)
  (make-date (local-time-nanosecond t)
	     (local-time-second t)
	     (local-time-minute t)
	     (local-time-hour t)
	     (local-date-day d)
	     (local-date-month d)
	     (local-date-year d)
	     o))
(define 2digits ($do (d ($repeat ($in-set dec-digit) 2))
		     ($return (string->number (list->string d)))))
(define date-fullyear ($do (d ($repeat ($in-set dec-digit) 4))
			   ($return (string->number (list->string d)))))

;; TODO check?
(define date-of-month 2digits)
(define date-of-day 2digits)

(define time-delim ($or ($eqv? #\T) ($eqv? #\space)))

(define time-hour 2digits)
(define time-minute 2digits)
(define time-second 2digits)
(define (->nanosecond digits n)
  (if (zero? n)
      n
      (let* ((len (length digits))
	     (base (/ 1000000000 (expt 10 len))))
	(* base n))))
(define time-secfrac
  ($do (($eqv? #\.))
       (m ($many ($in-set dec-digit) 1))
       ($return (->nanosecond m (string->number (list->string m))))))
(define time-numoffset
  ($do (s ($or ($eqv? #\+) ($eqv? #\-)))
       (h time-hour)
       (($eqv? #\:))
       (m time-minute)
       ($return (let ((off (+ (* 3600 h) m)))
		  (if (eqv? s #\-)
		      (- off)
		      off)))))
(define time-offset
  ($or ($do (($eqv? #\Z)) ($return 0))
       time-numoffset))

(define partial-time
  ($do (h time-hour)
       (($eqv? #\:))
       (m time-minute)
       (($eqv? #\:))
       (s time-second)
       (f ($optional time-secfrac 0))
       ($return (make-local-time f s m h))))

(define full-date
  ($do (y date-fullyear)
       (($eqv? #\-))
       (m date-of-month)
       (($eqv? #\-))
       (d date-of-day)
       ($return (make-local-date d m y))))

(define offset-date-time
  ($do (d full-date)
       time-delim
       (p partial-time)
       (o time-offset)
       ($return (->date d p o))))

(define local-date-time
  ($do (d full-date)
       time-delim
       (t partial-time)
       ($return (->date d t (local-tz-offset)))))
(define local-date full-date)
(define local-time partial-time)
(define date-time
  ($or offset-date-time
       ($when (< +toml-version-0.4.0+ (*toml-version*)) local-date-time)
       ($when (< +toml-version-0.4.0+ (*toml-version*)) local-date)
       ($when (< +toml-version-0.4.0+ (*toml-version*)) local-time)))

;; Array
(define ws-comment-newline
  ($many ($or wschar
	      ($seq ($optional comment) newline))))
(define array-sep ($eqv? #\,))
(define array-values
  ($or ($do ws-comment-newline
	    (v val)
	    ws
	    array-sep
	    (v* array-values)
	    ($return (cons v v*)))
       ($do ws-comment-newline
	    (v val)
	    ws
	    (($optional array-sep))
	    ($return (list v)))))
(define array
  ($do (($eqv? #\[))
       (v ($optional array-values '()))
       ws-comment-newline
       (($eqv? #\]))
       ($return v)))

;; Table
(define std-table-open ($seq ($eqv? #\[) ws))
(define std-table-close ($seq ($eqv? #\]) ws))

(define (find-table otable k)
  (define table (if (vector? otable) otable (car (last-pair otable))))
  (define len (vector-length table))
  (let loop ((i 0))
    (cond ((= i len) #f)
	  ((equal? (car (vector-ref table i)) k) (vector-ref table i))
	  (else (loop (+ i 1))))))
(define (last-element store)
  (if (vector? store)
      store ;; current was standard
      (car (last-pair store))))
(define (keys->table type k keys)
  (define root-table (*current-root-table*))
  (define (rec table k . keys)
    (cond ((and table (find-table table k)) =>
	   (lambda (store)
	     (define (make-tail)
	       (let ((v (case type
			  ((standard) `#(,store))
			  ((array) (vector (append! store (list (vector))))))))
		 (*current-table* (cons type v))
		 (values #f #f)))
	     (define (process-tail)
	       (let* ((v (last-element (cdr store)))
		      (n (apply rec v keys)))
		 (cond ((eq? v n) (values #f #f))
		       ((vector? (cdr store))
			(values (vector-append v n) #t))
		       (else
			(let loop ((h store))
			  (cond ((eq? (cadr h) v)
				 (set-cdr! h (list (vector-append v n)))
				 (values #f #f))
				(else (loop (cdr store)))))))))
	     (let-values (((tail update?)
			   (if (null? keys) (make-tail) (process-tail))))
	       (when update? (set-cdr! store tail))
	       table)))
	  ((null? keys)
	   (let ((r `#((,k . ,(case type
				((standard) (vector))
				((array) '()))))))
	     (*current-table* (cons type r))
	     r))
	  (else `#((,k . ,(apply rec table keys))))))
  (let* ((r (apply rec root-table k keys))
	 (current? (eq? r root-table)))
    (when (or (not root-table) (not current?))
      (*current-root-table* r))
    (and (not current?) r)))

(define std-table
  ($do std-table-open 
       (k simple-key)
       (k* ($many ($seq dot-sep simple-key)))
       std-table-close
       ($return (keys->table 'standard k k*))))

(define array-table-open ($seq ($eqv? #\[) ($eqv? #\[) ws))
(define array-table-close ($seq ($eqv? #\]) ($eqv? #\]) ws))

(define array-table
  ($do array-table-open
       (k simple-key)
       (k* ($many ($seq dot-sep simple-key)))
       array-table-close
       ($return (keys->table 'array k k*))))

;; inlien table
(define inline-table-sep ($seq ws ($eqv? #\,) ws))
(define inline-table-keyvals-non-empty
  ($do (k (key 'standard))
       keyval-sep
       (v val)
       (kv* ($optional
	     ($seq inline-table-sep
		   inline-table-keyvals-non-empty)
	     '()))
       ($return (cons (cons k v) kv*))))
(define inline-table-keyvals
  ($optional inline-table-keyvals-non-empty '()))
(define inline-table
  ($do (($seq ($eqv? #\{) ws))
       (r inline-table-keyvals)
       (($seq ws ($eqv? #\})))
       ($return (list->vector r))))

(define table
  ($or std-table array-table))

(define val
  ($or string
       boolean
       array
       inline-table
       date-time
       float
       integer))

(define (resolve-table k v)
  (define (maybe-last-element e)
    (if (null? e)
	e
	(last-element e)))
  (define (get-target-key k)
    (define l (vector-length k))
    (let ((v (vector-ref k (- l 1))))
      (if (zero? (vector-length (cdr v)))
	  v
	  (get-target-key (cdr v)))))
  (cond ((vector? k) ;; dotted key
	 (set-cdr! (get-target-key k) v)
	 k)
	((*current-table*) =>
	 (lambda (t)
	   (let* ((type (car t))
		  (store (vector-ref (cdr t) 0))
		  (v* (maybe-last-element (cdr store))))
	     (let ((v (cond ((not k) v) ;; dotted key
			    ((null? v*) `#((,k . ,v)))
			    (else (vector-append v* `#((,k . ,v)))))))
	       (case type
		 ((standard) (set-cdr! store v))
		 ((array)
		  (if (null? v*)
		      (set-cdr! store (list v))
		      (let loop ((h store))
			(cond ((null? h)) ;; error case should never happen
			      ((eq? (cadr h) v*)
			       (set-cdr! h (list v)))
			      (else (loop (cdr h))))))))
	       #f))))
	(else (vector (cons k v)))))

(define keyval
  ($do (k (key 'standard))
       keyval-sep
       (v val)
       ($return (resolve-table k v))))

(define expression
  ($or ($do ws (t table) ws (($optional comment)) ($return t))
       ;; toplevel keyval is converted to table format
       ($do ws (k keyval) ws (($optional comment)) ($return k))
       ($do ws (($optional comment)) ($return #f))))

(define toml-parser
  ($parameterize ((*current-table* #f)
		  (*current-root-table* #f))
    ($do (e expression)
	 (e* ($many ($do newline (e expression) ($return e))))
	 ;; merge expressions
	 ($return (vector-concatenate (filter values (cons e e*)))))))
)
