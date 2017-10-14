;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; regex.scm - regular expression library
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

(library (sagittarius regex)
    (export compile-regex
	    compile-regex-ast
	    regex-matcher
	    regex-matches
	    regex-find
	    regex-looking-at
	    regex-group
	    regex-capture-count
	    ;; pred
	    regex-pattern?
	    regex-matcher?
	    ;; accessor
	    regex-before
	    regex-after
	    regex-first
	    regex-last
	    regex-group-start
	    regex-group-end

	    ;; flags
	    CASE-INSENSITIVE
	    COMMENTS
	    MULTILINE
	    LITERAL
	    DOTALL
	    ;; deprecated but for backward compatibility...
	    (rename (UNICODE UNICODE-CASE))
	    UNICODE

	    
	    ;; syntax-sugar
	    regex

	    ;; wrapper APIs
	    matches
	    looking-at

	    ;; modify
	    regex-replace-all
	    regex-replace-first
	    regex-replace

	    ;; utility
	    regex-fold
	    string-split
	    regex-match-let
	    regex-match-if
	    regex-match-cond
	    regex-match-case


	    ;; clos
	    <pattern> <matcher>
	    ;; accessor
	    regex-ast
	    regex-pattern
	    regex-flags
	    ;; enable #/regex/
	    :export-reader-macro

	    ;; misc
	    parse-char-set-string
	    char-set->regex-string
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius regex impl)
	    (clos user))

  (define regex compile-regex)

  ;; complete match
  (define (matches reg text)
    (let ((matcher (regex-matcher reg text)))
      (if (regex-matches matcher)
	  matcher
	  #f)))

  (define (looking-at reg text)
    (let ((matcher (regex-matcher reg text)))
      (if (regex-looking-at matcher)
	  matcher
	  #f)))

  ;; since version 0.3.3, we have object-apply and
  ;; CLOS libraries are in lib.
  (define-method object-apply ((self <pattern>) (s <string>))
    (looking-at self s))
  (define-method object-apply ((self <pattern>) (s <bytevector>))
    (looking-at self s))
  (define-method object-apply ((self <matcher>) (group <integer>))
    (regex-group self group))
  (define-method object-apply ((self <matcher>) (group <symbol>))
    (case group
      ((before) (regex-before self))
      ((after)  (regex-after self))
      (else     (regex-group group))))
  
  (define (default-finish from md str acc) acc)
  ;; SRFI-115 thing
  (define (regex-fold rx kons knil str 
		      :optional (finish default-finish)
		      (start 0) (end (string-length str)))
    (let ((m (regex-matcher rx str start end)))
      (let loop ((i start) (from start) (acc knil))
	(if (and (< i end) (regex-find m))
	    (let* ((first (regex-first m))
		   (last  (regex-last m))
		   (off (if (= first last) 1 0)))
	      (loop (if (and (= i last) (< last end))
			(+ last 1)
			last)
		    last
		    (kons from m str acc)))
	    (finish from #f str acc)))))
  ;; now this is a bit inefficient...
  (define (string-split text str/pattern 
			:optional (start 0) (end (string-length text)))
    (let ((p (cond ((regex-pattern? str/pattern) str/pattern)
		   ((string? str/pattern) (regex str/pattern))
		   ((char? str/pattern) (regex (list->string  
						(list str/pattern))))
		   (else (assertion-violation
			  'string-split
			  "string or regex-pattern required" str/pattern)))))
      (regex-fold 
       p
       (lambda (from md str a) 
	 (let* ((i (regex-first md))
		(e (regex-last md)))
	   (if (= i e)
	       (cons (+ e 1) (cons (substring str (car a) (+ e 1)) (cdr a)))
	       (let ((s (substring str (car a) i)))
		 (if (zero? (string-length s))
		     (cons e (cdr a))
		     (cons e (cons s (cdr a))))))))
       (cons start '())
       text
       (lambda (from md str a)
	 (if (= from end)
	     (reverse! (cdr a))
	     (reverse! (cons (substring str (car a) end) (cdr a)))))
       start
       end)))

  ;; from Gauche, modified to use syntax-case
  (define-syntax regex-match-bind*
    (lambda (x)
      (syntax-case x ()
	((_ ?n ?match () ?form ...)
	 #'(begin ?form ...))
	((_ ?n ?match (#f ?vars ...) ?form ...)
	 #'(regex-match-bind* (+ ?n 1) ?match (?vars ...) ?form ...))
	((_ ?n ?match (?var ?vars ...) ?form ...)
	 #'(let ((?var (regex-group ?match ?n)))
	   (regex-match-bind* (+ ?n 1) ?match (?vars ...) ?form ...))))))

  (define-syntax regex-match-let
    (lambda (x)
      (syntax-case x ()
	((_ ?expr (?var ...) ?form ...)
	 #'(cond (?expr => (lambda (m) 
			     (regex-match-bind* 0 m (?var ...) ?form ...)))
		 (else (assertion-violation 'regex-match-let
					    "match failed" '?expr))))
	(_ (syntax-violation 'regex-match-let
			     "malformed regex-match-let"
			     (unwrap-syntax x))))))

  (define-syntax regex-match-if
    (lambda (x)
      (syntax-case x ()
	((_ ?expr (?var ...) ?then ?else)
	 #'(cond (?expr => (lambda (m)
			     (regex-match-bind* 0 m (?var ...) ?then)))
		 (else ?else)))
	(_ (syntax-violation 'regex-match-if
			     "malformed regex-match-if"
			     (unwrap-syntax x))))))

  (define-syntax regex-match-cond
    (lambda (x)
      (syntax-case x (?? else =>)
	((_) #'#f)
	((_ (else ?form ...))
	 #'(begin ?form ...))
	((_ (?? ?expr => ?obj) ?clause ...)
	 #'(cond (?expr => ?obj) (else (regex-match-cond ?clause ...))))
	((_ (?? ?expr ?form ...) ?clause ...)
	 #'(if ?expr (begin ?form ...) (regex-match-cond ?clause ...)))
	((_ (?matchexp ?bind ?form ...) ?clause ...)
	 #'(regex-match-if ?matchexp ?bind
			   (begin ?form ...)
			   (regex-match-cond ?clause ...)))
	(_ (syntax-violation 'regex-match-cond
			     "malformed regex-match-cond"
			     (unwrap-syntax x))))))

  (define-syntax regex-match-case
    (lambda (x)
      (syntax-case x (?? else =>)
	((_ #t ?temp ?strp) #'#f)
	((_ #t ?temp ?strp (else => ?proc))
	 #'(?proc ?temp))
	((_ #t ?temp ?strp (else ?form ...))
	 #'(begin ?form ...))
	((_ #t ?temp ?strp (?? ?proc => ?obj) ?clause ...)
	 #'(cond ((?proc ?temp) => ?obj)
		 (else (regex-match-case #t ?temp ?strp ?clause ...))))
	((_ #t ?temp ?strp (?? ?proc ?form ...) ?clause ...)
	 #'(if (?proc ?temp)
	       (begin ?form ...)
	       (regex-match-case #t ?temp ?strp ?clause ...)))
	((_ #t ?temp ?strp (?re ?bind ?form ...) ?clause ...)
	 #'(regex-match-if (and ?strp (looking-at ?re ?temp))
			   ?bind
			   (begin ?form ...)
			   (regex-match-case #t ?temp ?strp ?clause ...)))
	((_ #t ?temp strip ?clause)
	 (syntax-violation 'regex-match-case
			   "malformed regex-match-case"
			   (unwrap-syntax x)))
	((_ ?str ?clause ...)
	 #'(let* ((temp ?str)
		(strp (string? temp)))
	   (regex-match-case #t temp strp ?clause ...))))))
)
