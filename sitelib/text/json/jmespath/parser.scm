;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/jmespath/parser.scm - JMESPath parser
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

;; Reference
;; http://jmespath.org/specification.html

;;; Left recursion elimiated version of expression
;; left recursion elimination
;; e ::= e a | a ->
;; e ::= a e'
;; e' ::= a e' | ε 

;; expression ::= "@" # current-node
;;              | "*" # wild card
;;              | "!" expression # not-expression
;;              | "(" expression ")" # paren-expression
;;                # multi-select-list
;;              | "[" "]" # empty handling specially
;;              | "[" (expression *( "," expression) ) "]"
;;                # multi-select-hash
;;              | "{" (keyval-expr *( "," keyval-expr) ) "}"
;;                # function-expression
;;              | unquoted-string ( no-args / one-or-more-args )
;;              | "`" json-value "`"  # literal
;;              | "'" *raw-string-char "'" # raw-string
;;              | identifier !("[" | ".")
;;              | bracket-specifier # part of index-expression
;;              | sub-expression # expression "." ( bla ...)
;;              | index-expression # expression bracket-specifier
;;              | comparator-expression
;;              | or-expression
;;              | and-expression
;;              | pipe-expression
;; sub-expression ::= "." ( identifier
;;                        | multi-select-list
;;                        | multi-select-hash
;;                        | function-expression
;;                        | "*")
;;                  | ε 
;; index-expression :: = bracket-specifier expression | ε 
;; comparator-expression ::= comparator expression
;; or-expression ::= "||" expression
;; and-expression ::= "&&" expression
;; pipe-expression ::= "|" expression

#!nounbound
(library (text json jmespath parser)
    (export jmespath:unquoted-string
	    jmespath:quoted-string
	    jmespath:identifier
	    jmespath:not-expression
	    jmespath:paren-expression
	    jmespath:multi-select-list
	    jmespath:multi-select-hash
	    jmespath:function-expression
	    jmespath:bracket-specifier
	    jmespath:literal
	    jmespath:raw-string
	    jmespath:expression)
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (text json parser)
	    (srfi :14))

(define $cs $char-set-contains?)
(define ws ($cs (char-set #\space #\tab #\newline #\return)))
(define (op token)
  ($seq ($many ws)
	(apply $seq (map $eqv? (string->list token)))
	($many ws)))
(define jmespath:unquoted-string
  (let ((first-set ($cs (char-set-union (ucs-range->char-set #x41 (+ #x5A 1))
					(ucs-range->char-set #x61 (+ #x7A 1))
					(char-set #\_))))
	(next-set ($cs (char-set-union
			(char-set-intersection char-set:ascii
					       char-set:letter+digit)
			(char-set #\_)))))
    ($do (c first-set)
	 (c* ($many next-set))
	 ($return (apply string c c*)))))
(define jmespath:unescaped-char
  ($cs (char-set-union
	(ucs-range->char-set #x20 (+ #x21 1))
	(ucs-range->char-set #x23 (+ #x5B 1))
	(ucs-range->char-set #x5D (+ #x10FFFF 1)))))
(define jmespath:escaped-char
  ($seq ($eqv? #\\)
	($or ($eqv? #\")
	     ($eqv? #\\)
	     ($eqv? #\/)
	     ($seq ($eqv? #\b) ($return #\backspace))
	     ($seq ($eqv? #\f) ($return #\page))
	     ($seq ($eqv? #\n) ($return #\newline))
	     ($seq ($eqv? #\r) ($return #\return))
	     ($seq ($eqv? #\t) ($return #\tab))
	     ($do (($eqv? #\x))
		  (c* ($repeat ($cs char-set:hex-digit) 4))
		  ($return (integer->char
			    (string->number (list->string c*) 16)))))))
(define jmespath:quoted-string
  ($do (($eqv? #\"))
       (c* ($many ($or jmespath:unescaped-char jmespath:escaped-char) 1))
       (($eqv? #\"))
       ($return (list->string c*))))

(define jmespath:identifier
  ($or jmespath:unquoted-string
       jmespath:quoted-string))

(define jmespath:not-expression
  ($do ((op "!")) (e jmespath:expression*) ($return `(not ,e))))
(define jmespath:paren-expression
  ;; mark this is paren
  ($do ((op "(")) (e jmespath:expression*) ((op ")")) ($return `($g ,e))))
(define jmespath:multi-select-list
  ($do ((op "["))
       (e ($do (e jmespath:expression*)
	       (c* ($many ($seq (op ",") jmespath:expression*)))
	       ($return (cons e c*))))
       ((op "]"))
       ($return e)))
(define jmespath:keyval-expr
  ($do (k jmespath:identifier)
       ((op ":"))
       (v jmespath:expression*)
       ($return (cons k v))))
(define jmespath:multi-select-hash
  ($do ((op "{"))
       (e ($do (e jmespath:keyval-expr)
	       (c* ($many ($seq (op ",") jmespath:keyval-expr)))
	       ($return (cons e c*))))
       ((op "}"))
       ($return (list->vector e))))
(define function-arg
  ($or ($do ((op "&")) (e jmespath:expression*) ($return `(& ,e)))
       ($lazy jmespath:expression*)))
(define one-or-more
  ($do ((op "("))
       (args ($do (arg function-arg)
		  (arg* ($many ($seq (op ",") function-arg)))
		  ($return (cons arg arg*))))
       ((op ")"))
       ($return args)))
(define no-arg ($seq (op "(") (op ")")))
(define jmespath:function-expression
  ($do (name jmespath:unquoted-string)
       (arg ($or no-arg one-or-more))
       ($return `(,(string->symbol name) ,@arg))))
(define star ($do ((op "*")) ($return '*)))

(define jmespath:number
  (let ((num-set ($cs (char-set-intersection
		       char-set:ascii char-set:digit))))
    ($do (sign ($optional ($eqv? #\-) #\+))
	 (n* ($many num-set 1))
	 ($return (string->number (apply string sign n*))))))

(define jmespath:slice-expression
  ($do (n1 ($optional jmespath:number))
       ((op ":"))
       (n2 ($optional jmespath:number))
       (n3 ($optional ($seq (op ":") ($optional jmespath:number 1)) 1))
       ;; start stop step
       ($return `(slice ,n1 ,n2 ,n3))))

(define jmespath:bracket-specifier
  ($or ($do ((op "[?"))
	    (v jmespath:expression*)
	    ((op "]"))
	    ($return `(filter ,v)))
       ($do ((op "["))
	    (v ($or jmespath:slice-expression
		    ($do (v ($or jmespath:number star)) ($return `(index ,v)))))
	    ((op "]"))
	    ($return v))
       ($seq (op "[]") ($return '(flatten)))))

(define jmespath:literal
  ($do ((op "`"))
       (v json:parser)
       ((op "`"))
       ($return `(quote ,v))))

(define raw-string-set
  ($cs (char-set-union
	(ucs-range->char-set #x20 (+ #x26 1))
	(ucs-range->char-set #x28 (+ #x5B 1))
	(ucs-range->char-set #x5D (+ #x10FFFF 1)))))
(define jmespath:raw-string-char
  ($or raw-string-set
       ($seq ($eqv? #\\) ($or raw-string-set ($eqv? #\\) ($eqv? #\')))))
(define jmespath:raw-string
  ($do ((op "'"))
       (c* ($many jmespath:raw-string-char))
       ((op "'"))
       ($return `(quote ,(list->string c*)))))
;; β
(define beta
  ($or star
       ($seq (op "@") ($return '@))
       jmespath:not-expression
       jmespath:paren-expression
       jmespath:multi-select-list
       jmespath:multi-select-hash
       jmespath:literal
       jmespath:function-expression
       jmespath:raw-string
       jmespath:bracket-specifier
       ;; used by function-expression so must be here
       jmespath:identifier))

(define jmespath:comparator
  ($or ($seq (op "<=") ($return '<=))
       ($seq (op "<")  ($return '<))
       ($seq (op "==") ($return '=))
       ($seq (op ">=") ($return '>=))
       ($seq (op ">")  ($return '>))
       ($seq (op "!=") ($return '!=))))
(define jmespath:pipe (op "|"))
(define jmespath:or (op "||"))
(define jmespath:and (op "&&"))

(define $epsilon (lambda (in) (return-result '() in)))
(define (handle-group e)
  (let ((e1 (cadr e)))
    `($g (,(caadr e1) ,(car e1) ,@(cdadr e1)))))
(define (handle-sequence op e e*)
  #;(begin (display op) (newline)
	 (display e) (newline)
	 (display e*) (newline) (newline))
  (if (null? e*)
      (cond ((not (pair? e)) (list op e))
	    ((eq? (car e) op) e)
	    ((eq? (car e) '$g)
	     (list op (handle-group e)))
	    ((and (pair? (car e)) (pair? (cadr e)))
	     (if (eq? op (caadr e))
		 (cons* op (car e) (cdadr e))
		 (cons* (caadr e) (list op (car e)) (cdadr e))))
	    (else (list op e)))
      (cons e e*)))
(define jmespath:pipe-expression
  ($do (e ($seq jmespath:pipe ($lazy jmespath:expression*)))
       (e* ($lazy jmespath:expression**))
       ($return (handle-sequence 'pipe e e*))))
(define jmespath:or-expression
  ($do (e ($seq jmespath:or ($lazy jmespath:expression*)))
       (e* ($lazy jmespath:expression**))
       ($return (handle-sequence 'or e e*))))
(define jmespath:and-expression
  ($do (e ($seq jmespath:and ($lazy jmespath:expression*)))
       (e* ($lazy jmespath:expression**))
       ($return (handle-sequence 'and e e*))))

(define jmespath:comparator-expression
  ($do (c jmespath:comparator)
       (e2 jmespath:expression*)
       ($return `(,c ,e2))))

(define (merge-ref r r2)
  #;(begin (display r) (newline)
	 (display r2) (newline)
	 (newline))
  (cond ((null? r2) `(ref ,r))
	((eq? 'ref (car r2)) `(ref ,r ,@(cdr r2)))
	((pair? (car r2)) (cons (merge-ref r (car r2)) (cdr r2)))
	((eq? 'index (car r2)) `(ref ,r ,r2))
	(else (list `(ref ,r) r2))))

(define jmespath:sub-expression
  ($do (e ($seq (op ".")
		($or jmespath:identifier
		     jmespath:multi-select-list
		     jmespath:multi-select-hash
		     jmespath:function-expression
		     star)))
       (e* ($lazy jmespath:expression**))
       ($return (merge-ref e e*))))

(define jmespath:index-expression
  ($do (b jmespath:bracket-specifier)
       (e jmespath:expression**)
       ($return (merge-ref b e))))

(define jmespath:expression**
  ($or jmespath:index-expression
       jmespath:sub-expression
       jmespath:comparator-expression
       jmespath:pipe-expression ;; pipe must be before the or
       jmespath:or-expression
       jmespath:and-expression
       $epsilon))

(define jmespath:expression*
  (letrec ((handle-sequence (lambda (f e*)
			      #;(begin (display f) (newline)
				     (display e*) (newline)
				     (newline))
			      (cond ((null? e*) f)
				    ((pair? (car e*))
				     (cons (handle-sequence f (car e*))
					   (cdr e*)))
				    ((and (pair? f) (pair? (car f)))
				     (cons* (car e*)
					    (resolve-sequence f)
					    (cdr e*)))
				    ((and (pair? e*) (eq? 'index (car e*)))
				     `(ref ,f ,e*))
				    (else `(,(car e*) ,f ,@(cdr e*)))))))
    ($do (f beta)
	 (e* jmespath:expression**)
	 ($return (handle-sequence f e*)))))

(define (resolve-sequence e)
  (define (resolve e)
    (let ((e0 (car e)) (e1 (cadr e)))
      ;; we put e0 into the first deepest command of the first nested
      ;; expression
      (let loop ((e1 e1) (e* (cdr e1)))
	(cond ((null? e*) `(,(car e1) ,e0 ,@(cdr e1)))
	      ((and (pair? (car e*)) (memq (caar e*) '(pipe or and)))
	       (cons* (car e1)
		      (loop (car e*) (cdar e*))
		      (cddr e1)))
	      (else `(,(car e1) ,e0 ,@(cdr e1)))))))
  (define (flatten-group e)
    (cond ((pair? e)
	   (if (eq? (car e) '$g)
	       (let ((e1 (cadr e)))
		 (flatten-group 
		  (if (and (pair? e1) (pair? (car e1)))
		      (handle-group e)
		       e1)))
	       (cons (flatten-group (car e))
		     (flatten-group (cdr e)))))
	  (else e)))
  (flatten-group
   (if (and (pair? e) (pair? (car e)))
       (resolve e)
       e)))
(define jmespath:expression
  ($do (e jmespath:expression*)
       ($return (resolve-sequence e))))

)
