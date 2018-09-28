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
	    jmespath:expression

	    parse-jmespath
	    jmespath-parse-error?
	    )
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (text json parser)
	    (text json jmespath conditions)
	    (match)
	    (srfi :14 char-sets)
	    (srfi :121 generators)
	    (srfi :127 lseqs))

(define-condition-type &jmespath:parse &jmespath
  make-jmespath-parse-error jmespath-parse-error?)

(define (jmespath-parse-error message . irr)
  (raise (condition
	  (make-jmespath-parse-error)
	  (make-who-condition 'jmespath:expression)
	  (make-message-condition message)
	  (make-irritants-condition irr))))
(define ($error message . irr)
  (lambda (in)
    (jmespath-parse-error message irr)))

(define $cs $char-set-contains?)
(define ws ($cs (char-set #\space #\tab #\newline #\return)))
(define (op token)
  ($seq ($many ws)
	(apply $seq (map $eqv? (string->list token)))
	($many ws)))
(define jmespath:backquote ($cs (char-set #\`)))

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
(define jmespath:code-point
  ($do (c* ($repeat ($cs char-set:hex-digit) 4))
       ($return (string->number (list->string c*) 16))))
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
	     ($do (($eqv? #\u))
		  (cp jmespath:code-point)
		  ($if (<= #xd800 cp #xdbff)
		       ($do (($eqv? #\\)) (($eqv? #\u))
			    (cp2 jmespath:code-point)
			    ($if (<= #xdc00 cp2 #xdfff)
				 ($return (integer->char
					   (+ #x10000
					      (* (- cp #xd800) #x400)
					      (- cp2 #xdc00))))
				 ($error "Invalid unicode code point" cp cp2)))
		       ($lazy ($return (integer->char cp))))))))
(define jmespath:quoted-string
  ($do (($eqv? #\"))
       (c* ($many ($or jmespath:escaped-char jmespath:unescaped-char) 1))
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
	    (v jmespath:expression)
	    ((op "]"))
	    ($return `(filter ,v)))
       ($do ((op "["))
	    (v ($or jmespath:slice-expression
		    ($do (v jmespath:number) ($return `(index ,v)))
		    ($seq star ($return '(*)))))
	    ((op "]"))
	    ($return v))
       ($seq (op "[]") ($return '(flatten)))))

(define jmespath:literal
  ($do ((op "`"))
       (v ($parameterize ((*json:escape-required* jmespath:backquote))
	    ($guard (e ((json-parse-error? e)
			($error "Failed to parse JSON" (condition-message e))))
	      json:parser)))
       ((op "`"))
       ($return `(quote ,v))))

;; This is fxxking lie. The compiliance test even expect to accept
;; all characters including control chars (e.g. newline) as the
;; implementations do.
;; (define raw-string-set
;;   ($cs (char-set-union
;; 	(ucs-range->char-set #x20 (+ #x26 1))
;; 	(ucs-range->char-set #x28 (+ #x5B 1))
;; 	(ucs-range->char-set #x5D (+ #x10FFFF 1)))))
(define jmespath:raw-string-char
  ($or ($seq ($eqv? #\\) ($eqv? #\'))
       ($seq ($eqv? #\\) ($eqv? #\\) ($return "\\\\"))
       ($cs (char-set-difference char-set:full (char-set #\')))))
(define (jmespath:raw-string-char->string c*)
  (let-values (((out extract) (open-string-output-port)))
    (for-each (lambda (c)
		(if (char? c) (put-char out c) (put-string out c))) c*)
    (extract)))
(define jmespath:raw-string
  ($do (($many ws)) (($eqv? #\'))
       (c* ($many jmespath:raw-string-char))
       (($eqv? #\')) (($many ws))
       ($return `(quote ,(jmespath:raw-string-char->string c*)))))
;; β
(define beta
  ($or star
       ($seq (op "@") ($return '@))
       jmespath:bracket-specifier
       jmespath:not-expression
       jmespath:paren-expression
       jmespath:multi-select-list
       jmespath:multi-select-hash
       jmespath:literal
       jmespath:function-expression
       jmespath:raw-string
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

(define ($g? x) (eq? '$g x))
(define (handle-group e)
  (define (strip-nested-group e)
    ;; ($g ($g e)) -> ($g e)
    (match e
      (((? $g?) ((? $g?) e*)) (strip-nested-group (cadr e)))
      (else e)))
  (let ((e1 (cadr (strip-nested-group e))))
    ;; handling ((ref a b c) (or (ref d e f))) case here
    (if (and (= (length e1) 2) (pair? (cadr e1)))
	`($g (,(caadr e1) ,(car e1) ,@(cdadr e1)))
	(list '$g e1))))
(define (handle-sequence op e e*)
  (define (op? x) (eq? op x))
  #;(begin (display op) (newline)
	 (display e) (newline)
	 (display e*) (newline) (newline))
  (if (null? e*)
      (match e
	(((? op?) r ...) e) ;; no extra
	(((? $g?) r ...) (list op (handle-group e)))
	((e0 ((? op?) r ...)) (cons* op e0 r))
	((e0 (? pair? e1)) (cons* (car e1) (list op e0) (cdr e1)))
	(_ (list op e)))
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
       (e jmespath:expression*)
       ($return `(,c ,e))))

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
		($or jmespath:multi-select-list
		     jmespath:multi-select-hash
		     jmespath:function-expression
		     jmespath:identifier
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
  (let ()
    (define comparators '(< <= = >= > !=))
    (define (comparator? x) (memq x comparators))
    (define (conjunction? x) (memq x '(pipe or and)))
    (define (handle-comparator e)
      (let ((e0 (car e)) (e1 (cadr e)))
	(if (= (length e1) 2)
	    `(,(car e1) ,e0 ,@(cdr e1))
	    ;; I don't know what this is...
	    e)))
    (define (handle-sequence f e*)
      #;(begin (display f) (newline)
	     (display e*) (newline)
	     (newline))
      (if (null? e*)
	  (match f
	    (((? pair?) ((? comparator?) r* ...)) (handle-comparator f))
	    (else f))
	  (match e*
	    (((? pair? e0) e1 ...) ;; e* = ((ref a b) expr) pattern
	     ;; we need to add 'f' to e0
	     (handle-sequence (cons (handle-sequence f e0) e1) '()))
	    (((? (lambda (x) (eq? 'index x))) ignore ...) ;; merging index
	     `(ref ,f ,e*))
	    (((? comparator? cmp) ((? conjunction? c) e1 e2 ...))
	     ;; (= (or 'a (= a '2))) -> ((= f 'a) (or (= a '2)))
	     `((,cmp ,f ,e1) (,c ,@e2)))
	    ;; e* = (pipe expr)
	    ;; f  = expr
	    (else `(,(car e*) ,f ,@(cdr e*))))))
    ($do (f beta)
	 (e* jmespath:expression**)
	 ($return (handle-sequence f e*)))))

(define (resolve-sequence e)
  (define (resolve e)
    (if (pair? (cadr e))
	(let ((e0 (car e)) (e1 (cadr e)))
	  ;; we put e0 into the first deepest command of the first nested
	  ;; expression
	  (let loop ((e1 e1) (e* (cdr e1)))
	    (cond ((null? e*) `(,(car e1) ,e0 ,@(cdr e1)))
		  ((and (pair? (car e*)) (memq (caar e*) '(pipe or and)))
		   (cons* (car e1)
			  (loop (car e*) (cdar e*))
			  (cddr e1)))
		  (else `(,(car e1) ,e0 ,@(cdr e1))))))
	e))
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
  ;; (display e) (newline)
  (flatten-group
   (if (and (pair? e) (pair? (car e)))
       (if (null? (cdr e))
	   e
	   (resolve e))
       e)))
(define jmespath:expression
  ($do (e jmespath:expression*)
       ($return (resolve-sequence e))))

(define (parse-jmespath path)
  (define lseq (generator->lseq (string->generator path)))
  (let-values (((s v nl) (jmespath:expression lseq)))
    (unless (and (parse-success? s) (null? nl))
      (jmespath-parse-error "Failed to parse JMESPath" path))
    v))
)
