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
  ($do ((op "!")) (e jmespath:expression) ($return `(not ,e))))
(define jmespath:paren-expression
  ;; mark this is paren
  ($do ((op "(")) (e jmespath:expression) ((op ")")) ($return `($g ,e))))
(define jmespath:multi-select-list
  ($do ((op "["))
       (e ($do (e jmespath:expression)
	       (c* ($many ($seq (op ",") jmespath:expression)))
	       ($return (cons e c*))))
       ((op "]"))
       ($return e)))
(define jmespath:keyval-expr
  ($do (k jmespath:identifier)
       ((op ":"))
       (v jmespath:expression)
       ($return (cons k v))))
(define jmespath:multi-select-hash
  ($do ((op "{"))
       (e ($do (e jmespath:keyval-expr)
	       (c* ($many ($seq (op ",") jmespath:keyval-expr)))
	       ($return (cons e c*))))
       ((op "}"))
       ($return (list->vector e))))
(define function-arg
  ($or ($do ((op "&")) (e jmespath:expression) ($return `(& ,e)))
       ($lazy jmespath:expression)))
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

(define jmespath:pipe-expression
  ($do (e ($seq jmespath:pipe ($lazy jmespath:expression*)))
       ($return (list 'pipe e))))
(define jmespath:or-expression
  ($do (e ($seq jmespath:or ($lazy jmespath:expression*)))
       ($return (list 'or e))))
(define jmespath:and-expression
  ($do (e ($seq jmespath:and ($lazy jmespath:expression*)))
       ($return (list 'and e))))

(define jmespath:comparator-expression
  ($do (c jmespath:comparator)
       (e jmespath:expression*)
       ($return (list c e))))

(define (resolve-ref e e*)
  (define (add-ref e e*)
    (match e
      (('ref i* ...) `(,@e ,@e*))
      ((a . d) `(ref ,@e ,@e*))
      ;; this will never happen...
      (else `(ref ,e ,@e*))))
  #;(begin (display e) (newline)
	 (display e*) (newline)
	 (newline))
  (match e*
    (() (add-ref e e*)) ;; ε
    (('ref i* ...) (add-ref e i*)) ;; merge it
    ((('index n) e*) (resolve-ref `(,@e (index ,n)) e*))
    ((('*) e*) (resolve-ref `(,@e (*)) e*))
    (((? symbol? c) ('ref i* ...) e* ...)
     `(,c ,(add-ref e '()) (ref ,@i*) ,@e*))
    ;; 'a.b | c' or so
    (((? symbol? c) e0 e* ...)
     `(,c ,(add-ref e '()) ,e0 ,@e*))
    ;; ???
    (else (add-ref e (list e*)))))
  
(define jmespath:sub-expression
  ($do (e ($many ($seq (op ".")
		       ($or jmespath:multi-select-list
			    jmespath:multi-select-hash
			    jmespath:function-expression
			    jmespath:identifier
			    star)) 1))
       (e* ($lazy jmespath:expression**))
       ($return (resolve-ref e e*))))

(define jmespath:index-expression
  ($do (b jmespath:bracket-specifier)
       (e jmespath:expression**)
       ($return (list b e))))

(define jmespath:expression**
  ($or jmespath:index-expression
       jmespath:sub-expression
       jmespath:comparator-expression
       jmespath:pipe-expression
       jmespath:or-expression
       jmespath:and-expression
       $epsilon))

(define (conjunction? x) (memq x '(pipe or and)))
(define ($g? x) (eq? '$g x))
(define jmespath:expression*
  (let ()
    (define comparators '(< <= = >= > !=))
    (define (comparator? x) (memq x comparators))
    ;; e0 op e1 -> (op e0 e1)
    (define (resolve-operators e0 e1)
      #;(begin (write e0) (newline)
	     (write e1) (newline)
	     (newline))
      (match e1
	(() e0) ;; ε
	((('index n) e)
	 (match e0
	   (('ref e* ...) (resolve-operators `(ref ,@e* (index ,n)) e))
	   (else (resolve-operators `(ref ,e0 (index ,n)) e))))
	;; pipe or and
	;; (pipe (or e1 e2)) -> e0 | e1 || e2
	;; however the priority should go to pipe since it's before or
	(((? conjunction? c1) ((? conjunction? c2) e1 e2))
	 (if (eq? c1 c2)
	     ;; (or (or e ...)) so just inject
	     `(,c2 ,e0 ,e1 ,e2)
	     ;; we do recursively
	     `(,c2 ,(resolve-operators e0 `(,c1 ,e1)) ,e2)))
	(((? conjunction? c1) ('ref i* ...) e e* ...)
	 ;; here we need to inject e0 into e1 if the expression
	 ;; is under the context of sub-expression.
	 ;; means e1 must be one of the sub-expression rule after '.'
	 (if (or (string? e0) ;; identifier
		 (vector? e0)  ;; multi-select-hash
		 ;; function-expression or multi-select-list
		 (and (pair? e0) (not ($g? (car e0))))
		 (eq? '* e0))
	     ;; okay, we need to resolve the operator priority.
	     (resolve-operators `(ref ,e0 ,@i*) `(,c1 ,e ,@e*))
	     (cons* (car e1) e0 (cdr e1))))
	;; (= (or e e* ...))
	;; with this parser, comparator will be constructed as if it's
	;; unary operator (e.g. (= "a")). so it always requires beta
	;; to be injected, however if the conjunction is there, it'd be
	;; like the above so handle it
	(((? comparator? cmp) ((? conjunction? c) e e* ...))
	 `(,c (,cmp ,e0 ,e) ,@e*))
	(else (cons* (car e1) e0 (cdr e1)))))
    ($do (f beta)
	 (e* jmespath:expression**)
	 ($return (resolve-operators f e*)))))

(define (resolve-sequence e)
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
  (define (flatten-group e)
    (match e
      (((? $g?) ((? pair? x) rest)) (flatten-group (handle-group e)))
      (((? $g?) e0) (flatten-group e0))
      ((a . d) (cons (flatten-group a) (flatten-group d)))
      (_ e)))
  (define (flatten-operator e)
    (match e
      (((? conjunction? c1) ((? conjunction? c2) e1* ...) e2* ...)
       (if (eq? c1 c2)
	   (flatten-operator `(,c1 ,@e1* ,@e2*))
	   (cons* c1 (flatten-operator (cons c2 e1*)) e2*)))
      ((a . d) (cons a (flatten-operator d)))
      (else e)))
  ;;(display e) (newline)
  (flatten-operator (flatten-group e)))
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
