;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sql/parser.scm - SQL parser
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

(library (text sql parser)
    (export parse-sql
	    sql-parser-error?
	    sql-parser-position
	    sql-parser-expected)
    (import (rnrs)
	    (packrat)
	    (text sql scanner)
	    (srfi :13 strings)
	    (sagittarius)) ;; for port-info

;; packrat specific tokenizer
(define (make-generator p)
  (let ((file (cond ((car (port-info p))) (else "<?>")))
	(scanner (make-sql-scanner p)))
    (lambda ()
      (let-values (((token pos line) (scanner)))
	(values (make-parse-position file line pos) token)))))

;; consider the following case:
;;   select f.u&"$42" uescape '$' from foo f;
;; the parser should not change the structure of SQL thus
;; we can't resolve what unicode delimited identifiers 
;; contain. so those qualified identifiers need to be
;; a list with prefix value (can't concatenate)
;;
;; use '~ to represents qualifier because it seems slot access
;; e.g. 'foo.bar' = (~ foo bar)
;; 
(define (concate-identifier base follow)
  (if (and (pair? follow) (eq? (car follow) '~))
      `(~ ,base ,(cdr follow))
      `(~ ,base ,follow)))

(define (construct-table name as&cols)
  (if as&cols
      (let ((as (car as&cols))
	    (cols (cadr as&cols)))
	(if (null? cols)
	    `(as ,name ,as)
	    ;; `from` foo f(a b) -> (as foo (f a b))
	    `(as ,name (,as ,@cols))))
      name))

;; I think '^' is common enough for string concatenation.
;; we can't use '||'
(define (concate-character f c)
  (if (pair? c) 
      `(^ ,f ,@(cdr c))
      `(^ ,f ,c)))

(define (resolve-term* op term)
  (if (and (pair? term) (eq? op (car term)))
      term
      `(,op ,term)))
;; handling numeric operators
;; 1*1 -> (* 1 1)
;; term  ::= factor
;; term* ::= (/ term) | (* term)
;; factor ::= value | (+ value) | (- value)
;; value is value expression so can be column, string, number, etc.
(define (resolve-numeric-term factor term*)
  (let ((expr (car term*)))
    `(,expr ,factor ,@(cdr term*))))

(define sql-parser
  (packrat-parser
   (begin
     stmt)
   (stmt ((s <- query-specification) s)
	 ;; TODO more
	 )
   ;; 7.12 query specification
   (query-specification (('select c <- select-list t <- table-expression) 
			 (cons* 'select c t))
			;; select (distinct|all) column from table
			(('select q <- set-qualifier 
				  c <- select-list 
				  t <- table-expression) 
			 (cons* 'select q c t))
			;; extension: select 1+1; or so
			(('select c <- select-list) (list 'select c)))
   (select-list (('#\*) '*)
		((s <- select-sublist) s))
   (select-sublist ((d <- derived-column s* <- select-sublist*) (cons d s*))
		   ((q <- qualified-asterisk s* <- select-sublist*) 
		    (cons q s*)))
   (select-sublist* (('#\, s <- select-sublist) s)
		    (() '()))
   ;; TODO 'select foo.*' or so
   (qualified-asterisk ((c <- identifier-chain '#\. '#\*) 
			(concate-identifier c '(*))))

   (derived-column ((v <- value-expression 'as c <- column-name) 
		    (list 'as v c))
		   ((v <- value-expression c <- column-name) 
		    (list 'as v c))
		   ((v <- value-expression) v))
   ;; qualified-identifier
   (identifier-chain  ((i <- identifier i* <- identifier-chain*)
		       (if (null? i*)
			   i
			   (concate-identifier i i*))))
   (identifier-chain* (('#\. i <- identifier-chain) i)
		      (() '()))

   (column-name ((i <- identifier) i))

   (set-qualifier (('distinct) 'distinct)
		  (('all) 'all))

   (column-reference ((i <- identifier-chain) i)
		     (('module '#\. i <- identifer '#\. m <- column-name)
		      `(module ,i ,m)))

   ;; table
   (table-expression ((f <- from-clause 
		       w <- where-clause
		       g <- group-by-clause
		       h <- having-clause
		       wd <- window-clause)
		      `(,f ,@w ,@g ,@h ,@wd)))
   (from-clause (('from t <- table-reference-list) (cons 'from t)))

   (table-reference-list ((t <- table-reference t* <- table-reference-list*)
			  (cons t t*)))
   (table-reference-list* (('#\, t <- table-reference-list) t)
			  (() '()))
   ;; TODO sample clause
   (table-reference #;((t <- table-primary-or-join s <- sample-clause) 
		     (cons t s))
		    ((t <- table-primary-or-join) t))
   (table-primary-or-join ((t <- table-primary) t)
			  #;((j <- join-clause) j))
   ;; TODO properly done
   ;; helper for AS alias(col ...)
   (table-as-expr (('as n <- correlation-name 
		    '#\( l <- derived-column-list '#\))
		   (list n l))
		  (('as n <- correlation-name) (list n '()))
		  ((n <- correlation-name '#\( l <- derived-column-list '#\))
		   (list n l))
		  ((n <- correlation-name) (list n '()))
		  )
   (table-as-expr* ((as <- table-as-expr) as)
		   (() #f))
   (correlation-name ((n <- identifier) n))
   
   (table-primary ((t <- tables as <- table-as-expr*) 
		   (construct-table t as))
		  ((t <- alias-tables as <- table-as-expr)
		   (construct-table t as))
		  (('#\( t <- joined-table '#\)) t))
   (tables ((t <- table-or-query-name) t)
	   ((t <- only-spec) t))
   (alias-tables ((t <- derived-table) t)
		 ((t <- lateral-derived-table) t)
		 ((t <- collection-derived-table) t)
		 ((t <- table-function-derived-table) t))
   
   (only-spec (('only '#\( t <- table-or-query-name '#\)) `(only ,t)))
   (lateral-derived-table (('lateral t <- table-subquery) `(lateral ,t)))
   (collection-derived-table 
    (('unnest '#\( v <- collection-value-expression '#\) 'with 'ordinality)
     ;; TODO what's this?
     `(unnest/ordinality ,t))
    (('unnest '#\( v <- collection-value-expression '#\)) `(unnest ,t)))
   (table-function-derived-table 
    (('table '#\( v <- collection-value-expression '#\)) `(table ,t)))
   (derived-table ((d <- table-subquery) d))

   (table-or-query-name ((t <- table-name) t)
			((q <- query-name) q))
   (derived-column-list ((l <- column-name-list) l))
   (column-name-list ((c <- column-name c* <- column-name-list*) (cons c c*)))
   (column-name-list* (('#\, c <- column-name-list) c)
		      (() '()))
   (table-name ((l <- local-or-schema-qualified-name) l))
   (query-name ((i <- identifier) l))
   
   ;; where
   (where-clause (('where s <- search-condition) (list (cons w s)))
		 (() '()))

   (search-condition ((b <- boolean-value-expression) b))

   (group-by-clause (('group 'by g* group-by-element-list)
		     (list (list 'group-by g*)))
		    (('group 'by s <- set-qualifier g* <- group-by-element-list)
		     (list (list 'group-by s g*)))
		    (() '()))
   ;; TODO 
   (group-by-element-list (() '()))
   ;; having
   (having-clause (('having s <- search-condition) (list (cons 'having s)))
		  (() '()))
   ;; window
   (window-clause (('windows w* <- window-definition-list) 
		   (list (cons 'windows w*)))
		  (() '()))
   ;; TODO
   (window-definition-list (() '()))

   ;; 6.25 value expression
   ;; value
   (value-expression ((c <- common-value-expression) c)
		     ((b <- boolean-value-expression) b)
		     ((r <- row-value-expression) r))

   (common-value-expression ((n <- numeric-value-expression) n)
			    ((s <- string-value-expression)  s)
			    ;;((d <- datetime-value-expression) d)
			    ;;((i <- interval-value-expression) i)
			    ;;((u <- user-define-type-value-expression) u)
			    ;;((r <- reference-value-expression) r)
			    ((v <- value-expression-primary) v)
			    #;((c <- collection-value-expression) c))
   ;; these 2 are the same so we don't need it
   ;; (user-define-type-value-expression ((v <- value-expression-primary) v)
   ;; (reference-value-expression ((v <- value-expression-primary) v)
   (collection-value-expression ((a <- array-value-expression) a)
				((m <- multiset-value-expression) m))
   ;; 6.26 numeric value expression
   (numeric-value-expression ((t <- term n* <- numeric-value-expression*)
			      (if (null? n*)
				  t
				  (resolve-numeric-term t n*))))
   (numeric-value-expression* (('#\+ n <- numeric-value-expression) 
			       (resolve-term* '+ n))
			      (('#\- n <- numeric-value-expression)
			       (resolve-term* '- n))
			      ;; rather ugly
			      (((! 'concat)) '()))
   (term ((f <- factor t <- term*) (if (null? t) f (resolve-numeric-term f t))))
   (term* (('#\* t <- term) (resolve-term* '* t))
	  (('#\/ t <- term) (resolve-term* '/ t))
	  ;; rather ugly
	  (((! 'concat)) '()))

   (factor ((s <- sign n <- numeric-primary) (list s n))
	   ((n <- numeric-primary) n))
   (sign (('#\+) '+)
	 (('#\-) '-))
   (numeric-primary ((v <- value-expression-primary) v)
		    #;((f <- numeric-value-function) f))

   ;; 6.29 string value expression
   (string-value-expression ((c <- character-value-expression) c)
			    ;; no blob, it's the same
			    #;((b <- blob-value-expression) b))
   (character-value-expression ((f <- character-factor c <- concatenation)
				(if (null? c)
				    f
				    (concate-character f c))))
   (concatenation (('concat f <- character-value-expression) f)
		  (() '()))
   (character-factor ((c <- character-primary cl <- collate-clause)
		      (cons c cl))
		     ((c <- character-primary) c))
   (character-primary ((v <- value-expression-primary) v)
		      #;((s <- string-value-function) s))

   ;; 6.3
   (value-expression-primary ((p <- parenthesized-value-expression) p)
			     ((n <- nonparenthesized-value-expression) n))
   (parenthesized-value-expression (('#\( v <- value-expression '#\)) (list v)))
   (nonparenthesized-value-expression ((n <- 'number) n)
				      ((b <- 'bit-string) b)
				      ((s <- 'string) s)
				      ((c <- column-reference) c)
				      ;;((s <- set-function-specification) s)
				      ;;((w <- window-function) w)
				      ((s <- scalar-subquery) s)
				      ;;((c <- case-expression) c)
				      ;;((c <- cast-specification) c)
				      ;;((f <- field-reference) f)
				      ;;((s <- subtype-treatment) s)
				      ;;((m <- method-invocation) m)
				      ;;((s <- static-method-invocation) s)
				      ;;((a <- attribute-or-method-reference) a)
				      ;;((r <- reference-resolution) r)
				      ;;((c <- collection-value-constructor) c)
				      ;;((a <- array-element-reference) a)
				      ;;((m <- multiset-element-reference) m)
				      ;;((r <- routine-invocation) r)
				      ;;((n <- next-value-expression) n)
				      )
   
   ;; 7.1 row value constructor
   (row-value-constructor-predicant ((c <- common-value-expression) c)
				    ((b <- boolean-predicand) b)
				    #;((e <- explicit-row-value-constructor) e))
   ;; 7.2 row value expression
   (row-value-expression ((n <- nonparenthesized-value-expression) n)
			 #;((e <- explicit-row-value-constructor) e))
   (row-value-predicand ((n <- nonparenthesized-value-expression) n)
			((r <- row-value-constructor-predicant) r))

   ;; 
   (boolean-value-expression ((t <- boolean-term 
			       b* <- boolean-value-expression*) 
			      ;; TODO handle 'or
			      (cons t b*)))
   (boolean-value-expression* (('or b <- boolean-term) (list 'or b))
			      (() '()))
   (boolean-term ((f <- boolean-factor t* <- boolean-term*) (cons f t*)))
   (boolean-term* (('and b <- boolean-term) (list 'and b))
		  (() ()))
   (boolean-factor (('not t <- boolean-test) (list 'not t))
		   ((t <- boolean-test) t))
   (boolean-test ((b <- boolean-primary 'is 'not t <- truth-value)
		  `(not (= ,b ,t)))
		 ((b <- boolean-primary 'is t <- truth-value) `(= ,b ,t))
		 ((b <- boolean-primary) b))
   (truth-value (('true) #t)
		(('false) #f)
		(('unknown) 'unknown))
   (boolean-primary ((b <- boolean-predicand) b)
		    ((p <- predicate) p))
   (boolean-predicand ((p <- parenthesized-boolean-value-expression) p)
		      ((n <- nonparenthesized-value-expression) n))
   (parenthesized-boolean-value-expression
    (('#\( b <- boolean-value-expression '#\)) (list b)))


   ;; 7.13 query expression
   (query-expression ((w <- with-clause q <- query-expression-body)
		      ;; -> (with ((as name query cols ...) ...) body)
		      `(,@w ,q))
		     ((q <- query-expression-body) q))
   (with-clause (('with 'recursive l <- with-list) `(with recursive ,l))
		(('with l <- with-list) `(with ,l)))
   (with-list ((w <- with-list-element w* <- with-list-element*) (cons w w*)))
   (with-list-element ((n <- query-name '#\( c <- column-name-list '#\)
			'as '#\( q <- query-expression '#\) 
			s <- search-or-cycle-clause)
		       ;; TODO search or cycle clause
		       `(as ,n ,q ,@c))
		      ((n <- query-name
			'as '#\( q <- query-expression '#\) 
			s <- search-or-cycle-clause)
		       ;; TODO search or cycle clause
		       `(as ,n ,q ,@c)))
   (with-list-element* (('#\, w <- with-list-element) w)
		       (() '()))
   (query-expression-body ((q <- non-join-query-expression) q)
			  ((j <- joined-table) j))
   ;; TODO 
   (non-join-query-expression 
    ((t <- non-join-query-term t* <- non-join-query-expression*) 
     (if t*
	 (cons t t*) ;; todo merge union and except
	 t)))
   (non-join-query-expression* ((u <- union-or-except s <- set-qualifier* 
				 c <- corresponding-spec
				 q <- query-term)
				`(,u ,@s ,@c ,q))
			       (() #f))
   (union-or-except (('union) 'union)
		    (('except) 'except))
   (set-qualifier*  ((s <- set-qualifier) (list s))
		    (() '()))
   (corresponding-spec (('corresponding) (list 'corresponding))
		       (('corresponding 'by '#\( c <- column-name-list '#\))
			;; TODO should we put 'by'?
			`((corresponding ,@c)))
		       (() '()))
   (query-term ((q <- non-join-query-term) q)
	       ((j <- joined-table) j))
   (non-join-query-term ((q <- non-join-query-primary) q)
			((q <- query-term 'intersect s <- set-qualifier*
			  c <- corresponding-spec qp <- query-primary)
			 ;; TODO should this be like this?
			 `(intersect ,@s ,q ,@c ,qp)))
   (query-primary ((q <- non-join-query-primary) q)
		  ((j <- joined-table) j))
   (non-join-query-primary ((s <- simple-table) s)
			   (('#\( q <- non-join-query-expression '#\))
			    (list q)))
   (simple-table ((q <- query-specification) q)
		 ;; ((t <- table-value-constructor) t)
		 ((t <- explicit-table) t))
   (explicit-table (('table t <- table-or-query-name) `(table ,t)))

   ;; 7.7 joine table
   ;; TODO 
   (joined-table (() '()))

   ;; subquery
   (scalar-subquery ((s <- subquery) s))
   (row-subquery ((s <- subquery) s))
   (table-subquery ((s <- subquery) s))
   ;; TODO how to represents?
   (subquery (('#\( q <- query-expression '#\)) q))

   ;; 8.1 predicate
   (predicate ((c <- comparison-predicate) c)
	      ;;((b <- between-predicate) b)
	      ;;((i <- in-predicate) i)
	      ;;((p <- like-predicate) p)
	      ;;((p <- similar-predicate) p)
	      ;;((p <- null-predicate) p)
	      ;;((p <- qualified-comparison-predicate) p)
	      ;;((p <- exists-predicate) p)
	      ;;((p <- unique-predicate) p)
	      ;;((p <- normalized-predicate) p)
	      ;;((p <- match-predicate) p)
	      ;;((p <- overlaps-predicate) p)
	      ;;((p <- distinct-predicate) p)
	      ;;((p <- member-predicate) p)
	      ;;((p <- submultiset-predicate) p)
	      ;;((p <- set-predicate) p)
	      ;;((p <- type-predicate) p)
	      )
   (comparison-predicate ((r1 <- row-value-predicand
			   op <- comp-op
			   r2 <- row-value-predicand)
			  (list op r1 r2)))
   (comp-op (('#\=) '=)
	    (('<>) '<>)
	    (('#\<) '<)
	    (('#\>) '>)
	    (('<=) '<=)
	    (('>=) '>=))

   ;; 10.7 collate
   (collate-clause (('collate c <- identifier-chain) (list 'collate c)))


   (local-or-schema-qualified-name ((l <- local-or-schema-qualifier '#\. 
				     i <- identifier)
				    (if (eq? l 'module)
					`(module ,i)
					(concate-identifier l i)))
				   ((i <- identifier) 
				    i))
   (local-or-schema-qualifier (('module) 'module)
			      ((i <- identifier-chain) i))
   ;; identifier
   ;; we need to resolve unicode here as well
   (identifier ((i <- 'identifier 'uescape c <- 'string)
		(unless (and (pair? i) (eq? (car i) 'unicode))
		  (raise-sql-parse-error 'parse-sql 
					 "invalid use of UESCAPE" #f #f))
		;; (unicode (! "foo") uescape "c")
		`(,@i uescape ,c))
	       ((i <- 'identifier) i))
   
   ))

;; almost the same as &markdown-parser-error
;; TODO should merge it
(define-condition-type &sql-parser-error &error
  make-parser-error sql-parser-error?
  (position sql-parser-position)
  (expected sql-parser-expected))

(define (raise-sql-parse-error who msg pos expected)
  (raise (condition (make-parser-error pos expected)
		    (make-who-condition who)
		    (make-message-condition msg))))
(define (parse-sql in)
  (let ((result (sql-parser (base-generator->results (make-generator in)))))
    (if (parse-result-successful? result)
	(parse-result-semantic-value result)
	(let ((e (parse-result-error result)))
	  (raise-sql-parse-error 'parse-sql
				 (parse-error-messages e)
				 (parse-error-position e)
				 (parse-error-expected e))))))

)
