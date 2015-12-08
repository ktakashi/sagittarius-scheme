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

(define (symbol-append . args)
  (string->symbol (string-concatenate (map symbol->string args))))

;; handling operators
;;
;; form numeric 
;; 1*1 -> (* 1 1)
;; term  ::= factor
;; term* ::= (/ term) | (* term)
;; factor ::= value | (+ value) | (- value)
;; value is value expression so can be column, string, number, etc.
;;
;; for boolean 
;; term   :: = factor
;; term*  :: = (and term)
;; factor :: = value | (or value)
(define (resolve-term factor term*)
  (if (null? term*)
      factor
      (let ((expr (car term*)))
	`(,expr ,factor ,@(cdr term*)))))

(define sql-parser
  (packrat-parser
   (begin
     ;; some keyword looks like symbols are not reserved keyword
     ;; e.g.) SETS, used by GROUPING SETS
     ;; to avoid adding unnecessary keyword in scanner, we need to
     ;; check value of the identifier.
     (define (=? sym)
       (lambda (results)
	 (let ((s (parse-results-token-value results)))
	   (if (eq? s sym)
	       (make-result sym (parse-results-next results))
	       (make-expected-result
		(parse-results-position results) sym)))))
     stmt)
   (stmt ((s <- query-specification) s)
	 ((s <- query-expression) s) ;; for 'with x as ...' thing
	 ;; TODO more
	 )
   ;; NOTE:
   ;; SQL 2003 BNF seems not allow to connect SELECT with UNION
   ;; I don't know if this is oversight or proper specification
   ;; but restricting like that is very inconvenient. so we
   ;; need to do like this
   (query-specification ((s <- select-clause q <- query-specification*)
			 (resolve-term s q))
			((s <- select-clause) s))
   (query-specification* ((u <- union-or-except s <- set-quantifier?
			   q <- query-specification)
			  (if (null? s)
			      `(,u ,q)
			      `(,(symbol-append u '- (car s)) ,q)))
			 (('intersect s <- set-quantifier?
			   q <- query-specification)
			  (if (null? s)
			      `(intersect ,q)
			      `(,(symbol-append 'intersect '- (car s)) ,q))))

   ;; NOTE:
   ;; SQL 2003 BNF doesn't allow SELECT to have ORDER BY. This may
   ;; be specified in the specification itself but not in BNF. So
   ;; we just need to put like this.
   ;; TODO: should we put this in `table-expression`?
   (select-clause ((s <- select-w/o-order o <- order-by-clause)
		   `(,@s ,o))
		  ((s <- select-w/o-order) s))

   ;; 14.1 order by clause
   (order-by-clause (('order 'by s <- sort-specification-list) 
		     (cons 'order-by s)))
   (sort-specification-list 
    ((s <- sort-specification s* <- sort-specification-list*) (cons s s*)))
   (sort-specification-list* (('#\, s <- sort-specification-list) s)
			     (() '()))
   (sort-specification ((s <- sort-key o <- ordering-specification
			 n <- null-ordering)
			(if (and (null? o) (null? n))
			    s
			    `(,s ,@o ,@n))))
   (sort-key ((v <- value-expression) v))
   ;; interestingly, these are *not* reserved keyword
   (ordering-specification (((=? 'asc)) '(asc))
			   (((=? 'desc)) '(desc))
			   (() '()))
   (null-ordering (((=? 'nulls) (=? 'first)) '(nulls-first))
		  (((=? 'nulls) (=? 'last)) '(nulls-last))
		  (() '()))

   ;; 7.12 query specification
   (select-w/o-order (('select c <- select-list t <- table-expression) 
		      (cons* 'select c t))
		     ;; select (distinct|all) column from table
		     (('select q <- set-quantifier 
			       c <- select-list 
			       t <- table-expression) 
		      (cons* 'select q c t))
		     ;; extension: select 1+1; or so
		     (('select c <- select-list) (list 'select c)))
   (select-list (('#\*) '*)
		((s <- select-sublist) s))
   (select-sublist ((q <- qualified-asterisk s* <- select-sublist*) 
		    (cons q s*))
		   ((d <- derived-column s* <- select-sublist*) (cons d s*))
		   )
   (select-sublist* (('#\, s <- select-sublist) s)
		    (() '()))
   ;; TODO 'select foo.*' or so
   (qualified-asterisk ((a <- all-field-reference) a)
		       ((c <- identifier-chain '#\. '#\*)
			(concate-identifier c '*)))

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

   (set-quantifier (('distinct) 'distinct)
		    (('all) 'all))

   ;; didn't know 'select (select * from foo).*;' is valid...
   (all-field-reference ((c <- value-expression-primary '#\. '#\*
			  'as '#\( c* <- column-name-list '#\))
			 ;; (as (~ (select * from foo) *) (#f a b c))
			 (list 'as (concate-identifier c '*)
			       (cons #f c*)))
			((c <- value-expression-primary '#\. '#\*)
			 (concate-identifier c '*)))

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
   (table-reference #;((t <- table-primary-or-joined-table s <- sample-clause) 
		     (cons t s))
		    ((t <- table-primary-or-joined-table) t))
   (table-primary-or-joined-table ((j <- joined-table)  j)
				  ((t <- table-primary) t))
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
   (query-name ((i <- identifier) i))
   
   ;; where
   (where-clause (('where s <- search-condition) (list (list 'where s)))
		 (() '()))

   (search-condition ((b <- boolean-value-expression) b))

   (group-by-clause (('group 'by g* <- grouping-element-list)
		     (list (cons 'group-by g*)))
		    (('group 'by s <- set-quantifier
		      g* <- grouping-element-list)
		     (list (cons* 'group-by s g*)))
		    (() '()))
   ;; TODO 
   (grouping-element-list ((g <- grouping-element g* <- grouping-element-list*)
			   (cons g g*)))
   (grouping-element-list* (('#\, g <- grouping-element-list) g)
			   (() '()))
   (grouping-element ((o <- ordinary-grouping-set) o)
		     ((o <- rollup-list) o)
		     ((o <- cube-list) o)
		     ((o <- grouping-sets-specification) o)
		     ((o <- empty-grouping-set) o))
   
   (ordinary-grouping-set ((g <- grouping-column-reference) g)
			  (('#\( g <- grouping-column-reference-list '#\)) g))
   (grouping-column-reference ((r <- column-reference c <- collate-clause)
			       (cons r c))
			      ((c <- column-reference) c))
   (grouping-column-reference-list ((g <- grouping-column-reference
				     g* <- grouping-column-reference-list*)
				    (cons g g*)))
   (grouping-column-reference-list* (('#\, g* <- grouping-column-reference-list)
				     g*)
				    (() '()))
   ;; rollup
   (rollup-list (('rollup '#\( l <- ordinary-grouping-set-list '#\))
		 (cons 'rollup l)))
   (ordinary-grouping-set-list 
    ((o <- ordinary-grouping-set o* <- ordinary-grouping-set-list*) 
     (cons o o*)))
   (ordinary-grouping-set-list* (('#\, o <- ordinary-grouping-set-list) o)
				(() '()))

   ;; cube list
   (cube-list (('cube '#\( o <- ordinary-grouping-set-list '#\)) 
	       (cons 'cube o)))
   
   ;; grouping sets specification
   (grouping-sets-specification
    (('grouping (=? 'sets)
      '#\( g <- grouping-set-list '#\)) (cons 'grouping-sets g)))
   (grouping-set-list ((g <- grouping-set g* <- grouping-set-list*)
		       (cons g g*)))
   (grouping-set-list* (('#\, g <- grouping-set-list) g)
		       (() '()))
   (grouping-set ((o <- ordinary-grouping-set) o)
		 ((o <- rollup-list) o)
		 ((o <- cube-list) o)
		 ((o <- grouping-sets-specification) o)
		 ((o <- empty-grouping-set) o))
   ;; empty grouping set
   (empty-grouping-set (('#\( '#\)) '()))

   ;; having
   (having-clause (('having s <- search-condition) (list (cons 'having s)))
		  (() '()))
   ;; window
   (window-clause (('window w* <- window-definition-list)
		   (list (cons 'window w*)))
		  (() '()))

   (window-definition ((n <- identifier 'as w <- window-specification)
		       (list 'as n w)))
   (window-definition-list ((w <- window-definition
			     w* <- window-definition-list*)
			    (cons w w*)))
   (window-definition-list* (('#\, w <- window-definition-list) w)
			    (() '()))

   (window-specification (('#\( w <- window-specification-details '#\)) w))
   (window-specification-details ((e <- identifier?
				   p <- window-partition-clause?
				   o <- window-order-clause?
				   f <- window-frame-caluse?)
				  `(,@e ,@p ,@o ,@f)))
   (identifier? ((i <- identifier) (list i))
		(() '()))
   (window-partition-clause? (('partition 'by
			       c <- window-partition-column-reference-list)
			      `((partition-by ,@c)))
			     (() '()))

   (window-partition-column-reference-list 
    ((w <- window-partition-column-reference 
      w* <- window-partition-column-reference-list*)
     (cons w w*)))
   (window-partition-column-reference-list*
    (('#\, w <- window-partition-column-reference-list) w)
    (() '()))
   (window-partition-column-reference 
    ((c <- column-reference l <- collate-clause) (cons c l))
    ((c <- column-reference) c))

   (window-order-clause? (('order 'by s <- sort-specification-list)
			  (list (cons 'order-by s)))
			 (() '()))

   (window-frame-caluse? ((u <- window-frame-units
			   e <- window-frame-extent
			   w <- window-frame-exclusion)
			  (cons* u e w)))
   (window-frame-units (('rows) 'rows)
		       (('range) 'range))
   (window-frame-extent ((s <- window-frame-start) s)
			((b <- window-frame-between) b))
   (window-frame-start (((=? 'unbounded) (=? 'preceding)) 'unbounded-preceding)
		       ((w <- window-frame-preceding) w)
		       (('current 'row) 'current-row))
   (window-frame-preceding ((u <- unsigned-value-specification (=? 'preceding))
			    (list 'preceding u)))
   (window-frame-between (('between w1 <- window-frame-bound 
			   'and w2 <- window-frame-bound)
			  (list 'between w1 w2)))
   (window-frame-bound ((w <- window-frame-start) w)
		       (((=? 'unbounded) (=? 'following)) 'unbounded-following)
		       ((w <- window-frame-following) w))
   (window-frame-following ((u <- unsigned-value-specification (=? 'following))
			    (list 'following u)))
   (window-frame-exclusion (((=? 'exclude) 'current 'row) 'exclude-current-row)
			   (((=? 'exclude) 'group)        'exclude-group)
			   (((=? 'exclude) 'ties)         'exclude-group)
			   (((=? 'exclude) 'no (=? 'others))
			    'exclude-no-others))

   ;; 6.25 value expression
   ;; value
   (value-expression ((c <- common-value-expression) c)
		     ((b <- boolean-value-expression) b)
		     ((r <- row-value-expression) r))

   ;; FIXME this doens't work properly since alots of things are
   ;;       in common on underlying expressions.
   (common-value-expression ((n <- numeric-value-expression) n)
			    ((s <- string-value-expression)  s)
			    ((d <- datetime-value-expression) d)
			    ((i <- interval-value-expression) i)
			    ;; these 2 are not needed at all.
			    ;;((u <- user-define-type-value-expression) u)
			    ;;((r <- reference-value-expression) r)
			    ((c <- collection-value-expression) c)
			    ((v <- value-expression-primary) v))

   ;; 6.30  datetime value expression
   (datetime-value-expression 
    ((d <- datetime-term d* <- datetime-value-expression*) (cons d d*))
    ;; TODO this may be left side recursion
    ((i <- interval-value-expression '#\+ d <- datetime-term) (cons i d)))

   (datetime-value-expression* (('#\+ i <- interval-term) (list '+ i))
			       (('#\- i <- interval-term) (list '- i))
			       (((! concat-or-multiset)) '()))
   (datetime-term ((d <- datetime-factor) d))
   (datetime-factor ((p <- datetime-primary t <- timezone) (list p t))
		    ((p <- datetime-primary) p))
   (datetime-primary ((v <- value-expression-primary) v)
		     ((f <- datetime-value-function) f))
   (timezone (('at 'local i <- interval-primary) `(at local ,i))
	     (('at 'time (=? 'zone) i <- interval-primary) `(at time-zone ,i)))

   ;; 6.31  datetime value function
   (datetime-value-function ((c <- 'current_date) 'current_date)
			    ((c <- current-time-value-function) c)
			    ((c <- current-timestamp-value-function) c)
			    ((c <- current-local-time-value-function) c)
			    ((c <- current-local-timestamp-value-function) c))
   (current-time-value-function (('current_time '#\( p <- time-precision '#\))
				 `(current_time ,p))
				(('current_time) 'current_time))
   (current-timestamp-value-function 
    (('current_timestamp '#\( p <- time-precision '#\))
     `(current_timestamp ,p))
    (('current_timestamp) 'current_timestamp))
   (current-local-time-value-function 
    (('localtime '#\( p <- time-precision '#\))
     `(localtime ,p))
    (('localtime) 'localtime))
   (current-local-timestamp-value-function 
    (('localtimestamp '#\( p <- time-precision '#\))
     `(localtimestamp ,p))
    (('localtimestamp) 'localtimestamp))

   ;; 6.32 interval value expression
   (interval-value-expression 
    ((i <- interval-term i* <- interval-value-expression*) (cons i i*))
    (('#\( d <- datetime-value-expression '#\- t <- datetime-term '#\) 
      q <- interval-qualifier)
     `(,q (- ,d ,t))))
   (interval-term ((i <- interval-factor i* <- interval-term*) (cons i i*))
		  ((t <- term '#\* f <- interval-factor) (list '* t f)))
   (interval-term* (('#\* f <- factor) (list '* f))
		   (('#\/ f <- factor) (list '* f)))
   (interval-factor ((s <- sign f <- interval-primary) (list s f))
		    ((f <- interval-primary) f))
   (interval-primary ((v <- value-expression-primary q <- interval-qualifier)
		      (cons v q))
		     ((v <- value-expression-primary) v)
		     ((v <- interval-value-function) v))

   ;; 6.33 interval value function
   (interval-value-function (('abs '#\( e <- interval-value-expression '#\))
			     `(abs ,e)))
   ;; 10.1 interval quelifier
   ;; TODO better representation
   (interval-qualifier ((s <- start-field 'to e <- end-field)
			`(to ,s ,e))
		       ((d <- single-datetime-field) d))
   (start-field ((n <- non-second-primary-datetime-field
		  '#\( i <- interval-leading-field-precision '#\))
		 (list n i))
		((n <- non-second-primary-datetime-field) n))
   (end-field ((n <- non-second-primary-datetime-field) n)
	      (('second '#\( i <- interval-fractional-seconds-precision '#\))
	       `(second ,i)))
   (single-datetime-field 
    ((n <- non-second-primary-datetime-field 
      '#\( i <- interval-leading-field-precision '#\)) (list n i))
    ((n <- non-second-primary-datetime-field) n)
    (('second 
      '#\( i <- interval-leading-field-precision
         '#\, s <- interval-fractional-seconds-precision
      '#\))
     (list 'second (list i s)))
    (('second '#\( i <- interval-leading-field-precision '#\))
     (list 'second i))
    (('second) 'second))
   (non-second-primary-datetime-field (('year) 'year)
				      (('month) 'month)
				      (('day) 'day)
				      (('hour) 'hour)
				      (('minute) 'minute))
   ;; TODO we should make sure this is unsigned integer
   (interval-leading-field-precision ((n <- 'number) n))
   (interval-fractional-seconds-precision ((n <- 'number) n))

   ;; these 2 are the same so we don't need it
   ;; (user-define-type-value-expression ((v <- value-expression-primary) v)
   ;; (reference-value-expression ((v <- value-expression-primary) v)
   (collection-value-expression ((m <- multiset-value-expression) m)
				((a <- array-value-expression) a))
   ;; 6.35 array value expression
   ;; I think this won't reach since string-value-expression has the
   ;; same expression.
   (array-value-expression ((f <- array-factor a* <- array-concatenation)
			    (if (null? a*)
				f
				(concate-character f a*))))
   (array-concatenation (('concat f <- array-factor) f)
			(() '()))
   (array-factor ((v <- value-expression-primary) v))

   ;; TODO nested MULTISET handling
   ;; NB: Not even sure which RDBMS supports MULTISET so can't determine
   ;;     how it should be nested. For now, first one is the outer most
   ;;     then second one, so on.
   ;;     (PostgreSQL and SQLite3 don't support it).
   (multiset-value-expression 
    ((t <- multiset-term m <- multiset-term*)
     ;; i don't think there's a chance that m is null but
     ;; just in case
     ;; NB in such a case, then input should already be
     ;;    consumed by string or numeric value expression.
     (if (null? m)
	 t
	 `(,(car m) ,t ,(cadr m)))))
   (multiset-term ((v <- multiset-primary m <- multiset-term**)
		   ;; same as above comment
		   (if (null? m)
		       v
		       `(,(car m) ,v ,(cadr m)))))
   (multiset-term* (('multiset u <- union-or-except s <- set-quantifier?
		     m <- multiset-term)
		    (if (null? s)
			`(,(symbol-append 'multiset- u) ,m)
			`(,(symbol-append 'multiset- u '- (car s)) ,m)))
		   ;; avoid ||
		   ;; TODO do we need this?
		   (((! 'concat)) '()))
   (multiset-term** (('multiset 'intersect s <- set-quantifier?
		      m <- multiset-term)
		     (if (null? s)
			 `(multiset-intersect ,m)
			 `(,(symbol-append 'multiset-intersect '- (car s)) ,m)))
		    (((! 'concat)) '()))

   (multiset-primary ((v <- value-expression-primary) v)
		     ((m <- mutiset-value-function) m))
   (mutiset-value-function (('set '#\( m <- multiset-value-expression '#\)) m))

   ;; 6.26 numeric value expression
   ;; to determine if empty set can be return as numeric value
   (concat-or-multiset (('concat) 'concat)
		       (('multiset) 'multiset))

   (numeric-value-expression ((t <- term n* <- numeric-value-expression*)
			      (resolve-term t n*)))
   (numeric-value-expression* (('#\+ n <- numeric-value-expression) 
			       (resolve-term* '+ n))
			      (('#\- n <- numeric-value-expression)
			       (resolve-term* '- n))
			      ;; rather ugly
			      (((! concat-or-multiset)) '()))
   (term ((f <- factor t <- term*) (resolve-term f t)))
   (term* (('#\* t <- term) (resolve-term* '* t))
	  (('#\/ t <- term) (resolve-term* '/ t))
	  ;; rather ugly
	  (((! concat-or-multiset)) '()))

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
		  ;; if there's multiset keyword following then
		  ;; we musn't proceed
		  (((! 'multiset)) '()))
   (character-factor ((c <- character-primary cl <- collate-clause)
		      (cons c cl))
		     ((c <- character-primary) c))
   (character-primary ((v <- value-expression-primary) v)
		      #;((s <- string-value-function) s))

   ;; 6.1 data type
   (data-type ((c <- collection-type) c)
	      ((p <- predefined-type) p)
	      ((r <- row-type) r)
	      ((r <- reference-type) r)
	      ((i <- identifier-chain) i))
   (predefined-type ((t <- character-string-type 
		      s <- charset? c <- collate-clause?)
		     `(,t ,@s ,@c))
		    ((t <- national-character-string-type c <- collate-clause?)
		     `(,t ,@c))
		    ((t <- binary-larget-object-string-type) t)
		    ((t <- numeric-type) t)
		    ((t <- boolean-type) t)
		    ((t <- datetime-type) t)
		    ((t <- interval-type) t))
   ;; i'm lazy...
   (length? (('#\( n <- 'number u <- large-length-units'#\)) (list n u))
	    (('#\( n <- 'number '#\)) (list n))
	    (() '()))
   (large-length-units (((=? 'characters)) 'characters)
		       (((=? 'code_units)) 'code_units)
		       (((=? 'octets))     'octets))

   (character-string-type ((c <- ctype l <- length?) (cons c l)))
   (ctype (('character)                     'character)
	  (('char)                          'char)
	  (('character 'varying)            'character-varying)
	  (('char 'varying)                 'char-varying)
	  (('varchar)                       'varchar)
	  (('character 'large (=? 'object)) 'character-large-object)
	  (('char 'large (=? 'object))      'char-large-object)
	  (('clob)                          'clob))

   (national-character-string-type ((c <- nctype l <- length?) (cons c l)))
   (nctype (('national 'character)           'national-character)
	   (('national 'char)                'national-char)
	   (('nchar)                         'nchar)
	   (('national 'character 'varying)  'national-character-varying)
	   (('national 'char 'varying)       'national-char-varying)
	   (('nchar 'varying)                'nchar-varying)
	   (('national-char-varying 'character 'large (=? 'object)) 
	    'national-character-large-object)
	   (('nchar 'large (=? 'object))     'nchar-large-object)
	   (('nclob)                         'nclob))

   (binary-larget-object-string-type
    (('binary 'large (=? 'object) l <- length?) (cons 'binary-large-object l))
    (('blob l <- length?)                       (cons 'blob l)))

   (numeric-type ((e <- exact-numeric-type) e)
		 ((i <- approximate-numeric-type) i))
   (precision-scale? (('#\( p <- 'number '#\, s <- scale '#\)) (list p s))
		     (('#\( p <- 'number '#\))                 (list p))
		     (()                                       '()))
   (exact-numeric-type (('numeric l <- precision-scale?) (cons 'numeric l))
		       (('decimal l <- precision-scale?) (cons 'decimal l))
		       (('dec l <- precision-scale?)     (cons 'dec l))
		       (('smallint)                      'smallint)
		       (('integer)                       'integer)
		       (('int)                           'int)
		       (('bigint)                        'bigint)
		       ;; TODO need more?
		       )
   (approximate-numeric-type (('float '#\( p <- 'number '#\)) (list 'float p))
			     (('float) 'float)
			     (('real)  'real)
			     (('double 'precision) 'double-precision)
			     ;; TODO need more?
			     )
   (boolean-type (('boolean) 'boolean))
   (datetime-type (('date) 'date)
		  (('time s <- time-spec?) (if (null? s) 'time (cons 'time s)))
		  (('timestamp s <- time-spec?) 
		   (if (null? s) 'timestamp (cons 'timestamp s))))
   (time-spec? (('#\( n <- 'number w <- with-or-without-timezone) (list n w))
	       (('#\( n <- 'number) (list n))
	       (() '()))
   (with-or-without-timezone (('with 'time (=? 'zone)) 'with-time-zone)
			     (('without 'time (=? 'zone)) 'without-time-zone))

   (interval-type (('interval q <- interval-qualifier) (list 'interval q)))
   (row-type (('row r <- row-type-body) (cons 'row r)))
   (row-type-body (('#\( f <- field-definition-list '#\)) f))
   (field-definition-list ((f <- field-definition f* <- field-definition-list*)
			   (cons f f*)))
   (field-definition-list* (('#\, f <- field-definition-list) f)
			   (() '()))

   (reference-type (('ref '#\( t <- referenced-type '#\) s <- scope-clause?)
		    (cons* 'ref t s)))
   (scope-clause? ((s <- scope-clause) (list s))
		  (() '()))
   (scope-clause (('scope t <- local-or-schema-qualified-name) (list 'scope t)))
   (referenced-type ((i <- identifier-chain) i))

   (collection-type ((a <- array-type) a)
		    ((m <- multiset-type) m))
   ;; auxiliary without collection-type
   (simple-data-type ((p <- predefined-type) p)
		     ((r <- row-type) r)
		     ((r <- reference-type) r)
		     ((i <- identifier-chain) i))
   ;; FIXME for now we assume int array[1] int array[1] is invalid SQL
   ;;       thus no nested array.
   ;; NB: PostgreSQL allow users to do int array[][] to create multi
   ;;     dimention array.
   ;; TODO should we do (array data-type) format?
   (array-type ((d <- simple-data-type 'array '#\[ n <- 'number '#\])
		`(,d array ,n))
	       ((d <- simple-data-type 'array)
		`(,d array)))
   (multiset-type ((d <- simple-data-type 'multiset) `(,d multiset)))

   ;; 6.2 field definition
   (field-definition 
    ;; TODO better representation
    ;;((n <- identifier t <- data-type r <- reference-scope-check) (list n t r))
    ((n <- identifier t <- data-type) (list n t)))

   ;; 6.3 value expression primary
   ;; we do it like this to do field reference without left side recursion.
   (value-expression-primary ((v <- no-field-reference-value
			       v* <- value-expression-primary*)
			      (if (null? v*)
				  v
				  (concate-identifier v v*))))
   (value-expression-primary* (('#\. v <- value-expression-primary) v)
			      (() '()))
   (no-field-reference-value ((p <- parenthesized-value-expression) p)
			     ((n <- nonparenthesized-value-expression) n))

   (parenthesized-value-expression (('#\( v <- value-expression '#\)) v))
   (nonparenthesized-value-expression ((s <- set-function-specification) s)
				      ((w <- window-function) w)
				      ((s <- scalar-subquery) s)
				      ((c <- case-expression) c)
				      ((c <- cast-specification) c)
				      ;; This shouldn't be uncommented
				      ;; see the comment on the definition
				      ;;((f <- field-reference) f)
				      ((n <- next-value-expression) n)
				      ((s <- subtype-treatment) s)
				      ;;((m <- method-invocation) m)
				      ;;((s <- static-method-invocation) s)
				      ;;((a <- attribute-or-method-reference) a)
				      ;;((r <- reference-resolution) r)
				      ;;((c <- collection-value-constructor) c)
				      ;;((a <- array-element-reference) a)
				      ;;((m <- multiset-element-reference) m)
				      ((r <- routine-invocation) r)
				      
				      ;; these 2 must be the last
				      ;; otherwise it'd take all expressions
				      ((c <- column-reference) c)
				      ((u <- unsigned-value-specification) u)

				      )
   
   ;; 6.4 value specification and taget specification
   (value-specification ((l <- literal) l)
			((v <- general-value-specification) v))
   (unsigned-value-specification ((u <- unsigned-literal) u)
				 ((g <- general-value-specification) g))
   (general-value-specification
    ((s <- host-parameter-specification) s)
    ((s <- identifier) s) ;; SQL parameter
    (('#\?) '?) ;; dynamic parameter specification
    ;; NB we don't care embedded variables
    ((s <- current-collation-specification) s)
    (('current_default_transform_group) 'current_default_transform_group)
    (('current_path) 'current_path)
    (('current_role) 'current_role)
    (('current_transform_group_for_type p <- identifier-chain)
     (cons 'current_transform_group_for_type p))
    (('current_user) 'current_user)
    (('session_user) 'session_user)
    (('system_user) 'system_user)
    (('user) 'user)
    (('value) 'value))
   (simple-value-specification ((l <- literal) l)
			       ((s <- host-parameter-specification) s)
			       ((s <- identifier) s)) ;; SQL parameter
   
   (target-specification
    ((s <- host-parameter-specification) s)
    (('#\?) '?) ;; dynamic parameter specification
    ((s <- target-array-element-specification) s)
    ((s <- column-reference) s)
    ((s <- identifier) s)) ;; SQL parameter

   (current-collation-specification (((=? 'current-collation)
				      '#\( s <- string-value-expression '#\))
				     (list 'current-collation s)))

   (target-array-element-specification
    ((t <- target-array-reference '#\[ s <- simple-value-specification '#\])
     `(array-ref ,t ,s)))
   (target-array-reference ((c <- column-reference) c)
			   ((i <- identifier) i))

   ;; 6.5 contextually typed value specification
   (contextually-typed-value-specification
    ((i <- implicitly-typed-value-specification) i)
    ((d <- default-specification) d))
   (implicitly-typed-value-specification ((n <- null-specification) n)
					 ((a <- empty-specification) a))
   (null-specification (('null) 'null))
   (empty-specification (('array '#\[ '#\]) '(array))
			(('multiset '#\[ '#\]) '(multiset)))
   (default-specification (('default) 'default))

   ;; 6.9 set function specification
   (set-function-specification ((a <- aggregate-function) a)
			       ((g <- grouping-operation) g))
   (grouping-operation (('grouping '#\( c <- column-reference-list '#\))
			(cons 'grouping c)))
   (column-reference-list ((c <- column-reference c* <- column-reference-list*)
			   (cons c c*)))
   (column-reference-list* (('#\, c <- column-reference-list) c)
			   (() '()))

   ;; 6.10 window function
   (window-function ((t <- window-function-type 'over 
		      w <- window-name-or-specification)
		     ;; TODO should we do like this?
		     `(,t 'over ,w)))
   (window-function-type ((t <- rank-function-type '#\( '#\)) (list t))
			 (((=? 'row_number)) (list 'row_number))
			 ((a <- aggregate-function) a))
   (rank-function-type (((=? 'rank))         'rank)
		       (((=? 'dense_rank))   'dense_rank)
		       (((=? 'percent_rank)) 'percent_rank)
		       (((=? 'cume_dist))    'cume_dist))
   (window-name-or-specification ((w <- identifier) w)
				 ((w <- window-specification) w))

   ;; 6.11 case expression
   (case-expression ((a <- case-abbreviation) a)
		    ((s <- case-specification) s))
   (case-abbreviation (((=? 'nullif) 
			'#\( v1 <- value-expression '#\, 
			     v2 <- value-expression '#\))
		       (list 'nullif v1 v2))
		      (((=? 'coalesce) 
			'#\( v <- value-expression-list '#\))
		       (cons 'coalesce v)))
   (case-specification ((s <- simple-case) s)
		       ((s <- searched-case) s))
   (simple-case (('case 
		     o <- case-operand 
		     w <- simple-when-clause-list
		     e <- else-clause?
		  'end)
		 `(case ,o ,@w ,@e)))
   (searched-case (('case w <- searched-when-clause-list e <- else-clause? 'end)
		   `(case ,@w ,@e)))

   (simple-when-clause-list ((w <- simple-when-clause 
			      w* <- simple-when-clause-list)
			     (cons w w*))
			    (() '()))
   (simple-when-clause (('when w <- when-operand 'then r <- result)
			`(when ,w ,r)))
   (searched-when-clause-list ((w <- searched-when-clause 
				w* <- searched-when-clause-list)
			       (cons w w*))
			      (() '()))
   (searched-when-clause (('when w <- search-condition 'then r <- result)
			  `(when ,w ,r)))
   (else-clause? (('else r <- result) `(else ,r))
		 (() '()))

   (case-operand ((r <- row-value-predicand) r))
   ;; NB: seems not all RDBMS supports this
   ;;     (e.g. seems PostgreSQL and SQLite3 support only row-value-predicand)
   (when-operand ((w <- comparison-predicate-2) w)
		 ((w <- between-predicate-2) w)
		 ((w <- in-predicate-2) w)
		 ((w <- like-predicate-2) w)
		 ((w <- similar-predicate-2) w)
		 ((w <- null-predicate-2) w)
		 ((w <- quantified-comparison-predicate-2) w)
		 ((w <- match-predicate-2) w)
		 ((w <- overlaps-predicate-2) w)
		 ((w <- distinct-predicate-2) w)
		 ((w <- member-predicate-2) w)
		 ((w <- submultiset-predicate-2) w)
		 ((w <- set-predicate-2) w)
		 ((w <- type-predicate-2) w)
		 ((w <- row-value-predicand) w))

   (result ((r <- value-expression) r)
	   (('null) 'null))

   ;; 6.12 cast specification
   (cast-specification (('cast '#\( o <- cast-operand 'as t <- cast-target '#\))
			`(cast ,o ,t)))
   (cast-operand ((v <- value-expression) v)
		 ((v <- implicitly-typed-value-specification) v))
   (cast-target ((d <- data-type) d)
		((d <- identifier-chain) d))

   ;; 6.13 next value expression
   (next-value-expression (((=? 'next) 'value 'for s <- identifier-chain)
			   (list 'next-value-for s)))

   ;; 6.14 field reference
   ;; this is implicit left side recursion which (packrat) can't handle.
   ;; the purpose (i believe) of this is allowing this type of this:
   ;;  select cast(a as user_type) . its_member;
   ;; the `user_type` is (i believe) should be a user defined type which
   ;; has `its_member` as its slot. so the SQL can access the slot value.
   ;; we do this in value-expression-primary level.
;;    (field-reference ((v <- value-expression-primary '#\. i <- identifier)
;; 		     (concate-identifier v i)))

   ;; 6.15 subtype treatment
   (subtype-treatment (('treat 
			'#\( o <- subtype-operand 'as t <- target-subtype '#\))
		       `(treat ,o ,t)))
   (subtype-operand ((v <- value-expression) v))
   (target-subtype ((r <- reference-type) r)
		   ((i <- identifier-chain) i))

   ;; 7.1 row value constructor
   (row-value-constructor-predicant ((c <- common-value-expression) c)
				    ((b <- boolean-predicand) b)
				    #;((e <- explicit-row-value-constructor) e))
   ;; 7.2 row value expression
   (row-value-expression ((n <- nonparenthesized-value-expression) n)
			 #;((e <- explicit-row-value-constructor) e))
   (row-value-predicand ((n <- nonparenthesized-value-expression) n)
			((r <- row-value-constructor-predicant) r))

   ;; boolean value expression
   (boolean-value-expression ((t <- boolean-term 
			       b* <- boolean-value-expression*)
			      (resolve-term t b*)))
   (boolean-value-expression* (('or b <- boolean-term) (list 'or b))
			      (() '()))
   (boolean-term ((f <- boolean-factor t* <- boolean-term*) 
		  (resolve-term f t*)))
   (boolean-term* (('and b <- boolean-term) 
		   (list 'and b))
		  (() '()))
   (boolean-factor (('not t <- boolean-test) (list 'not t))
		   ((t <- boolean-test) t))
   (boolean-test ((b <- boolean-primary 'is 'not t <- truth-value)
		  `(not (= ,b ,t)))
		 ((b <- boolean-primary 'is t <- truth-value) `(= ,b ,t))
		 ((b <- boolean-primary) b))
   (truth-value (('true) #t)
		(('false) #f)
		(('unknown) 'unknown))
   (boolean-primary ((p <- predicate) p)
		    ((b <- boolean-predicand) b))
   (boolean-predicand ((p <- parenthesized-boolean-value-expression) p)
		      ((n <- nonparenthesized-value-expression) n))
   (parenthesized-boolean-value-expression
    (('#\( b <- boolean-value-expression '#\)) b))


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
		       `(as ,n ,q ,@c))
		      ((n <- query-name
			'as '#\( q <- query-expression '#\) 
			s <- search-or-cycle-clause)
		       `(as ,n ,q)))
   (with-list-element* (('#\, w <- with-list-element) w)
		       (() '()))
   ;; TODO 
   (search-or-cycle-clause (() '()))
   (query-expression-body ((q <- non-join-query-expression) q)
			  ((j <- joined-table) j))
   ;; TODO 
   (non-join-query-expression 
    ((t <- non-join-query-term t* <- non-join-query-expression*) 
     (resolve-term t t*)))
   (non-join-query-expression* ((u <- union-or-except s <- set-quantifier? 
				 c <- corresponding-spec
				 q <- query-term)
				(if (null? s)
				    `(,u ,@c ,q)
				    `(,(symbol-append u '- (car s)) ,@c ,q)))
			       (() '()))
   (union-or-except (('union) 'union)
		    (('except) 'except))
   (set-quantifier?  ((s <- set-quantifier) (list s))
		      (() '()))
   (corresponding-spec (('corresponding) (list 'corresponding))
		       (('corresponding 'by '#\( c <- column-name-list '#\))
			`((corresponding-by ,@c)))
		       (() '()))
   ;; differ from SQL 2003 BNF becuase of removing left side recursion.
   ;; TODO may not be correct
   (query-term ((q <- non-join-query-term) q)
	       ((j <- joined-table) j))
   (non-join-query-term ((q <- non-join-query-primary
			  q* <- non-join-query-term*)
			 (resolve-term q q*)))
   (non-join-query-term* (('intersect s <- set-quantifier?
			    c <- corresponding-spec qp <- query-primary)
			  ;; TODO should this be like this?
			  (if (null? s)
			      `(intersect ,@c ,qp)
			      `(,(symbol-append 'intersect- (car s))
				,@c ,qp)))
			 (() '()))
   (query-primary ((q <- non-join-query-primary) q)
		  ((j <- joined-table) j))
   (non-join-query-primary ((s <- simple-table)  s)
			   (('#\( q <- non-join-query-expression '#\)) q))
   (simple-table ((q <- query-specification) q)
		 ;; TODO
		 ;; ((t <- table-value-constructor) t)
		 ((t <- explicit-table) t))
   (explicit-table (('table t <- table-or-query-name) `(table ,t)))

   ;; 7.7 joine table
   (joined-table ((j <- cross-join) j)
		 ((j <- qualified-join) j)
		 ((j <- natural-join) j)
		 ((j <- union-join) j))

   (cross-join ((t1 <- table-primary 'cross 'join t2 <- table-primary)
		`(cross-join ,t1 ,t2)))
   (qualified-join ((t1 <- table-primary j <- join-type
		    'join t2 <- table-reference js <- join-specification)
		    `(,(symbol-append j '- 'join) ,t1 ,t2 ,js))
		   ((t1 <- table-primary 'join t2 <- table-reference
		     j <- join-specification)
		    `(join ,t1 ,t2 ,j)))
   (natural-join ((t1 <- table-primary 'natural j <- join-type
		   'join t2 <- table-primary)
		  `(,(symbol-append 'natural '- j '- 'join) ,t1 ,t2))
		 ((t1 <- table-primary 'natural 'join t2 <- table-primary)
		    `(natural-join ,t1 ,t2)))
   (union-join ((t1 <- table-primary 'union 'join t2 <- table-primary)
		`(union-join ,t1 ,t2)))
   (join-specification ((c <- join-condition) c)
		       ((c <- named-columns-join) c))
   (join-condition (('on s <- search-condition) `(on ,s)))
   (named-columns-join (('using '#\( c <- column-name-list '#\)) 
			(cons 'using c)))

   (join-type (('inner) 'inner)
	      ((o <- outer-join-type 'outer) (symbol-append o '- 'outer))
	      ((o <- outer-join-type) o))
   (outer-join-type (('left) 'left)
		    (('right) 'right)
		    (('full) 'full))

   ;; subquery
   (scalar-subquery ((s <- subquery) s))
   (row-subquery ((s <- subquery) s))
   (table-subquery ((s <- subquery) s))
   (subquery (('#\( q <- query-expression '#\)) q))

   ;; 8.1 predicate
   (predicate ((c <- comparison-predicate) c)
	      ((b <- between-predicate) b)
	      ((i <- in-predicate) i)
	      ((p <- like-predicate) p)
	      ((p <- similar-predicate) p)
	      ((p <- null-predicate) p)
	      ((p <- quantified-comparison-predicate) p)
	      ((p <- exists-predicate) p)
	      ((p <- unique-predicate) p)
	      ((p <- normalized-predicate) p)
	      ((p <- match-predicate) p)
	      ((p <- overlaps-predicate) p)
	      ((p <- distinct-predicate) p)
	      ((p <- member-predicate) p)
	      ((p <- submultiset-predicate) p)
	      ((p <- set-predicate) p)
	      ((p <- type-predicate) p)
	      )

   ;; NB: predicate-2 suffix thing seems needed for when operand
   ;;     so it's deliberately there.
   ;; 8.2 comparison predicate
   (comparison-predicate ((r1 <- row-value-predicand
			   r2 <- comparison-predicate-2)
			  (cons* (car r2) r1 (cdr r2))))
   (comparison-predicate-2 ((op <- comp-op
			     r2 <- row-value-predicand)
			    (list op r2)))
   (comp-op (('#\=) '=)
	    (('<>) '<>)
	    (('#\<) '<)
	    (('#\>) '>)
	    (('<=) '<=)
	    (('>=) '>=))

   ;; 8.3 between predicate
   (between-predicate ((r0 <- row-value-predicand r1 <- between-predicate-2)
		       `(,(car r1) ,r0 ,@(cdr r1))))

   (between-predicate-2 (('not t <- between-type
			  r1 <- row-value-predicand
			  'and
			  r2 <- row-value-predicand)
			 `(,(symbol-append 'not- t) ,r1 ,r2))
			((t <- between-type
			  r1 <- row-value-predicand
			  'and
			  r2 <- row-value-predicand)
			 `(,t ,r1 ,r2)))
   (between-type (('between 'asymmetric) 'between-asymmetric)
		 (('between 'symmetric)  'between-symmetric)
		 (('between)             'between))

   ;; 8.4 in predicate
   (in-predicate ((r <- row-value-predicand r1 <- in-predicate-2)
		  `(,(car r1) ,r ,@(cdr r1))))
   (in-predicate-2 (('not 'in v <- in-predicate-value)
		    `(not-in ,v))
		   (('in v <- in-predicate-value)
		    `(in ,v)))
   (in-predicate-value ((t <- table-subquery) t)
		       (('#\( v <- in-value-list '#\)) v))
   (in-value-list ((v <- row-value-expression v* <- in-value-list*)
		   (cons v v*)))
   (in-value-list* (('#\, v <- in-value-list) v)
		   (() '()))


   ;; 8.5 like predicate
   ;; NB: we don't make difference between character and blob
   (like-predicate ((r0 <- row-value-predicand r1 <- like-predicate-2)
		    `(,(car r1) ,r0 ,@(cdr r1))))
   (like-predicate-2 (('not l <- like-operator 
			r1 <- character-value-expression 
			e <- character-escape)
		      `(,(symbol-append 'not- l) ,r1 ,@e))
		     ((l <- like-operator
		       r1 <- character-value-expression
		       e <- character-escape)
		      `(,l ,r1 ,@e)))
   (like-operator (('like) 'like)
		  ;; ilike is *not* a keyword so compare with value.
		  (((=? 'ilike)) 'ilike)) ;; for PostgreSQL

   ;; 8.6 similar predicate
   (similar-predicate ((r0 <- row-value-predicand r1 <- similar-predicate-2)
		       (cons* (car r1) r0 (cdr r1))))
   (similar-predicate-2 (('not 'similar 'to p <- similar-pattern 
			  e <- character-escape)
			 `(not-similar-to ,p ,@e))
			(('similar 'to p <- similar-pattern 
			  e <- character-escape)
			 `(similar-to ,p ,@e)))
   (similar-pattern ((p <- character-value-expression) p))
   ;; NB: we don't need BNF of regular expression.

   ;; 8.7 null predicate
   (null-predicate ((r <- row-value-predicand r2 <- null-predicate-2)
		    `(,(car r2) ,r)))
   (null-predicate-2 (('is 'not 'null) '(not-null?))
		     (('is 'null) '(null?)))

   ;; 8.8 quantified comparison predicate
   (quantified-comparison-predicate ((r0 <- row-value-predicand
				      r1 <- quantified-comparison-predicate-2)
				     (cons* (car r1) r0 (cdr r1))))
   (quantified-comparison-predicate-2 ((o <- comp-op
					q <- quantifier
					s <- table-subquery)
				       `(,(symbol-append o '- q) ,s)))
   (quantifier (('all) 'all)
	       (('some) 'some)
	       (('any) 'any))

   ;; 8.9 exists predicate
   (exists-predicate (('exists s <- table-subquery) (list 'exists s)))

   ;; 8.10 unique predicate
   (unique-predicate (('unique s <- table-subquery) (list 'unique s)))

   ;; 8.11 normalized predicate
   (normalized-predicate ((s <- string-value-expression 'is 'not 
			     (=? 'normalized))
			  `(not-normalized? ,s))
			 ((s <- string-value-expression 'is (=? 'normalized))
			  `(normalized? ,s)))

   ;; 8.12 match predicate
   (match-predicate ((r0 <- row-value-predicand r1 <- match-predicate-2)
		     (cons* (car r1) r0 (cdr r1))))
   (match-predicate-2 (('match 'unique m <- match-type t <- table-subquery)
		       `(,(if (null? m)
			      'match-unique
			      (symbol-append 'match-unique- (car m)))
			 ,t))
		      (('match m <- match-type t <- table-subquery)
		       `(,(if (null? m)
			      'match
			      (symbol-append 'match- (car m))) ,t)))
   (match-type (((=? 'simple)) '(simple))
	       (((=? 'partial)) '(partial))
	       (((=? 'full)) '(full))
	       (() '()))
   
   ;; 8.13 overlaps predicate
   (overlaps-predicate ((r0 <- row-value-predicand r1 <- overlaps-predicate-2)
			(cons* (car r1) r0 (cdr r1))))
   (overlaps-predicate-2 (('overlaps r <- row-value-predicand) 
			  (list 'overlaps r)))

   ;; 8.14 distinct predicate
   (distinct-predicate ((r0 <- row-value-predicand r1 <- distinct-predicate-2)
			(cons* (car r1) r0 (cdr r1))))
   (distinct-predicate-2 (('is 'distinct 'from r <- row-value-predicand) 
			  (list 'distinct-from? r)))

   ;; 8.15 member predicate
   (member-predicate ((r0 <- row-value-predicand r1 <- member-predicate-2)
			(cons* (car r1) r0 (cdr r1))))
   ;; TODO should we preserve lacking 'of' keyword?
   (member-predicate-2 (('not 'member 'of r <- multiset-value-expression) 
			(list 'not-member-of r))
		       (('not 'member r <- multiset-value-expression) 
			(list 'not-member-of r))
		       (('member 'of r <- multiset-value-expression) 
			(list 'member-of r))
		       (('member r <- multiset-value-expression) 
			(list 'member-of r)))

   ;; 8.16 submultiset predicate
   (submultiset-predicate ((r0 <- row-value-predicand 
			    r1 <- submultiset-predicate-2)
		      (cons* (car r1) r0 (cdr r1))))
   ;; TODO should we preserve lacking 'of' keyword?
   (submultiset-predicate-2
    (('not 'submultiset 'of r <- multiset-value-expression) 
     (list 'not-submultiset-of r))
    (('not 'submultiset r <- multiset-value-expression) 
     (list 'not-submultiset-of r))
    (('submultiset 'of r <- multiset-value-expression) 
     (list 'submultiset-of r))
    (('submultiset r <- multiset-value-expression) 
     (list 'submultiset-of r)))

   ;; 8.17 set predicate
   (set-predicate ((r0 <- row-value-predicand r1 <- set-predicate-2)
		   (cons* (car r1) r0 (cdr r1))))
   (set-predicate-2 (('is 'not (=? 'a) 'set) '(not-a-set?))
		    (('is (=? 'a) 'set) '(a-set?)))

   ;; 8.18 type predicate
   (type-predicate ((r0 <- row-value-predicand r1 <- type-predicate-2)
		    (cons* (car r1) r0 (cdr r1))))
   (type-predicate-2 (('is 'not 'of '#\( l <- type-list '#\))
		      (cons 'not-of? l))
		     (('is 'of '#\( l <- type-list '#\)) (cons 'of? l)))
   (type-list ((u <- user-define-type-specification u* <- type-list*) 
	       (cons u u*)))
   (type-list* (('#\, l <- type-list) l)
	       (() '()))
   (user-define-type-specification (('only i <- identifier-chain) 
				    (list 'only i))
				   ((i <- identifier-chain) i))

   ;; 10.4 routine invocation
   ;; TODO if we have this then we don't need other procedure invocation
   ;;      looks like things, do we?
   (routine-invocation ((n <- identifier-chain a <- sql-argument-list)
			(cons n a)))
   (sql-argument-list (('#\( a* <- sql-argument-list* '#\)) a*))
   (sql-argument-list* ((s <- sql-argument s* <- sql-argument-list**)
			(cons s s*)))
   (sql-argument-list** (('#\, s <- sql-argument-list*) s)
			(() '()))

   (sql-argument ((g <- generalized-expression) g)
		 ((v <- value-expression) v)
		 ((t <- target-specification) t))
   (generalized-expression ((v <- value-expression 'as i <- identifier-chain)
			    (list 'as v i)))

   ;; 10.7 collate
   (collate-clause (('collate c <- identifier-chain) (list 'collate c)))
   (collate-clause? ((c <- collate-clause) (list c))
		    (() '()))

   ;; 10.9 aggregate function
   (aggregate-function (('count '#\( '#\* '#\) f <- filter-clause?)
			(if (null? f)
			    '(count *)
			    `(count * ,@f)))
		       ((s <- general-set-function f <- filter-clause?)
			`(,@s ,@f))
		       ((b <- binary-set-function f <- filter-clause?)
			`(,@b ,@f))
		       ((o <- ordered-set-function f <- filter-clause?)
			`(,@o ,@f)))
   (general-set-function ((t <- set-function-type
			   '#\( q <- set-quantifier? e <- value-expression '#\))
			  ;; should it be like this?
			  `(,t ,@q ,e)))
   (set-function-type ((c <- computational-operation) c))
   (computational-operation (((=? 'avg))          'avg)
			    (((=? 'max))          'max)
			    (((=? 'min))          'min)
			    (((=? 'sum))          'sum)
			    (((=? 'every))        'every)
			    (((=? 'any))          'any)
			    (((=? 'some))         'some)
			    (((=? 'count))        'count)
			    (((=? 'stddev_pop))   'stddev_pop)
			    (((=? 'stddev_samp))  'stddev_samp)
			    (((=? 'var_pop))      'var_pop)
			    (((=? 'var_samp))     'var_samp)
			    (((=? 'collect))      'collect)
			    (((=? 'fusion))       'fusion)
			    (((=? 'intersection)) 'intersection))
   (filter-clause? ((f <- filter-clause) (list f))
		   (() '()))
   (filter-clause (('filter '#\( 'where c <- search-condition '#\))
		   (list 'filter c)))
   (binary-set-function ((t <- binary-set-function-type 
			  '#\( d <- numeric-value-expression '#\,
			       i <- numeric-value-expression '#\))
			 (list t d i)))
   (binary-set-function-type (((=? 'covar_pop))      'covar_pop)
			     (((=? 'covar_samp))     'covar_samp)
			     (((=? 'corr))           'corr)
			     (((=? 'regr_slope))     'regr_slope)
			     (((=? 'regr_intercept)) 'regr_intercept)
			     (((=? 'reger_count))    'reger_count)
			     (((=? 'regr_r2))        'regr_r2)
			     (((=? 'regr_avgx))      'regr_avgx)
			     (((=? 'regr_avgy))      'regr_avgy)
			     (((=? 'regr_sxx))       'regr_sxx)
			     (((=? 'regr_syy))       'regr_syy)
			     (((=? 'regr_sxy))       'regr_sxy))
   (ordered-set-function ((h <- hypothetical-set-function) h)
			 ((i <- inverse-distribution-function) i))
   (hypothetical-set-function ((t <- rank-function-type 
				'#\( v <- value-expression-list '#\)
				w <- within-group-specification)
			       (cons* t w v)))
   (inverse-distribution-function ((t <- inverse-distribution-function-type
				    '#\( v <- numeric-value-expression '#\)
				    w <- within-group-specification)
				   (list t w v)))
   (inverse-distribution-function-type 
    (((=? 'precentile_cont)) 'precentile_cont)
    (((=? 'precentile_disc)) 'precentile_disc))

   (value-expression-list ((v <- value-expression v* <- value-expression-list*)
			   (cons v v*)))
   (value-expression-list* (('#\, v <- value-expression-list) v)
			   (() '()))

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

   ;; string the same as above trick
   (string ((s <- 'string 'uescape c <- 'string)
	    (unless (and (pair? s) (eq? (car s) 'unicode))
		  (raise-sql-parse-error 'parse-sql 
					 "invalid use of UESCAPE" #f #f))
	    `(,@s uescape ,c))
	   ((s <- 'string) s))
   ;; escape
   (character-escape (('escape s <- 'string) `((escape ,s)))
		     (() '()))
   (host-parameter-specification (('#\: i <- identifier) (cons ': i)))
   
   ;; 5.3 literal
   (literal ((s <- signed-numeric-literal) s)
	    ((g <- general-literal) g))
   (unsigned-literal ((u <- 'number) u)
		     ((g <- general-literal) g))
   (general-literal ((s <- character-string-literal) s)
		    ((b <- 'bit-string) b)
		    ((d <- datetime-literal) d)
		    ((i <- interval-literal) i)
		    ((t <- 'true) t)
		    ((f <- 'false) f))
   ;; TODO check date string?
   (datetime-literal (('date s <- string) (list 'date s))
		     (('time s <- string) (list 'time s))
		     (('timestamp s <- string) (list 'timestamp s)))
   ;; TODO better representation
   (interval-literal (('interval s <- sign i <- string q <- interval-qualifier)
		      (list 'interval (list q s i)))
		     (('interval i <- string q <- interval-qualifier)
		      (list 'interval (list q i))))
   ;; TODO introducer thing
   (character-string-literal ((s <- string) s))
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
