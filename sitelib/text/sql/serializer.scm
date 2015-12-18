;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sql/serializer - S-SQL to SQL
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

(library (text sql serializer)
    (export ssql->sql
	    ssql->sql-indent ;; TODO

	    ;; parameters
	    *non-unicode-charset*
	    *unicode-escape*
	    *identifier-case*
	    *character-converter*
	    )
    (import (rnrs)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    ;; for +sql-special-character-set+
	    (text sql scanner)
	    ;; for bytevector->hex-string
	    (util bytevector)
	    (sagittarius)
	    (match))

;; FIXME not sure if we need to consider surrogate pair by default
;;       (and not even sure how many RDBMS supports U& syntax)
;;       so for now go easier way.
(define (default-converter ch)
  (let* ((s (number->string (char->integer ch) 16))
	 (l (string-length s)))
    (cond ((< l 4) (string-pad s 4))
	  ((= l 4) s)
	  ((< l 6) (string-pad s 6))
	  ((= l 6) s)
	  (else (error 'default-converter "Can't convert character" ch)))))
    

;; parameter for unicode escape
;;  - char-set: non-unicode character set, if characters not in this then 
;;              will be escaped
;;  - #f: don't escape (default)
(define *non-unicode-charset*
  (make-parameter #f (lambda (v) 
		       (cond ((not v) v)
			     ((char-set? v) v)
			     (else (assertion-violation '*non-unicode-charset*
				    "char-set or #f is required" v))))))
;; character for UESCAPE
(define *unicode-escape*
  (make-parameter #f (lambda (v) 
		       (cond ((not v) v)
			     ((char? v) v)
			     (else (assertion-violation '*unicode-escape*
				    "char or #f is required" v))))))
;; how to treat identifier case
;; - lower: converts to lower case
;; - upper: converts to upper case
;; - title: converts to title case
;; - #f   : preserve
(define *identifier-case* (make-parameter #f))

(define *character-converter* (make-parameter default-converter))

;; similar with srl:sxml->xml
(define (ssql->sql ssql :optional (port #f))
  (if port
      (write-ssql ssql port)
      (call-with-string-output-port (lambda (out) (write-ssql ssql out)))))

(define (ssql->sql-indent ssql :optional (port #f))
  (if port
      (write-ssql ssql port :indent 0)
      (call-with-string-output-port
       (lambda (out) (write-ssql ssql out :indent 0)))))

(define (write-ssql ssql out . opt)
  (if (pair? ssql)
      (apply (lookup-writer (car ssql)) ssql out opt)
      (apply write-value ssql out opt)))

(define *sql-writers* (make-eq-hashtable))
(define (lookup-writer name)
  (hashtable-ref *sql-writers* name default-sql-writer))

;; default is method invocation
;; TODO maybe we should raise an error if the name is pair
;;      (e.g. ((m) m) -> m()(m) is not a valid SQL anyway) 
(define (default-sql-writer ssql out . opt)
  (let ((name (car ssql))
	(args (cdr ssql)))
    (put-char out #\space)
    (cond ((pair? name)
	   (apply write-ssql name out opt)
	   (apply write-args args out opt))
	  (else
	   (write/case name out)
	   (apply write-args args out opt)))))

;; basically statement 
(define parenthesis-keywords '(select insert update delete))
  
(define (maybe-with-parenthesis ssql out :key (indent #f) :allow-other-keys opt)
  (if (and (pair? ssql) (memq (car ssql) parenthesis-keywords))
      (begin
	(put-char out #\space)
	(with-parenthesis out (apply write-ssql ssql out :indent indent opt)))
      (apply write-ssql ssql out :indent indent opt)))
(define (infix-operator-writer ssql out :key (indent #f) :allow-other-keys opt)
  (let ((name (car ssql))
	(args (cdr ssql)))
    ;; reset indent to 0 so that it values won't get too many spaces
    (apply maybe-with-parenthesis (car args) out :indent 0 opt)
    (put-char out #\space)
    (write/case name out)
    (apply maybe-with-parenthesis (cadr args) out :indent 0 opt)))

(define (write-args args out . opt)
  (put-char out '#\()
  (let loop ((args args) (first #t))
    (cond ((null? args) (put-char out '#\)))
	  (else
	   (unless first (put-string out ", "))
	   (let ((arg (car args)))
	     (if (pair? arg)
		 (apply write-ssql arg out opt)
		 (write-value arg out :value-space #f)))
	   (loop (cdr args) #f)))))


(define-syntax define-raw-sql-writer
  (lambda (x)
    (syntax-case x ()
      ((k (keyword ssql out . opt) body ...)
       (with-syntax ((name (datum->syntax #'k 
			    (string->symbol
			     (format "~a-writer" (syntax->datum #'keyword))))))
	 ;; no need to bind on Sagittarius but for my mental health
	 #'(define name
	     (let ((keyword (lambda (ssql out . opt) body ...)))
	       (hashtable-set! *sql-writers* 'keyword keyword)
	       keyword))))
      ;; alias
      ((k keyword alias)
       (with-syntax ((name (datum->syntax #'k 
			    (string->symbol
			     (format "~a-writer" (syntax->datum #'keyword))))))
	 ;; no need to bind on Sagittarius but for my mental health
	 #'(define name
	     (let ((keyword (hashtable-ref *sql-writers* 'alias #f)))
	       (unless keyword 
		 (error 'keyword (format "~a is not defined" 'alias)))
	       (hashtable-set! *sql-writers* 'keyword keyword)
	       keyword)))))))

(define-syntax define-sql-writer
  (syntax-rules (define)
    ;; allow internal define
    ((_ args "define" (d* ...) ((define expr ...) rest ...))
     (define-sql-writer args "define" (d* ... (define expr ...)) (rest ...)))
    ((_ (keyword ssql out . opt) "define" (defs ...) ((pattern exprs ...) ...))
     (define-raw-sql-writer (keyword ssql out . opt)
       defs ...
       (match ssql
	 (pattern exprs ...) ...
	 (else (assertion-violation 'keyword "incorrect input" ssql)))))
    ;; entry point
    ((_ (keyword ssql out . opt) clauses ...)
     (define-sql-writer (keyword ssql out . opt) "define" () (clauses ...)))
    ((_ keyword alias) (define-raw-sql-writer keyword alias))))

(define (next-indent indent) (and indent (+ indent 1)))
(define (write/comma out column columns :key (indent #f) :allow-other-keys opt)
  (put-newline/space out indent)
  (apply maybe-with-parenthesis column out :indent indent opt)
  (for-each (lambda (column)
	      (put-char out #\,)
	      (put-newline/space out indent)
	      (apply maybe-with-parenthesis column out :indent indent opt))
	    columns))
(define (write/comma* out columns . opt)
  (unless (null? columns)
    (apply write/comma out (car columns) (cdr columns) opt)))

;; ugly...
(define *in-parenthesis* (make-parameter #f))
(define-syntax with-parenthesis
  (syntax-rules ()
    ((_ out expr ...)
     (parameterize ((*in-parenthesis* #t))
       (put-char out #\() 
       expr ...  
       (put-char out #\))))))

;; select
(define-sql-writer (select ssql out :key (indent #f) :allow-other-keys opt)
  (define first-indent (if (*in-parenthesis*) 0 indent))
  (('select '* rest ...)
   (write/case "SELECT *" out :indent first-indent)
   (for-each (lambda (clause) 
	       (apply write-ssql clause out :indent indent opt)) rest))
  (('select (column columns ...) rest ...)
   (write/case "SELECT" out :indent first-indent)
   (apply write/comma out column columns :indent (next-indent indent) opt)
   (for-each (lambda (clause)
	       (apply write-ssql clause out :indent indent opt))
	     rest)))

(define (basic-insert out table cols override? vals :key (indent #f)
		      :allow-other-keys opt)
  (define ni (next-indent indent))
  (write/case "INSERT INTO" out :indent indent)
  (apply write-ssql table out :indent #f opt)
  (when cols 
    (with-parenthesis out 
      (apply write/comma* out cols :indent ni opt)))
  (when override? (put-char out #\space) (write/case override? out))
  (when vals
    (put-newline/space out indent)
    (write/case "VALUES" out)
    (with-parenthesis out (apply write/comma* out (car vals) :indent ni opt))
    (for-each (lambda (v) 
		(put-char out #\,)
		(put-newline out indent)
		(with-parenthesis out 
		  (apply write/comma* out v :indent ni opt)))
	      (cdr vals))))
(define (query-insert out table cols overriding? query . opt)
  (write/case "INSERT INTO" out)
  (apply write-ssql table out opt)
  (when cols (with-parenthesis out (write/comma* out cols opt)))
  (when overriding? (put-char out #\space) (write/case override? out))
  (put-char out #\space)
  (apply write-ssql query out opt))

(define-sql-writer (insert-into ssql out . opt)
  (('insert-into table (cols ...) ('values vals ...))
   (apply basic-insert out table cols #f vals opt))
  ;; overriding
  (('insert-into table (cols ...) symbol ('values vals ...))
   (apply basic-insert out table cols symbol vals opt))
  (('insert-into table ('values vals ...))
   (apply basic-insert out table #f #f vals opt))
  (('insert-into table symbol ('values vals ...))
   ;; overriding
   (apply basic-insert out table #f symbol vals opt))
  (('insert-into table 'default-values)
   (apply basic-insert out table #f 'default-values #f opt))
  ;; insert select
  (('insert-into table (cols ...) query) 
   (apply query-insert out table cols #f query opt))
  (('insert-into table (cols ...) symbol query) 
   (apply query-insert out table cols symbol query opt))
  (('insert-into table query)
   (apply query-insert out table #f #f query opt))
  (('insert-into table symbol query)
   (apply query-insert out table #f symbol query opt)))

(define-sql-writer (update ssql out :key (indent #f) :allow-other-keys opt)
  (define ni (next-indent indent))
  (('update table ('set! ('= lhs rhs) ...) clauses ...)
   (write/case "UPDATE" out)
   (apply write-ssql table out opt)
   (put-newline/space out indent)
   (write/case "SET" out)
   (put-newline out indent)
   (apply write-ssql (car lhs) out :indent ni opt)
   (put-string out " =")
   (apply maybe-with-parenthesis (car rhs) out :indent 0 opt)
   (for-each (lambda (lhs rhs) 
	       (put-char out #\,)
	       (put-newline out indent)
	       (apply write-ssql lhs out :indent ni opt)
	       (put-string out " =")
	       (apply maybe-with-parenthesis rhs out :indent 0 opt))
	     (cdr lhs) (cdr rhs))
   (for-each (lambda (clause) (apply write-ssql clause out :indent indent opt))
	     clauses)))

(define-sql-writer (delete-from ssql out :key (indent #f) :allow-other-keys opt)
  (define (write-delete out table where)
    (write/case "DELETE FROM" out)
    (apply write-ssql table out opt)
    (when where (apply write-ssql where out :indent indent opt)))

  (('delete-from table)
   (write-delete out table #f))
  (('delete-from table ('where condition))
   (write-delete out table (list 'where condition))))

(define (write-columns ssql out . opt)
  (define (write-column col out . opt)
    (match col
      ((name type)
       (apply write-ssql name out opt)
       (apply write-ssql type out opt))))
  (unless (null? ssql)
    (apply write-column (car ssql) out opt)
    (for-each (lambda (column)
		(put-char out #\,)
		(apply write-column column out opt)) (cdr ssql))))
;; create table
;; not complete this is only for my testing sake
(define-sql-writer (create-table ssql out . opt)
  (('create-table table (columns ...))
   (write/case "CREATE TABLE" out)
   (apply write-ssql table out opt)
   (with-parenthesis out
    (apply write-columns columns out opt))))

;; with
(define-sql-writer (with ssql out :key (indent #f) :allow-other-keys opt)
  (define (size type) (string-length (symbol->string type)))
  ((type (c c* ...) query)
   (let ((len (size type)))
     (define nl1 (next-indent indent))
     (define nl2 (next-indent nl1))

     (write/case (symbol-upcase type) out :indent indent)
     (apply write-ssql c out :indent nl2 opt)
     (for-each (lambda (as) 
		 (put-newline out indent)
		 (do ((i 0 (+ i 1))) ((= i len)) (put-char out #\space))
		 (apply write-ssql as out :indent nl2 opt))
	       c*)
     (put-newline/space out indent)
     (apply write-ssql query out :indent nl1 opt))))
   
(define-sql-writer with-recursive with)

(define-sql-writer (as ssql out :key (indent #f) :allow-other-keys opt)
  (define nl (next-indent indent))
  (('as a b)
   ;; (put-newline/space out indent)
   (apply maybe-with-parenthesis a out :indent indent opt)
   (write/case " AS" out)
   (apply maybe-with-parenthesis b out :indent indent opt)))

(define (write-table-reference table out opt)
  (if (and (pair? table) (not (eq? (car table) 'as)))
      ;; join
      (begin 
	(apply write-ssql (car table) out opt)
	(for-each (lambda (join)
		    (put-char out #\space)
		    (apply write-ssql join out opt)) (cdr table)))
      ;; normal or query
      (apply write-ssql table out opt)))

(define-sql-writer (from ssql out :key (indent #f) :allow-other-keys :rest opt)
  (('from table rest ...)
   (put-newline/space out indent)
   (write/case "FROM" out)
   (write-table-reference table out opt)
   (for-each (lambda (table) 
	       (put-char out #\,)
	       (write-table-reference table out opt)) rest)))

;; we don't check join type for alias
(define (symbol-upcase s) (string->symbol (string-upcase (symbol->string s))))
(define-sql-writer (join ssql out :key (indent #f) :allow-other-keys opt)
  ((type table . condition)
   (put-newline out indent)
   (write/case (symbol-upcase type) out)
   (apply write-ssql table out opt)
   (unless (null? condition)
     ;; must only be one condition
     (apply write-ssql (car condition) out opt))))

(define-sql-writer cross-join join)
(define-sql-writer inner-join join)
(define-sql-writer full-join join)
(define-sql-writer full-outer-join join)
(define-sql-writer left-join join)
(define-sql-writer left-outer-join join)
(define-sql-writer right-join join)
(define-sql-writer right-outer-join join)
(define-sql-writer union-join join)
;; thank you very much!!
(define-sql-writer natural-join join)
(define-sql-writer natural-inner-join join)
(define-sql-writer natural-full-join join)
(define-sql-writer natural-full-outer-join join)
(define-sql-writer natural-left-join join)
(define-sql-writer natural-left-outer-join join)
(define-sql-writer natural-right-join join)
(define-sql-writer natural-right-outer-join join)

(define-sql-writer (on ssql out . opt)
  (('on condition)
   (write/case " ON" out)
   (apply write-ssql condition out opt)))

(define-sql-writer (using ssql out . opt)
  (('using columns ...)
   (write/case " USING (" out)
   (unless (null? columns)
     (apply write-ssql (car columns) out opt)
     (for-each (lambda (column) 
		 (put-char out #\,)
		 (apply write-ssql column out opt)) 
	       (cdr columns)))
   (put-char out #\))))

(define-sql-writer (where ssql out :key (indent #f) :allow-other-keys opt)
  (('where condition)
   (put-newline/space out indent)
   (write/case "WHERE" out)
   (apply write-ssql condition out :indent (next-indent indent) opt)))

(define-sql-writer (order-by ssql out :key (indent #f) :allow-other-keys opt)
  (define (write-column column out)
    (if (pair? column)
	(let ((name (car column))
	      (attrs (cdr column)))
	  (apply write-ssql name out opt)
	  (for-each (lambda (attr) (apply write-ssql attr out opt)) attrs))
	(apply write-ssql column out opt)))
  (('order-by column columns ...)
   (put-newline/space out indent)
   (write/case "ORDER BY" out)
   (put-newline/space out indent)
   (write-column column out)
   (for-each (lambda (column) 
	       (put-char out #\,) 
	       (put-newline/space out indent)
	       (write-column column out))
	     columns)))

(define (set-quantifier? x) (or (eq? x 'distinct) (eq? x 'all)))

(define-sql-writer (group-by ssql out :key (indent #f) :allow-other-keys opt)
  (define (write-group-by set? column columns)
    (define nl (next-indent indent))
    (put-newline/space out indent)
    (write/case "GROUP BY" out)
    (when set? (put-char out #\space) (write/case (symbol-upcase set?) out))
    (put-newline/space out indent)
    (apply write-ssql column out :indent nl opt)
    (for-each (lambda (column)
		(put-char out #\,)
		(put-newline/space out indent)
		(apply write-ssql column out :indent nl opt)) columns))
  (('group-by  (? set-quantifier? x) column columns ...)
   (write-group-by x column columns))
  (('group-by  column columns ...)
   (write-group-by #f column columns)))

(define-syntax define-sql-variable-operator
  (syntax-rules ()
    ((_ name op nl keys ...)
     (define-sql-writer (name ssql out :key (indent #f) :allow-other-keys opt)
       (('name a b* (... ...))
	(apply write-ssql a out :indent indent opt)
	(for-each (lambda (condition) 
		    (when nl (put-newline/space out indent))
		    (write/case op out)
		    (apply write-ssql condition out 
			   keys ... :indent indent opt)) b*))))))
	
(define-sql-variable-operator and "AND" #t)
(define-sql-variable-operator or  "OR"  #t)
(define-sql-variable-operator ~   "."   #f :value-space #f)
(define-sql-variable-operator ^   " ||" #f)
(define-sql-variable-operator *   " *" #f)
(define-sql-variable-operator /   " /" #f)
(define-sql-variable-operator %   " %" #f)

;; (+ 1) -> +1
(define-sql-writer (variable/unary-operator ssql out :key (indent #f)
					    :allow-other-keys opt)
  ((name a b* ...)
   (if (null? b*)
       (begin 
	 (write/case name out :indent indent)
	 (apply write-ssql a out :indent #f opt))
       (begin 
	 (apply write-ssql a out :indent indent opt)
	 (for-each (lambda (c)
		     (write/case name out)
		     (apply write-ssql c out :indent indent opt)) b*)))))
(define-sql-writer + variable/unary-operator)
(define-sql-writer - variable/unary-operator)


;; unicode and delimited
;; at this moment, we just need to dump the string
;; just make sure #\" will be escaped
(define (write-escaped-delimited s out)
  (string-for-each (lambda (ch)
		     (case ch
		       ((#\") (put-string out "\"\""))
		       (else (put-char out ch)))) s))
(define (write-escaped-string s out)
  (string-for-each (lambda (ch)
		     (case ch
		       ((#\') (put-string out "''"))
		       (else (put-char out ch)))) s))

(define-sql-writer (! ssql out . opt)
  (('! id)
   (put-char out #\")
   (write-escaped-delimited id out)
   (put-char out #\")))

(define (escape-delimited s)
  (let-values (((out extract) (open-string-output-port)))
    (write-escaped-delimited s out)
    (extract)))

(define-sql-writer (unicode ssql out . opt)
  (('unicode (! id) 'uescape e)
   (put-char out #\space)
   (parameterize ((*unicode-escape* (string-ref e 0)))
     (handle-identifier (escape-delimited id) out :delimited #t :uescape #t)))
  (('unicode (! id))
   (put-char out #\space)
   (parameterize ((*unicode-escape* #\\))
     (handle-identifier (escape-delimited id) out :delimited #t :uescape #t)))
  (('unicode (? string? s) 'uescape e)
   (put-char out #\space)
   (parameterize ((*unicode-escape* (string-ref e 0)))
     (handle-string s out :uescape #t)))
  (('unicode (? string? s))
   (put-char out #\space)
   (parameterize ((*unicode-escape* #\\))
     (handle-string s out :uescape #t))))

;; a bit ugly...
(define-raw-sql-writer (binary-op ssql out . opt)
  (apply infix-operator-writer ssql out opt))
(define-sql-writer = binary-op)
(define-sql-writer <> binary-op)
(define-sql-writer >= binary-op)
(define-sql-writer <= binary-op)
(define-sql-writer > binary-op)
(define-sql-writer < binary-op)

(define-sql-writer like binary-op)
(define-sql-writer ilike binary-op)
(define-sql-writer similar-to binary-op)
(define-sql-writer not-similar-to binary-op)
(define-sql-writer match binary-op)
(define-sql-writer match-simple binary-op)
(define-sql-writer match-partial binary-op)
(define-sql-writer match-full binary-op)
(define-sql-writer match-unique binary-op)
(define-sql-writer match-unique-simple binary-op)
(define-sql-writer match-unique-partial binary-op)
(define-sql-writer match-unique-full binary-op)

(define-sql-writer member-of binary-op)
(define-sql-writer not-member-of binary-op)
(define-sql-writer member binary-op)
(define-sql-writer not-member binary-op)

(define-sql-writer submultiset-of binary-op)
(define-sql-writer not-submultiset-of binary-op)
(define-sql-writer submultiset binary-op)
(define-sql-writer not-submultiset binary-op)

;; special cases
(define-sql-writer (null? ssql out . opt)
  (('null? c1 c2)
   (apply write-ssql c1 out opt)
   (write/case " IS NULL " out)
   (apply write-ssql c2 out opt)))
(define-sql-writer (not-null? ssql out . opt)
  (('not-null? c1 c2)
   (apply write-ssql c1 out opt)
   (write/case " IS NOT NULL " out)
   (apply write-ssql c2 out opt)))

(define-sql-writer (normalized? ssql out . opt)
  (('normalized? c1 c2)
   (apply write-ssql c1 out opt)
   (write/case " IS NORMALIZED " out)
   (apply write-ssql c2 out opt)))
(define-sql-writer (not-normalized? ssql out . opt)
  (('not-normalized? c1 c2)
   (apply write-ssql c1 out opt)
   (write/case " IS NOT NORMALIZED " out)
   (apply write-ssql c2 out opt)))

(define-sql-writer (distinct-from? ssql out . opt)
  (('distinct-from? c1 c2)
   (apply write-ssql c1 out opt)
   (write/case " IS DISTINCT FROM " out)
   (apply write-ssql c2 out opt)))

;; in, not in, etc
(define (set-operator-writer ssql out . opt)
  (let ((name (car ssql))
	(args (cdr ssql)))
    (apply write-ssql (car args) out opt)
    (put-char out #\space)
    (write/case name out)
    (put-char out #\space)
    (let loop ((d (cadr args)) (first #t))
      (unless (null? d)
	(when first (put-char out #\,))
	(apply write-ssql (car d) out opt)
	(loop (cdr d) #f)))))
(define-raw-sql-writer (set-op ssql out . opt)
  (apply set-operator-writer ssql out opt))
(define-sql-writer in set-op)
(define-sql-writer not-in set-op)

(define-sql-writer (cast ssql out . opt)
  (('cast a b)
   (write/case " CAST(" out)
   (apply write-ssql a out opt)
   (write/case " AS " out)
   (apply write-ssql b out opt)
   (put-char out #\))))

(define-sql-writer (unary-op ssql out . opt)
  ((name operand)
   (put-char out #\space)
   (write/case name out)
   (apply write-ssql operand out opt)))
(define-sql-writer current-of unary-op)
(define-sql-writer local unary-op)
(define-sql-writer global unary-op)

;; TBD lot more to go...

;; meta values
(define-sql-writer (*TOP* ssql out . opt)
  ((_ s ...)
   (for-each (lambda (ssql) (apply write-ssql ssql out opt) (newline out)) s)))
(define-sql-writer (*COMMENT* ssql out . opt)
  ((_ comment)
   ;; we don't know if it's line or block so make it all block
   (put-string out "/*")
   (put-string out comment)
   (put-string out "*/")))

;;; atom value
(define (write-value ssql out :key (value-space #t) (indent #f) 
		     :allow-other-keys opt)
  (when value-space (put-char out #\space))
  (cond ((symbol? ssql) (handle-identifier ssql out))
	;; :key maybe the same as ? in some RDBMS
	((keyword? ssql) (write ssql out))
	((string? ssql) (handle-string ssql out))
	((number? ssql) (display ssql out))
	((bytevector? ssql) (handle-bit-string ssql out))
	(else (error 'ssql->sql "unknown value" ssql))))

;;; value writers
;; write SQL identifier
;; Do the followings:
;;  - delimited if needed
;;  - detect unicode
(define not-dot-set (char-set-complement! (string->char-set ".")))
;; _ is a valid identifier
(define non-identifier-chars (char-set-delete +sql-special-character-set+ #\_))
(define (handle-identifier ssql out :key (delimited #f) (uescape #f))
  (define charset (*non-unicode-charset*))
  (define escape (*unicode-escape*))
  (define converter (*character-converter*))
  (define (write/uescape s out)
    (let ((in (open-string-input-port s))
	  (uescape? (or uescape (and charset (not (string-every charset s)))))
	  (delimited? (or delimited 
			  (string-any non-identifier-chars s))))
      (when uescape? (write/case "U&\"" out))
      (when (and (not uescape?) delimited?) (put-char out #\"))
      (let-values (((sout extract) (open-string-output-port)))
	(let loop ()
	  (let ((ch (get-char in)))
	    (cond ((eof-object? ch)
		   ;; FIXME it may not a be a good result...
		   (if delimited?
		       (put-string out (extract))
		       (write/case (extract) out)))
		  ((or (not charset) (char-set-contains? charset ch))
		   (put-char sout ch)
		   (loop))
		  (else 
		   (let ((r (converter ch)))
		     (if escape
			 (put-char out escape)
			 (put-char out #\\))
		     (when (= (string-length r) 6) (put-char sout #\+))
		     (write/case r sout)
		     (loop)))))))

      (when (or uescape? delimited?) (put-char out #\"))
      (when (and uescape? escape (not (eqv? escape #\\)))
	(write/case " UESCAPE '" out)
	(if (eqv? escape #\')
	    (put-string out "''")
	    (put-char out escape))
	(put-char out #\'))))
  ;; ugly...
  (let ((fragments (if delimited
		       (list ssql)
		       (string-tokenize (symbol->string ssql) not-dot-set))))
    ;; handle '?'
    (cond ((and (null? (cdr fragments)) (string=? (car fragments) "?"))
	   (put-char out #\?))
	  ;; maybe we need to treat more specially? such as :foo thing?
	  (else (write/uescape (car fragments) out)
		(for-each (lambda (f) (put-char out #\.) (write/uescape f out))
			  (cdr fragments))))))

;; write SQL string
;; Do the followings:
;;  - escape quote(')
;;  - detect unicode
(define (handle-string ssql out :key (uescape #f))
  (define escape (*unicode-escape*))
  (define (escape-quote s)
    (define charset (*non-unicode-charset*))
    (define converter (*character-converter*))
    (let-values (((out extract) (open-string-output-port))
		 ((in) (open-string-input-port s)))
      (let loop ((uescape? uescape))
	(let ((ch (get-char in)))
	  (cond ((eof-object? ch) (values (extract) uescape?))
		((char=? ch #\') (put-string out "''") (loop uescape?))
		((not charset) (put-char out ch) (loop uescape?))
		((char-set-contains? charset ch)
		 (put-char out ch)
		 (loop uescape?))
		(else
		 (let ((r (converter ch)))
		   (if escape
		       (put-char out escape)
		       (put-char out #\\))
		   (when (= (string-length r) 6) (put-char out #\+))
		   (write/case r out)
		   (loop #t))))))))

  (let-values (((s uescape?) (escape-quote ssql)))
    (when uescape? (write/case "U&" out))
    (put-char out #\') 
    (put-string out s)
    (put-char out #\')
    (when (and uescape? escape (not (eqv? escape #\\)))
      (write/case " UESCAPE '" out)
      ;; how stupid is this?
      (if (eqv? escape #\')
	  (put-string out "''")
	  (put-char out escape))
      (put-char out #\'))))

(define (handle-bit-string bv out)
  ;; by default we put it like X'' instead of B''
  (write/case "X'" out)
  (write/case (bytevector->hex-string bv) out)
  (put-char out #\'))

;; write with case
(define not-minus-set (char-set-complement! (string->char-set "-")))

(define (write/case v out :key (indent #f) :allow-other-keys)
  (put-indent out indent)
  (cond ((string? v)
	 (case (*identifier-case*)
	   ((upper) (put-string out (string-upcase v)))
	   ((lower) (put-string out (string-downcase v)))
	   ((title) (put-string out (string-titlecase v)))
	   (else    (put-string out v))))
	((symbol? v)
	 (if (eq? v '-) ;; handle special case...
	     (display v out)
	     ;; 'sym-sym' would be printed 'sym sym'
	     (let ((s (string-tokenize (symbol->string v) not-minus-set)))
	       (write/case (car s) out)
	       (for-each (lambda (s) 
			   (put-char out #\space) (write/case s out :indent #f))
			 (cdr s)))))
	;; numbers?
	(else (display v out))))

(define (put-indent out indent) 
  (when indent
    (do ((times (* indent 2)) (i 0 (+ i 1)))
	((= i times))
      (put-char out #\space))))
(define (put-newline out indent) (when indent (newline out)))
(define (put-newline/space out indent) 
  (if indent (newline out) (put-char out #\space))
  (put-indent out indent))
)
