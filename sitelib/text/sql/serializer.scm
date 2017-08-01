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
    (cond ((< l 4) (string-pad s 4 #\0))
	  ((= l 4) s)
	  ((< l 6) (string-pad s 6 #\0))
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
		       (cond ((not v) #\\)
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
    (cond ((pair? name)
	   (apply write-ssql name out opt)
	   (apply write-args args out opt))
	  (else
	   (when name (write/case name out)) ;; as (a) -> (as ... (#f a))
	   (apply write-args args out opt)))))

;; basically statement 
(define parenthesis-keywords 
  '(select insert update delete
    union union-all union-distinct 
    except except-all except-distinct
    intersect intersect-all intersect-distinct ))
  
(define (maybe-with-parenthesis ssql out :key (indent #f) :allow-other-keys opt)
  (if (and (pair? ssql) (memq (car ssql) parenthesis-keywords))
      (begin
	(with-parenthesis out (apply write-ssql ssql out :indent indent opt)))
      (apply write-ssql ssql out :indent indent opt)))
(define (infix-operator-writer ssql out :key (indent #f) :allow-other-keys opt)
  (let ((name (car ssql))
	(args (cdr ssql)))
    ;; reset indent to 0 so that it values won't get too many spaces
    (apply maybe-with-parenthesis (car args) out :indent 0 opt)
    (put-char out #\space)
    (write/case name out)
    (put-char out #\space)
    (apply maybe-with-parenthesis (cadr args) out :indent indent opt)))

(define (write-args args out . opt)
  (put-char out '#\()
  (let loop ((args args) (first #t))
    (cond ((null? args) (put-char out '#\)))
	  (else
	   (unless first (put-string out ", "))
	   (let ((arg (car args)))
	     (if (pair? arg)
		 (apply write-ssql arg out opt)
		 (write-value arg out)))
	   (loop (cdr args) #f)))))


(define-syntax define-raw-sql-writer
  (lambda (x)
    (syntax-case x ()
      ((k (keyword ssql out . opt) body ...)
       (with-syntax ((name (datum->syntax #'k 
			    (string->symbol
			     (format "~a-writer" (syntax->datum #'keyword)))))
		     (safe-key (datum->syntax #'k 
				(string->symbol
				 (format "~a" (syntax->datum #'keyword))))))
	 ;; no need to bind on Sagittarius but for my mental health
	 #'(define name
	     (let ((safe-key (lambda (ssql out . opt) body ...)))
	       (hashtable-set! *sql-writers* 'keyword safe-key)
	       safe-key))))
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

(define (next-indent indent :optional (n 1)) (and indent (+ indent n)))
(define (write/comma out column columns :key (indent #f) :allow-other-keys opt)
  ;; (put-newline/space out indent)
  (apply maybe-with-parenthesis column out :indent indent opt)
  (for-each (lambda (column)
	      (put-char out #\,)
	      (put-newline/space out indent)
	      (apply maybe-with-parenthesis column out
		     :indent indent opt))
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
  ((name '* rest ...)
   (write/case (symbol-upcase name) out :indent first-indent)
   (write/case " *" out :indent first-indent)
   (for-each (lambda (clause) 
	       (put-newline/space out indent)
	       (apply write-ssql clause out :indent indent opt)) rest))
  ((name (column columns ...) rest ...)
   (write/case (symbol-upcase name) out :indent first-indent)
   (put-char out #\space)
   ;; (string-length "SELECT ") = 7
   (apply write/comma out column columns :indent (next-indent indent 7) opt)
   (for-each (lambda (clause)
	       (put-newline/space out indent)
	       (apply write-ssql clause out :indent indent opt))
	     rest)))
(define-sql-writer select-distinct select)

(define (basic-insert out table cols override? vals :key (indent #f)
		      :allow-other-keys opt)
  (define ni (next-indent indent))
  (write/case "INSERT INTO " out :indent indent)
  (apply write-ssql table out :indent #f opt)
  (when cols 
    (put-char out #\space)
    (with-parenthesis out 
      (apply write/comma* out cols :indent #f opt)))
  (when override? (put-char out #\space) (write/case override? out))
  (when vals
    (put-newline/space out indent)
    (write/case "VALUES " out)
    (with-parenthesis out 
      (apply write/comma* out (car vals) :indent #f opt))
    (let ((vi (next-indent indent 7)))
      (for-each (lambda (v) 
		  (put-char out #\,)
		  (put-newline/space out vi)
		  (with-parenthesis out 
		    (apply write/comma* out v :indent #f opt)))
		(cdr vals)))))
(define (query-insert out table cols overriding? query :key (indent #f) 
		      :allow-other-keys opt)
  (write/case "INSERT INTO " out)
  (apply write-ssql table out opt)
  (when cols 
    (put-char out #\space)
    (with-parenthesis out 
      (apply write/comma* out cols :indent #f opt)))
  (when overriding? (put-char out #\space) (write/case overriding? out))
  (put-newline/space out indent)
  (apply write-ssql query out :indent indent opt))

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
  (define set-indent (next-indent indent 4))
  (('update table ('set! ('= lhs rhs) ...) clauses ...)
   (write/case "UPDATE " out)
   (apply write-ssql table out opt)
   (put-newline/space out indent)
   (write/case "SET " out)
   ;; (put-newline/space out indent)
   (apply write-ssql (car lhs) out :indent set-indent opt)
   (put-string out " = ")
   (apply maybe-with-parenthesis (car rhs) out :indent #f opt)
   (for-each (lambda (lhs rhs) 
	       (put-char out #\,)
	       (put-newline/space out set-indent)
	       (apply write-ssql lhs out :indent #f opt)
	       (put-string out " = ")
	       (apply maybe-with-parenthesis rhs out :indent #f opt))
	     (cdr lhs) (cdr rhs))
   (for-each (lambda (clause) 
	       (put-newline/space out indent)
	       (apply write-ssql clause out :indent indent opt))
	     clauses)))

(define-sql-writer (delete-from ssql out :key (indent #f) :allow-other-keys opt)
  (define (write-delete out table where)
    (write/case "DELETE FROM " out)
    (apply write-ssql table out opt)
    (when where 
      (put-newline/space out indent)
      (apply write-ssql where out :indent indent opt)))

  (('delete-from table)
   (write-delete out table #f))
  (('delete-from table ('where condition))
   (write-delete out table (list 'where condition))))

(define (write-constraint ssql out :key (indent #f) :allow-other-keys opt)
  (define (emit-constraint name type column rest)
    (when name 
      (write/case "CONSTRAINT " out)
      (apply write-ssql name out opt)
      (put-char out #\space))
    ;; UNIQUE(col), PRIMERY KEY(col) or so
    ;; default writer can handle it so just do it like this
    (cond ((null? column)
	   (write/case (symbol-upcase type) out)) ;; not-null
	  ((eq? type 'references) ;; sucks!!!
	   (write/case "REFERENCES " out)
	   (apply write-ssql (car column) out opt)
	   (unless (null? (cdr column))
	     (with-parenthesis out (apply write/comma* out (cdr column) opt))))
	  ((eq? type 'foreign-key)
	   (write/case "FOREIGN KEY " out)
	   (with-parenthesis out (apply write/comma* out (car column) opt))
	   (put-char out #\space)
	   (emit-constraint #f 'references (cdadr column) '()))
	  (else
	   (apply write-ssql `(,(symbol-upcase type) ,@column)
		  out :indent #f opt)))
    (unless (null? rest)
      (for-each (lambda (c)
		  (put-char out #\space)
		  (write/case (symbol-upcase c) out)) rest)))
  (match ssql
    ;; references thing
    (('constraint (type columns ...) rest ...)
     (emit-constraint #f type columns rest))
    (('constraint name (type columns ...) rest ...)
     (emit-constraint name type columns rest))
    ;; sucks...
    (('constraint name single rest ...)
     (emit-constraint name single '() rest))
    (('constraint single rest ...)
     (emit-constraint #f single '() rest))))
  
(define (write-column-definition ssql out :key (indent #f)
				 :allow-other-keys opt)
  (define (emit-constraint name type column rest)
    (when name 
      (write/case "CONSTRAINT " out)
      (apply write-ssql name out opt)
      (put-char out #\space))
    ;; UNIQUE(col), PRIMERY KEY(col) or so
    ;; default writer can handle it so just do it like this
    (cond ((null? column)
	   (write/case (symbol-upcase type) out)) ;; not-null
	  ((eq? type 'references) ;; sucks!!!
	   (write/case "REFERENCES " out)
	   (apply write-ssql (car column) out opt)
	   (unless (null? (cdr column))
	     (with-parenthesis out (apply write/comma* out (cdr column) opt))))
	  ((eq? type 'foreign-key)
	   (write/case "FOREIGN KEY " out)
	   (with-parenthesis out (apply write/comma* out (car column) opt))
	   (put-char out #\space)
	   (emit-constraint #f 'references (cdadr column) '()))
	  (else
	   (apply write-ssql `(,(symbol-upcase type) ,@column)
		  out :indent #f opt)))
    (unless (null? rest)
      (for-each (lambda (c)
		  (put-char out #\space)
		  (write/case (symbol-upcase c) out)) rest)))
  (define (write-column col out)
    ;; FIXME it's the same as basic one...
    (define (emit type)
      (put-char out #\space)
      (match type
	;; references thing
	(('constraint rest ...) (apply write-constraint type out opt))
	;; array
	((type 'array . rest)
	 (apply write-ssql type out :indent #f opt)
	 (write/case " ARRAY" out)
	 (unless (null? rest)
	   (unless (and (null? (cdr rest))
			(integer? (car rest))
			(exact? (car rest)))
	     (assertion-violation 'write-column
				  "array element must be exact integer"
				  (car rest)))
	   (put-char out #\[)
	   (put-string out (number->string (car rest)))
	   (put-char out #\])))
	;; default
	((a . d) (apply write-ssql type out :indent #f opt))
	(_ (write/case (symbol-upcase type) out))))
    (match col
      (('constraint rest ...)
       (apply write-constraint col out opt))
      ((name types ...)
       ;; assume the first one is always symbol
       ;;(emit name #f)
       (write/case name out)
       (for-each emit types))))
  (write-column ssql out))

(define (write-columns ssql out :key (indent #f) :allow-other-keys opt)
  (unless (null? ssql)
    (put-indent out indent)
    (apply write-column-definition (car ssql) out :indent indent opt)
    (for-each (lambda (column)
		(put-char out #\,)
		(put-newline/space out indent)
		(apply write-column-definition column out :indent indent opt))
	      (cdr ssql))))
;; create table
;; not complete this is only for my testing sake
(define-sql-writer (create-table ssql out :key (indent #f) 
				 :allow-other-keys opt)
  (('create-table table (columns ...))
   (write/case "CREATE TABLE " out)
   (apply write-ssql table out opt)
   (put-char out #\space)
   (with-parenthesis out
    (put-newline out indent)
    ;; indent 4 would be nice, i think
    (apply write-columns columns out :indent (next-indent indent 4) opt)
    (put-newline out indent))))

(define-sql-writer (create-schema ssql out :key (indent #f)
				  :allow-other-keys opt)
  (('create-schema name param ...)
   (write/case "CREATE SCHEMA " out)
   (apply write-ssql name out opt)
   (for-each
    (lambda (p)
      (put-newline/space out indent)
      (apply write-ssql p out :indent indent opt)) param)))
(define-sql-writer (authorization ssql out . opt)
  (('authorization auth)
   (write/case "AUTHORIZATION " out)
   (apply write-ssql auth out opt)))
(define-sql-writer (path ssql out . opt)
  (('path p)
   (write/case "PATH " out)
   (apply write-ssql p out opt)))
(define-sql-writer (default-character-set ssql out . opt)
  (('default-character-set c)
   (write/case "DEFAULT CHARACTER SET " out)
   (apply write-ssql c out opt)))

;; (default something)
(define-sql-writer (default ssql out . opt)
  (('default ssql)
   (write/case "DEFAULT " out)
   (apply write-ssql ssql out opt)))

(define-sql-writer (generated-always-as-identity ssql out . opt)
  ((name options ...)
   (write/case (symbol-upcase name) out)
   (unless (null? options)
     (put-char out #\space)
     (with-parenthesis out
       (let loop ((options options) (first #t))
	 (unless (null? options)
	   (unless first (put-char out #\space))
	   (if (pair? (car options))
	       (apply write-ssql (car options) out opt)
	       (write/case (symbol-upcase (car options)) out))
	   (loop (cdr options) #f)))))))
(define-sql-writer generated-by-default-as-identity 
  generated-always-as-identity)

(define-sql-writer (start-with ssql out . opt)
  ((name v)
   (write/case (symbol-upcase name) out)
   (put-char out #\space)
   ;; v must be a number so we can actually just write it
   (apply write-ssql v out opt)))
(define-sql-writer increment-by start-with)
(define-sql-writer maxvalue start-with)
(define-sql-writer minvalue start-with)

(define-sql-writer (create-sequence ssql out :key (indent #f) 
				    :allow-other-keys opt)
  (('create-sequence name options ...)
   (write/case "CREATE SEQUENCE " out)
   (apply write-ssql name out :indent #f opt)
   (unless (null? options)
     (let ((nl (next-indent indent 4)))
       (for-each (lambda (o)
		   (put-newline/space out nl)
		   (apply write-ssql o out :indent #f opt))
		 options)))))
				    

(define-sql-writer (alter-table ssql out :key (indent #f) :allow-other-keys opt)
  (define (add/drop-column? x) 
    (memq x '(add-column drop-column drop-constraint)))
  (define write-alter-column-action
    (match-lambda
     ('drop-default (write/case " DROP DEFAULT" out))
     (('set-default v)
      (write/case " SET DEFAULT " out)
      (apply write-ssql v out opt))
     (('add-scope v) 
      (write/case " ADD SCOPE " out)
      (apply write-ssql v out opt))
     ('drop-scope (write/case " DROP SCOPE" out))
     (('drop-scope behaviour)
      (write/case " DROP SCOPE " out)
      (write/case (symbol-upcase behaviour) out))
     ;; PostgreSQL specific...
     ((? (lambda (s) (memq s '(set-not-null drop-not-null))) x)
      (put-char out #\space)
      (write/case (symbol-upcase x) out))
     (('set sequence-option)
      (write/case " SET " out)
      (apply write-ssql sequence-option out opt))))
      
  (('alter-table name ((? add/drop-column? x) definition ...))
   (write/case "ALTER TABLE " out)
   (apply write-ssql name out opt)
   (put-char out #\space)
   (write/case (symbol-upcase x) out)
   (put-char out #\space)
   ;; this works perfectly for drop-column and drop-constraint
   ;; as well because it considers 'cascade' or 'restrict' as 
   ;; type. rather abusing though...
   (write-column-definition definition out))
  (('alter-table name ('add-constraint rest ...))
   (write/case "ALTER TABLE " out)
   (apply write-ssql name out opt)
   (write/case " ADD " out)
   (write-constraint (cons 'constraint rest) out))
  (('alter-table name (alter-column c action))
   (write/case "ALTER TABLE " out)
   (apply write-ssql name out opt)
   (write/case " ALTER COLUMN " out)
   (apply write-ssql c out opt)
   (write-alter-column-action action)))
   
(define-sql-writer (grant ssql out :key (indent #f) :allow-other-keys opt)
  (define (write-privileges privileges)
    (let loop ((privileges privileges) (first #t))
      (unless (null? privileges)
	(unless first (put-string out ", "))
	(match (car privileges)
	  ((action args ...) 
	   (write/case (symbol-upcase action) out)
	   (with-parenthesis out (apply write/comma* out args opt)))
	  (action (write/case (symbol-upcase action) out)))
	(loop (cdr privileges) #f))))
  (define (write-object object)
    (if (or (symbol? object) (eq? (car object) '~))
	(apply write-ssql object out opt)
	(begin
	  (write/case (symbol-upcase (car object)) out)
	  (put-char out #\space)
	  (apply write-ssql (cadr object) out opt))))

  (define (write-rest rest)
    (for-each (lambda (i)
		(put-char out #\space)
		(let ((name (car i)))
		  (write/case (symbol-upcase name) out)
		  ;; granted-by
		  (unless (null? (cdr i))
		    (put-char out #\space)
		    (write/case (symbol-upcase (cadr i)) out))))
		  rest))

  ;; privilege
  (('grant privileges ('on target) ('to grantee ...) rest ...)
   (write/case "GRANT " out)
   (write-privileges privileges)
   (write/case " ON " out)
   (write-object target)
   (write/case " TO " out)
   (apply write/comma* out grantee opt)
   (write-rest rest))
  ;; role
  (('grant roles ('to grantee ...) rest ...)
   (write/case "GRANT " out)
   (apply write/comma* out roles opt)
   (write/case " TO " out)
   (apply write/comma* out grantee opt)
   (write-rest rest)))

;; with
(define-sql-writer (with ssql out :key (indent #f) :allow-other-keys opt)
  (define (size type) (string-length (symbol->string type)))
  ((type (c c* ...) query)
   (let ((len (size type)))
     (define nl1 (next-indent indent 2))
     (define nl2 (next-indent nl1 2))

     (write/case (symbol-upcase type) out :indent indent)
     (put-newline/space out indent)
     (when indent (put-indent out nl2))
     (apply write-ssql c out :indent nl2 opt)
     (for-each (lambda (as) 
		 (put-char out #\,)
		 (put-newline/space out indent)
		 (when indent (put-indent out nl2))
		 (apply write-ssql as out :indent nl2 opt))
	       c*)
     (put-newline/space out indent)
     (when indent (put-indent out nl1))
     (apply write-ssql query out :indent nl1 opt))))
   
(define-sql-writer with-recursive with)

(define-sql-writer (as ssql out :key (indent #f) :allow-other-keys opt)
  (define nl (next-indent indent))
  (('as a)
   (write/case "AS " out)
   ;; should be data type but just in case...
   (apply maybe-with-parenthesis a out :indent indent opt))
  (('as a b)
   ;; (put-newline/space out indent)
   (apply maybe-with-parenthesis a out :indent indent opt)
   (write/case " AS " out)
   (apply maybe-with-parenthesis b out :indent indent opt)))

(define (write-table-reference table out :key (indent #f) :allow-other-keys opt)
  (define (join? s)
    (and (symbol? s)
	 (or (eq? s 'join)
	     (eq? s 'inner-join)
	     ;; ok the rest is not really used so check like this
	     (string-suffix? "-join" (symbol->string s)))))
  (match table
    ((name ((? join? join) join-table ...) rest ...)
     (apply write-table-reference name out :indent indent opt)
     (for-each (lambda (join)
		 (put-newline/space out indent)
		 (apply write-table-reference join out :indent indent opt))
	       (cdr table)))
    (('as ('select rest ...) name)
     ;; add indent of '('
     (let ((ni (next-indent indent 1)))
       (apply write-ssql table out :indent ni opt)))
    (('as ('values rest ...) name)
     (apply write-ssql table out :indent indent opt))
    (else (apply write-ssql table out opt))))

(define-sql-writer (from ssql out :key (indent #f) :allow-other-keys opt)
  (define ni (next-indent indent 5))
  (('from table rest ...)
   (write/case "FROM " out)
   (if (null? rest)
       (apply write-table-reference table out :indent ni opt)
       (begin
	 (apply write-table-reference table out :indent ni opt)
	 (for-each (lambda (table) 
		     (put-char out #\,)
		     (put-newline/space out ni)
		     (write-table-reference table out :indent ni opt))
		   rest)))))

;; we don't check join type for alias
(define (symbol-upcase s) (string->symbol (string-upcase (symbol->string s))))
(define-sql-writer (join ssql out :key (indent #f) :allow-other-keys opt)
  ((type table . condition)
   (write/case (symbol-upcase type) out)
   (put-char out #\space)
   (apply write-ssql table out opt)
   (unless (null? condition)
     (put-char out #\space)
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
   (write/case "ON " out)
   (apply write-ssql condition out opt)))

(define-sql-writer (using ssql out . opt)
  (('using columns ...)
   (write/case "USING (" out)
   (unless (null? columns)
     (apply write-ssql (car columns) out opt)
     (for-each (lambda (column) 
		 (put-char out #\,)
		 (apply write-ssql column out opt)) 
	       (cdr columns)))
   (put-char out #\))))

(define-sql-writer (values ssql out :key (indent #f) :allow-other-keys opt)
  (define ni (next-indent indent 8))
  (('values val vals ...)
   (write/case "(VALUES " out)
   (with-parenthesis out (apply write/comma* out val opt))
   (for-each (lambda (v)
	       (put-char out #\,)
	       (put-newline/space out ni)
	       (with-parenthesis out
		 (apply write/comma* out v opt)))
	     vals)
   (put-char out #\))))

(define-sql-writer (where ssql out :key (indent #f) :allow-other-keys opt)
  (define (write-condition condition out)
    (define offset (match condition
		     (('and rest ...) 2)
		     (('or  rest ...) 3)
		     (else 6)))
    (apply write-ssql condition out :indent (next-indent indent offset) opt))
  (('where condition)
   (write/case "WHERE " out)
   (write-condition condition out)))

(define-sql-writer (order-by ssql out :key (indent #f) :allow-other-keys opt)
  (define order-by-indent (next-indent indent 9))
  (define (write-column column out)
    (if (and (pair? column) (not (eq? (car column) '~)))
	(let ((name (car column))
	      (attrs (cdr column)))
	  (apply write-ssql name out opt)
	  (for-each (lambda (attr) 
		      (put-char out #\space) 
		      (write/case attr out)) attrs))
	(apply write-ssql column out opt)))
  (('order-by column columns ...)
   (write/case "ORDER BY " out)
   (write-column column out)
   (for-each (lambda (column) 
	       (put-char out #\,) 
	       (put-newline/space out order-by-indent)
	       (write-column column out))
	     columns)))

(define-sql-writer (limit ssql out . opt)
  (('limit n)
   (write/case "LIMIT " out)
   (write-value n out)))
(define-sql-writer (offset ssql out . opt)
  (('offset n)
   (write/case "OFFSET " out)
   (write-value n out)))

(define (set-quantifier? x) (or (eq? x 'distinct) (eq? x 'all)))

(define-sql-writer (group-by ssql out :key (indent #f) :allow-other-keys opt)
  (define (group-by-keyword? x) (memq x '(rollup cube grouping-sets)))

  (define (write-group-by set? column columns)
    (define nl (next-indent indent))
    (define (write-group-column column)
      (match column
	(((? group-by-keyword? x) rest ...)
	 (apply write-ssql column out :indent nl opt))
	(('~ rest ...) (apply write-ssql column out :indent nl opt))
	((c1 c2 ...)
	 (with-parenthesis out (apply write/comma out c1 c2 opt)))
	(() (display "()" out))
	(c (apply write-ssql column out :indent nl opt))))

    (put-newline/space out indent)
    (write/case "GROUP BY" out)
    (when set? (put-char out #\space) (write/case (symbol-upcase set?) out))
    (put-newline/space out indent)
    (write-group-column column)
    (for-each (lambda (column)
		(put-char out #\,)
		(put-newline/space out indent)
		(write-group-column column))
	      columns))
  (('group-by  (? set-quantifier? x) column columns ...)
   (write-group-by x column columns))
  (('group-by  column columns ...)
   (write-group-by #f column columns)))

(define-sql-writer (partition-by ssql out . opt)
  (('partition-by id)
   (write/case "PARTITION BY " out)
   (apply write-ssql id out opt)))

(define (write-window-function func* out indent opt)
  (put-char out #\()
  (do ((func* func* (cdr func*)) (first #t #f))
      ((null? func*))
    (unless first (put-newline/space out #f))
    (apply write-ssql (car func*) out opt))
  (put-char out #\)))

(define-sql-writer (window ssql out :key (indent #f) :allow-other-keys opt)
  (define write-as
    (match-lambda
     (('as name defs ...)
      (apply write-ssql name out opt)
      (write/case " AS " out)
      (write-window-function defs out #f opt))))
  (('window def)
   (write/case "WINDOW " out)
   (write-as def))
  (('window def defs ...)
   (write/case "WINDOW" out)
   (let ((nl (next-indent indent 2)))
     (for-each (lambda (def)
		 (put-newline/space out nl)
		 (write-as def)) (cdr ssql)))))

(define-sql-writer (over ssql out . opt)
  (('over agg (? symbol? x))
   (apply write-ssql agg out opt)
   (write/case " OVER " out)
   (apply write-ssql x out opt))
  (('over agg specs ...)
   (apply write-ssql agg out opt)
   (write/case " OVER " out)
   (write-window-function specs out #f opt)))

(define-sql-writer (case ssql out :key (indent #f) :allow-other-keys opt)
  (define nl (next-indent indent 2))
  (define write-when
    (match-lambda
     (('when condition then)
      (put-newline/space out nl)
      (write/case "WHEN " out)
      (apply write-ssql condition out :indent #f opt)
      (write/case " THEN " out)
      ;; FIXME then then is query or something...
      (apply write-ssql then out :indent #f opt))))
  (('case (? symbol? a) when* ...)
   (write/case "CASE " out)
   (apply write-ssql a out :indent #f opt)
   (put-char out #\space)
   (for-each write-when when*)
   (put-newline/space out indent)
   (write/case "END" out))
  (('case when* ...)
   (write/case "CASE " out)
   (for-each write-when when*)
   (put-newline/space out indent)
   (write/case "END" out)))

;; and/or needs to cooporate in this case
;; (or (and (or e1 e1' ...) e2 ...) e3 ...)
;; this must be like this
;; (e1 OR e1') AND e2 OR e3

(define-syntax define-sql/priority
  (syntax-rules ()
    ((_ name (op ...) sql-name indent?)
     (define-sql-writer (name ssql out :key (indent #f) :allow-other-keys opt)
       (define (write/check a)
	 ;; FIXME complex condition won't be indented
	 (if (and (pair? a) (memq (car a) '(op ...)))
	     (begin
	       (with-parenthesis out 
		(apply write-ssql a out :indent #f opt)))
	     (apply write-ssql a out :indent #f opt)))
       (('name a b* (... ...))
	(write/check a)
	(for-each (lambda (condition)
		    (when indent? (put-newline/space out indent))
		    (write/case sql-name out)
		    (write/check condition)) b*))))))

(define-sql/priority and (or)  "AND " #t)
(define-sql/priority or  (and) "OR "  #t)
(define-sql/priority *   (+ -) "*"  #f)
(define-sql/priority /   (+ -) "/"  #f)
(define-sql/priority %   (+ -) "%"  #f)

(define-syntax define-sql-variable-operator
  (syntax-rules ()
    ((_ name op)
     (define-sql-writer (name ssql out :key (indent #f) :allow-other-keys opt)
       (define (need-parenthesis? s)
	 (and (pair? s) (not (eq? (car s) '~))))
       (('name a b* (... ...))
	(if (need-parenthesis? a)
	    (with-parenthesis out (apply write-ssql a out :indent indent opt))
	    (apply write-ssql a out :indent indent opt))
	(for-each (lambda (c) 
		    (write/case op out)
		    (if (need-parenthesis? c)
			(with-parenthesis out
			 (apply write-ssql c out :indent indent opt))
			(apply write-ssql c out :indent indent opt)))
		    b*))))))

(define-sql-variable-operator ~   ".")
(define-sql-variable-operator ^   " || ")
(define-sql-variable-operator ->  "->")
(define-sql-variable-operator ::  "::")
;; in case it's not keyword
(define-sql-variable-operator |::|  "::")

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
		     (put-char out #\space)
		     (write/case name out)
		     (put-char out #\space)
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
(define-sql-writer (binary-op ssql out . opt)
  ((name a)
   (write/case (symbol-upcase name) out)
   (put-char out #\space)
   (apply write-ssql a out opt))
  (_
   (apply infix-operator-writer ssql out opt)))
(define-sql-writer = binary-op)
(define-sql-writer <> binary-op)
(define-sql-writer >= binary-op)
(define-sql-writer <= binary-op)
(define-sql-writer > binary-op)
(define-sql-writer < binary-op)
(define-sql-writer =-all  binary-op)
(define-sql-writer <>-all binary-op)
(define-sql-writer >=-all binary-op)
(define-sql-writer <=-all binary-op)
(define-sql-writer >-all  binary-op)
(define-sql-writer <-all  binary-op)

(define-sql-writer =-some  binary-op)
(define-sql-writer <>-some binary-op)
(define-sql-writer >=-some binary-op)
(define-sql-writer <=-some binary-op)
(define-sql-writer >-some  binary-op)
(define-sql-writer <-some  binary-op)

(define-sql-writer =-any  binary-op)
(define-sql-writer <>-any binary-op)
(define-sql-writer >=-any binary-op)
(define-sql-writer <=-any binary-op)
(define-sql-writer >-any  binary-op)
(define-sql-writer <-any  binary-op)

(define-sql-writer like binary-op)
(define-sql-writer ilike binary-op)
(define-sql-writer not-like binary-op)
(define-sql-writer not-ilike binary-op)
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

(define-sql-writer overlaps binary-op)

;; between family
(define-sql-writer (between ssql out . opt)
  ((name col s e)
   (apply write-ssql col out opt)
   (put-char out #\space)
   (write/case (symbol-upcase name) out)
   (put-char out #\space)
   (apply write-ssql s out opt)
   (write/case " AND " out)
   (apply write-ssql e out opt)))
(define-sql-writer between-symmetric between)
(define-sql-writer between-asymmetric between)
(define-sql-writer not-between between)
(define-sql-writer not-between-symmetric between)
(define-sql-writer not-between-asymmetric between)

;; escape
(define-sql-writer (escape ssql out . opt)
  (('escape s1 s2)
   (apply write-ssql s1 out opt)
   (write/case " ESCAPE" out)
   (apply write-ssql s2 out opt)))

;; special cases
(define-syntax define-is-predicate
  (syntax-rules ()
    ((_ type op not?)
     (define-sql-writer (type ssql out :key (indent #f) :allow-other-keys opt)
       (define (write-it c1)
	 (when c1
	   (apply write-ssql c1 out :indent indent opt)
	   (put-char out #\space))
	 (write/case "IS " out)
	 (when not? (write/case "NOT " out))
	 (write/case op out))
       (('type) (write-it #f))
       (('type c) (write-it c))))))

(define-is-predicate null? "NULL" #f)
(define-is-predicate not-null? "NULL" #t)
(define-is-predicate normalized? "NORMALIZED" #f)
(define-is-predicate not-normalized? "NORMALIZED" #t)
(define-is-predicate a-set? "A SET" #f)
(define-is-predicate not-a-set? "A SET" #t)

(define-sql-writer (distinct-from? ssql out . opt)
  (('distinct-from? c1 c2)
   (apply write-ssql c1 out opt)
   (write/case " IS DISTINCT FROM " out)
   (apply write-ssql c2 out opt)))

;; in, not in, etc
(define-sql-writer (set-op ssql out . opt)
  ((name a ('select rest ...))
   (apply write-ssql a out opt)
   (put-char out #\space)
   (write/case (symbol-upcase name) out)
   (put-char out #\space)
   (with-parenthesis out (apply write-ssql (cons 'select rest) out opt)))
  ((name a (v v* ... ))
   (apply write-ssql a out opt)
   (put-char out #\space)
   (write/case (symbol-upcase name) out)
   (put-char out #\space)
   (with-parenthesis out (apply write/comma out v v* opt))))
(define-sql-writer in set-op)
(define-sql-writer not-in set-op)

(define (write-sql-type ssql out :key (indent #f) :allow-other-keys opt)
  (match ssql 
    ((? symbol? type)
     (write/case (symbol-upcase type) out))
    ((type 'array)
     (write/case (symbol-upcase type) out)
     (put-char out #\space)
     (write/case "ARRAY" out))
    ((type 'array n)
     (write/case (symbol-upcase type) out)
     (put-char out #\space)
     (write/case "ARRAY[" out)
     (apply write-ssql n out :indent (next-indent indent 6) opt)
     (put-char out #\]))
    ;; blob(1k) or so
    ((type (? number? n))
     (write/case (symbol-upcase type) out)
     (put-char out #\space)
     (display n out))
    ;; blob(1k octet) or so
    ((type (? number? n) unit)
     (write/case (symbol-upcase type) out)
     (put-char out #\()
     (display n out)
     (put-char out #\space)
     (write/case (symbol-upcase unit) out)
     (put-char out #\)))
    ;; ref
    (('ref id)
     (write/case "REF" out)
     (with-parenthesis out (apply write-ssql id out opt)))
    (_ (error 'write-ssql "incorrect type" ssql))))


(define-sql-writer (cast ssql out . opt)
  ((type a b)
   (write/case (symbol-upcase type) out)
   (put-char out #\()
   (apply write-ssql a out opt)
   (write/case " AS " out)
   (apply write-sql-type b out opt)
   (put-char out #\))))
(define-sql-writer treat cast)

(define-sql-writer (unary-op ssql out . opt)
  ((name operand)
   (write/case (symbol-upcase name) out)
   (put-char out #\space)
   (apply write-ssql operand out opt)))
(define-sql-writer current-of unary-op)
(define-sql-writer local unary-op)
(define-sql-writer global unary-op)
(define-sql-writer new unary-op)
(define-sql-writer next-value-for unary-op)

;; multiset
(define-sql-writer (multiset ssql out . opt)
  ((name t1 t2)
   (apply write-ssql t1 out opt)
   (put-char out #\space)
   (write/case (symbol-upcase name) out)
   (put-char out #\space)
   (apply write-ssql t2 out opt)))
(define-sql-writer multiset-union              multiset)
(define-sql-writer multiset-union-all          multiset)
(define-sql-writer multiset-union-distinct     multiset)
(define-sql-writer multiset-except             multiset)
(define-sql-writer multiset-except-all         multiset)
(define-sql-writer multiset-except-distinct    multiset)
(define-sql-writer multiset-intersect          multiset)
(define-sql-writer multiset-intersect-all      multiset)
(define-sql-writer multiset-intersect-distinct multiset)

;; union, intersect, except
(define-sql-writer (union ssql out :key (indent #f) :allow-other-keys opt)
  ((name q1 q* ...)
   (apply write-ssql q1 out :indent indent opt)
   (put-newline/space out indent)
   (for-each (lambda (q)
	       (write/case (symbol-upcase name) out)
	       (put-newline/space out indent)
	       (apply write-ssql q out :indent indent opt)) q*)))
(define-sql-writer except             union)
(define-sql-writer intersect          union)
(define-sql-writer union-all          union)
(define-sql-writer except-all         union)
(define-sql-writer intersect-all      union)
(define-sql-writer union-distinct     union)
(define-sql-writer except-distinct    union)
(define-sql-writer intersect-distinct union)

;; year, month, day, hour, minute and second
(define-sql-writer (year ssql out . opt)
  ((name v . maybe-precision)
   (apply write-ssql v out opt)
   (put-char out #\space)
   (write/case (symbol-upcase name) out)
   (unless (null? maybe-precision)
     (put-char out #\space)
     (apply write-ssql (car maybe-precision) out opt))))
(define-sql-writer month year)
(define-sql-writer day year)
(define-sql-writer hour year)
(define-sql-writer minute year)
(define-sql-writer second year)

;; is of, is not of
(define-syntax define-of?
  (syntax-rules ()
    ((_ name not?)
     (define-sql-writer (name ssql out . opt)
       (('name a b (... ...))
	(apply write-ssql a out opt)
	(write/case " IS" out)
	(when not? (write/case " NOT" out))
	(write/case " OF" out)
	(with-parenthesis out (apply write/comma* out b opt)))))))
(define-of? of? #f)
(define-of? not-of? #t)

(define-sql-writer (only ssql out . opt)
  (('only id)
   (write/case " ONLY " out) (apply write-ssql id out opt)))

;; interval
(define-sql-writer (interval ssql out . opt)
  (define (write-interval-qualifier qualifier out)
    (define (write-single q out)
      (match q
	((n precision ...)
	 (write/case (symbol-upcase n) out)
	 (unless (null? precision)
	   (with-parenthesis out
	     (write-ssql (car precision) out)
	     (for-each (lambda (p)
			 (put-string out ", ")
			 (write-ssql p out))
		       (cdr precision)))))
	(n (write/case (symbol-upcase n) out))))
    (match qualifier
      (('to start end)
       (write-single start out)
       (write/case " TO " out)
       (write-single end out))
      (_
       (write-single qualifier out))))
  (('interval v qualifier)
   (write/case "INTERVAL " out)
   (write-ssql v out)
   (put-char out #\space)
   (write-interval-qualifier qualifier out)))

;; aggregate functions
;; this is needed because it can take filter clause...
(define-sql-writer (filter ssql out . opt)
  (('filter search)
   (write/case "FILTER" out)
   (with-parenthesis out (apply write-ssql (list 'where search) out opt))))
(define-sql-writer (count ssql out . opt)
  (define (emit name quantifier? v maybe-filter)
    (write/case (symbol-upcase name) out)
    (with-parenthesis out
      (when quantifier? 
	(write/case (symbol-upcase quantifier?) out)
	(put-char out #\space))
      (apply write-ssql v out opt))
    (unless (null? maybe-filter)
      (put-char out #\space)
      (apply write-ssql (car maybe-filter) out opt)))
  ((name v . maybe-filter)
   (emit name #f v maybe-filter))
  ((name (? set-quantifier? x) v . maybe-filter)
   (emit name x v maybe-filter)))
(define-sql-writer avg count)
(define-sql-writer max count)
(define-sql-writer min count)
(define-sql-writer sum count)
(define-sql-writer every count)
(define-sql-writer any count)
(define-sql-writer some count)
(define-sql-writer count count)
(define-sql-writer stddev_pop count)
(define-sql-writer stddev_samp count)
(define-sql-writer var_pop count)
(define-sql-writer var_samp count)
(define-sql-writer collect count)
(define-sql-writer fusion count)
(define-sql-writer intersection count)

(define-sql-writer (array-ref ssql out . opt)
  (('array-ref a n)
   (apply write-ssql a out opt)
   (put-char out #\[)
   (apply write-ssql n out opt)
   (put-char out #\])))
(define-sql-writer (array ssql out :key (indent #f) :allow-other-keys opt)
  ((name n ...)
   (let ((s (symbol->string name)))
     (write/case (string-upcase s) out)
     (put-char out #\[)
     (apply write/comma* out n 
	    :indent (next-indent indent (+ (string-length s) 1)) opt)
     (put-char out #\]))))
(define-sql-writer multiset array)

(define-sql-writer (module ssql out . opt)
  (('module a)
   (write/case " MODULE." out)
   (apply write-ssql a out opt)))

;; commit
;; it's not really good one but for laziness
(define-sql-writer (commit ssql out . opt)
  (define (emit-extra e)
    (put-char out #\space)
    (if (pair? e)
	(apply write-ssql e out opt)
	(write/case (symbol-upcase e) out)))
  ((name rest ...)
   (write/case (symbol-upcase name) out)
   (for-each emit-extra rest)))
(define-sql-writer commit-work commit)
(define-sql-writer rollback commit)
(define-sql-writer rollback-work commit)

(define-sql-writer (to-savepoint ssql out . opt)
  ((type name)
   (write/case (symbol-upcase type) out)
   (put-char out #\space)
   (write/case name out)))
(define-sql-writer savepoint to-savepoint)
(define-sql-writer release-savepoint to-savepoint)

;; begin, start transaction
(define-sql-writer (begin ssql out . opt)
  (define (write-one-mode mode out)
    (match mode
      (('diagnostics-size v)
       (write/case "DIAGNOSTICS SIZE " out)
       (write-ssql v out))
      ((name value)
       (write/case (symbol-upcase name) out)
       (put-char out #\space)
       (write/case (symbol-upcase value) out))
      (name (write/case (symbol-upcase name) out))))
  ((name rest ...)
   (write/case (symbol-upcase name) out)
   (unless (null? rest)
     (put-char out #\space)
     (write-one-mode (car rest) out)
     (for-each (lambda (mode)
		 (put-string out ", ")
		 (write-one-mode mode out)) (cdr rest)))))
(define-sql-writer start-transaction begin)
;; TBD lot more to go...

;; meta values
(define-sql-writer (*TOP* ssql out . opt)
  ((_ s ...)
   (for-each (lambda (ssql) 
	       (apply write-ssql ssql out opt)
	       (when (and (pair? ssql) (not (eq? (car ssql) '*COMMENT*)))
		 (put-char out #\;))
	       (newline out)) s)))
(define-sql-writer (*COMMENT* ssql out . opt)
  ((_ comment)
   ;; we don't know if it's line or block so make it all block
   (put-string out "/*")
   (put-string out comment)
   (put-string out "*/")))

;;; atom value
(define (write-value ssql out :key (indent #f) 
		     :allow-other-keys opt)
  (cond ((symbol? ssql) (handle-identifier ssql out))
	;; :key maybe the same as ? in some RDBMS
	((keyword? ssql) (write ssql out))
	((string? ssql) (handle-string ssql out))
	((number? ssql) (display ssql out))
	((bytevector? ssql) (handle-bit-string ssql out))
	((boolean? ssql) 
	 (if ssql (write/case "TRUE" out) (write/case "FALSE" out)))
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
  (define (write/uescape-rec s out)
    (let ((in (open-string-input-port s))
	  (uescape? (or uescape (and charset (not (string-every charset s)))))
	  (delimited? (or delimited 
			  (string-any non-identifier-chars s))))
      (if uescape?
	  (write/case "U&\"" out)
	  (if delimited? (put-char out #\")))
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
		     (put-char sout escape)
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
  (define (write/uescape f out)
    (if (string=? f "*")
	(put-char out #\*)
	(write/uescape-rec f out)))
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
		   (put-char out escape)
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

(define special-symbols '(- *))
(define (write/case v out :key (indent #f) :allow-other-keys)
  (cond ((string? v)
	 (case (*identifier-case*)
	   ((upper) (put-string out (string-upcase v)))
	   ((lower) (put-string out (string-downcase v)))
	   ((title) (put-string out (string-titlecase v)))
	   (else    (put-string out v))))
	((symbol? v)
	 (if (memq v special-symbols) ;; handle special case...
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
  (if indent
      (do ((times indent) (i 0 (+ i 1)))
	  ((= i times))
	(put-char out #\space))
      (put-char out #\space)))
(define (put-newline out indent) (when indent (newline out)))
(define (put-newline/space out indent) 
  (when indent (newline out))
  (put-indent out indent))
)
