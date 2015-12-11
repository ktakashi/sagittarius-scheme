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
	    ;; ssql->sql-indent ;; TODO

	    ;; parameters
	    *non-unicode-charset*
	    *unicode-escape*
	    *use-upper?*
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
;; writes SQL with upper case or not
;; default #f
(define *use-upper?* (make-parameter #t))

(define *character-converter* (make-parameter default-converter))

;; similar with srl:sxml->xml
(define (ssql->sql ssql :optional (port #f))
  (if port
      (write-ssql ssql port)
      (call-with-string-output-port (lambda (out) (write-ssql ssql out)))))

(define (write-ssql ssql out . opt)
  (if (pair? ssql)
      (apply (lookup-writer (car ssql)) ssql out opt)
      (write-value ssql out)))

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
	   (write/case name out)
	   (apply write-args args out opt)))))

(define (infix-operator-writer ssql out . opt)
  (let ((name (car ssql))
	(args (cdr ssql)))
    (apply write-ssql (car args) out opt)
    (put-char out #\space)
    (write/case name out)
    (put-char out #\space)
    (apply write-ssql (cadr args) out opt)))

(define (write-args args out . opt)
  (put-char out '#\()
  (let loop ((args args) (first #t))
    (cond ((null? args) (put-char out '#\)))
	  (else
	   (unless first (put-char out '#\,))
	   (apply write-ssql (car args) out opt)
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
  (syntax-rules ()
    ((_ (keyword ssql out . opt) (pattern exprs ...) ...)
     (define-raw-sql-writer (keyword ssql out . opt)
       (match ssql
	 (pattern exprs ...) ...
	 (else (assertion-violation 'keyword "incorrect input" ssql)))))
    ((_ keyword alias) (define-raw-sql-writer keyword alias))))

(define (write/comma out column columns opt)
  (apply write-ssql column out opt)
  (for-each (lambda (column) 
	      (put-char out #\,) 
	      (apply write-ssql column out opt)) columns))
(define (write/comma* out columns opt)
  (unless (null? columns)
    (write/comma out (car columns) (cdr columns) opt)))

(define-syntax with-parenthesis
  (syntax-rules ()
    ((_ out expr ...)
     (begin (put-char out #\() expr ...  (put-char out #\))))))

;; select
(define-sql-writer (select ssql out . opt)
  (('select '* rest ...)
   (write/case "SELECT *" out)
   (for-each (lambda (clause) (apply write-ssql clause out opt)) rest))
  (('select (column columns ...) rest ...)
   (write/case "SELECT " out)
   (write/comma out column columns opt)
   (for-each (lambda (clause) (apply write-ssql clause out opt)) rest)))

(define (basic-insert out opt table cols override? vals)
  (write/case "INSERT INTO " out)
  (apply write-ssql table out opt)
  (when cols (with-parenthesis out (write/comma* out cols opt)))
  (when override? (put-char out #\space) (write/case override? out))
  (when vals
    (write/case " VALUES" out)
    (with-parenthesis out (write/comma* out (car vals) opt))
    (for-each (lambda (v) 
		(put-char out #\,)
		(with-parenthesis out (write/comma* out v opt)))
	      (cdr vals))))
(define (query-insert out opt table cols overriding? query)
  (write/case "INSERT INTO " out)
  (apply write-ssql table out opt)
  (when cols (with-parenthesis out (write/comma* out cols opt)))
  (when overriding? (put-char out #\space) (write/case override? out))
  (put-char out #\space)
  (apply write-ssql query out opt))

(define-sql-writer (insert-into ssql out . opt)
  (('insert-into table (cols ...) ('values vals ...))
   (basic-insert out opt table cols #f vals))
  ;; overriding
  (('insert-into table (cols ...) symbol ('values vals ...))
   (basic-insert out opt table cols symbol vals))
  (('insert-into table ('values vals ...))
   (basic-insert out opt table #f #f vals))
  (('insert-into table symbol ('values vals ...))
   ;; overriding
   (basic-insert out opt table #f symbol vals))
  (('insert-into table 'default-values)
   (basic-insert out opt table #f 'default-values #f))
  ;; insert select
  (('insert-into table (cols ...) query) 
   (query-insert out opt table cols #f query))
  (('insert-into table (cols ...) symbol query) 
   (query-insert out opt table cols symbol query))
  (('insert-into table query)
   (query-insert out opt table #f #f query))
  (('insert-into table symbol query)
   (query-insert out opt table #f symbol query)))
   
   
(define-sql-writer (as ssql out . opt)
  (('as a b)
   (put-char out #\()
   (apply write-ssql a out opt)
   (put-char out #\))
   (write/case " AS " out)
   (apply write-ssql b out opt)))

(define-sql-writer (from ssql out . opt)
  (('from table rest ...)
   (write/case " FROM " out)
   (if (and (pair? table) (not (eq? (car table) 'as)))
       ;; join
       (begin 
	 (apply write-ssql (car table) out opt)
	 (for-each (lambda (join)
		     (put-char out #\space)
		     (apply write-ssql join out opt)) (cdr table)))
       ;; normal or query
       (apply write-ssql table out opt))
   (write/comma* out rest opt)))

;; we don't check join type for alias
(define-sql-writer (join ssql out . opt)
  ((type table . condition)
   (write/case type out)
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
   (write/case "USING(" out)
   (unless (null? columns)
     (apply write-ssql (car columns) out opt)
     (for-each (lambda (column) 
		 (put-char out #\,)
		 (apply write-ssql column out opt)) 
	       (cdr columns)))
   (put-char out #\))))

(define-sql-writer (where ssql out . opt)
  (('where condition)
   (write/case " WHERE " out)
   (apply write-ssql condition out opt)))


(define-sql-writer (and ssql out . opt)
  (('and condition conditions ...)
   (apply write-ssql condition out opt)
   (for-each (lambda (condition) 
	       (write/case " AND " out)
	       (apply write-ssql condition out opt)) conditions)))

(define-sql-writer (or ssql out . opt)
  (('or condition conditions ...)
   (apply write-ssql condition out opt)
   (for-each (lambda (condition) 
	       (write/case " OR " out)
	       (apply write-ssql condition out opt)) conditions)))

(define-sql-writer (~ ssql out . opt)
  (('~ id ids ...)
   (apply write-ssql id out opt)
   (for-each (lambda (id) 
	       (put-char out #\.)
	       (apply write-ssql id out opt)) ids)))

(define-sql-writer (^ ssql out . opt)
  (('^ id ids ...)
   (apply write-ssql id out opt)
   (for-each (lambda (id) 
	       (put-string out "||")
	       (apply write-ssql id out opt)) ids)))

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

;; TBD lot more to go...

;;; atom value
(define (write-value ssql out)
  (cond ((symbol? ssql) (handle-identifier ssql out))
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
(define (handle-identifier ssql out)
  (define charset (*non-unicode-charset*))
  (define escape (*unicode-escape*))
  (define converter (*character-converter*))
  (define (write/uescape s out)
    (let ((in (open-string-input-port s))
	  (uescape? (and charset (not (string-every charset s))))
	  (delimited? (string-any +sql-special-character-set+ s)))
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
	(write/case " uescape '" out)
	(if (eqv? escape #\')
	    (put-string out "''")
	    (put-char out escape))
	(put-char out #\'))))

  (let ((fragments (string-tokenize (symbol->string ssql) not-dot-set)))
    (write/uescape (car fragments) out)
    (for-each (lambda (f) (put-char out #\.) (write/uescape f out))
	      (cdr fragments))))

;; write SQL string
;; Do the followings:
;;  - escape quote(')
;;  - detect unicode
(define (handle-string ssql out)
  (define escape (*unicode-escape*))
  (define (escape-quote s)
    (define charset (*non-unicode-charset*))
    (define converter (*character-converter*))
    (let-values (((out extract) (open-string-output-port))
		 ((in) (open-string-input-port s)))
      (let loop ((uescape? #f))
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
    (when uescape? (write/case "u&" out))
    (put-char out #\') 
    (put-string out s)
    (put-char out #\')
    (when (and uescape? escape (not (eqv? escape #\\)))
      (write/case " uescape '" out)
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
(define (write/case v out)
  (cond ((string? v)
	 (if (*use-upper?*)
	     (put-string out (string-upcase v))
	     (put-string out (string-downcase v))))
	((symbol? v)
	 ;; 'sym-sym' would be printed 'sym sym'
	 (let ((s (string-tokenize (symbol->string v) not-minus-set)))
	   (write/case (car s) out)
	   (for-each (lambda (s) (put-char out #\space) (write/case s out))
		     (cdr s))))
	;; numbers?
	(else (display v out))))

)
