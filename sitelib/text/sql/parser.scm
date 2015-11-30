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
;; for now use '@ to represents qualifier
;; e.g. 'foo.bar' = (@ foo bar)
;; TODO should we use other? (of cource '.' would be the best if we can)
;;      or should we make like (foo @ bar)?
(define (concate-identifier base follow)
  (if (eq? (car follow) '@)
      `(@ ,base ,(cdr follow))
      `(@ ,base ,follow)))

(define sql-parser
  (packrat-parser
   (begin
     stmt)
   (stmt ((s <- select-stmt) s)
	 ;; TODO more
	 )
   (select-stmt (('select c <- select-list t <- table-expression) 
		 (cons* 'select c t))
		;; select (distinct|all) column from table
		(('select q <- set-qualifier 
			  c <- select-list 
			  t <- table-expression) 
		 (cons* 'select q c t))
		;; extension: select 1+1; or so
		(('select c <- select-list) (list 'select c)))
   (select-list (('#\*) '*)
		((s <- select-sublist s* <- select-sublist*) (cons s s*)))
   (select-sublist ((d <- derived-column) d)
		   ((q <- qualified-asterisk) q))
   (select-sublist* (('#\, s <- select-sublist) s)
		    (() '()))
   ;; TODO 'select foo.*' or so
   (qualified-asterisk ((c <- identifier-chain '#\. '#\*) 
			(concate-identifier c '(*))))
   (identifier-chain  ((i <- identifier i* <- identifier-chain*)
		       (if (null? i*)
			   i
			   (concate-identifier i i*))))
   (identifier-chain* (('#\. i <- identifier-chain) i)
		      (() '()))

   (derived-column ((v <- value-expression) v)
		   ((v <- value-expression 'as c <- column-name) 
		    (list 'as v c)))
   (column-name ((i <- identifier) i))
   ;; value
   (value-expression ((n <- 'number) n)
		     ((s <- 'string) s) ;; TODO ESCAPE+unicode
		     ((b <- 'bit-string) b)
		     ;; TODO more
		     ((c <- column-reference) c))

   (set-qualifier (('distinct) 'distinct)
		  (('all) 'all))

   (column-reference ((i <- identifier-chain) i))

   ;; table
   (table-expression ((f <- from-clause 
		       w <- where-clause
		       g <- group-by-clause
		       h <- having-clause
		       wd <- window-clause)
		      `(,f ,w ,g ,h wd)))
   (from-clause (('from t <- table-reference-list) (list 'from t)))
   (table-reference-list ((t <- table-reference t* <- table-reference-list*)
			  (cons t t*))
			 )
   (table-reference-list* (('#\, t <- table-reference) t)
			  (() '()))
   ;; TODO sample clause
   (table-reference ((t <- table-primary-or-join) t))
   (table-primary-or-join ((t <- table-primary) t)
			  #;((j <- join-clause) j))
   ;; TODO properly done
   (table-primary ((t <- identifier) t))

   ;; where
   (where-clause (('where s <- search-condition) (cons w s)))

   (search-condition ((b <- boolean-value-expression) b))
   ;; FIXME
   (boolean-value-expression (() '()))
   (group-by-clause (('group 'by g* group-by-element-list)
		     (list 'group-by g*))
		    (('group 'by s <- set-qualifier g* <- group-by-element-list)
		     (list 'group-by s g*)))
   ;; TODO 
   (group-by-element-list (() '()))
   ;; having
   (having-clause (('having s <- search-condition) (cons 'having s)))
   ;; window
   (window-clause (('windows w* <- window-definition-list) (cons 'windows w*)))
   ;; TODO
   (window-definition-list (() '()))

   ;; identifier
   ;; we need to resolve unicode here as well
   (identifier ((i <- 'identifier 'uescape c <- 'string)
		(unless (and (pair? i) (eq? (car i) 'unicode))
		  (raise-sql-parse-error 'parse-sql 
					 "invalid use of UESCAPE" #f #f))
		;; (unicode (! "foo") uescape "c")
		`(,@i 'uescape ,c))
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
