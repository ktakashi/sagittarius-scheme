;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; packrat.scm - Packrat parser
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

;; Originally from chicken scheme's egg
;;   http://wiki.call-cc.org/eggref/4/packrat

;; modified to handle extra syntax
#!nobacktrace
(library (packrat)
    (export parse-result?
	    parse-result-successful?
	    parse-result-semantic-value
	    parse-result-next
	    parse-result-error

	    parse-results?
	    parse-results-position
	    parse-results-base
	    parse-results-next

	    parse-error?
	    parse-error-position
	    parse-error-expected
	    parse-error-messages

	    make-parse-position
	    parse-position?
	    parse-position-file
	    parse-position-line
	    parse-position-column

	    top-parse-position
	    update-parse-position
	    parse-position->string

	    ;;empty-results
	    ;;make-results

	    make-error-expected
	    make-error-message
	    make-result
	    make-expected-result
	    make-message-result

	    prepend-base
	    prepend-semantic-value

	    base-generator->results
	    results->result

	    parse-position>?
	    parse-error-empty?
	    merge-parse-errors
	    merge-result-errors

	    parse-results-token-kind
	    parse-results-token-value

	    packrat-check-base
	    packrat-check
	    packrat-or
	    packrat-unless
	    packrat-many

	    packrat-parser)
    (import (rnrs)
	    (rnrs r5rs)
	    (srfi :1)
	    ;; for format
	    (sagittarius)
	    ;; (sagittarius control) ;; for define-inline
	    (clos user) ;; to print parse-position better
	    (pp))

;; ****WARNING****
;; using define-inline for packrat-* procedure is very attempting
;; however the result wouldn't be so stunning. the inlining indeed
;; improves a bit of runtime performance however it also increase
;; a huge amount of compile time. as the concrete example, 
;; (text sql parser) uses (packrat) heavily and it's LoC is close
;; to 2000 (it'll be more in very near future). to compile this
;; library took 3.5 sec without inlining and over 8 sec with
;; inlining. the improvements of runtime performance is only
;; 1 sec for running its test (see test/tests/text/sql.scm).
;; so in total 7 sec loss if it's the first shot.

;; Packrat Parser Library
;;
;; Copyright (c) 2004, 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;; Copyright (c) 2005 LShift Ltd. <query@lshift.net>
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Requires: SRFI-1, SRFI-9, SRFI-6. See the documentation for more
;; details.

;; Sagittarius, use R6RS record
(define-record-type (parse-result make-parse-result parse-result?)
  (fields 
   (immutable successful? parse-result-successful?)
   (immutable semantic-value parse-result-semantic-value)
   (immutable next parse-result-next)
   ;; ^^ #f, if eof or error; otherwise a parse-results
   (immutable error parse-result-error)
   ;; ^^ #f if none, but usually a parse-error structure
  ))

(define-record-type (parse-results make-parse-results parse-results?)
  (fields 
   (immutable position parse-results-position)
   ;; ^^ a parse-position or #f if unknown
   (immutable base parse-results-base) ;; a value, #f indicating 'none' or 'eof'
   (mutable next parse-results-next* set-parse-results-next!)
   ;; ^^ a parse-results, or a nullary function delivering same, or #f for
   ;; nothing next (eof)
   (mutable map parse-results-map set-parse-results-map!)
   ;; ^^ an alist mapping a nonterminal to a parse-result
  ))

(define-record-type (parse-error make-parse-error parse-error?)
  (fields
   (immutable position parse-error-position)
   ;; ^^ a parse-position or #f if unknown
   (immutable expected parse-error-expected) ;; set of things (lset)
   (immutable messages parse-error-messages) ;; list of strings
  ))

(define-record-type (parse-position make-parse-position parse-position?)
  (fields 
   (immutable file parse-position-file)
   (immutable line parse-position-line)
   (immutable column parse-position-column))
  )

(define-method write-object ((o parse-position) out)
  (format out "#<parse-position ~a[~a:~a]>" 
	  (parse-position-file o)
	  (parse-position-line o)
	  (parse-position-column o)))

(define (top-parse-position filename)
  (make-parse-position filename 1 0))

(define (update-parse-position pos ch)
  (if (not pos)
      #f
      (let ((file (parse-position-file pos))
	    (line (parse-position-line pos))
	    (column (parse-position-column pos)))
	(case ch
	 ((#\return) (make-parse-position file line 0))
	 ((#\newline) (make-parse-position file (+ line 1) 0))
	 ((#\tab) (make-parse-position file line (* (quotient (+ column 8) 8) 8)))
	 (else (make-parse-position file line (+ column 1)))))))

(define (parse-position->string pos)
  (if (not pos)
      "<??>"
      (string-append (parse-position-file pos) ":"
		     (number->string (parse-position-line pos)) ":"
		     (number->string (parse-position-column pos)))))

(define (empty-results pos)
  (make-parse-results pos #f #f '()))

(define (make-results pos base next-generator)
  (make-parse-results pos base next-generator '()))

(define (make-error-expected pos str)
  (make-parse-error pos (list str) '()))

(define (make-error-message pos msg)
  (make-parse-error pos '() (list msg)))

(define (make-result semantic-value next)
  (make-parse-result #t semantic-value next #f))

(define (make-expected-result pos str)
  (make-parse-result #f #f #f (make-error-expected pos str)))

(define (make-message-result pos msg)
  (make-parse-result #f #f #f (make-error-message pos msg)))

(define (prepend-base pos base next)
  (make-parse-results pos base next '()))

(define (prepend-semantic-value pos key result next)
  (make-parse-results pos #f #f
		      (list (cons key (make-result result next)))))

(define (base-generator->results generator)
  ;; Note: applies first next-generator, to get first result
  (define (results-generator)
    (let-values (((pos base) (generator)))
      (if (not base)
	  (empty-results pos)
	  (make-results pos base results-generator))))
  (results-generator))

(define (parse-results-next results)
  (let ((next (parse-results-next* results)))
    (if (procedure? next)
	(let ((next-value (next)))
	  (set-parse-results-next! results next-value)
	  next-value)
	next)))

(define (results->result results key fn)
  (let ((results-map (parse-results-map results)))
    (cond
     ((assv key results-map) => cdr)
     (else (let ((result (fn)))
	     (set-parse-results-map! results (cons (cons key result) results-map))
	     result)))))

(define (parse-position>? a b)
  (cond
   ((not a) #f)
   ((not b) #t)
   (else (let ((la (parse-position-line a)) (lb (parse-position-line b)))
	   (or (> la lb)
	       (and (= la lb)
		    (> (parse-position-column a) (parse-position-column b))))))))

(define (parse-error-empty? e)
  (and (null? (parse-error-expected e))
       (null? (parse-error-messages e))))

(define (merge-parse-errors e1 e2)
  (cond
   ((not e1) e2)
   ((not e2) e1)
   (else
    (let ((p1 (parse-error-position e1))
	  (p2 (parse-error-position e2)))
      (cond
       ((or (parse-position>? p1 p2) (parse-error-empty? e2)) e1)
       ((or (parse-position>? p2 p1) (parse-error-empty? e1)) e2)
       (else (make-parse-error p1
			       (lset-union equal?
					   (parse-error-expected e1)
					   (parse-error-expected e2))
			       (append (parse-error-messages e1) (parse-error-messages e2)))))))))

(define (merge-result-errors result errs)
  (make-parse-result (parse-result-successful? result)
		     (parse-result-semantic-value result)
		     (parse-result-next result)
		     (merge-parse-errors (parse-result-error result) errs)))

;---------------------------------------------------------------------------

(define (parse-results-token-kind results)
  (let ((base (parse-results-base results)))
    (and base (car base))))

(define (parse-results-token-value results)
  (let ((base (parse-results-base results)))
    (and base (cdr base))))

(define (packrat-check-base token-kind k)
  (lambda (results)
    (let ((base (parse-results-base results)))
      (if (eqv? (and base (car base)) token-kind)
	  ((k (and base (cdr base))) (parse-results-next results))
	  (make-expected-result (parse-results-position results)
				(if (not token-kind)
				    "end-of-file"
				    token-kind))))))

(define (packrat-check parser k)
  (lambda (results)
    (let ((result (parser results)))
      (if (parse-result-successful? result)
	  (merge-result-errors ((k (parse-result-semantic-value result))
				(parse-result-next result))
			       (parse-result-error result))
	  result))))

(define (packrat-or p1 p2)
  (lambda (results)
    (let ((result (p1 results)))
      (if (parse-result-successful? result)
	  result
	  (merge-result-errors (p2 results)
			       (parse-result-error result))))))

(define (packrat-unless explanation p1 p2)
  (lambda (results)
    (let ((result (p1 results)))
      (if (parse-result-successful? result)
	  (make-message-result (parse-results-position results)
			       explanation)
	  (p2 results)))))

;; want *, + and ?

(define (packrat-many parser n m k)
  (lambda (results)
    (define (>=? many max) (and max (>= many max)))
    (define (return-results vs result k)
      (merge-result-errors ((k (map parse-result-semantic-value vs))
			    (parse-result-next result))
			   (parse-result-error result)))
    (when (and (zero? n) (eqv? n m))
      (error 'packrat-many "both min and max are zero" n m))
    (let loop ((i 0) (vs '()) (s #f) (results results))
      (if (>=? i m)
	  (return-results (reverse! vs) s k)
	  (let ((result (parser results)))
	    (cond ((parse-result-successful? result)
		   (loop (+ i 1) (cons result vs) result
			 (parse-result-next result)))
		  ;; no matching in *
		  ((zero? n)
		   (return-results (reverse! vs) 
				   (make-result #f results)
				   k))
		  ((<= n i)
		   ;; is this correct???
		   (return-results (reverse! vs) (or s result) k))
		  (else result)))))))

(define (packrat-peek parser k)
  (lambda (results)
    (let ((result (parser results)))
      (if (parse-result-successful? result)
	  (merge-result-errors ((k (parse-result-semantic-value result))
				results)
			       (parse-result-error result))
	  result))))

;---------------------------------------------------------------------------

;;(define (object->external-representation o)
;;  (let ((s (open-output-string)))
;;    (write o s)
;;    (get-output-string s)))
;; Sagittarius uses format
(define (object->external-representation o)
  (format "~s" o))

(define-syntax packrat-parser
  (syntax-rules (<- quote ! @ / = + * ? &)
    ((_ start (nonterminal (alternative body0 body ...) ...) ...)
     (let ()
       (define nonterminal
	 (lambda (results)
	   (results->result results 'nonterminal
			    (lambda ()
			      ((packrat-parser #f "alts" nonterminal
					       ((begin body0 body ...) alternative) ...)
			       results)))))
       ...
       start))

    ((_ #f "alts" nt (body alternative))
     (packrat-parser #f "alt" nt body alternative))

    ((_ #f "alts" nt (body alternative) rest0 rest ...)
     (packrat-or (packrat-parser #f "alt" nt body alternative)
		 (packrat-parser #f "alts" nt rest0 rest ...)))

    ((_ #f "alt" nt body ())
     (lambda (results) (make-result body results)))

    ((_ #f "alt" nt body ((! fails ...) rest ...))
     (packrat-unless (string-append "Nonterminal " (symbol->string 'nt)
				    " expected to fail "
				    (object->external-representation '(fails ...)))
		     (packrat-parser #f "alt" nt #t (fails ...))
		     (packrat-parser #f "alt" nt body (rest ...))))

    ((_ #f "alt" nt body ((/ alternative ...) rest ...))
     (packrat-check (packrat-parser #f "alts" nt (#t alternative) ...)
		    (lambda (result) (packrat-parser #f "alt" nt body (rest ...)))))

    ;; extra
    ((_ #f "alt" nt body ((= n m val val* ...) rest ...))
     (packrat-many (packrat-parser #f "expr" nt () (val val* ...))
		   n m
		   (lambda (dummy)
		     (packrat-parser #f "alt" nt body (rest ...)))))
    ((_ #f "alt" nt body ((+ val val* ...) rest ...))
     (packrat-parser #f "alt" nt body ((= 1 #f val val* ...) rest ...)))
    ((_ #f "alt" nt body ((* val val* ...) rest ...))
     (packrat-parser #f "alt" nt body ((= 0 #f val val* ...) rest ...)))
    ((_ #f "alt" nt body ((? val val* ...) rest ...))
     (packrat-parser #f "alt" nt body ((= 0 1 val val* ...) rest ...)))
    ;; a bit awkward but don't want to change the structure
    ((_ #f "alt" nt body (var <- (= n m val val* ...) rest ...))
     (packrat-many (packrat-parser #f "expr" nt () (val val* ...))
		   n m
		   (lambda (var)
		     (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body (var <- (+ val val* ...) rest ...))
     (packrat-parser #f "alt" nt body (var <- (= 1 #f val val* ...) rest ...)))
    ((_ #f "alt" nt body (var <- (* val val* ...) rest ...))
     (packrat-parser #f "alt" nt body (var <- (= 0 #f val val* ...) rest ...)))
    ((_ #f "alt" nt body (var <- (? val val* ...) rest ...))
     (packrat-parser #f "alt" nt body (var <- (= 0 1 val val* ...) rest ...)))

    ;; peek
    ((_ #f "alt" nt body ((& val) rest ...))
     (packrat-peek (packrat-parser #f "expr" nt () val)
		   (lambda (result)
		     (packrat-parser #f "alt" nt body (rest ...)))))

    ;; TODO this should be merged with "expr" entry
    ((_ #f "alt" nt body (var <- 'val rest ...))
     (packrat-check-base 'val
			 (lambda (var)
			   (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body (var <- @ rest ...))
     (lambda (results)
       (let ((var (parse-results-position results)))
	 ((packrat-parser #f "alt" nt body (rest ...)) results))))

    ;; (var <- (/ expr expr* ...))
    ((_ #f "alt" nt body (var <- (/ alternative ...) rest ...))
     (packrat-check (packrat-parser #f "expr" nt () (/ alternative ...))
		    (lambda (var) 
		      (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body (var <- val rest ...))
     (packrat-check val
		    (lambda (var)
		      (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body ('val rest ...))
     (packrat-check-base 'val
			 (lambda (dummy)
			   (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body (val rest ...))
     (packrat-check val
		    (lambda (dummy)
		      (packrat-parser #f "alt" nt body (rest ...)))))
    ;; extra
    ;; it's a bit ugly and duplicated code but works fine.
    ;; the expression rule won't have bindings (sort of impossible
    ;; with this macro model). so following is invalid;
    ;;   (+ i+ <- item+ i* <- item*)
    ;; instead of using above use this;
    ;;   (vs <- (+ item+ item*)) ;; -> vs ((+ *) (+ *))
    ;; TODO refactor it
    ((_ #f "expr" nt var 'expr)
     (lambda (results) 
       (let ((base (parse-results-base results)))
	 (if (and base (eqv? (car base) 'expr))
	     (make-result (cdr base) (parse-results-next results))
	     (make-expected-result (parse-results-position results)
				   "dummy")))))
    ((_ #f "expr" nt var (/ e1 e2 ...))
     (packrat-or (packrat-parser #f "expr" nt var e1)
		 (packrat-parser #f "expr" nt var (/ e2 ...))))
    ((_ #f "expr" nt var (/))
     (lambda (results)
       (make-expected-result (parse-results-position results)
			     (string-append "no match for "
					    (symbol->string 'nt)))))
    ((_ #f "expr" nt var (= n m val val* ...)) 
     (packrat-many (packrat-parser #f "expr" nt var (val val* ...))
		   n m
		   (lambda (value)
		     (lambda (results) (make-result value results)))))
    ((_ #f "expr" nt var (+ val val* ...))
     (packrat-parser #f "expr" nt var (= 1 #f val val* ...)))
    ((_ #f "expr" nt var (* val val* ...))
     (packrat-parser #f "expr" nt var (= 0 #f val val* ...)))
    ((_ #f "expr" nt var (? val val* ...))
     (packrat-parser #f "expr" nt var (= 0 1  val val* ...)))
    ;; to accept (+ ((token "...")) ...) thing
    ;; a bit awkward but this can be useful
    ((_ #f "expr" nt (var ...) ((e1) e2 ...))
     ;; new expression doesn't need to inherit the temporary variables.
     (packrat-check e1
		    (lambda (tmp)
		      ;; add temporary variable (may not be used in the end)
		      (packrat-parser #f "expr" nt 
				      (tmp var ...) (e2 ...)))))
    ((_ #f "expr" nt (var ...) (e1 e2 ...))
     ;; new expression doesn't need to inherit the temporary variables.
     (packrat-check (packrat-parser #f "expr" nt () e1)
		    (lambda (tmp)
		      ;; add temporary variable (may not be used in the end)
		      (packrat-parser #f "expr" nt 
				      (tmp var ...) (e2 ...)))))

    ;; handling special case for convenience.
    ((_ #f "expr" nt (var) ()) 
     (lambda (results) (make-result var results)))
    ((_ #f "expr" nt (var ...) ())
     ;; the veriable is reverse order
     (lambda (results) (make-result (reverse! (list var ...)) results)))
    ((_ #f "expr" nt var expr) expr)
    ))

'(define (x)
  (sc-expand
   '(packrat-parser expr
		    (expr ((a <- mulexp '+ b <- mulexp)
			   (+ a b))
			  ((a <- mulexp) a))
		    (mulexp ((a <- simple '* b <- simple)
			     (* a b))
			    ((a <- simple) a))
		    (simple ((a <- 'num) a)
			    (('oparen a <- expr 'cparen) a)))))

)
