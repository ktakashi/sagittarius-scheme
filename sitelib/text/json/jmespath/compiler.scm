;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/jmespath/compiler.scm - JMESPath compiler
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
#!nounbound
(library (text json jmespath compiler)
    (export jmespath:compile 
	    jmespath-error-expression
	    jmespath-error-arguments
	    jmespath-compile-error?
	    jmespath-runtime-error?)
    (import (rnrs)
	    (text json jmespath conditions)
	    (util list)
	    (match)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :133 vectors))

(define-record-type jmespath-context
  (fields json parent))
(define $j jmespath-context-json)
(define $c make-jmespath-context)
(define (context*->context c* p) ($c (map $j c*) p))

(define-condition-type &jmespath:expression &jmespath
  dummy dummy? ;; we don't use this
  (expression jmespath-error-expression))
(define-condition-type &jmespath:compile &jmespath:expression
  make-jmespath-compile-error jmespath-compile-error?)
(define-condition-type &jmespath:runtime &jmespath:expression
  make-jmespath-runtime-error jmespath-runtime-error?
  (arguments jmespath-error-arguments))

(define (jmespath-compile-error message expression)
  (raise (condition
	  (make-jmespath-compile-error expression)
	  (make-assertion-violation)
	  (make-who-condition 'jmespath:compile)
	  (make-message-condition message)
	  (make-irritants-condition expression))))
(define (jmespath-runtime-error who message expression . arguments)
  (raise (condition
	  (make-jmespath-runtime-error expression arguments)
	  (make-who-condition who)
	  (make-message-condition message))))

(define (make-root-context json)
  (make-jmespath-context json #f))
(define (make-result-context json parent)
  (make-jmespath-context json parent))

;; from OrExpressions section
(define (false-value? v)
  (or (null? v)
      (and (vector? v) (zero? (vector-length v)))
      (and (string? v) (zero? (string-length v)))
      (not v)
      (eqv? v 'null)))
;; hmmmm, we need to utilise this
(define (json=? a b)
  (define (entry=? a b)
    (and (json=? (car a) (car b))
	 (json=? (cdr a) (cdr b))))
  (define (key-compare a b) (string<? (car a) (car b)))
  (cond ((and (string? a) (string? b)) (string=? a b))
	;; 1 and 1.0 are not the same so can't be = or equal?
	((and (number? a) (number? b)) (eqv? a b))
	((and (vector? a) (vector? b))
	 (vector-every entry=?
		       (vector-sort key-compare a)
		       (vector-sort key-compare b)))
	((and (list? a) (list? b)) (for-all json=? a b))
	(else (eq? a b))))

;; receives AST parsed by (text json jmespath compiler)
;; and returns a procedure takes one argument, sexp JSON
(define (jmespath:compile jmespath-ast)
  (let-values (((type expression) (compile-expression jmespath-ast)))
    (lambda (json)
      (define root-context (make-root-context json))
      ($j (expression root-context)))))

;; e -> context -> context
(define (compile-expression e)
  (cond ((string? e) (jmespath:compile-identifier e))
	((eq? e '*) (jmespath:compile-wildcard-expression e))
	((eq? e '@) (jmespath:compile-current-expression e))
	((pair? e)
	 (let ((e0 (car e)))
	   (if (symbol? e0)
	       (case (car e)
		 ((ref) (jmespath:compile-sub-expression e))
		 ((not) (jmespath:compile-not-expression e))
		 ((index) (jmespath:compile-index-expression e))
		 ((slice) (jmespath:compile-slice-expression e))
		 ((flatten) (jmespath:compile-flatten-expression e))
		 ((filter) (jmespath:compile-filter-expression e))
		 ((or) (jmespath:compile-or-expression e))
		 ((and) (jmespath:compile-and-expression e))
		 ((< <= = >= > !=) (jmespath:compile-comparator-expression e))
		 ((pipe) (jmespath:compile-pipe-expression e))
		 ((quote) (jmespath:compile-literal-expression e))
		 ((&) (jmespath:compile-expression-reference e))
		 ((*) (jmespath:compile-array-wildcard-expression e))
		 (else (jmespath:compile-function e)))
	       (jmespath:compile-multi-select-list e))))
	((vector? e) (jmespath:compile-multi-select-hash e))
	(else (jmespath-compile-error "Unknown expression" e))))

(define-syntax define-compiler
  (syntax-rules ()
    ((_ type (name e) body ...)
     (define (name arg)
       (values 'type ((lambda (e) body ...) arg))))))
(define-syntax json-lambda
  (syntax-rules ()
    ((_ (json) expr ...)
     (lambda (context)
       (define (inner json) expr ...)
       (make-result-context (inner ($j context)) context)))))

(define-compiler identifier (jmespath:compile-identifier s)
  (define key=? (lambda (k&v) (and (string=? s (car k&v)) k&v)))
  (define (get json)
    (cond ((vector-any key=? json) => cdr)
	  (else 'null)))
  (json-lambda (json)
    (if (vector? json) (get json) 'null)))

(define (not-null? e) (not (eq? 'null e)))
(define-compiler wildcard (jmespath:compile-wildcard-expression e)
  (json-lambda (json)
    (if (vector? json)
	(filter not-null? (map cdr (vector->list json)))
	'null)))

(define-compiler current (jmespath:compile-current-expression e)
  (lambda (context) context))

;; the projection seems only ocucres in sub expression
;; (I couldn't read it from the spec but example are working like that...)
;; here we need to handle special cases like [? expr ][]
(define-compiler sub (jmespath:compile-sub-expression e)
  (let ((e (fold-right
	    (lambda (e0 acc)
	      (define (->pred pred)
		(lambda (context) (pred ($j context))))
	      (let-values (((type e*) (compile-expression e0)))
		(case type
		  ((filter array* slice wildcard flatten)
		   (let ((pred (->pred (if (memq type '(wildcard flatten))
					   not-null?
					   not-false-value?))))
		     (lambda (context)
		       (define (->result c*) (context*->context c* context))
		       (let* ((next-context (e* context))
			      (v ($j next-context)))
			 (if (eq? v 'null)
			     ($c 'null context)
			     (->result
			      (filter pred
				      (map (lambda (v)
					     (acc ($c v context))) v))))))))
		  (else
		   (lambda (context)
		     (let ((next-context (e* context)))
		       (if (eq? ($j next-context) 'null)
			   ($c 'null context)
			   (acc next-context))))))))
	    (lambda (context) context)
	    (cdr e))))
    (lambda (context) (e context))))

(define (not-false-value? e) (not (false-value? e)))
(define-compiler array* (jmespath:compile-array-wildcard-expression e)
  (json-lambda (json)
    (if (list? json)
	(filter not-false-value? json)
	'null)))

(define-compiler index (jmespath:compile-index-expression e)
  (let ((n (cadr e)))
    (if (and (exact? n) (integer? n))
	(json-lambda (json)
	  (if (list? json)
	      (let ((l (length json)))
		(cond ((and (<= 0 n) (< n l))
		       (list-ref json n))
		      ((and (negative? n) (< (- (abs n) 1) l))
		       (list-ref json (+ l n)))
		      (else 'null)))
	      'null))
	(jmespath-compile-error
	 "Index must have either exact integer or *" e))))

(define-compiler slice (jmespath:compile-slice-expression e)
  (define (pget n l default)
    (cond ((not n) default)
	  ((negative? n) (max (+ l n) 0))
	  ((< l n) l)
	  (else n)))
  (define (nget n l default)
    (cond ((not n) default)
	  ((negative? n) (max (+ l n) -1))
	  ((<= l n) (- l 1))
	  (else n)))
  (define (do-slice start end step e0)
    (unless (number? step) (jmespath-compile-error "step must be a number" e))
    (when (zero? step) (jmespath-compile-error "step can't be 0" e))
    (let ((cmp (if (negative? step) <= >=))
	  (e0 (compile-expression-w/o-type e0)))
      (lambda (context)
	(let* ((next (e0 context))
	       (json ($j next)))
	  ($c (if (list? json)
		  (let ((l (length json)))
		    (let-values (((s e)
				  (if (negative? step)
				      (values (nget start l (- l 1))
					      (nget end l -1))
				      (values (pget start l 0)
					      (pget end l l)))))
		      ;; TODO slow?
		      (let loop ((i s) (r '()))
			(if (cmp i e)
			    (filter not-null? (reverse! r))
			    (loop (+ i step) (cons (list-ref json i) r))))))
		  'null)
	      context)))))
  (match e
    ((_) (do-slice #f #f 1 '@))
    ((_ s) (do-slice s #f 1 '@))
    ((_ s e) (do-slice s e 1 '@))
    ((_ s e step) (do-slice s e step '@))
    ((_ s e step e0) (do-slice s e step e0))))

(define (compile-expression-w/o-type e)
  (let-values (((t e) (compile-expression e)))
    e))
(define-compiler flatten (jmespath:compile-flatten-expression e)
  (unless (or (null? (cdr e)) (null? (cddr e)))
    (jmespath-compile-error "flatten must have one or two elements" e))
  (let ((e0 (compile-expression-w/o-type (if (null? (cdr e)) '@ (cadr e)))))
    (lambda (context)
      (let* ((n (e0 context))
	     (v ($j n)))
	($c (if (list? v)
		;; lazy
		(filter not-null?
			(append-map
			 (lambda (v) (if (list? v) v (list v))) v))
		'null)
	    context)))))

(define-compiler filter (jmespath:compile-filter-expression e)
  (let ((e0 (compile-expression-w/o-type (cadr e))))
    (lambda (context)
      (let ((json ($j context)))
	($c (if (list? json)
		(filter-map
		 (lambda (elm)
		   (let ((v ($j (e0 ($c elm context)))))
		     (and (not (false-value? v)) elm))) json)
		'null)
	    context)))))

(define-compiler or (jmespath:compile-or-expression e)
  (let ((e* (map compile-expression-w/o-type (cdr e))))
    (lambda (context)
      (let loop ((e* e*) (v ($c 'null context)))
	(if (null? e*)
	    v
	    (let ((v ((car e*) context)))
	      (if (false-value? ($j v))
		  (loop (cdr e*) v)
		  v)))))))

(define-compiler and (jmespath:compile-and-expression e)
  (let ((e* (map compile-expression-w/o-type (cdr e))))
    (lambda (context)
      (let loop ((e* e*) (v ($c 'null context)))
	(if (null? e*)
	    v
	    (let ((v2 ((car e*) context)))
	      (if (false-value? ($j v2))
		  v2
		  (loop (cdr e*) v2))))))))

(define-compiler not (jmespath:compile-not-expression e)
  (let ((e (compile-expression-w/o-type (cadr e))))
    (lambda (context)
      (let ((v (e context)))
	;; false-like values?
	($c (false-value? ($j v)) context)))))

(define-compiler multi-list (jmespath:compile-multi-select-list e)
  (let ((e* (map compile-expression-w/o-type e)))
    (lambda (context)
      (let ((json ($j context)))
	(if (eq? 'null json)
	    context
	    (context*->context (map (lambda (e) (e context)) e*) context))))))

(define-compiler multi-hash (jmespath:compile-multi-select-hash e)
  (let ((e* (vector-map (lambda (k&v)
			  (cons (car k&v)
				(compile-expression-w/o-type (cdr k&v)))) e)))
    (lambda (context)
      (let ((json ($j context)))
	(if (eq? 'null json)
	    context
	    ($c (vector-map (lambda (n&e)
			      (let ((v ((cdr n&e) context)))
				(cons (car n&e) ($j v)))) e*)
		context))))))
  
(define-compiler comparator (jmespath:compile-comparator-expression e)
  (let ((cmp (car e))
	(le (compile-expression-w/o-type (cadr e)))
	(re (compile-expression-w/o-type (caddr e))))
    (lambda (context)
      (let ((lhs ($j (le context)))
	    (rhs ($j (re context))))
	($c (case cmp
	      ((< <= >= >)
	       (if (and (number? lhs) (number? rhs))
		   (cond ((eq? cmp '<)  (< lhs rhs))
			 ((eq? cmp '>)  (> lhs rhs))
			 ((eq? cmp '<=) (<= lhs rhs))
			 ((eq? cmp '>=) (>= lhs rhs)))
		   'null))
	      ((=) (json=? lhs rhs))
	      ((!=) (not (json=? lhs rhs))))
	    context)))))

(define-compiler pipe (jmespath:compile-pipe-expression e)
  (let ((e* (map compile-expression-w/o-type (cdr e))))
    (lambda (context)
      (fold-left (lambda (context e) (e context)) context e*))))

(define-compiler literal (jmespath:compile-literal-expression e)
  (let ((v (cadr e)))
    (lambda (context) (make-root-context v))))

;; This must only be used by function but this is easier for me
(define-compiler expr-ref (jmespath:compile-expression-reference e)
  (let ((e (compile-expression-w/o-type (cadr e))))
    (lambda (context) ($c e context))))

(define-compiler function (jmespath:compile-function e)
  (define (lookup-function name)
    (cond ((assq name +jmespath:buildin-functions+) => cdr)
	  ;; TODO user defined function
	  (else #f)))
  (let ((func (lookup-function (car e)))
	(e* (map compile-expression-w/o-type (cdr e))))
    (unless func (jmespath-compile-error "No such function" (car e)))
    (lambda (context)
      (let ((args (map (lambda (e) (e context)) e*)))
	(guard (ex ((jmespath-runtime-error? ex) (raise ex))
		   (else (apply jmespath-runtime-error (car e)
				(condition-message ex) e args)))
	  (apply func context e args))))))

(define-syntax define-function
  (syntax-rules ()
    ((_ (name c e a ...) body ...)
     (define (name c e a ...)
       (define (inner c e a ...) body ...)
       ($c (inner c e ($j a) ...) c)))
    ((_ (name c e a ... . r) body ...)
     (define (name c e a ... . r)
       (define (inner c e a ... . r) body ...)
       ($c (apply inner c e ($j a) ... (map $j r)) c)))))

(define-function (jmespath:abs-function context expression argument)
  (unless (number? argument)
    (jmespath-runtime-error 'abs "Number required" expression argument))
  (abs argument))

(define-function (jmespath:avg-function context expression argument)
  (unless (and (list? argument) (for-all number? argument))
    (jmespath-runtime-error 'avg "Array of number required"
			    expression argument))
  (if (null? argument)
      'null
      (let* ((len (length argument))
	     (v (/ (fold + 0 argument) len)))
	(if (integer? v) v (inexact v)))))

(define-function (jmespath:contains-function context expression subject search)
  (cond ((list? subject) (exists (lambda (e) (json=? e search)) subject))
	((string? subject)
	 (and (string? search)
	      (string-contains subject search)
	      #t))
	(else
	 (jmespath-runtime-error 'contains "Array or string required"
				 expression subject search))))

(define-function (jmespath:ceil-function context expression value)
  (if (number? value)
      (exact (ceiling value))
      ;; Example of the specification says returning null
      ;; Tutorial implementation raised an error.
      ;; Specification itself saying if the type is mismatched
      ;; raise an error. so follow it
      (jmespath-runtime-error 'ceil "Number required" expression value)))

(define-function (jmespath:ends-with-function context expression subject suffix)
  (unless (and (string? subject) (string? suffix))
    (jmespath-runtime-error 'ends_with "String required"
			    expression subject suffix))
  (string-suffix? suffix subject))

(define-function (jmespath:floor-function context expression value)
  (if (number? value)
      (exact (floor value))
      (jmespath-runtime-error 'floor "Number required" expression value)))

(define-function (jmespath:join-function context expression glue strings)
  (if (and (string? glue) (for-all string? strings))
      (string-join strings glue)
      (jmespath-runtime-error 'join "String and array of string required"
			      expression glue strings)))

(define-function (jmespath:keys-function context expression obj)
  (if (vector? obj)
      (vector->list (vector-map car obj)) ;; TODO performance?
      (jmespath-runtime-error 'keys "Object required" expression obj)))

(define-function (jmespath:length-function context expression subject)
  (cond ((vector? subject) (vector-length subject))
	((list? subject) (length subject))
	((string? subject) (string-length subject))
	(else
	 (jmespath-runtime-error 'length "String, array or object required"
				 expression subject))))

(define-function (jmespath:map-function context expression expr array)
  (unless (list? array)
    (jmespath-runtime-error 'map "array required" expression array))
  (map (lambda (e) ($j (expr ($c e context)))) array))

(define-function (jmespath:max-function context expression array)
  (unless (list? array)
    (jmespath-runtime-error 'max "array required" expression array))
  (cond ((null? array) 'null)
	((for-all number? array) (apply max array))
	((for-all string? array)
	 ;; TODO create string-max?
	 (let loop ((s (car array)) (s* (cdr array)))
	   (cond ((null? s*) s)
		 ((string< s (car s*)) (loop (car s*) (cdr s*)))
		 (else (loop s (cdr s*))))))
	(else
	 (jmespath-runtime-error 'max "array of number or string required"
				 expression array))))

(define-syntax define-by-function
  (syntax-rules (comparator)
    ((_ name func (comparator cmp))
     (define-by-function name func
       (lambda (p&v)
	 (cond ((reduce (lambda (x identity)
			  (cond ((not identity) x)
				((cmp (cdr x) (cdr identity)) x)
				(else identity)))
			#f p&v) => car)
	       (else 'null)))))
    ((_ name func reducer)
     (define-function (name context expression array expr)
       (unless (list? array)
	 (jmespath-runtime-error 'func "array required" expression array))
       (unless (procedure? expr)
	 (jmespath-runtime-error 'func "expression required" expression expr))
       (let ((p&v (map (lambda (e)
			 (let ((v ($j (expr ($c e context)))))
			   (cons e v))) array)))
	 (unless (or (for-all (lambda (v) (string? (cdr v))) p&v)
		     (for-all (lambda (v) (number? (cdr v))) p&v))
	   (jmespath-runtime-error 'func 
	    "Expression returned non number nor non string"
	    expression (map car p&v)))
	 (reducer p&v))))))
(define-syntax define-number/string-comparison
  (syntax-rules ()
    ((_ name n<> s<>)
     (define (name a b)
       (if (and (number? a) (number? b))
	   (n<> a b)
	   (s<> a b))))))
(define-number/string-comparison ns> > string>?)
(define-by-function jmespath:max-by-function max_by (comparator ns>))

(define-function (jmespath:merge-function context expression . objects)
  (unless (for-all vector? objects)
    (jmespath-runtime-error 'merge "Object required" expression objects))
  (let ((ht (make-hashtable string-hash string=?)))
    (for-each (lambda (obj)
		(vector-for-each (lambda (k&v)
				   (hashtable-set! ht (car k&v) (cdr k&v)))
				 obj)) objects)
    (let-values (((keys values) (hashtable-entries ht)))
      (vector-map cons keys values))))

(define-function (jmespath:min-function context expression array)
  (unless (list? array)
    (jmespath-runtime-error 'min "array required" expression array))
  (cond ((null? array) 'null)
	((for-all number? array) (apply min array))
	((for-all string? array)
	 ;; TODO create string-max?
	 (let loop ((s (car array)) (s* (cdr array)))
	   (cond ((null? s*) s)
		 ((string> s (car s*)) (loop (car s*) (cdr s*)))
		 (else (loop s (cdr s*))))))
	(else
	 (jmespath-runtime-error 'min "array of number or string required"
				 expression array))))
(define-number/string-comparison ns< < string<?)
(define-by-function jmespath:min-by-function min_by (comparator ns<))
  
(define-function (jmespath:not-null-function context expression e . e*)
  (if (eq? 'null e)
      (let loop ((e* e*))
	(cond ((null? e*) 'null)
	      ((eq? (car e*) 'null) (loop (cdr e*)))
	      (else (car e*))))
      e))

(define-function (jmespath:reverse-function context expression argument)
  (cond ((list? argument) (reverse argument))
	((string? argument) (string-reverse argument))
	(else (jmespath-runtime-error 'reverse "array or string required"
				      expression argument))))

(define-function (jmespath:sort-function context expression array)
  (unless (list? array)
    (jmespath-runtime-error 'sort "array required" expression array))
  (cond ((null? array) '())
	((for-all number? array) (list-sort < array))
	((for-all string? array) (list-sort string<? array))
	(else
	 (jmespath-runtime-error 'sort "array of number or string required"
				 expression array))))

(define-by-function jmespath:sort-by-function sort_by
  (lambda (p&v)
    (map car (list-sort (lambda (a b) (ns< (cdr a) (cdr b))) p&v))))

(define-function (jmespath:starts-with-function context expression subject prefix)
  (unless (and (string? subject) (string? prefix))
    (jmespath-runtime-error 'starts_with "String required"
			    expression subject prefix))
  (string-prefix? prefix subject))

(define-function (jmespath:sum-function context expression argument)
  (unless (and (list? argument) (for-all number? argument))
    (jmespath-runtime-error 'sum "Array of number required"
			    expression argument))
  (fold + 0 argument))

(define-function (jmespath:to-array-function context expression argument)
  (if (list? argument)
      argument
      (list argument)))

(define-function (jmespath:to-string-function context expression argument)
  ;; JMESPath requires us not to emit extra space...
  (define (json-write x port)
    (define (write-ht vec port)
      (display "{" port)
      (vector-fold (lambda (need-comma? e)
		     (let ((k (car e)) (v (cdr e)))
		       (when need-comma? (display "," port))
		       (if (string? k)
			   (write k port)
			   (jmespath-runtime-error 'to_string
			    "Invalid JSON table key" expression argument))
		       (display ":" port)
		       (write-any v port)
		       #t)) #f vec)
      (display "}" port))
    (define (write-array arr port)
      (display "[" port)
      (fold-left (lambda (need-comma? e)
		   (when need-comma? (display "," port))
		   (write-any e port)
		   #t) #f arr)
      (display "]" port))
    (define (write-any x port)
      (cond ((vector? x) (write-ht x port))
	    ((list? x) (write-array x port))
	    ((eq? x 'null) (display "null" port))
	    ((symbol? x) (write (symbol->string x) port))
	    ((or (string? x) (number? x)) (write x port))
	    ((boolean? x) (display (if x "true" "false") port))
	    (else (jmespath-runtime-error 'to_string
		   "Invalid JSON object" expression argument))))
    (write-any x port))
  (if (string? argument)
      argument
      (let-values (((out extract) (open-string-output-port)))
	(json-write argument out)
	(extract))))

(define-function (jmespath:to-number-function context expression argument)
  (cond ((string? argument) (or (string->number argument) 'null))
	((number? argument) argument)
	(else 'null)))

(define-function (jmespath:type-function context expression argument)
  (cond ((string? argument) "string")
	((boolean? argument) "boolean")
	((number? argument) "number")
	((vector? argument) "object")
	((list? argument) "array")
	(else "null"))) ;; not really but go safer

(define-function (jmespath:values-function context expression obj)
  (if (vector? obj)
      (vector->list (vector-map cdr obj)) ;; TODO performance?
      (jmespath-runtime-error 'keys "Object required" expression obj)))

(define (jmespath:parent-function context expression node)
  (let ((parent (jmespath-context-parent node)))
    (or parent ($c 'null node))))

(define +jmespath:buildin-functions+
  `(
    (abs . ,jmespath:abs-function)
    (avg . ,jmespath:avg-function)
    (contains . ,jmespath:contains-function)
    (ceil . ,jmespath:ceil-function)
    (ends_with . ,jmespath:ends-with-function)
    (floor . ,jmespath:floor-function)
    (join . ,jmespath:join-function)
    (keys . ,jmespath:keys-function)
    (length . ,jmespath:length-function)
    (map . ,jmespath:map-function)
    (max . ,jmespath:max-function)
    (max_by . ,jmespath:max-by-function)
    (merge . ,jmespath:merge-function)
    (min . ,jmespath:min-function)
    (min_by . ,jmespath:min-by-function)
    (not_null . ,jmespath:not-null-function)
    (reverse . ,jmespath:reverse-function)
    (sort . ,jmespath:sort-function)
    (sort_by . ,jmespath:sort-by-function)
    (starts_with . ,jmespath:starts-with-function)
    (sum . ,jmespath:sum-function)
    (to_array . ,jmespath:to-array-function)
    (to_string . ,jmespath:to-string-function)
    (to_number . ,jmespath:to-number-function)
    (type . ,jmespath:type-function)
    (values . ,jmespath:values-function)
    ;; This is not standard but we want it
    (parent . ,jmespath:parent-function)
    ))
)
