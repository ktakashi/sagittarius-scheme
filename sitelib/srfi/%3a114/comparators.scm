;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; comparators.scm - SRFI-114 comparators
;;;  
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :114 comparators)
    (export comparator? comparator-comparison-procedure? 
	    comparator-hash-function?
	    boolean-comparator char-comparator char-ci-comparator 
	    string-comparator string-ci-comparator symbol-comparator
	    exact-integer-comparator integer-comparator rational-comparator
	    real-comparator complex-comparator number-comparator
	    pair-comparator list-comparator vector-comparator
	    bytevector-comparator
	    default-comparator
	    make-comparator make-inexact-real-comparator make-vector-comparator 
	    make-bytevector-comparator make-list-comparator
	    make-vectorwise-comparator make-listwise-comparator
	    make-car-comparator make-cdr-comparator make-pair-comparator
	    make-improper-list-comparator make-selecting-comparator
	    make-refining-comparator make-reverse-comparator
	    make-debug-comparator
	    eq-comparator eqv-comparator equal-comparator
	    comparator-type-test-procedure comparator-equality-predicate 
	    comparator-comparison-procedure comparator-hash-function
	    comparator-test-type comparator-check-type comparator-equal? 
	    comparator-compare comparator-hash
	    make-comparison< make-comparison> make-comparison<=
	    make-comparison>= make-comparison=/< make-comparison=/>
	    if3 if=? if<? if>? if<=? if>=? if-not=?
	    =? <? >? <=? >=?
	    make= make<  make> make<= make>=
	    in-open-interval? in-closed-interval? in-open-closed-interval? 
	    in-closed-open-interval?
	    comparator-min comparator-max
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius comparators)
	    (clos user)
	    (only (scheme base) exact-integer?))

  (define-constant %undef (undefined))

  (define-syntax if3
    (syntax-rules ()
      ((_ p less equal greater)
       ;; hmmm should we extend to negative, 0 or positive?
       (case p
	 ((-1) less)
	 ((0)  equal)
	 ((1)  greater)
	 (else => (lambda (v) (error 'if3 "bad comparison value" v)))))))

  (define-syntax if=?
    (syntax-rules ()
      ((_ p then)
       (if=? p then %undef))
      ((_ p then els)
       (if3 p els then els))))

  (define-syntax if<?
    (syntax-rules ()
      ((_ p less)
       (if<? p less %undef))
      ((_ p less els)
       (if3 p less els els))))

  (define-syntax if<=?
    (syntax-rules ()
      ((_ p less=)
       (if<=? p less= %undef))
      ((_ p less= els)
       (if3 p less= less= els))))

  (define-syntax if>?
    (syntax-rules ()
      ((_ p greater)
       (if>? p greater %undef))
      ((_ p greater els)
       (if3 p els els greater))))

  (define-syntax if>=?
    (syntax-rules ()
      ((_ p greater=)
       (if>=? p greater= %undef))
      ((_ p greater= els)
       (if3 p els greater= greater=))))

  (define-syntax if-not=?
    (syntax-rules ()
      ((_ p els)
       (if-not=? p els %undef))
      ((_ p els then)
       (if3 p els then els))))
#|
  (define-record-type (<comparator> make-comparator comparator?)
    (fields (immutable type-test   comparator-type-test-procedure)
	    (immutable equality    comparator-equality-predicate)
	    (immutable comparison  comparator-comparison-procedure)
	    (immutable hash        comparator-hash-function)
	    (immutable comparison? comparator-comparison-procedure?)
	    (immutable hash?       comparator-hash-function?))
    (protocol (lambda (p)
		(lambda (type-test equality comparison hash)
		  (p (if (eq? type-test #t) (lambda (x) #t) type-test)
		     (if (eq? equality #t)
			 (lambda (x y) (eqv? (comparison x y) 0))
			 equality)
		     (or comparison 
			 (lambda (x y)
			   (error 'comparator "compasison not supported")))
		     (or hash (lambda (x y)
				(error 'compasison "hashing not supported")))
		     (if comparison #t #f)
		     (if hash #t #f))))))
|#
  (define (default-comparison a b)
    (let ((a-type (object-type a))
	  (b-type (object-type b)))
      (cond ((< a-type b-type) -1)
	    ((> a-type b-type) 1)
	    (else (compare a b)))))
  ;; I believe equal-hash is sufficient since we can extend
  ;; with object-hash
  (define default-comparator 
    (make-comparator #t #t default-comparison equal-hash))
#|
  (define (comparator-test-type comparator obj)
    ((comparator-type-test-procedure comparator) obj))
  (define (comparator-check-type comparator obj)
    (or (comparator-test-type comparator obj)
	(error 'comparator-check-type "comparator type check failed"
	       comparator obj)))
  (define (comparator-equal? c o1 o2)
    ((comparator-equality-predicate c) o1 o2))
  (define (comparator-compare c o1 o2)
    ((comparator-comparison-procedure c) o1 o2))
  (define (comparator-hash c o)
    ((comparator-hash-function c) o))
|#
  ;; comparison predicate constructors
  (let-syntax ((define-make= (syntax-rules ()
			       ((_ name comparison)
				(define (name comparator)
				  (lambda (a b . rest)
				    (apply comparison comparator a b rest)))))))
    (define-make= make=  =?)
    (define-make= make<  <?)
    (define-make= make>  >?)
    (define-make= make<= <=?)
    (define-make= make>= >=?))

  (letrec-syntax ((define-interval? 
		    (syntax-rules ()
		      ((_ name before)
		       (define-interval? name before before))
		      ((_ name before after)
		       (define name
			 (case-lambda
			  ((a b c) (name default-comparator a b c))
			  ((comparator a b c)
			   (and (before comparator a b)
				(after comparator b c)))))))))
    (define-interval? in-open-interval?        <?)
    (define-interval? in-closed-interval?      <=?)
    (define-interval? in-open-closed-interval? <? <=?)
    (define-interval? in-closed-open-interval? <=? <?))

  ;; comparison predicates
  (let-syntax ((define-predicate
		 (syntax-rules ()
		   ((_ name chain)
		    (define (name maybe-comp obj . args)
		      (cond ((comparator? maybe-comp)
			     (apply chain maybe-comp obj args))
			    ((null? args)
			     (chain default-comparator maybe-comp obj))
			    (else
			     (apply chain default-comparator 
				    maybe-comp obj args))))))))
    (define-predicate =?  chain=?)
    (define-predicate <?  chain<?)
    (define-predicate >?  chain>?)
    (define-predicate <=? chain<=?)
    (define-predicate >=? chain<=?))

  ;; chain is not easy to define with macro...
  ;; FIXME it's ugly...
  (let-syntax ((define-chain
		 (syntax-rules ()
		   ((_ name pred)
		    (define (name comparator a b . rest)
		      (let ((p pred))
			(if (p comparator a b)
			    (if (null? rest) #t (apply name comparator b rest))
			    #f))))))
	       (pred-lambda
		(syntax-rules ()
		  ((_ v) (lambda (c a b) (eqv? (comparator-compare c a b) v)))))
	       (not-pred-lambda
		(syntax-rules ()
		  ((_ v)
		   (lambda (c a b) (not (eqv? (comparator-compare c a b) v)))))))
    (define-chain chain=? comparator-equal?)
    (define-chain chain<? (pred-lambda -1))
    (define-chain chain>? (pred-lambda 1))
    (define-chain chain<=? (not-pred-lambda 1))
    (define-chain chain>=? (not-pred-lambda -1)))

  (let-syntax ((define-minmax (syntax-rules ()
				((_ name comparison)
				 (define name
				   (case-lambda
				    ((c a) a)
				    ((c a b) (if (comparison c a b) a b))
				    ((c a b . rest)
				     (name c a (apply name c b rest)))))))))
    (define-minmax comparator-min <?)
    (define-minmax comparator-max >?))

  ;; standard comparetors
  (define (boolean-comparison x y)
    (cond ((and x y) 0)
	  (x 1)
	  (y -1)
	  (else 0))) ;;???
  (define (boolean-hash x) (if x 1 0))
  (define boolean-comparator 
    (make-comparator boolean? boolean=? boolean-comparison boolean-hash))

  (define char-comparison (make-comparison=/< char=? char<?))
  (define char-hash char->integer) ;; it's always positive
  (define char-comparator
    (make-comparator char? char=? char-comparison char-hash))

  (define char-ci-comparison (make-comparison=/< char-ci=? char-ci<?))
  (define (char-ci-hash c) (char->integer (char-foldcase c)))
  (define char-ci-comparator
    (make-comparator char? char-ci=? char-ci-comparison char-ci-hash))

;;  (define string-comparison (make-comparison=/< string=? string<?))
;;  (define string-comparator
;;    (make-comparator string? string=? string-comparison string-hash))

  (define string-ci-comparison (make-comparison=/< string-ci=? string-ci<?))
  (define string-ci-comparator
    (make-comparator string? string-ci=? string-ci-comparison string-ci-hash))

  (define symbol-comparison (make-comparison=/< symbol=? symbol<?))
  (define symbol-comparator
    (make-comparator symbol? symbol=? symbol-comparison symbol-hash))

  ;; numbers
  (define real-comparison (make-comparison< <))
  (define (complex-comparison a b)
    (let ((real-result (real-comparison (real-part a) (real-part b))))
      (if (zero? real-result)
	  (real-comparison (imag-part a) (imag-part b))
	  real-result)))

  (define number-comparator 
    (make-comparator number? = complex-comparison eqv-hash))
  (define complex-comparator 
    (make-comparator complex? = complex-comparison eqv-hash))
  (define real-comparator 
    (make-comparator real? = real-comparison eqv-hash))
  (define rational-comparator 
    (make-comparator rational? = real-comparison eqv-hash))
  (define integer-comparator 
    (make-comparator integer? = real-comparison eqv-hash))
  (define exact-integer-comparator 
    (make-comparator exact-integer? = real-comparison eqv-hash))

  ;; inexact real
  (define (inexact-real? o) (and (real? o) (inexact? o)))

  (define (make-inexact-real-comparison epsilon rounding nan-handling)
    (lambda (a b)
      (define (nan-comparison handling which)
	(case handling
	  ((error) (error 'inexact-real-comparison 
			  "Attempt to compare NaN with non-NaN"))
	  ((min) (if (eq? which 'a-nan) -1 1))
	  ((max) (if (eq? which 'a-nan) 1 -1))
	  (else (error "Invalid nan-handling specification" handling))))
      ;; Return an appropriately rounded number
      (define (rounded x symbol)
	(cond ((eq? symbol 'round) (round x))
	      ((eq? symbol 'ceiling) (ceiling x))
	      ((eq? symbol 'floor) (floor x))
	      ((eq? symbol 'truncate) (truncate x))
	      (else (error "invalid rounding specification" symbol))))

      ;; Return a number appropriately rounded to epsilon
      (define (rounded-to x epsilon symbol)
	;; http://srfi.schemers.org/srfi-114/post-mail-archive/msg00003.html
	(if (procedure? symbol)
	    (symbol x epsilon)
	    (rounded (/ x epsilon) symbol)))

      (let ((a-nan? (nan? a)) (b-nan? (nan? b)))
	(cond ((and a-nan? b-nan?) 0)
	      (a-nan? (nan-comparison nan-handling 'a-nan))
	      (b-nan? (nan-comparison nan-handling 'b-nan))
	      (else (real-comparison
		     (rounded-to a epsilon rounding)
		     (rounded-to b epsilon rounding)))))))

  (define (make-inexact-real-comparator epsilon rounding nan-handlin)
    (make-comparator inexact-real? #t
		     (make-inexact-real-comparison epsilon rounding nan-handlin)
		     eqv-hash))

  ;; the least fixnum is 32 bit environment's 2^30
  ;; we may use gretest-fixnum for this however if we want to make sure
  ;; the internal hash size which uses uint32_t then it's better to
  ;; stuck with lowest value.
  (define-constant +limit+ (expt 2 30))
  (define (make-listwise-comparison comparison nil? kar kdr)
    (lambda (a b)
      (define (rec a b)
	(let ((a-null? (nil? a)) (b-null? (nil? b)))
	  (cond ((and a-null? b-null?) 0)
		(a-null? -1)
		(b-null? 1)
		(else
		 (let ((result (comparison (kar a) (kar b))))
		   (if (zero? result)
		       ;; reference implemenation calling comparison
		       ;; but i think that's wrong...
		       (rec (kdr a) (kdr b))
		       result))))))
      (rec a b)))
  ;; listwise and vectorwise hash using mod instead of modulo
  (define (make-listwise-hash hash nil? kar kdr)
    (lambda (x)
      (let loop ((x x) (result 5381))
	(if (nil? x)
	    result ;; reference implemenation return 0 here but that's wrong
	    (let* ((prod (mod (* result 33) +limit+))
		   ;; reference implemenation calling modulo with one
		   ;; argument ...
		   (sum (mod (+ prod (hash (kar x))) +limit+)))
	      (loop (kdr x) sum))))))

  (define (make-vectorwise-comparison comparison length ref)
    (lambda (a b)
      (let ((a-len (length a))
	    (b-len (length b)))
	(cond ((< a-len b-len) -1)
	      ((> a-len b-len) 1)
	      (else 
	       (let loop ((index 0))
		 (if (= index a-len)
		     0
		     (let ((result (comparison (ref a index) (ref b index))))
		       (if (zero? result)
			   (loop (+ index 1))
			   result)))))))))

  (define (make-vectorwise-hash hash length ref)
    (lambda (x)
      (let loop ((index (- (length x) 1)) (result 5381))
	(if (zero? index)
	    result
	    (let* ((prod (mod (* result 33) +limit+))
		   (sum (mod (+ prod (hash (ref x index)) +limit+))))
	      (loop (- index 1) sum))))))

  (define (make-listwise-comparator test comparator nil? kar kdr)
    (make-comparator 
     test #t
     (make-listwise-comparison
      (comparator-comparison-procedure comparator) nil? kar kdr)
     (make-listwise-hash
      (comparator-hash-function comparator) nil? kar kdr)))

  (define (make-vectorwise-comparator test comparator length ref)
    (make-comparator
     test #t
     (make-vectorwise-comparison
      (comparator-comparison-procedure comparator) length ref)
     (make-vectorwise-hash
      (comparator-hash-function comparator) length ref)))

  ;; list
  (define (make-list-comparator comparator)
    (make-listwise-comparator
     (lambda (x) (or (null? x) (pair? x))) comparator null? car cdr))
  (define list-comparator (make-list-comparator default-comparator))

  ;; vector
  (define (make-vector-comparator comparator)
    (make-vectorwise-comparator vector? comparator vector-length vector-ref))
  (define vector-comparator (make-vector-comparator default-comparator))

  ;; bytevector
  (define (make-bytevector-comparator comparator)
    (make-vectorwise-comparator bytevector? comparator
				bytevector-length bytevector-u8-ref))
  (define bytevector-comparator (make-bytevector-comparator default-comparator))

  ;; pair comparator
  (define (make-car-comparator comparator)
    (make-comparator 
     pair? #t 
     (lambda (a b) (comparator-compare comparator (car a) (car b)))
     (lambda (obj) (comparator-hash comparator (car obj)))))
  (define (make-cdr-comparator comparator)
    (make-comparator 
     pair? #t 
     (lambda (a b) (comparator-compare (cdr a) (cdr b)))
     (lambda (obj) (comparator-hash comparator (cdr obj)))))

  (define (make-pair-comparison car-comparator cdr-comparator)
    (lambda (a b)
      (let ((result (comparator-compare car-comparator (car a) (car b))))
	(if (zero? result)
	    (comparator-compare cdr-comparator (cdr a) (cdr b))
	    result))))
  (define (make-pair-hash car-comparator cdr-comparator)
    (lambda (obj)
      (+ (comparator-hash car-comparator (car obj))
	 (comparator-hash cdr-comparator (cdr obj)))))
  (define (make-pair-comparator car-comparator cdr-comparator)
    (make-comparator pair? #t
		     (make-pair-comparison car-comparator cdr-comparator)
		     (make-pair-hash car-comparator cdr-comparator)))
  (define pair-comparator
    (make-pair-comparator default-comparator default-comparator))

  ;; improper list
  (define (make-improper-list-comparison comparator)
    (let ((pair-comparison (make-pair-comparison comparator comparator)))
      (lambda (a b)
	(define (type o)
	  (cond ((null? o) 0)
		((pair? o) 1)
		(else 2)))
	(let ((result (real-comparison (type a) (type b))))
	  (cond ((not (zero? result)) result)
		((null? a) 0)
		((pair? a) (pair-comparison a b))
		(else (comparator-compare comparator a b)))))))
  (define (make-improper-list-hash comparator)
    ;; I think this is good enough
    (lambda (obj) (comparator-hash comparator obj)))

  (define (make-improper-list-comparator comparator)
    (make-comparator #t #t 
		     (make-improper-list-comparison comparator)
		     (make-improper-list-hash comparator)))

  ;; wrapped ones
  ;; symbol-hash is eq-hash
  ;;(define eq-comparator (make-comparator #t eq? #f eq-hash))
  ;;(define eqv-comparator (make-comparator #t eqv? #f eqv-hash))
  ;;(define equal-comparator (make-comparator #t equal? #f equal-hash))

  ;;; copied from reference implementation
;;; Selecting comparator: finds the first one that type-tests
  (define (matching-comparator obj comparators)
    (cond ((null? comparators) #f)
	  ((comparator-test-type (car comparators) obj) (car comparators))
	  (else (matching-comparator obj (cdr comparators)))))

  (define (selected-type-test . comparators)
    (lambda (obj)
      (if (matching-comparator obj comparators) #t #f)))

  (define (selected-equality-predicate comparators)
    (lambda (a b)
      (let ((comparator (matching-comparator a comparators)))
	(if comparator
	    (comparator-equal? comparator a b)
	    (error 'selected-equality-predicate
		   "no comparator can be selected" comparators)))))

  (define (selected-comparison-procedure comparators)
    (lambda (a b)
      (let ((comparator (matching-comparator a comparators)))
	(if comparator
	    (comparator-compare comparator a b)
	    (error 'selected-comparison-procedure
		   "no comparator can be selected" comparators)))))

  (define (selected-hash-function comparators)
    (lambda (obj)
      (let ((comparator (matching-comparator obj comparators)))
	(if comparator
	    (comparator-hash comparator obj)
	    (error 'selected-hash-function
		   "no comparator can be selected" comparators)))))

  (define (make-selecting-comparator . comparators)
    (make-comparator
     (selected-type-test comparators)
     (selected-equality-predicate comparators)
     (selected-comparison-procedure comparators)
     (selected-hash-function comparators)))

;;; Refining comparator: uses all type-matching comparators
;;; until one is found that can discriminate
  (define (refined-equality-predicate comparators)
    (lambda (a b)
      (let loop ((comparator (matching-comparator a comparators))
		 (first? #t))
	(if comparator
	    (if (comparator-equal? a b)
		(loop (matching-comparator a comparators) #f)
		#f)
	    (or (not first?) 
		(error 'refined-equality-predicate
		       "no comparator can be selected" comparators))))))

  (define (refined-comparison-procedure comparators)
    (lambda (a b)
      (let loop ((comparator (matching-comparator a comparators))
		 (first? #t))
	(if comparator
	    (let ((result (comparator-compare a b)))
	      (if (eqv? result 0)
		  (loop (matching-comparator a comparators) #f)
		  result))
	    (if first? 
		(error 'refined-comparison-procedure
		       "no comparator can be selected")
		0)))))

  (define (refined-hash-function comparators)
    (lambda (obj)
      (let loop ((comparators comparators) (last-comparator #f))
	(if (null? comparators)
	    (if last-comparator
		(comparator-hash last-comparator obj)
		(error 'refined-hash-function "no comparator can be selected"))
	    (if (comparator-test-type (car comparators) obj)
		(loop (cdr comparators) (car comparators))
		(loop (cdr comparators) last-comparator))))))

  (define (make-refining-comparator . comparators)
    (make-comparator
     (selected-type-test comparators)
     (refined-equality-predicate comparators)
     (refined-comparison-procedure comparators)
     (refined-hash-function comparators)))

;;; Reverse the sense of the comparator
  (define (make-reverse-comparator comparator)
    (make-comparator
     (comparator-type-test-procedure comparator)
     (comparator-equality-predicate comparator)
     (lambda (a b) (- (comparator-compare comparator a b)))
     (comparator-hash-function comparator)))

;;; Handy debug-assert procedures for debugging comparators
  (define (debug-assert bool who what)
    (if (not bool)
	(error who (string-append "failure in " (symbol->string who)))))

  (define (debug-deny bool who what) (debug-assert (not bool) who what))

;;; Checkers for debugging comparators

  (define (check-type-test comparator a)
    (debug-assert (comparator-test-type comparator a) 'type 'validity))

  (define (check-reflexive-equality comparator a)
    (debug-assert (comparator-equal? comparator a a) 'equality 'reflexive))

  (define (check-reflexive-comparison comparator a)
    (debug-assert (eqv? (comparator-compare comparator a a) 0)
		  'comparison 'reflexive))

  (define (check-symmetric-equality comparator a b)
    (if (comparator-equal? a b)
	(debug-assert (comparator-equal? b a) 'equality 'symmetric))
    (if (not (comparator-equal? a b))
	(debug-deny (comparator-equal? b a) 'equality 'symmetric)))

  (define (check-asymmetric-comparison comparator a b)
    (debug-assert (eqv?
		   (comparator-compare a b)
		   (- (comparator-compare a b)))
		  'comparison 'asymmetric))

  (define (check-transitive-equality comparator a b c)
    (and (comparator-equal? a b) (comparator-equal? b c)
	 (debug-assert (comparator-equal? a c) 'equality 'transitive))
    (and (comparator-equal? a b) (not (comparator-equal? b c))
	 (debug-deny (comparator-equal? a c) 'equality 'transitive))
    (and (not (comparator-equal? a b)) (comparator-equal? b c)
	 (debug-deny (comparator-equal? a c) 'equality 'transitive)))

  (define (check-transitive-comparison comparator a b c)
    (define <= (<=? comparator))
    (and (<= b a) (<= a c) (debug-assert (<= b c) 'comparison 'transitive))
    (and (<= c a) (<= a b) (debug-assert (<= c b) 'comparison 'transitive))
    (and (<= a b) (<= b c) (debug-assert (<= a c) 'comparison 'transitive))
    (and (<= c b) (<= b a) (debug-assert (<= c a) 'comparison 'transitive))
    (and (<= a c) (<= c b) (debug-assert (<= a b) 'comparison 'transitive))
    (and (<= b c) (<= c a) (debug-assert (<= b a) 'comparison 'transitive)))

  (define (check-hash-value value)
    (debug-assert (and (positive? value) (exact-integer? value))
		  'validity 'hash-value))

  (define (check-all comparator a b c c?)
    (check-type-test comparator a)
    (check-type-test comparator b)
    (if c? (check-type-test comparator c))
    (check-reflexive-equality comparator a)
    (check-reflexive-equality comparator b)
    (if c? (check-reflexive-equality comparator c))
    (check-reflexive-comparison comparator a)
    (check-reflexive-comparison comparator b)
    (if c? (check-reflexive-comparison comparator c))
    (check-symmetric-equality comparator a b)
    (if c? (check-symmetric-equality comparator b c))
    (if c? (check-symmetric-equality comparator a c))
    (check-asymmetric-comparison comparator a b)
    (if c? (check-asymmetric-comparison comparator b c))
    (if c? (check-asymmetric-comparison comparator a c))
    (if c? (check-transitive-equality comparator a b c))
    (if c? (check-transitive-comparison comparator a b c)))

  (define (make-debug-comparator comparator)
    (let ((c #f) (c? #f))
      (comparator-comparison-procedure? comparator)
      (make-comparator
       (comparator-type-test-procedure comparator)
       (lambda (a b)
	 (check-all comparator a b c c?)
	 (when (not c?) (set! c a) (set! c? #t))
	 (comparator-equal? comparator))
       (if (comparator-comparison-procedure? comparator)
	   (lambda (a b)
	     (check-all comparator a b c c?)
	     (when (not c?) (set! c b) (set! c? #t))
	     (comparator-compare comparator))
	   #f)
       (if (comparator-hash-function? comparator)
	   (lambda (obj)
	     (let ((value (comparator-hash comparator obj)))
	       (check-hash-value value)
	       value))
	   #f))))


  )
