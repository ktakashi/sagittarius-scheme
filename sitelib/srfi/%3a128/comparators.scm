;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a128/comparators.scm - Comparators (reduced)
;;;  
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :128 comparators)
    (export (rename (s114:comparator? comparator?))
	    comparator-ordered? comparator-hashable?
	    
	    ;; Constructors
	    make-comparator 
	    (rename (s114:make-pair-comparator make-pair-comparator))
	    make-list-comparator make-vector-comparator
	    make-eq-comparator make-eqv-comparator make-equal-comparator

	    ;; standard hash functions
	    boolean-hash 
	    char-hash char-ci-hash 
	    string-hash string-ci-hash
	    symbol-hash 
	    number-hash
	    
	    ;; bounds and salt
	    hash-bound hash-salt

	    ;; default comparators
	    make-default-comparator default-hash comparator-register-default!

	    ;; accessors and invokers
	    (rename (s114:comparator-type-test-procedure 
		     comparator-type-test-predicate)
		    (s114:comparator-equality-predicate
		     comparator-equality-predicate))
	    comparator-ordering-predicate 
	    (rename (s114:comparator-hash-function comparator-hash-function)
		    (s114:comparator-test-type comparator-test-type)
		    (s114:comparator-check-type comparator-check-type)
		    (s114:comparator-hash comparator-hash))

	    ;; conparison predicate
	    =? <? >? <=? >=?

	    ;; syntax
	    comparator-if<=>)
    (import (rnrs)
	    (prefix (srfi :114 comparators) s114:)
	    (only (srfi :126) hash-salt)
	    (srfi :18) ;; for lock
	    (sagittarius) ;; for eqv-hash
	    )

;; we build this comparator atop srfi-114 (builtin) comparator
;; comparator?
(define (comparator-ordered? c)
  (unless (s114:comparator? c)
    (assertion-violation 'comparator-ordered? "not a comparator" c))
  (s114:comparator-comparison-procedure? c))

(define (comparator-hashable? c)
  (unless (s114:comparator? c)
    (assertion-violation 'comparator-hashable? "not a comparator" c))
  (s114:comparator-hash-function? c))

(define (make-comparator type-test equality ordering hash)
  (unless (or (procedure? type-test) (procedure? equality))
    (assertion-violation 'make-comparator 
			 "type-test and equality are required"))
  (s114:make-comparator type-test equality 
			;; convert the ordering to SRFI-114 style
			(and ordering 
			     (lambda (x y)
			       (cond ((ordering x y) -1)
				     ((equality x y) 0)
				     (else           1))))
			hash))

;; make-pair-comparator

;; seems different argument order then SRFI-114
(define (make-list-comparator element-comparator type-test empty? head tail)
  (s114:make-listwise-comparator type-test element-comparator empty? head tail))
(define (make-vector-comparator element-comparator type-test length ref)
  (s114:make-vectorwise-comparator type-test element-comparator length ref))

(define (make-eq-comparator) s114:eq-comparator)
(define (make-eqv-comparator) s114:eqv-comparator)
(define (make-equal-comparator) s114:equal-comparator)

(define (boolean-hash x . ignore) (if x 1 0))
(define (char-hash x . ignore) (char->integer x))
(define (char-ci-hash x . ignore) (char->integer (char-foldcase x)))
;; string-hash
;; string-ci-hash
;; symbol-hash
(define (number-hash x . ignore) (eqv-hash x))

;; NB: internally, hashtable uses hash value of int32_t but it is ok to return
;;     more than that (it can handle it bigger number). however it's better
;;     to limit this up to greatest-fixnum
(define-syntax hash-bound (syntax-rules () ((_) (greatest-fixnum))))
;; there's no reason to be separated implementation from SRFI-126 hash-salt
;; (in some case it might be even better since it has the mean to specify
;;  the salt value)
;; hash-salt

;; seems more trouble than resolving issue if we use SRFI-114 default
;; comparators. it seems basically compatible but requirement doesn't
;; really match since SRFI-114 comparators uses bunch of builtin 
;; things. so just create it one
(define *registered-comparators* '())
(define *registration-lock* (make-mutex))

(define (lookup-comparator o)
  (cond ((assp (lambda (s) (s o)) *registered-comparators*) => cdr)
	(else #f)))

(define (default-hash o)
  (define (combine hv1 hv2) (+ (* hv1 5) hv2))
  (cond ((pair? o) 
	 (mod (combine (default-hash (car o)) (default-hash cdr o)) 
	      (hash-bound)))
	((boolean? o) (boolean-hash o))
	((char? o) (char-hash o))
	((string? o) (string-hash o))
	((symbol? o) (symbol-hash o))
	((number? o) (number-hash o))
	((vector? o)
	 (let loop ((r 0) (i 0))
	   (if (= i (vector-length o))
	       r
	       (loop (combine r (default-hash (vector-ref o i))) (+ i 1)))))
	((lookup-comparator o) =>
	 (lambda (comp) (s114:comparator-hash comp o)))
	(else equal-hash)))

(define (pair/null? o) (or (pair? o) (null? o)))
;; there must not be an duplicated type comparators
(define (default-equality a b)
  (cond ((and (boolean? a) (boolean? b)) (boolean=? a b))
	((and (char? a) (char? b)) (char=? a b))
	((and (symbol? a) (symbol? b)) (eq? a b))
	((and (bytevector? a) (bytevector? b)) (bytevector=? a b))
	((and (number? a) (number? b)) (= a b))
	((and (string? a) (string? b)) (string=? a b))
	((and (vector? a) (vector? b))
	 (and (= (vector-length a) (vector-length b))
	      (let loop ((i 0))
		(or (= (vector-length a) i)
		    (and (default-equality (vector-ref a i) (vector-ref b i))
			 (loop (+ i 1)))))))
	((and (pair/null? a) (pair/null? b))
	 (let loop ((a a) (b b))
	   (cond ((and (null? a) (null? b)))
		 ((or (null? a) (null? b)) #f)
		 ((and (pair? a) (pair? b))
		  (and (default-equality (car a) (car b))
		       (loop (cdr a) (cdr b))))
		 ;; last
		 (else (default-equality a b)))))
	(else
	 (let ((a-comp (lookup-comparator a))
	       (b-comp (lookup-comparator b)))
	   (and a-comp b-comp 
		(eq? a-comp b-comp)
		(s114:comparator-equal? a-comp a b))))))

(define (default-ordering a b)
  (cond ((and (boolean? a) (boolean? b)) (and (not (boolean=? a b)) (not a)))
	((and (char? a) (char? b)) (char<? a b))
	;; symbol<? is in (sagittarius)
	((and (symbol? a) (symbol? b)) (symbol<? a b))
	;; bytevector<? is in (sagittarius)
	((and (bytevector? a) (bytevector? b)) (bytevector<? a b))
	((and (number? a) (number? b)) (< a b))
	((and (string? a) (string? b)) (string<? a b))
	((and (vector? a) (vector? b))
	 (cond ((< (vector-length a) (vector-length b)) #t)
	       ((> (vector-length a) (vector-length b)) #f)
	       (else
		(let loop ((i 0))
		  (if (= (vector-length a) i)
		      #f ;; a is not less so #f
		      (or (default-ordering (vector-ref a i) (vector-ref b i))
			  (loop (+ i 1))))))))
	((and (pair/null? a) (pair/null? b))
	 (let loop ((a a) (b b))
	   (cond ((and (null? a) (null? b)) #f)
		 ((null? a) #t)
		 ((null? b) #f)
		 ((and (pair? a) (pair? b))
		  (or (default-ordering (car a) (car b))
		      (loop (cdr a) (cdr b))))
		 (else (default-ordering a b)))))
	(else 
	 (let ((a-comp (lookup-comparator a))
	       (b-comp (lookup-comparator b)))
	   (if (and a-comp b-comp (eq? a-comp b-comp))
	       (negative? (s114:comparator-compare a-comp a b))
	       ;; specified condition #1
	       ;; All object of type a compare less than all object of type b
	       #f)))))

(define *default-comparator*
  (make-comparator #t
		   default-equality
		   default-ordering
		   default-hash))

;; we don't need to make it each time since plugged comparaters
;; are resolved runtime
(define (make-default-comparator) *default-comparator*)

(define (comparator-register-default! comparator) 
  (let ((pred (s114:comparator-type-test-procedure comparator)))
    (mutex-lock! *registration-lock*)
    (cond ((assq pred *registered-comparators*)
	   (mutex-unlock! *registration-lock*)
	   (error 'comparator-register-default!
		  "duplicated type predicate comparator" comparator))
	  (else
	   (set! *registered-comparators* 
		 (cons (cons pred comparator) *registered-comparators*))
	   (mutex-unlock! *registration-lock*)))))

;; now this is a bit tricky to do it
;; even though this is an accessor we can't simply return the
;; acutual procedure because of the different returning values.
;; so we need to convert it.
(define (comparator-ordering-predicate comp)
  (let ((proc (s114:comparator-comparison-procedure comp)))
    (lambda (x y) (negative? (proc x y)))))

(let-syntax ((define-predicate
	       (syntax-rules ()
		 ((_ name s114)
		  (define (name comparator obj1 obj2 . rest)
		    (apply s114 comparator obj1 obj2 rest))))))
  (define-predicate =?  s114:=?)
  (define-predicate <?  s114:<?)
  (define-predicate >?  s114:>?)
  (define-predicate <=? s114:<=?)
  (define-predicate >=? s114:>=?))

(define-syntax comparator-if<=>
  (syntax-rules ()
    ((_ o1 o2 less-than equal-to greater-than)
     (comparator-if<=> (make-default-comparator) o1 o2
		       less-than equal-to greater-than))
    ((_ comp o1 o2 less-than equal-to greater-than)
     ;; we use SRFI-114 directly thus, -1, 0 or 1
     (let ((t1 o1)
	   (t2 o2)
	   (c comp))
       (if (negative? (s114:comparator-compare c t1 t2))
	   less-than
	   (if (s114:comparator-equal? c t1 t2)
	       equal-to
	       greater-than))))))

)
	    
