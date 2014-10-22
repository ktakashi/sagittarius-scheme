(import (rnrs)
	(srfi :114)
	(srfi :64))

(test-begin "SRFI-114 comparators")

;; predicates
(test-assert "comparator? (default-comparator)" 
	     (comparator? default-comparator))
(test-assert "comparator-comparison-procedure? (default-comparator)"
	     (comparator-comparison-procedure? default-comparator))
(test-assert "comparator-hash-function? (default-comparator)"
	     (comparator-hash-function? default-comparator))

;; standard comparators
(define-syntax test-existance
  (syntax-rules ()
    ((_ comparator rest ...)
     (begin
       (test-assert (format "comparator? (~a)" 'comparator) 
		    (comparator? comparator))
       (test-assert (format "comparator-comparison-procedure? (~a)" 'comparator)
		    (comparator-comparison-procedure? comparator))
       (test-assert (format "comparator-hash-function? (~a)" 'comparator)
		    (comparator-hash-function? comparator))
       (test-existance rest ...)))
    ((_) (values))))

(test-existance boolean-comparator char-comparator char-ci-comparator 
		string-comparator string-ci-comparator symbol-comparator
		exact-integer-comparator integer-comparator rational-comparator
		real-comparator complex-comparator number-comparator
		pair-comparator list-comparator vector-comparator
		bytevector-comparator)

;; wrapped equality predicates
(define-syntax test-wrapped
  (syntax-rules ()
    ((_ c rest ...)
     (begin
       (test-assert (format "comparator? (~a)" 'c) (comparator? c))
       (test-assert (format "comparator-hash-function? (~a)" 'c)
		    (comparator-hash-function? c))
       (test-wrapped rest ...)))
    ((_) (values))))

(test-wrapped eq-comparator eqv-comparator equal-comparator)

;; accessors
(test-assert "comparator-type-test-procedure" 
	     (comparator-type-test-procedure boolean-comparator))
(test-assert "comparator-equality-predicate" 
	     (comparator-equality-predicate boolean-comparator))
(test-assert "comparator-comparison-procedure" 
	     (comparator-comparison-procedure boolean-comparator))
(test-assert "comparator-hash-function" 
	     (comparator-hash-function boolean-comparator))

(define-syntax test-applicators
  (syntax-rules ()
    ((_ (c type error-type eq lt gt hash) rest ...)
     (begin
       (let ((cp c))
	 (when (comparator-type-test-procedure cp)
	   (test-assert (format "test ~a" 'c) (comparator-test-type cp type))
	   (test-assert (format "test not ~a" 'c)
			(not (comparator-test-type cp error-type)))
	   (test-assert (format "check ~a" 'c) (comparator-check-type cp type))
	   (test-error (format "check not ~a" 'c) condition?
		       (comparator-check-type cp error-type)))
	 (when (comparator-equality-predicate cp)
	   (test-assert (format "equal ~a" 'c) 
			(comparator-equal? cp type eq)))
	 (when (comparator-comparison-procedure? cp)
	   (test-equal (format "compare eq ~a" 'c) 0
		       (comparator-compare cp type eq))
	   (test-equal (format "compare lt ~a" 'c) -1
		       (comparator-compare cp lt gt))
	   (test-equal (format "compare gt ~a" 'c) 1
		       (comparator-compare cp gt lt)))
	 (when (comparator-hash-function cp)
	   (let ((h hash))
	     (if h
		 (test-equal (format "hash ~a" 'c) h
			     (comparator-hash cp type))
		 ;; it's hard to compute
		 (test-assert (format "hash ~a" 'c) 
			      (integer? (comparator-hash cp type)))))))
       (test-applicators rest ...)))
    ((_) (values))))

(test-applicators (boolean-comparator #t 'a #t #f #t 1)
		  (char-comparator #\b "a" #\b #\a #\c (char->integer #\b))
		  (char-ci-comparator #\A "a" #\A #\a #\c 
				      (char->integer (char-foldcase #\a)))
		  (string-comparator "abc" 'abc "abc" "ABC" "def"
				     (string-hash "abc"))
		  (string-ci-comparator "bcd" 'abc "bcD" "abc" "DEF"
				     (string-ci-hash "bcd"))
		  (symbol-comparator 'bcd 123 'bcd 'abc 'def #f)
		  (number-comparator 123 'abc 123 100 200 #f)
		  (complex-comparator 1+1i "abc" 1+1i 0+1i 2+1i #f)
		  (real-comparator 123.0 1+1i 123 100.0 200 #f)
		  (rational-comparator 123/10 1+1i 123/10 100 200 #f)
		  (integer-comparator 123.0 1.1 123 100 200 #f)
		  (exact-integer-comparator 123 1.1 123 100 200 #f))

;; TODO more tests...

(test-end)
