;; -*- mode: scheme; coding: utf-8; -*-
#!r7rs
(define-library (tests r7rs test)
  (import (scheme base)
	  (scheme write)
	  (scheme complex)
	  (scheme inexact))
  (export test-equal test-error test-true test-false
	  test-approximate  test-unspecified test-values
	  test-alts
	  (rename test test-equal)
	  test-begin test-end)
  (begin
    (define-record-type err (make-err c) err?
      (c err-err-c))
    (define-record-type expected-exception (make-expected-exception)
      expected-exception?)
    (define-record-type multiple-result (make-multiple-results values)
      multiple-results?
      (values multiple-results-values))
    (define-record-type approx (make-approx value) approx?
      (value approx-value))
    (define-record-type alts (make-alts values) alts?
    (values))

    (define-syntax test
      (syntax-rules ()
	((_ expected expr)
	 (begin
	   (run-test 'expr
		     (catch-exns (lambda () expr))
		     expected)))))

    (define-syntax test-true
      (syntax-rules ()
	((_ expr)
	 (test #t expr))))

    (define-syntax test-false
      (syntax-rules ()
	((_ expr)
	 (test #f expr))))

    (define (catch-exns thunk)
      (guard (c (#t (make-err c)))
	(call-with-values thunk
	  (lambda x
	    (if (= 1 (length x))
		(car x)
		(make-multiple-results x))))))

    (define-syntax test-approximate
      (syntax-rules ()
	((_ expected expr)
	 (run-test 'expr
		   (make-approx expr)
		   (make-approx expected)))))

    (define-syntax test-alts
      (syntax-rules ()
	((_ expr e0 e ...)
	 (run-test 'expr
		   expr
		   (make-alts (list e0 e ...))))))

    (define (good-enough? x y)
      ;; relative error should be with 0.1%, but greater
      ;; relative error is allowed when the expected value is near zero.
      (define (infinite? x) (not (finite? x)))
      (cond ((not (number? x)) #f)
	    ((not (number? y)) #f)
	    ((or (not (real? x))
		 (not (real? y)))
	     (and (good-enough? (real-part x) (real-part	y))
		  (good-enough? (imag-part x) (imag-part	y))))
	    ((infinite? x)
	     (=	x (* 2.0 y)))
	    ((infinite? y)
	     (= (* 2.0 x) y))
	    ((nan? y)
	     (nan? x))
	    ((> (magnitude y) 1e-6)
	     (< (/ (magnitude (- x y))
		   (magnitude y))
		1e-3))
	    (else
	     (< (magnitude (- x y)) 1e-6))))

    (define-syntax test-error
      (syntax-rules ()
	((_ expr)
	 (test (guard (c (#t (make-expected-exception)))
		 expr)
	       (make-expected-exception)))))

    (define-syntax test-values
      (syntax-rules ()
	((_ expr val ...)
	 (run-test 'expr
		   (catch-exns (lambda () expr))
		   (make-multiple-results (list val ...))))))

    (define-syntax test-unspecified
      (syntax-rules ()
	((_ expr)
	 (test (begin expr 'unspec) 'unspec))))

    (define checked 0)
    (define failures '())

    (define (same-result? got expected)
      (cond
       [(and (real? expected) (nan? expected))
	(and (real? got) (nan? got))]
       [(expected-exception? expected)
	(expected-exception? got)]
       [(approx? expected)
	(and (approx? got)
	     (good-enough? (approx-value expected)
			   (approx-value got)))]
       [(multiple-results? expected)
	(and (multiple-results? got)
	     (= (length (multiple-results-values expected))
		(length (multiple-results-values got)))
	     ;; R7RS does not have for-all or every
	     (let loop ((e (multiple-results-values expected))
			(got (multiple-results-values got)))
	       (cond ((and (null? e) (null? got)) #t)
		     ((or (null? e) (null? got)) #f)
		     ((same-result? (car e) (car got))
		      (loop (cdr e) (cdr got)))
		     (else #f))))]
       [(alts? expected)
	;; R7RS does not have exists or any
	(let loop ((e (alts-values expected)))
	  (cond ((null? e) #f)
		((same-result? got (car e)))
		(else (loop (cdr e)))))]
       [else (equal? got expected)]))

    (define (run-test expr got expected)
      (set! checked (+ 1 checked))
      (unless (same-result? got expected)
	(set! failures
	      (cons (list expr got expected)
		    failures))))

    (define (write-result prefix v)
      (cond ((multiple-results? v)
	     (for-each (lambda (v)
			 (write-result prefix v))
		       (multiple-results-values v)))
	    ((approx? v)
	     (display prefix)
	     (display "approximately ")
	     (write (approx-value v)))
	    ((alts? v)
	     (write-result (string-append prefix "   ")
			   (car (alts-values v)))
	     (for-each (lambda (v)
			 (write-result (string-append prefix "OR ")
				       v))
		       (cdr (alts-values v))))
	    (else (display prefix) (write v))))

    (define (report-test-results)
      (if (null? failures)
	  (begin
	    (display checked)
	    (display " tests passed\n"))
	  (begin
	    (display (length failures))
	    (display " tests failed:\n\n")
	    (for-each (lambda (t)
			(display "Expression:\n ")
			(write (car t))
			(display "\nResult:")
			(write-result "\n " (cadr t))
			(display "\nExpected:")
			(write-result "\n " (caddr t))
			(display "\n\n"))
		      (reverse failures))
	    (display (length failures))
	    (display " of ")
	    (display checked)
	    (display " tests failed.\n"))))

    (define (test-begin msg)
      (display msg)(newline))

    (define test-end report-test-results)
    )
)