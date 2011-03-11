;; -*- scheme -*-
;; simple test frame work for sagittarius scheme
(library (sagittarius test)
    (export assert-equal?
	    assert-true?
	    assert-false?
	    run-test
	    reporter)
    (import (rnrs)
	    (core)
	    (sagittarius))

  (define (reporter expected actual type)
    (display "test-case: ")
    (unless (null? expected)
      (write/ss expected)
      (display " "))
    (display type)
    (unless (null? actual)
      (display " ")
      (write/ss actual))
    (newline))
  
  (define-syntax assert-equal?
    (syntax-rules ()
      ((_ expected actual)
       (let ((e expected)
	     (a actual))
	 (reporter 'expected 'actual 'equals)
	 (or (equal? e a)
	     (error 'assert-equal
		    (format "~s expected but got ~s" e a)))))))
  
  (define-syntax assert-true?
    (syntax-rules ()
      ((_ test)
       (reporter 'test '() "is true")
       (or test
	   (error 'assert-true
		  (format "~a returns false" 'test))))))

  (define-syntax assert-false?
    (syntax-rules ()
      ((_ test)
       (reporter 'test '() "is false")
       (or (not test)
	   (error 'assert-true
		  (format "~a returns false" 'test))))))

  (define-syntax run-test
    (syntax-rules ()
      ((_ test)
       (begin
	 test
	 (run-test)))
      ((_ test more ...)
       (begin
	 test
	 (run-test more ...)))
      ((_)
       (begin
	 (display "test success")
	 (newline)))))

)