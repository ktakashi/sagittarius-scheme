;; -*- scheme -*-
;; simple test frame work for sagittarius scheme
(library (sagittarius test)
    (export assert-equal?
	    assert-true?
	    assert-false?
	    run-test run-test-aux
	    reporter report-tests
	    report-failed report-success)
    (import (rnrs)
	    (core)
	    (sagittarius)
	    (sagittarius test helper))

  (define (reporter expected actual type)
    (receive (out proc) (open-string-output-port)
      (display "test-case: " out)
      (unless (null? expected)
	(write/ss expected out)
	(display " " out))
      (display type out)
      (unless (null? actual)
	(display " " out)
	(write/ss actual out))
      ;; todo thread-id
      (push-report (proc))))
  
  (define (report-failed who msg)
    (receive (out proc) (open-string-output-port)
      (display "    " out)
      (display 'case out)
      (display ": " out)
      (display msg out)
      (push-error (proc))))

  (define (report-success)
    (push-success #t)
    #t)

  (define (report-tests)
    (show-report))
  
  (define-syntax assert-equal?
    (syntax-rules ()
      ((_ expected actual)
       (let ((e expected)
	     (a actual))
	 (reporter 'expected 'actual 'equals)
	 (or (and (equal? e a)
		  (report-success))
	     (report-failed 'assert-equal
			    (format "~s: expected ~s but got ~s"
				    '(assert-equal? expected actual) e a)))))))
  
  (define-syntax assert-true?
    (syntax-rules ()
      ((_ test)
       (begin
	 (reporter 'test '() "is true")
	 (or (and test
		  (report-success))
	     (report-failed 'assert-true
			    (format "~s: returned false"
				    '(assert-true? test))))))))

  (define-syntax assert-false?
    (syntax-rules ()
      ((_ test)
       (begin
	 (reporter 'test '() "is false")
	 (or (and (not test)
		  (report-success))
	     (report-failed 'assert-true
			    (format "~s: returned true"
				    '(assert-false? test))))))))

  (define-syntax run-test-aux
    (syntax-rules ()
      ((_ test)
       (begin
	 test
	 (run-test-aux)))
      ((_ test more ...)
       (begin
	 test
	 (run-test-aux more ...)))
      ((_)
       (report-tests))))

  (define-syntax run-test
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((reporter (generate-reporter))
	     (tests   (cdr form)))
	 (current-reporter 0 reporter)
	 `(begin
	    (run-test-aux ,@tests))))))

)