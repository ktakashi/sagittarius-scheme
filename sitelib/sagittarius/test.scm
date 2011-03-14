;; -*- scheme -*-
;; simple test frame work for sagittarius scheme
(library (sagittarius test)
    (export assert-equal?
	    assert-true?
	    assert-false?
	    run-test run-test-aux
	    reporter report-tests
	    report-failed)
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
      (display who out)
      (display ":" out)
      (display msg out)
      (push-report (proc))))

  (define (report-tests)
    (show-report))
  
  (define-syntax assert-equal?
    (syntax-rules ()
      ((_ expected actual)
       (let ((e expected)
	     (a actual))
	 (reporter 'expected 'actual 'equals)
	 (or (equal? e a)
	     (report-failed 'assert-equal
			    (format "~s expected but got ~s" e a)))))))
  
  (define-syntax assert-true?
    (syntax-rules ()
      ((_ test)
       (begin
	 (reporter 'test '() "is true")
	 (or test
	     (report-failed 'assert-true
			    (format "~a returns false" 'test)))))))

  (define-syntax assert-false?
    (syntax-rules ()
      ((_ test)
       (begin
	 (reporter 'test '() "is false")
	 (or (not test)
	     (report-failed 'assert-true
			    (format "~a returns false" 'test)))))))

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
       (begin
	 (display "test finished")(newline)
	 (report-tests)))))

  (define-syntax run-test
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((reporter (generate-reporter))
	     (tests   (cdr form)))
	 (current-reporter 0 reporter)
	 `(begin
	    (run-test-aux ,@tests))))))

)