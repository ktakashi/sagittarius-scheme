;; -*- mode:scheme; coding:utf-8; -*-
#!r7rs
(define-library (tests r7rs write)
  (import (scheme base)
	  (scheme write)
	  (tests r7rs test))
  (export run-r7rs-write-tests)
  (begin
    (define-syntax test-io-unspecified
      (syntax-rules ()
	((_ expect test)
	 (test-equal expect
		     (let ((p (open-output-string)))
		       (parameterize ((current-output-port p))
			 test)
		       (get-output-string p))))))

    (define (run-r7rs-write-tests)
      (test-io-unspecified "12" (when (= 1 1.0) (display "1") (display "2")))
      (test-io-unspecified "" (unless (= 1 1.0) (display "1") (display "2")))
      (test-io-unspecified "4 plus 1 equals 5"
			   (begin (display "4 plus 1 equals ")
				  (display (+ 4 1))))
      )
    )
)