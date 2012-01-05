;; -*- mode: scheme; coding: utf-8; -*-
#!r7rs
(define-library (tests r7rs char)
  (import (scheme base)
	  (scheme char)
	  (tests r7rs test))
  (export run-r7rs-char-tests)

  (define (run-r7rs-char-tests)
    (test-equal '("b" "c") (member "B"
				   '("a" "b" "c")
				   string-ci=?))
    (test-equal 3 (digit-value #\3))
    (test-equal 4 (digit-value #\x0664))
    (test-false (digit-value #\x0EA6))
    )
 
)