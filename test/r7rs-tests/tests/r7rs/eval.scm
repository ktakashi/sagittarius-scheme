;; -*- mode:scheme; coding:utf-8; -*-
#!r7rs
(define-library (tests r7rs eval)
  (import (scheme base)
	  (scheme eval)
	  (tests r7rs test))
  (export run-r7rs-eval-tests)
  (begin
    (define (run-r7rs-eval-tests)
      (test-equal 21 (eval '(* 7 3) (scheme-report-environment 7)))
      (test-equal 20 (let ((f (eval '(lambda (f x) (f x x))
				    (null-environment 7))))
		       (f + 10)))
      )
    )
)