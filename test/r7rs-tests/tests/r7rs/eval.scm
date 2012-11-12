;; -*- mode:scheme; coding:utf-8; -*-
#!r7rs
(define-library (tests r7rs eval)
  (import (scheme base)
	  (scheme eval)
	  (scheme r5rs)
	  (tests r7rs test))
  (export run-r7rs-eval-tests)
  (begin
    (define (run-r7rs-eval-tests)
      (test-equal 21 (eval '(* 7 3) (environment '(scheme base))))
      (test-equal 20 (let ((f (eval '(lambda (f x) (f x x))
				    (null-environment 5))))
		       (f + 10)))
      )
    )
)
