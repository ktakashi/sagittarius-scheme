;; -*- mode: scheme; coding: utf-8; -*-
#!r7rs
(define-library (tests r7rs case-lambda)
  (import (scheme base)
	  (scheme case-lambda)
	  (tests r7rs test))
  (export run-r7rs-case-lambda-tests)
  (begin
    (define range
      (case-lambda
       ((e) (range 0 e))
       ((b e) (do ((r '() (cons e r))
		   (e (- e 1) (- e 1)))
		  ((< e b) r)))))

    (define (run-r7rs-case-lambda-tests)
      ;; 4.2.9
      (test-equal '(0 1 2) (range 3))
      (test-equal '(3 4) (range 3 5))

      )
    )
)
