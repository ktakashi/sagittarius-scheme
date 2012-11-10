;; -*- mode:scheme; coding:utf-8; -*-
#!r7rs
(define-library (tests r7rs inexact)
  (import (scheme base)
	  (scheme inexact)
	  (tests r7rs test))
  (export run-r7rs-inexact-tests)
  (begin
    (define compose
      (lambda (f g)
	(lambda args
	  (f (apply g args)))))

    (define (run-r7rs-inexact-tests)
      (test-true (finite? 3))
      (test-false (finite? +inf.0))
      (test-false (finite? 3.0+inf.0i))

      (test-true (nan? +nan.0))
      (test-false (nan? 32))
      (test-true (nan? +nan.0+5.0i))
      (test-false (nan? 1+2i))

      (test-equal #(10 5 2 4 3 8) `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))

      (test-equal 13 (vector-ref '#(1 1 2 3 5 8 13 21)
				 (exact (round (* 2 (acos -1))))))
      ;; bug ((compose sqrt *) 12 75) can not test.
      (let ((f (compose sqrt *)))
	(test-equal 30 (f 12 75)))
      )
    )
)
