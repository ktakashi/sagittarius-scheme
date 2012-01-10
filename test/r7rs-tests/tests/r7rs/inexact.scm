;; -*- mode:scheme; coding:utf-8; -*-
#!r7rs
(define-library (tests r7rs inexact)
  (import (scheme base)
	  (scheme inexact)
	  (tests r7rs test))
  (export run-r7rs-inexact-tests)
  (begin
    (define (run-r7rs-inexact-tests)
      (test-equal #(10 5 2 4 3 8) `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))
      )
    )
)
