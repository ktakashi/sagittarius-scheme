#!/usr/bin/env sagittarius
(import (rnrs))

;; Test with smaller numbers first
(define test-cases
  '((15 4)        ; 15 / 4 = 3 rem 3
    (100 7)       ; 100 / 7 = 14 rem 2  
    (1024 5)      ; 1024 / 5 = 204 rem 4
    ((expt 2 100) (expt 2 50))  ; 2^100 / 2^50 = 2^50
    ((expt 3 100) (expt 2 50))  ; larger test
    ))

(for-each 
  (lambda (case)
    (let* ((dividend (eval (car case) (environment '(rnrs))))
           (divisor (eval (cadr case) (environment '(rnrs))))
           (quotient (div dividend divisor))
           (remainder (mod dividend divisor))
           (verification (= dividend (+ (* quotient divisor) remainder))))
      (print (format "Test: ~a / ~a" (car case) (cadr case)))
      (print (format "  Quotient: ~a" quotient))
      (print (format "  Remainder: ~a" remainder))
      (print (format "  Verification: ~a" verification))
      (print "")))
  test-cases)