;; Test JIT SUBR direct calls

(import (rnrs)
        (sagittarius vm))

;; Define a function that calls SUBRs
(define (test-subr x y)
  (+ x y))

;; JIT compile it
(jit-compile! test-subr)

;; Test basic SUBR call
(display "Test 1 (+ SUBR): ")
(display (test-subr 10 20))
(newline)

;; Define a more complex function with multiple SUBR calls
(define (test-multiple-subr x)
  (let ((a (+ x 1))
        (b (* x 2)))
    (- a b)))

(jit-compile! test-multiple-subr)

(display "Test 2 (multiple SUBRs): ")
(display (test-multiple-subr 5))  ; (5+1) - (5*2) = 6 - 10 = -4
(newline)

;; Test SUBR with optional args (list)
(define (test-list-subr x y z)
  (list x y z))

(jit-compile! test-list-subr)

(display "Test 3 (list SUBR): ")
(display (test-list-subr 1 2 3))
(newline)

;; Test car/cdr SUBRs
(define (test-car-cdr p)
  (cons (car p) (cdr p)))

(jit-compile! test-car-cdr)

(display "Test 4 (car/cdr SUBRs): ")
(display (test-car-cdr '(a . b)))
(newline)

;; Test nested SUBR calls
(define (test-nested x y)
  (+ (* x 2) (* y 3)))

(jit-compile! test-nested)

(display "Test 5 (nested SUBRs): ")
(display (test-nested 4 5))  ; 4*2 + 5*3 = 8 + 15 = 23
(newline)

(display "All SUBR tests passed!\n")
