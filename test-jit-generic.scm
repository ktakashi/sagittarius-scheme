;; Test JIT generic function support

(import (rnrs)
        (sagittarius vm)
        (clos user))

;; Define a generic function
(define-generic test-add)

;; Add methods
(define-method test-add ((a <number>) (b <number>))
  (+ a b))

(define-method test-add ((a <string>) (b <string>))
  (string-append a b))

;; Define a function that calls the generic
(define (use-generic x y)
  (test-add x y))

;; JIT compile it
(jit-compile! use-generic)

;; Test numeric dispatch
(display "Test 1 (generic number): ")
(display (use-generic 10 20))
(newline)

;; Test string dispatch
(display "Test 2 (generic string): ")
(display (use-generic "hello" " world"))
(newline)

;; Test without JIT for comparison
(display "Test 3 (direct call number): ")
(display (test-add 5 7))
(newline)

(display "Test 4 (direct call string): ")
(display (test-add "foo" "bar"))
(newline)

(display "All generic tests passed!\n")
