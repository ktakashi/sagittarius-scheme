;; Test JIT exception handling

(import (rnrs)
        (sagittarius vm))

;; Define a simple function that can be JIT compiled
(define (test-cdr x)
  (cdr x))

;; JIT compile it
(jit-compile! test-cdr)

;; Test with valid input
(display "Test 1 (valid pair): ")
(display (test-cdr '(a . b)))
(newline)

;; Test with guard to catch exception from JIT code
(display "Test 2 (guard with JIT): ")
(guard (e (else (display "Caught exception: ")
                (display (condition-message e))
                (newline)))
  (test-cdr 'not-a-pair))

;; Test that we can continue after exception
(display "Test 3 (continue after exception): ")
(display (test-cdr '(1 2 3)))
(newline)

(display "All tests passed!\n")
