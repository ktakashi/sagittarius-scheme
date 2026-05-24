;;; Test APPLY instruction in JIT
(import (rnrs)
        (srfi :64))

;; A simple JIT-compiled closure to apply
(define (add-all . args)
  (if (null? args)
      0
      (+ (car args) (apply add-all (cdr args)))))

;; Call it a few times to trigger JIT compilation
(add-all 1 2 3)
(add-all 1 2 3)
(add-all 1 2 3)
(add-all 1 2 3)
(add-all 1 2 3)
(add-all 1 2 3)
(add-all 1 2 3)
(add-all 1 2 3)
(add-all 1 2 3)
(add-all 1 2 3)

(test-begin "JIT APPLY tests")

;; Test 1: basic apply with list
(test-equal "apply with list" 6 (apply + '(1 2 3)))

;; Test 2: apply with explicit args + list
(test-equal "apply with explicit args" 15 (apply + 1 2 3 '(4 5)))

;; Test 3: apply on JIT-compiled closure
(test-equal "apply on JIT closure" 10 (add-all 1 2 3 4))

;; Test 4: apply with empty list
(test-equal "apply with empty list" 6 (apply + 1 2 3 '()))

;; Test 5: nested apply
(define (apply-twice f args1 args2)
  (+ (apply f args1) (apply f args2)))

(test-equal "nested apply" 15 (apply-twice + '(1 2) '(3 4 5)))

;; Test 6: tail-recursive apply
(define (sum-list lst acc)
  (if (null? lst)
      acc
      (sum-list (cdr lst) (+ (car lst) acc))))

;; Force JIT compilation
(sum-list '(1 2 3) 0)
(sum-list '(1 2 3) 0)
(sum-list '(1 2 3) 0)
(sum-list '(1 2 3) 0)
(sum-list '(1 2 3) 0)
(sum-list '(1 2 3) 0)
(sum-list '(1 2 3) 0)
(sum-list '(1 2 3) 0)
(sum-list '(1 2 3) 0)
(sum-list '(1 2 3) 0)

(test-equal "sum with tail recursion" 55 (sum-list '(1 2 3 4 5 6 7 8 9 10) 0))

(test-end "JIT APPLY tests")

(format #t "All JIT APPLY tests passed!~%")
