#lang racket

;; Test case: dynamic-wind + call/cc in delimited context
;; This tests whether after-thunks are called when escaping through
;; a delimited continuation

(define l '())
(define (add! a b)
  (set! l (append l (list (cons a b)))))

(define (void-fn . args) #f)

(define x (make-parameter 0))

(let ([k (parameterize ([x 5])
           (dynamic-wind
               (lambda () (add! 1 (x)))
               (lambda () (parameterize ([x 6])
                           (let ([k+e (call/cc (lambda (k) (cons k void-fn)))])
                             (add! 2 (x))
                             ((cdr k+e))
                             (car k+e))))
               (lambda () (add! 3 (x)))))])
  (parameterize ([x 7])
    (call/cc
     (lambda (c)
       (k (cons void-fn c))))))

(printf "Result: ~a\n" l)
(printf "Expected: ((1 . 5) (2 . 6) (3 . 5) (1 . 5) (2 . 6) (3 . 5))\n")
