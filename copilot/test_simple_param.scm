#!read-macro=sagittarius/regex
(import (scheme base) (scheme write) (sagittarius continuations) (sagittarius parameters) (srfi :64 testing))

(test-begin "Simple Param Test")

(test-equal
  '((1 . 5) (2 . 6) (3 . 5) (1 . 5) (2 . 6) (3 . 5))
  (let* ([x (make-parameter 0)]
         [l '()]
         [void (lambda arg* #f)]
         [add! (lambda (a b)
                 (set! l (append l (list (cons a b)))))])
    (let ([k (parameterize ([x 5])
	       (dynamic-wind
                   (lambda () (add! 1 (x)))
                   (lambda () (parameterize ([x 6])
				(let ([k+e (call/cc (lambda (k) (cons k void)))])
                                  (add! 2 (x))
                                  ((cdr k+e))
                                  (car k+e))))
                   (lambda () (add! 3 (x)))))])
      (parameterize ([x 7])
	(call/cc
         (lambda (c)
           (k (cons void c))))))
    l))

(test-end)
