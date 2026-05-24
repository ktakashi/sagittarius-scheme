(import (rnrs)
        (sagittarius continuations)
        (sagittarius parameters))

(define l '())
(define (add! a b)
  (set! l (append l (list (cons a b)))))

(define x (make-parameter 0))

(display "Test with call/delim-cc + parameterize/dw")
(newline)

(define result
  (call/prompt
   (lambda ()
     (let ((k (parameterize/dw ((x 5))
                (dynamic-wind
                  (lambda () (add! 1 (x)))
                  (lambda () (parameterize/dw ((x 6))
                               (let ((k+e (call/delim-cc (lambda (k) (cons k (lambda args #f))))))
                                 (add! 2 (x))
                                 ((cdr k+e))
                                 (car k+e))))
                  (lambda () (add! 3 (x)))))))
       (parameterize/dw ((x 7))
         (call/delim-cc
          (lambda (c)
            (k (cons (lambda args #f) c)))))
       l))))

(display result)
(newline)
(display "Expected: ((1 . 5) (2 . 6) (3 . 5) (1 . 5) (2 . 6) (3 . 5))")
(newline)
