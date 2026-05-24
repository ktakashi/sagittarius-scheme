 #lang racket
 (require racket/control)

#;(import (except (rnrs) call/cc call-with-current-continuation)
	(rename (sagittarius continuations)
                (call/delim-cc call/cc)
                (call-with-delimited-current-continuation
                 call-with-current-continuation)))

(display 
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
          (k (cons void c))))))l))
(newline)
