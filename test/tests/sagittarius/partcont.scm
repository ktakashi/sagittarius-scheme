(import (rnrs)
	(sagittarius partcont)
	(srfi :64 testing))

(test-begin "partial continuation")

(define k #f)

(test-equal 9 (+ 1 (reset (* 2 (shift cont 3) 4)) 5))
(test-equal 30 (+ 1 (reset (* 2 (shift cont (cont 3)) 4)) 5))
(test-equal 9 (+ 1 (reset (* 2 (shift cont (set! k cont) 3) 4)) 5))
(test-equal 24 (k 3))

(define (for-each-tree fn tree)
  (cond ((null? tree) #f)
        ((pair? tree)
         (for-each-tree fn (car tree))
         (for-each-tree fn (cdr tree)))
        (else 
         (fn tree))))

(define (make-iter1 proc . args)
  (letrec ((iter (lambda ()
                   (apply
                     proc
                     (lambda (x) (shift k (set! iter k) x))
                     args)
                   #f)))
    (lambda () (reset (iter)))))

(let ((a (make-iter1 for-each-tree '(a (b (c d e) f) g))))
  (test-equal 'a (a))
  (test-equal 'b (a))
  (test-equal 'c (a))
  (test-equal 'd (a))
  (test-equal 'e (a))
  (test-equal 'f (a))
  (test-equal 'g (a))
  (test-equal #f (a)))

(test-end)