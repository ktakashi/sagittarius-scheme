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

;; from Gauche
(test-equal "reset" 6
	    (+ 1 (reset (+ 2 3))))
(test-equal "reset" '(1 2)
	    (cons 1 (reset (cons 2 '()))))

(test-equal "shift, ignoring k" 4
	    (+ 1 (reset (+ 2 (shift k 3)))))
(test-equal "shift, ignoring k" '(1 3)
	    (cons 1 (reset (cons 2 (shift k (list 3))))))

(test-equal "calling pc" 10
	    (+ 1 (reset (+ 2 (shift k (+ 3 (k 4)))))))
(test-equal "calling pc" '(1 3 2 4)
	    (cons 1 (reset (cons 2 (shift k (cons 3 (k (cons 4 '()))))))))

(test-equal "calling pc multi" 14
	    (+ 1 (reset (+ 2 (shift k (+ 3 (k 5) (k 1)))))))
(test-equal "calling pc multi" '(1 3 2 2 4)
	    (cons 1 (reset (cons 2 (shift k (cons 3 (k (k (cons 4 '())))))))))

;; 'amb' example in Gasbichler&Sperber ICFP2002 paper
(let ()
  (define (eta x) (list (x)))                    ; unit
  (define (extend f l) (apply append (map f l))) ; bind
  (define (reflect meaning) (shift k (extend k meaning)))
  (define (reify thunk) (reset (eta thunk)))
  (define (amb* . t)
    (reflect (apply append (map reify t))))
  (let-syntax ([amb
                (syntax-rules () [(amb x ...) (amb* (lambda () x) ...)])])
    (define (www)
      (let ((f (lambda (x) (+ x (amb 6 4 2 8) (amb 2 4 5 4 1)))))
        (reify (lambda () (f (f (amb 0 2 3 4 5 32)))))))

    (define (wwww)
      (let ((f (lambda (x) (+ x (amb 6 4 2 8) (amb 2 4 5 4 1)))))
        (reify (lambda () (f (f (f (amb 0 2 3 4 5 32))))))))

    (test-equal "www" 2400 (length (www)))
    (test-equal "wwww" 48000 (length (wwww)))
    ))

;; inversion of iterator
(let ()
  (define (inv lis)
    (define (kk)
      (reset (for-each (lambda (e) (shift k (set! kk k) e)) lis)
             (set! kk (lambda () (eof-object)))
             (eof-object)))
    (lambda () (kk)))
  (define iter (inv '(1 2 3 4 5)))
  (test-equal "inversion" 1 (iter))
  (test-equal "inversion" 2 (iter))
  (test-equal "inversion" 3 (iter))
  (test-equal "inversion" 4 (iter))
  (test-equal "inversion" 5 (iter))
  (test-equal "inversion" (eof-object) (iter))
  (test-equal "inversion" (eof-object) (iter)))


(test-end)