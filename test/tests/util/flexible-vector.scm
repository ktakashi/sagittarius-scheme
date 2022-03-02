(import (rnrs)
	(util flexible-vector)
	(srfi :64))

(test-begin "Flexible vector")
(let ()
  (define fv (make-flexible-vector 2 'ok))
  (test-assert (flexible-vector-set! fv 0 #t))
  (test-equal 1 (flexible-vector-size fv))
  (test-equal '#(#t) (flexible-vector->vector fv))
  (test-equal '(#t) (flexible-vector->list fv))
  
  (test-assert (flexible-vector-insert! fv 0 #f))
  (test-equal 2 (flexible-vector-size fv))
  (test-equal '#(#f #t) (flexible-vector->vector fv))
  (test-equal '(#f #t) (flexible-vector->list fv))
  
  (test-equal #f (flexible-vector-set! fv 0 #t))
  (test-equal 2 (flexible-vector-size fv))
  (test-equal '#(#t #t) (flexible-vector->vector fv))
  
  (test-assert (flexible-vector-insert! fv 3 #f))
  (test-equal 4 (flexible-vector-size fv))
  (test-equal '#(#t #t ok #f) (flexible-vector->vector fv))

  (flexible-vector-set! fv 5 #t)
  (test-assert (flexible-vector-set! fv 5 #t))
  (test-equal 6 (flexible-vector-size fv))
  (test-equal '#(#t #t ok #f ok #t) (flexible-vector->vector fv)))

(define (test-w/default fv)
  (test-assert (flexible-vector-set! fv 0 #t))
  (test-equal 3 (flexible-vector-size fv))
  (test-equal '#(#t 2 3) (flexible-vector->vector fv))

  (test-assert (flexible-vector-insert! fv 3 #t))
  (test-equal 4 (flexible-vector-size fv))
  (test-equal '#(#t 2 3 #t) (flexible-vector->vector fv))

  (test-assert (flexible-vector-insert! fv 3 #f))
  (test-equal 5 (flexible-vector-size fv))
  (test-equal '#(#t 2 3 #f #t) (flexible-vector->vector fv)))
(test-w/default (vector->flexible-vector '#(1 2 3) 'ok))
(test-w/default (list->flexible-vector '(1 2 3) 'ok))

(let ()
  (define fv (vector->flexible-vector '#(1)))
  (test-equal 1 (flexible-vector-delete! fv 0))
  (test-equal 0 (flexible-vector-size fv))
  (test-error (flexible-vector-delete! fv 0))

  (test-equal fv (flexible-vector-insert! fv 0 1 2 3 4))
  (test-equal #(1 2 3 4) (flexible-vector->vector fv))
  (test-equal 4 (flexible-vector-size fv))

  (test-equal fv (flexible-vector-insert! fv 2 5 6 7))
  (test-equal #(1 2 5 6 7 3 4) (flexible-vector->vector fv))
  (test-equal 7 (flexible-vector-size fv))
  )

(let ((fv1 (flexible-vector 1 2 3))
      (fv2 (flexible-vector 4 5 6))
      (fv3 (flexible-vector 7)))
  (test-equal fv1 (flexible-vector-append! fv1 fv2 fv3))
  (test-equal 7 (flexible-vector-size fv1))
  (test-equal #(1 2 3 4 5 6 7) (flexible-vector->vector fv1))
  )

(let ((fv1 (flexible-vector 1 2 3))
      (fv2 (flexible-vector 2 3 3))
      (fv3 (flexible-vector 3 3 3)))
  
  (test-equal 3 (flexible-vector-any (lambda (a b c) (and (= a b c) a))
				     fv1 fv2 fv3))
  (test-equal #f (flexible-vector-any (lambda (a b c) (and (= a b c 4) a))
				      fv1 fv2 fv3))

  (test-equal 3 (flexible-vector-every
		 (lambda (a b c) (and (<= a b c) a)) fv1 fv2 fv3))
  (test-equal #f (flexible-vector-every
		  (lambda (a b c) (and (< a b c) a)) fv1 fv2 fv3))

  (let ((fv (flexible-vector-append fv1 fv2 fv3)))
    (test-equal 2 (flexible-vector-index (lambda (a) (= a 3)) fv))
    (test-equal 8 (flexible-vector-index-right (lambda (a) (= a 3)) fv)))
  )
  

(test-end)
