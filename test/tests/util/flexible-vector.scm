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
  
  (test-assert (flexible-vector-set! fv 0 #t))
  (test-equal 2 (flexible-vector-size fv))
  (test-equal '#(#t #t) (flexible-vector->vector fv))
  
  (test-assert (flexible-vector-insert! fv 3 #f))
  (test-equal 4 (flexible-vector-size fv))
  (test-equal '#(#t #t ok #f) (flexible-vector->vector fv))
  
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

(test-end)
