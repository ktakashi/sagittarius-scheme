#;(import (core exceptions)
	(core)
	(pp)
	(sagittarius test))
#;(run-test
 (define v (vector 1 #f #f))
 (assert-true? (vector? v)))

(define (values . args)
  (call/cc (lambda (cc) (apply cc args))))
(display (values 1 2 3))