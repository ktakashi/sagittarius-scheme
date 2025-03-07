(import (rnrs)
	(sagittarius)
	(sagittarius crypto math prime)
	(srfi :64))

(define (test-prime name tests)
  (define (check test)
    (let ((id (vector-ref test 0))
	  (comment (vector-ref test 1))
	  (value (vector-ref test 2))
	  (result (vector-ref test 3)))
      ;; We don't accept negative prime
      (unless (string=? "negative of a prime" comment)
	(test-equal (format "~a ~d (~a): ~a" name id result comment)
		    result
		    (probable-prime? value)))))
  (for-each check tests))

(test-begin "Prime number test vectors")
(include "./testvectors/prime.scm")
(test-end)
