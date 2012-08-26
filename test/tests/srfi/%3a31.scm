(import (rnrs)
	(srfi :31 rec)
	(srfi :64 testing))

(test-begin "SRFI-31 tests")

(define F (rec (F N)
	       ((rec (G K L)
		     (if (zero? K) L
			 (G (- K 1) (* K L)))) N 1)))
(test-assert (procedure? F))
(test-equal 1 (F 0))
(test-equal 3628800 (F 10))

(test-end)
