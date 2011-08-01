;; -*- scheme -*-
(library (tests r6rs+ vector)
    (export run-r6rs+-vector-tests)
    (import (rnrs)
	    (srfi :64))

  (define v '#(1 2 3 4 5 6))
  (define l '(1 2 3 4 5 6))

  (define (run-r6rs+-vector-tests)
    ;; fallback
    (test-assert "vector fallback" (boolean? (vector-ref v 6 #f)))
    (test-equal "vector->list with start" 
		'(2 3 4 5 6)
		(vector->list v 1))
    (test-equal "vector->list with start and end" 
		'(2 3)
		(vector->list v 1 3))
    (test-equal "list->vector with start"
		'#(2 3 4 5 6)
		(list->vector l 1))
    (test-equal "list->vector with start and end"
		'#(2 3)
		(list->vector l 1 3))

    )
)

