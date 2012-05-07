;; -*- scheme -*-

(import (rnrs)
	(srfi :64 testing))

(define v '#(1 2 3 4 5 6))
(define l '(1 2 3 4 5 6))

(test-begin "Sagittarius extension of vector and bytevector tests")
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

(test-equal "vector-fill!"
	    '(#(1 1 1 #f #f #f #f #f #f #f)
	      #(#f #f #f 2 2 2 #f #f #f #f)
	      #(3 3 3 3 3 3 3 3 3 3))
	    (list (let ((v (make-vector 10 #f)))
		    (vector-fill! v 1 0 3)
		    v)
		  (let ((v (make-vector 10 #f)))
		    (vector-fill! v 2 3 6)
		    v)
		  (let ((v (make-vector 10 #f)))
		    (vector-fill! v 3)
		    v)
		  ))

(test-equal "bytevector-copy"
	    #vu8(2 3 4 5 6)
	    (bytevector-copy #vu8(1 2 3 4 5 6) 1))

(test-equal "bytevector-copy"
	    #vu8(2 3 4 5)
	    (bytevector-copy #vu8(1 2 3 4 5 6) 1 5))

(test-end)

