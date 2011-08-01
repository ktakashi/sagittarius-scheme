;; -*- scheme -*-
(library (tests r6rs+ lists)
    (export run-r6rs+-lists-tests)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (srfi :64))

  (define (run-r6rs+-lists-tests)
    ;; map
    (test-equal "map with different length"
		'((a . d) (b . e) (c . f))
		(map cons '(a b c d e f) '(d e f)))
    ;; for-each
    (test-equal "for-each with different length"
		'((g . j) (h . k) (i . l))
		(let* ((r `((a . d) (b . e) (c . f)))
		       (c r))
		   (for-each (lambda (a b)
			       (let ((t (car c)))
				 (set! c (cdr c))
				 (set-car! t a)
				 (set-cdr! t b)))
			     '(g h i j k l) '(j k l))
		   r))
			     
    )
)
