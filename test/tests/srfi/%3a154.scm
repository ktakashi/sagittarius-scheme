(import (rnrs)
	(srfi :64)
	(srfi :39)
	(srfi :154))

(test-begin "SRFI 154")

(test-assert "Dynamic extents"
	     (dynamic-extent? (current-dynamic-extent)))

(test-equal "Parameter bindings"
	    'b
	    (let*
		((x (make-parameter 'a))
		 (de (parameterize
			 ((x 'b))
		       (current-dynamic-extent))))
	      (parameterize
		  ((x 'c))
		(with-dynamic-extent
		 de
		 (lambda ()
		   (x))))))

(test-equal "Dynamically closed procedures"
	    'a
	    (let* ((x (make-parameter 'a))
		   (getter (dynamic-lambda () (x))))
	      (parameterize ((x 'b))
		(getter))))

(test-equal "Multiple values"
	    '(1 2)
	    (call-with-values
		(lambda ()
		  (with-dynamic-extent (current-dynamic-extent)
		    (lambda ()
		      (values 1 2))))
	      list))

(test-equal "Nested with-dynamic-extent"
	    1
	    (let* ((x (make-parameter 1))
		   (e1 (current-dynamic-extent))
		   (e2 (parameterize ((x 2))
			 (current-dynamic-extent))))
	      (with-dynamic-extent e2 (lambda ()
					(with-dynamic-extent e1 (lambda ()
								  (x)))))))

(test-end "SRFI 154")
