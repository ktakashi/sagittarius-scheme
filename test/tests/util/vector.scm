(import (rnrs)
	(util vector)
	(srfi :64))

(test-begin "Vector utilities")

(test-equal '#(1 3 5) (vector-filter odd? '#(1 2 3 4 5)))
(test-equal '#(1 3 5) (vector-remove even? '#(1 2 3 4 5)))

(test-end)
