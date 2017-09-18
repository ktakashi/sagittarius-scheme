(import (rnrs)
	(srfi :145)
	(srfi :64))

(test-begin "SRFI-145: Assumptions")

(test-equal 'ok (assume 'ok))
(test-error assertion-violation? (assume #f))

(test-end)
