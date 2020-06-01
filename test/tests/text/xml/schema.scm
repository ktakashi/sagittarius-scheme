(import (rnrs)
	(text xml schema)
	(srfi :64))

(test-begin "XML Schema")

(test-assert (xs:duration? (xs:make-duration "P1Y")))
(let ((d (xs:make-duration "P1Y")))
  (test-equal 12 (xs:duration-months d))
  (test-equal 0 (xs:duration-seconds d)))

(test-end)
