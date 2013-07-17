(import (rnrs)
	(rnrs eval)
	(srfi :111 boxes)
	(srfi :64)
	(only (sagittarius) current-library))

(test-begin "SRFI-111 boxes")

(test-assert "box?" (box? (box 1)))

(test-equal "unbox" 'a (unbox (box 'a)))
(test-equal "unbox (autoboxing)" 'a (unbox 'a))

(test-equal "set-box!" 2 (let ((b (box 1)))
			   (set-box! b 2)
			   (unbox b)))


(test-end)