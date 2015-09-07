(import (rnrs)
	(srfi :124)
	(srfi :64))

(test-begin "SRFI 124 - Ephemerons")

(test-assert "ephemeron?" (make-ephemeron 'a 'b))
(test-equal "ephemeron-key" 'a
	    (ephemeron-key (make-ephemeron 'a 'b)))
(test-equal "ephemeron-datum" 'b
	    (ephemeron-datum (make-ephemeron 'a 'b)))
(test-assert "ephemeron-broken?"
	     (not (ephemeron-broken? (make-ephemeron 'a 'b))))

(test-end)
