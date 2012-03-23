;; -*- scheme -*-
(import (rnrs)
	(srfi :64 testing))
(test-begin "(run-r6rs+-string-tests)")
;; fallback
(test-assert "string-ref fallback" (boolean? (string-ref "abc" 3 #f)))
(test-end)


