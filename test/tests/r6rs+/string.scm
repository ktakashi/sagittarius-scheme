;; -*- scheme -*-
(import (rnrs)
	(srfi :64 testing))
(test-begin "Sagittarius extension of string tests")
;; fallback
(test-assert "string-ref fallback" (boolean? (string-ref "abc" 3 #f)))
(test-equal "string-copy" 
	    "bcdef"
	    (string-copy "abcdef" 1))
(test-equal "string-copy" 
	    "bcd"
	    (string-copy "abcdef" 1 4))
(test-end)


