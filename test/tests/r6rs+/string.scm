;; -*- scheme -*-
(library (tests r6rs+ string)
    (export run-r6rs+-string-tests)
    (import (rnrs)
	    (srfi :64))

  (define (run-r6rs+-string-tests)
    ;; fallback
    (test-assert "string-ref fallback" (boolean? (string-ref "abc" 3 #f)))
    )
)

