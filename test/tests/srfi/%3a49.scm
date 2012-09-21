;; how should we test this?
#< (srfi :49) >
(import (rnrs) (srfi :49) (srfi :64))

(test-begin "SRFI-49 tests")
(test-assert "srfi-49-load"
	     (srfi-49-load (string-append (current-directory)
					  "/test/data/srfi-49-test.scm")))

(test-equal "srfi-49-read"
	    '(let ((foo (+ 1 2)) (bar (+ 3 4))) (+ foo bar))
	    (srfi-49-read (open-string-input-port
			   "let
 group
   foo (+ 1 2)
   bar (+ 3 4)
 + foo bar
")))

define (fac x)
 if (= x 0) 1
  * x
   fac (- x 1)

(test-equal "srfi-49 style fac" 3628800 (fac 10))

(test-end)
