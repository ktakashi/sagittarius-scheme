;; all tests for extensions
(add-load-path ".")
(add-dynamic-load-path "../build")
(import (threads test)
	(time test)
	(srfi :64 testing))

(test-begin "extension test")
;; time must be first. it is used in thread test
(run-time-test)
(run-threads-test)
(test-end)