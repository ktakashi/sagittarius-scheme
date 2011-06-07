;; all tests for extensions
(add-load-path ".")
(add-dynamic-load-path "../build")
(import (threads test))

(run-threads-test)