(add-load-path "./lib")
(add-load-path "./sitelib")

;; for R6RS test sutes
(print "testing R6RS test suite")
(add-load-path "./test/r6rs-test-suite")
(load "./test/r6rs-test-suite/tests/r6rs/run.sps")
(flush-output-port (current-output-port))

;; for sitelib
(print "testing sitelib")
(add-load-path "./test")
(add-load-path "./ext/regex")
(add-dynamic-load-path "./build")
(load "./test/tests.scm")
(flush-output-port (current-output-port))

;; for extensions
(print "testing extensions")
(set-current-directory "ext")
(add-load-path "../lib")
(add-load-path "../sitelib")
(load "./all-tests.scm")
(flush-output-port (current-output-port))