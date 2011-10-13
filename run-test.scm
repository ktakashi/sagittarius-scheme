(add-load-path "./lib")
(add-load-path "./sitelib")
(import (srfi :0))

;; for R6RS test sutes
(print "testing R6RS test suite")
(add-load-path "./test/r6rs-test-suite")
(load "./test/r6rs-test-suite/tests/r6rs/run.sps")
(flush-output-port (current-output-port))

;; for sitelib
(print "testing sitelib")
(add-load-path "./test")
(add-load-path "./ext/regex")

(cond-expand
 (sagittarius.os.windows
  (add-dynamic-load-path "./build/modules"))
 (else
  (add-dynamic-load-path "./build")))
(load "./test/tests.scm")
(flush-output-port (current-output-port))

;; for extensions
(print "testing extensions")
(set-current-directory "ext")
(add-load-path "../lib")
(add-load-path "../sitelib")
(cond-expand
 (sagittarius.os.windows
  ;; all-tests adds dynamic-load-path however it's for non windows environment
  (add-dynamic-load-path "../build/modules"))
 (else
  #t))
(load "./all-tests.scm")
(flush-output-port (current-output-port))