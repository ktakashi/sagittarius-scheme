(import (rnrs) (srfi :64) (srfi :39))

(test-begin "SRFI 39 test")

;; issue 107
(let ()
  (define x (make-parameter 3 (lambda (x) (+ x 3))))
  (test-equal "param" 6 (x))
  (parameterize ((x 4)) (test-equal "in parameterize" 7 (x)))
  (test-equal "after" 6 (x)))

(test-end)