(import (rnrs)
	(util sparse)
	(srfi :64))

(test-begin "Sparse collection")

(test-assert "predict" (sparse-vector? (make-sparse-vector)))

;; TODO more test
(let ((spvec (make-sparse-vector)))
  (sparse-vector-set! spvec 0 'a)
  (test-equal "ref" 'a (sparse-vector-ref spvec 0))
  (test-equal "fallback" #f (sparse-vector-ref spvec 1 #f))
  (test-equal "size" 1 (sparse-vector-size spvec)))

(test-end)