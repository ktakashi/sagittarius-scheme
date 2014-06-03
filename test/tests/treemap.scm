(import (rnrs)
	(util treemap)
	(srfi :64))

(test-begin "Tree map")

;; TODO more tests
(let ()
  (define tm (make-rb-treemap compare))
  (test-equal "fallback" #f (treemap-ref tm "a"))
  (test-equal "fallback" 'fallback (treemap-ref tm "a" 'fallback))
  (treemap-set! tm "a" 1)
  (test-assert "contains" (treemap-contains? tm "a"))
  (treemap-set! tm "b" 2)
  (treemap-set! tm "c" 3)
  (test-equal "a" 1 (treemap-ref tm "a"))
  (test-equal "b" 2 (treemap-ref tm "b"))
  (test-equal "c" 3 (treemap-ref tm "c"))

  (test-equal "keys" '("a" "b" "c") (treemap-keys-list tm))
  (test-equal "values" '(1 2 3) (treemap-values-list tm))
  ;; entries
  (test-equal "entries" '(("a" "b" "c") (1 2 3))
	      (let-values ((r (treemap-entries-list tm))) r))

  (treemap-delete! tm "b")
  (test-assert "contains" (not (treemap-contains? tm "b")))
  (test-equal "deleted" #f (treemap-ref tm "b"))
  )

(test-end)