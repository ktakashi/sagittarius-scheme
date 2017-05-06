(import (rnrs)
	(naming context)
	(srfi :64))

(test-begin "Naming context")

(test-assert make-naming-context)
(test-assert (naming-context? (make-naming-context)))

(let ((ctx (make-naming-context)))
  (test-error name-not-found? (naming-context-lookup ctx "scheme:a/b/c"))
  (guard (e ((naming-error? e) (test-equal "scheme:a/b/c" (naming-error-name e)))
	    (else (test-assert "something is wrong" #f)))
    (naming-context-lookup ctx "scheme:a/b/c"))
  (test-error naming-error? (naming-context-lookup ctx "a/b/c"))
  (test-equal #f (naming-context-get-attribute ctx "attr"))
  (test-equal 'fallback (naming-context-get-attribute ctx "attr" 'fallback))
  (test-assert (naming-context-set-attribute! ctx "attr" 'value))
  (test-equal 'value (naming-context-get-attribute ctx "attr"))

  (test-assert (naming-context-bind! ctx "scheme:a/b/c" 'value))
  (test-equal 'value (naming-context-lookup ctx "scheme:a/b/c"))
  (let ((child (make-naming-context ctx)))
    (test-equal 'value (naming-context-lookup child "scheme:a/b/c"))
    (test-assert (naming-context-bind! child "scheme:a/b/c/d" 'value2))
    (test-equal 'value2 (naming-context-lookup child "scheme:a/b/c/d"))
    (test-error name-not-found? (naming-context-lookup ctx "scheme:a/b/c/d"))))

;; misc
(test-error (*default-naming-category-specification* #f))

(test-end)
