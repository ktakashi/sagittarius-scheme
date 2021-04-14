(import (rnrs)
	(record builder)
	(srfi :64))

(test-begin "Record builder")

(let ()
  (define-record-type base
    (fields a b c))
  (define-syntax base-builder
    (make-record-builder base ((a 'a) (c 'c symbol->string))))
  (define-record-type child
    (parent base)
    (fields d e f))
  (define-syntax child-builder
    (make-record-builder child ((a 'b))))

  (test-assert "base?" (base? (base-builder)))
  (test-equal "field a (default)" 'a (base-a (base-builder)))
  (test-equal "field b (default)" #f (base-b (base-builder)))
  (test-equal "field c (default)" "c" (base-c (base-builder)))
  (test-equal "field a (a provided)" 'b (base-a (base-builder (a 'b))))
  (test-error "non-existing property (base)" (base-builder (d 'd)))

  (test-assert "child?" (child? (child-builder)))
  (test-equal "field a (default)" 'b (base-a (child-builder)))
  (test-equal "field b (default)" #f (base-b (base-builder)))
  (test-equal "field d" 'd (child-d (child-builder (d 'd))))

  ;; from
  ;; result a = c, b = b
  (let ((c (child-builder (from (child-builder (a 'a) (b 'b))) (a 'c))))
    (test-equal 'c (base-a c))
    (test-equal 'b (base-b c)))
  
  )

(test-end)
