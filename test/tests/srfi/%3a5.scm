;; if this test runs in multi thread it's ok but
;; when it runs in single thread it will contaminate
;; the original let so wrap it in dummy library

(library (srfi :5 test)
    (export run-srfi-5)
    (import (except (rnrs) let)
	    (srfi :5 let)
	    (srfi :64))

  ;; from gauche
  (define (run-srfi-5)
    (define-syntax test* (identifier-syntax test-equal))
    (test-begin "SRFI 5 - let")
    (test* "let - standard" 3
	   (let ((x 1) (y 2))
	     (let ()
	       (+ x y))))

    (test* "let - standard" 1
	   (let ((x 1) (y 2))
	     (let ((y x) (x y))
	       (- x y))))

    (test* "let - standard" 1
	   (let ()
	     (define x 1)
	     (* x x)))

    (test* "let - standard, named" 55
	   (let loop ((x 1) (sum 0))
	     (if (> x 10) sum (loop (+ x 1) (+ sum x)))))

    (test* "let - signature style" 55
	   (let (loop (x 1) (sum 0))
	     (if (> x 10) sum (loop (+ x 1) (+ sum x)))))

    (test* "let - signature style" #t
	   (let (loop)
	     (procedure? loop)))

    (test* "let - rest binding" '(0 1 (2 3 4))
	   (let ((x 0) (y 1) . (z 2 3 4)) (list x y z)))

    (test* "let - rest binding, named" '((2 3 4) 0 (1))
	   (let loop ((x 0) (y 1) . (z 2 3 4))
	     (if (list? x) (list x y z) (loop z x y))))
    (test-end)
    )
)

(import (srfi :5 test))
(run-srfi-5)