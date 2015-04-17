(import (rnrs)
	(sagittarius debug)
	(srfi :64 testing))

(test-begin "Sagittarius debug")

(define-syntax test-expansion
  (syntax-rules ()
    ((_ expr)
     (test-expansion expr expr))
    ((_ expected expr)
     (test-equal 'expr 'expected (macroexpand 'expr)))))
;; const values
(test-expansion #t)
(test-expansion 'a)
(test-expansion #vu8(1 2))
(test-expansion '#(1 2))
(test-expansion '(1 2))

;; test for macroexpand
(test-expansion (apply cons '(1 2)))
(test-expansion (cons 1 2))
(test-expansion (values 1 2))

;; don't want to show #<unspecified>
;; NB: don't relay on the result. may change.
(test-expansion (when #f #t))
(test-expansion (unless #f #t))

(test-end)
