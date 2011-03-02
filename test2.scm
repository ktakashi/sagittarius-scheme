
(add-load-path "./lib")
(import (rnrs) (sagittarius vm))

(define-syntax hoge
  (syntax-rules ()
    ((_ r ...)
     (list '(r (r ...)) ...))))
(display (hoge 1 2 3))
