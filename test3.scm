(import (rnrs))
(define-syntax hoge
  (syntax-rules ()
    ((_ r ...)
     (list '(r (r ...)) ...))))
(display (%macroexpand
(hoge 1 2 3)))