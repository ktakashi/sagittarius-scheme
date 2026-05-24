(import (rnrs) (sagittarius compiler))

(define expr '(map values x))

(compile-p1 expr)
(compile-p2 expr)
(compile-p3 expr)
(compile-p4 expr)
(compile-p5 expr)
