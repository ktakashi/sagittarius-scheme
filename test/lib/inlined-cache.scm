(library (inlined-cache)
    (export run)
    (import (core) (closure-cache))

  (define (run) ((car *bar*))) ;; return #t
)
