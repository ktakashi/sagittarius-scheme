(library (closure-cache)
    (export *bar* *bar2*)
    (import (core))
  (define (foo-proc) #t)

  (define foo-proc2
    (let ((free #f)) 
      (lambda () (set! free #t) free)))
  (define *bar* (list foo-proc))
  (define *bar2* (list foo-proc2))
)
