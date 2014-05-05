;; -*- scheme -*-
#!core
(library (core r5rs)
  (export (rename (inexact exact->inexact) (exact inexact->exact))
          quotient
          remainder
          modulo
          delay delay-force
          force
	  make-promise)
  (import (core) (core promise))

  ) ;[end]
