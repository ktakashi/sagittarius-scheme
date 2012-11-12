;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme lazy)
    (export delay force delay-force make-promise promise?)
    (import (rnrs) (core r5rs))

  (define (promise? obj)
    (and (pair? obj)
	 (pair? (car obj))
	 (null? (cdr obj))
	 (boolean? (caar obj))
	 (procedure? (cdar obj))))
)
