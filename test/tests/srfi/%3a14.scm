;; -*- scheme -*-
(library (tests srfi :14)
    (export run-srfi-14-tests)
    (import (rnrs)
	    (srfi :64)
	    (srfi :14))

  (define cs1 (char-set #\a))
  (define char-list '(#\a #\b #\c))

  (define (run-srfi-14-tests)
    (test-assert 'char-set?     (char-set? cs1))
    (test-assert 'non-char-set? (not (char-set? #f)))
    (test-assert 'list->char-set (char-set? (list->char-set char-list)))
    )
)