;; -*- scheme -*-
(library (tests srfi :13)
    (export run-srfi-13-tests)
    (import (except (rnrs) string?) ;; to avoid confliction
	    (srfi :64)
	    (srfi :13))


  (define (run-srfi-13-tests)
    (test-begin "srfi-13-test")
    (test-assert 'string? (string? "abc"))
    (test-assert 'string-null? (string-null? ""))
    (test-assert 'string-null?-error (not (string-null? "abc")))
    (test-assert 'string-every (string-every (lambda (c)
					       (<= 0 (char->integer c) 128))
					     "abcdefg"))
    (test-end))
)