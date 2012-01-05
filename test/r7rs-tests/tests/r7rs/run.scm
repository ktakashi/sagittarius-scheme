;; -*- mode: scheme; coding: utf-8; -*-
#!r7rs
(import (tests r7rs test)
	(tests r7rs base)
	(tests r7rs lazy)
	(tests r7rs case-lambda)
	(tests r7rs char))

(test-begin "R7RS tests")
(run-r7rs-base-tests)
(run-r7rs-lazy-tests)
(run-r7rs-case-lambda-tests)
(run-r7rs-char-tests)
(test-end)
