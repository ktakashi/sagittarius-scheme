;; -*- mode: scheme; coding: utf-8; -*-
#!r7rs
(import (tests r7rs test)
	(tests r7rs base)
	(tests r7rs case-lambda)
	(tests r7rs char)
	;;(tests r7rs division)
	(tests r7rs eval)
	(tests r7rs inexact)
	(tests r7rs lazy)
	(tests r7rs process-context)
	(tests r7rs write)
	)

(test-begin "R7RS tests")
;; Just make sure we exports all required procedures
(run-r7rs-base-tests)
(run-r7rs-case-lambda-tests)
(run-r7rs-char-tests)
;;(run-r7rs-division-tests)
(run-r7rs-eval-tests)
(run-r7rs-inexact-tests)
(run-r7rs-lazy-tests)
(run-r7rs-process-context-tests)
(run-r7rs-write-tests)

;; extra for R7RS style srfi library
(test-true (cond-expand 
	    ((library (srfi 1)) #t)
	    (else #f)))

(test-end)
