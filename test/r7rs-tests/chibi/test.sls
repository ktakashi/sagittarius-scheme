;; -*- mode:scheme; coding:utf-8; -*-
;; from chibi scheme
(define-library (chibi test)
  (export
   test test-equal test-error test-assert test-not test-values
   test-group current-test-group
   test-begin test-end test-syntax-error test-propagate-info
   test-vars test-run test-exit
   current-test-verbosity current-test-epsilon current-test-comparator
   current-test-applier current-test-handler current-test-skipper
   current-test-group-reporter test-failure-count
   current-test-epsilon current-test-comparator)
  (import (scheme base)
          (scheme write)
          (scheme complex)
          (scheme process-context)
          (scheme time))
  (cond-expand
   (chibi
    (import (only (chibi) pair-source print-exception)))
   ;; sagittarius specific
   (sagittarius
    (import (only (rename (sagittarius vm debug) (source-info pair-source))
		  pair-source)
	    (only (sagittarius) report-error describe-condition))
    (begin (define print-exception report-error))
    )
   (else
    (begin
      (define (pair-source x) #f)
      (define print-exception write))))
  (include "test.scm"))
