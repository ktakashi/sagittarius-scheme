;; -*- mode:scheme; coding:utf-8; -*-
#!r7rs
(define-library (tests r7rs process-context)
  (import (scheme base)
	  (scheme process-context)
	  (tests r7rs test))
  (export run-r7rs-process-context-tests)
  (begin
    (define (run-r7rs-process-context-tests)
      (test-equal "OK" (get-environment-variable "R7RS_TEST"))
      (test-true (let loop ((alist (get-environment-variables)))
		   (cond ((null? alist))
			 ((and (string? (caar alist))
			       (string? (cdar alist)))
			  (loop (cdr alist)))
			 (else #f))))
      ))
)