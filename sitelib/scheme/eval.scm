;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme eval)
    (export environment eval null-environment scheme-report-environment)
    (import (rnrs) (rnrs eval))

  (define null-environment
    (lambda (n)
      (or (= n 7)
	  (assertion-violation 
	   'null-environment
	   (format "expected 7, but got ~r, as argument 2" n)))
      ;; need more?
      (environment '(only (scheme base)
			  define lambda quote quasiquote let
			  let* letrec letrec* let-values let*-values
			  if set! cond case and or when unless
			  begin do guard parameterize unquote
			  unquote-splicing define-syntax let-syntax
			  letrec-syntax => else syntax-rules ... _)
		   '(scheme case-lambda)
		   '(only (scheme lazy) lazy delay))))

  (define scheme-report-environment
    (lambda (n)
      (or (= n 7)
	  (assertion-violation 
	   'scheme-report-environment
	   (format "expected 7, but got ~r, as argument 2" n)))
      (environment '(scheme base)
		   '(scheme case-lambda)
		   '(scheme char)
		   '(scheme char normalization)
		   '(scheme complex)
		   '(scheme division)
		   '(scheme eval)
		   '(scheme file)
		   '(scheme inexact)
		   '(scheme lazy)
		   '(scheme load)
		   '(scheme process-context)
		   '(scheme read)
		   '(scheme time)
		   '(scheme write))
      ))
)