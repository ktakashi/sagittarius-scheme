;; -*- mode: scheme; coding: utf-8 -*-
;; Composable Continuation ported for Sagittarius
(library (sagittarius partcont)
    (export reset shift)
    (import (rnrs)
	    (sagittarius))

;;;; Shift & Reset
;;;; Composable Continuation Operators in Terms of CWCC

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.
  (define *meta-continuation*
    (lambda (value)
      (error '*meta-continuation* "No top-level RESET" value)))

  (define-syntax reset
    (syntax-rules ()
      ((reset body)
       (let ((mc *meta-continuation*))
	 (call-with-current-continuation
	  (lambda (k)
	    (set! *meta-continuation*
		  (lambda (value)
		    (set! *meta-continuation* mc)
		    (k value)))
	    (receive result body
	      ;;** do not beta-substitute!!
	      (apply *meta-continuation* result))))))))

  (define-syntax shift
    (syntax-rules ()
      ((shift var body)
       (call-with-current-continuation
	(lambda (k)
	  (receive result (let ((var (lambda value
				       (reset (apply k value)))))
			    body)
	    ;;** do not beta-substitute!!
	    (apply *meta-continuation* result)))))))
  )