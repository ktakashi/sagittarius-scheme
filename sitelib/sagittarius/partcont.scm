;; -*- mode: scheme; coding: utf-8 -*-
;; Composable Continuation ported for Sagittarius
(library (sagittarius partcont)
    (export reset shift)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius vm))
  
  (define-syntax reset
    (syntax-rules ()
      ((_ expr ...)
       (%apply0 (lambda () expr ...)))))

  (define (call/pc proc)
    (%call/pc (lambda (k) (proc (lambda args (reset (apply k args)))))))

  (define-syntax shift
    (syntax-rules ()
      ((_ var expr ...)
       (call/pc (lambda (var) expr ...)))))
  
  )