;; -*- scheme -*-
#!core
(library (core r5rs)
  (export (rename (inexact exact->inexact) (exact inexact->exact))
          quotient
          remainder
          modulo
          delay delay-force
          force
	  make-promise)
  (import (core) (core syntax))

  (define (promise done? proc)
    (list (cons done? proc)))

  (define-syntax delay-force
    (syntax-rules ()
      ((_ expr)
       (promise #f (lambda () expr)))))

  (define-syntax delay
    (syntax-rules ()
      ((_ expr)
       (delay-force (promise #t expr)))))

  (define (force promise)
    (if (promise-done? promise)
	(promise-value promise)
	(let ((promise* ((promise-value promise))))
	  (unless (promise-done? promise)
	    (promise-update! promise* promise))
	  (force promise))))

  (define (make-promise obj) (delay obj))

  (define (promise-done? x) (caar x))
  (define (promise-value x) (cdar x))
  (define (promise-update! new old)
      (set-car! (car old) (promise-done? new))
      (set-cdr! (car old) (promise-value new))
      (set-car! new (car old)))
  ) ;[end]
