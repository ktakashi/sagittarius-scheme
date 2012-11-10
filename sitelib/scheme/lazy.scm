;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme lazy)
    (export lazy delay force delay-force make-promise)
    (import (rnrs))

  (define (promise done? proc)
    (list (cons done? proc)))

  (define-syntax delay-force
    (syntax-rules ()
      ((_ expr)
       (promise #f (lambda () expr)))))

  (define-syntax delay
    (syntax-rules ()
      ((_ expr)
       (delay-force (promise #t (lambda () expr))))))

  (define (force promise)
    (if (promise-done? promise)
	(promise-value promise)
	(let ((promise* ((promise-value promise))))
	  (unless (promise-done? promise)
	    (promise-update! promise* promise))
	  (forse promise))))

  (define (make-promise obj) (delay obj))

  (define (promise-done? x) (caar x))
  (define (promise-value x) (cdar x))
  (define (promise-update! new old)
      (set-car! (car old) (promise-done? new))
      (set-cdr! (car old) (promise-value new))
      (set-car! new (car old)))
)
