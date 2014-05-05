;; -*- mode:scheme; coding: utf-8; -*-
(library (core promise)
    (export delay force delay-force make-promise promise?)
    (import (core)
	    (core record)
	    (core syntax))
  ;; should we make this applicable so that
  ;; we can support implicit force?
  (define-record-type (<promise> promise promise?)
    (fields (mutable done? promise-done? set-promise-done?!)
	    (mutable value promise-value set-promise-value!)))

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

  (define (promise-update! new old)
      (set-promise-done?! old (promise-done? new))
      (set-promise-value! old (promise-value new))
      (set-promise-done?! new (promise-done? old)))
)
