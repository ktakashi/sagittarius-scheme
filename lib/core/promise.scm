;; -*- mode:scheme; coding: utf-8; -*-
(library (core promise)
    (export delay force delay-force make-promise promise? eager)
    (import (core)
	    (core record)
	    (core syntax))
  ;; should we make this applicable so that
  ;; we can support implicit force?
  (define-record-type (<promise> promise promise?)
    (fields (mutable box promise-box set-promise-box!))
    (protocol
     (lambda (p)
       (lambda (done? value)
	 ;; this is cheaper 
	 (p (cons done? value))))))
  (define (promise-done? p) (car (promise-box p)))
  (define (promise-value p) (cdr (promise-box p)))

  ;; for SRFI-45
  (define (eager expr) (promise #t expr))

  (define-syntax delay-force
    (syntax-rules ()
      ((_ expr)
       (promise #f (lambda () expr)))))

  (define-syntax delay
    (syntax-rules ()
      ((_ expr)
       (delay-force (eager expr)))))
  
  (define (force promise)
    (if (promise? promise)
	(if (promise-done? promise)
	    (promise-value promise)
	    (let ((promise* ((promise-value promise))))
	      (cond ((promise? promise*)
		     (unless (promise-done? promise)
		       (promise-update! promise* promise))
		     (force promise))
		    (else promise*))))
	promise))
    
  (define (make-promise obj) (if (promise? obj) obj (delay obj)))

  (define (promise-update! new old)
    (let ((old-box (promise-box old))
	  (new-box (promise-box new)))
      (set-car! old-box (car new-box))
      (set-cdr! old-box (cdr new-box))
      ;; swap box
      (set-promise-box! new old-box)))
)
