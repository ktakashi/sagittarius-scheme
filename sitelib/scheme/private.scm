;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme private)
    (export define-values)
    (import (rnrs) (rnrs mutable-pairs))

  ;; FIXME, remove set-cdr!
  (define-syntax define-values
    (syntax-rules ()
      ((define-values () expr)
       (define dummy (let-values ((args expr)) #f)))
      ((define-values (var) expr)
       (define var expr))
      ((define-values (var0 var1 ... varn) expr)
       (begin
	 (define var0
	   (let-values ((tmp expr)) tmp))
	 (define var1
	   (let ((v (cadr var0)))
	     (set-cdr! var0 (cddr var0))
	     v))
	 ...
	 (define varn
	   (let ((v (cadr var0)))
	     (set! var0 (car var0))
	     v))))
      ((define-values (var0 var1 ... . var-dot) expr)
       (begin
	 (define var0
	   (let-values ((tmp expr)) tmp))
	 (define var1
	   (let ((v (cadr var0)))
	     (set-cdr! var0 (cddr var0))
	     v))
	 ...
	 (define var-dot
	   (let ((v (cdr var0)))
	     (set! var0 (car var0))
	     v))))
      ((define-values var expr)
       (define var (let-values ((tmp expr)) tmp)))))

)