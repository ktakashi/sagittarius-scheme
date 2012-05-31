;; -*- mode: scheme; coding: utf-8; -*-
;; This file is a part of Sagittarius Scheme system.
#!compatible
(library (sagittarius control)
    (export define-macro
	    define-optional
	    let-optionals*
	    get-optional
	    define-with-key ;; for compatibility
	    let-keywords
	    let-keywords*
	    begin0
	    let1
	    dotimes
	    dolist
	    check-arg
	    with-library)
    (import (core)
	    (rename (only (core) define) (define define-with-key))
	    (core base)
	    (core errors)
	    (core syntax)
	    (core misc)
	    (match)
	    (srfi :26 cut)
	    (sagittarius)
	    (sagittarius vm))

  ;; for compatibility
  (define-syntax define-optional
    (er-macro-transformer
     (lambda (form rename compare)
       (define (convert binds)
	 (let loop ((lst binds)
		    (r '()))
	   (cond ((null? lst) (reverse! r))
		 ((pair? (car lst))
		  (unless (eq? (identifier->symbol (caar lst)) 'optional)
		    (syntax-violation 'define-optional
				      "malformed define-optional bindings"
				      form binds))
		  (append (reverse! r) (cons :optional (cdar lst))))
		 (else (loop (cdr lst) (cons (car lst) r))))))
       (match form
	 ((_ (name . bindings) . body)
	  `(,(rename 'define) ,name
	    (,(rename 'lambda) ,(convert bindings) ,@body)))
	 (_ (syntax-violation 'define-optional
			      "malformed define-optional" form))))))

  (define-syntax get-optional
    (syntax-rules ()
      ((_ rest default-exp)
       (let ((maybe-arg rest))
         (if (pair? maybe-arg)
             (if (null? (cdr maybe-arg)) (car maybe-arg)
                 (assertion-violation 'get-optional
				   "too many optional arguments" maybe-arg))
             default-exp)))

      ((_ rest default-exp arg-test)
       (let ((maybe-arg rest))
         (if (pair? maybe-arg)
             (if (null? (cdr maybe-arg))
                 (let ((val (car maybe-arg)))
                   (if (arg-test val) val
                       (assertion-violation 'get-optional
					    "optional argument failed test"
					    'arg-test val)))
                 (assertion-violation 'get-optional
				      "too many optional arguments" maybe-arg))
             default-exp)))))

  (define-syntax begin0
    (syntax-rules ()
      ((begin0 exp exp2 ...)
       (receive r exp exp2 ... (apply values r)))))
  
  ;; from Gauche
  (define-syntax let1
    (syntax-rules ()
      ((let1 var exp . body)
       (let ((var exp)) . body))))

  (define-syntax dolist
    (syntax-rules ()
      ((_ (var lis res) . body)
       (begin (for-each (lambda (var) . body) lis)
	      (let ((var '())) res))      ;bound var for CL compatibility
       )
      ((_ (var lis) . body)
       (begin (for-each (lambda (var) . body) lis) '()))
      ((_ . other)
       (syntax-violation 'dolist
			 "malformed dolist"
			 '(dolist . other)))))

  (define-syntax dotimes
    (syntax-rules ()
      ((_ (var i res) . body)
       (do ((limit i)
	    (var 0 (+ var 1)))
	   ((>= var limit) res)
	 . body))
      ((_ (var i) . body)
       (do ((limit i)
	    (var 0 (+ var 1)))
	   ((>= var limit))
	 . body))
      ((_ . other)
       (syntax-violation 'dotimes
			 "malformed dotimes"
			 '(dotimes . other)))))

  (define-syntax check-arg
    (syntax-rules ()
      ((_ pred val proc)
       (if (pred val)
           val
           (assertion-violation
            'proc
            (format "invalid argument, assertion failed in expression ~s" 
		    (list 'pred 'val))
            (list pred val))))))

  (define-syntax with-library
    (syntax-rules ()
      ((_ lib var)
       (let ((g (find-binding 'lib 'var #f)))
	 (or (and g (gloc-ref g))
	     (assertion-violation 'with-library
				  "unbound variable" 'var))))))

)
