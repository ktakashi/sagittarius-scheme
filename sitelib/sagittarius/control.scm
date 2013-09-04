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
	    rlet1
	    dotimes
	    dolist
	    push!
	    pop!
	    check-arg
	    with-library
	    unwind-protect
	    ;; for convenient
	    ^a ^b ^c ^d ^e ^f ^g ^h ^i ^j ^k ^l ^m ^n
	    ^o ^p ^q ^r ^s ^t ^u ^v ^w ^x ^y ^z ^_ ^
	    ^a* ^b* ^c* ^d* ^e* ^f* ^g* ^h* ^i* ^j* ^k*
	    ^l* ^m* ^n* ^o* ^p* ^q* ^r* ^s* ^t* ^u* ^v* ^w*
	    ^x* ^y* ^z* ^_*
	    )
    (import (core)
	    (rename (only (core) define) (define define-with-key))
	    (core base)
	    (core errors)
	    (core syntax)
	    (core misc)
	    (match)
	    (shorten)
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

  (define-syntax rlet1
    (syntax-rules ()
      ((_ var exp body ...)
       (let ((var exp)) body ... var))))

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

  ;; from Gauche
  (define-syntax push!
    (syntax-rules ()
      [(_ "vars" ((var arg) ...) () proc val)
       (let ((getter proc)
	     (var arg) ...)
	 ((setter getter) var ... (cons val (getter var ...))))]
      [(_ "vars" ((var arg) ...) (arg0 arg1 ...) proc val)
       (push! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc val)]
      [(_ (proc arg ...) val)
       (push! "vars" () (arg ...) proc val)]
      [(_ loc val)
       (set! loc (cons val loc))]
      [(_ . other)
       (syntax-error "malformed push!" (push! . other))]))

  (define-syntax pop!
    (syntax-rules ()
      [(_ "vars" ((var arg) ...) () proc)
       (let ((getter proc)
	     (var arg) ...)
	 (let ((val (getter var ...)))
	   ((setter getter) var ... (cdr val))
	   (car val)))]
      [(_ "vars" ((var arg) ...) (arg0 arg1 ...) proc)
       (pop! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc)]
      [(_ (proc arg ...))
       (pop! "vars" () (arg ...) proc)]
      [(_ loc)
       (let ((val loc))
	 (set! loc (cdr val))
	 (car val))]
      [(_ . other)
       (syntax-error "malformed pop!" (pop! . other))]))


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
    (lambda (x)
      (syntax-case x ()
	((k lib expr ...)
	 (let ((real-lib (find-library #'lib #f)))
	   (unless real-lib
	     (assertion-violation 'with-library "no such library" #'lib))
	   (let loop ((e #'(expr ...)))
	     (if (null? (cdr e))
	       (if (identifier? (car e))
		   ;; most likely (with-library (lib) var)
		   ;; and it must return identifier bounded in the given
		   ;; library otherwise breaks cache.
		   (make-identifier (syntax->datum (car e)) '() real-lib)
		   (let ((r (eval (syntax->datum (car e)) real-lib)))
		     (datum->syntax #'k r)))
	       (begin
		 (eval (car e) real-lib)
		 (loop (cdr e))))))))))

  (define-syntax unwind-protect
    (lambda (x)
      (syntax-case x ()
	((_ body handler ...)
	 #'(let ((h (lambda () handler ...)))
	     (receive r (with-error-handler
			  (lambda (e) (h) (raise e))
			  (lambda () body)
			  #t)
	       (h)
	       (apply values r))))
	(_ (syntax-violation 'unwind-protect
			     "malformed unwind-protect" (unwrap-syntax x))))))
)
