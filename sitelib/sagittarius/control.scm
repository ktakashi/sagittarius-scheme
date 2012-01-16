;; -*- mode: scheme; coding: utf-8; -*-
;; This file is a part of Sagittarius Scheme system.
#!compatible
(library (sagittarius control)
    (export define-macro
	    define-optional
	    let-optionals*
	    get-optional
	    define-with-key
	    let-keywords
	    let-keywords*
	    begin0
	    let1
	    check-arg)
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax)
	    (core misc)
	    (match)
	    (srfi :2 and-let*)
	    (srfi :26 cut)
	    (sagittarius))

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
	  `(,(rename 'define-with-key) ,name
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


  ;; from Gauche
  (define-syntax let-optionals*
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((_let (rename 'let))     (_if (rename 'if))
	     (_null? (rename 'null?)) (_unless (rename 'unless))
	     (_car (rename 'car))     (_cdr (rename 'cdr))
	     (_assertion-violation (rename 'assertion-violation))
	     (_undefined (rename 'undefined)))
	 (define (rec arg vars&inits&test rest body)
	   (cond ((null? (cdr vars&inits&test))
		  `((,_let ((,(caar vars&inits&test)
			     (,_if (,_null? ,arg)
				   ,(cadar vars&inits&test)
				   (,_car ,arg)))
			    ,@(if (null? rest)
				  '()
				  `((,rest (,_if (,_null? ,arg)
						 '()
						 (,_cdr ,arg))))))
			(,_unless ,(cddar vars&inits&test)
			   (,_assertion-violation 'let-optionals*
				"optional argument test failed"
				,(cddar vars&inits&test)))
			,@body)))
		 (else
		  (let ((g (gensym))
			(v (caar vars&inits&test))
			(i (cadar vars&inits&test))
			(t (cddar vars&inits&test)))
		    `((,_let ((,v (,_if (,_null? ,arg) ,i (,_car ,arg)))
			      (,g (,_if (,_null? ,arg) '() (,_cdr ,arg))))
			 (,_unless ,t
			   (,_assertion-violation 'let-optionals*
				"optional argument test failed" ,v))
			 ,@(rec g (cdr vars&inits&test) rest body)))))))
	 (define (improper-map1 p l)
	   (let loop ((lst l)
		      (r '()))
	     (cond ((null? lst) (reverse! r))
		   ((not (pair? lst)) (reverse! r))
		   (else (loop (cdr lst) (cons (p (car lst)) r))))))
	 (match form
	   ((_ arg specs . body)
	    (let ((g (gensym)))
	      `(,_let ((,g ,arg))
		 ,@(rec g (improper-map1
			   (lambda (s)
			     (cond ((and (pair? s) (pair? (cdr s)))
				    (cond ((null? (cddr s))
					   (cons* (car s) (cadr s) #t))
					  ((null? (cdddr s))
					   (cons* (car s) (cadr s) (caddr s)))
					  (else
					   (syntax-violation 'let-optionals*
					    "malformed let-optionals* bindings"
					    form specs))))
				   ((variable? s)
				    (cons* s `(,_undefined) #t))
				   (else 
				    (syntax-violation 'let-optionals*
				     "malformed let-optionals* bindings"
				     form specs))))
			   specs)
			(cdr (last-pair specs))
			body))))
	   (_ (syntax-violation 'let-optionals*
				"malformed let-optionals*" form)))))))

  (define-syntax let-keywords
    (er-macro-transformer
     (lambda (form rename compare)
       (match form
	 ((_ arg specs . body)
	  (%let-keywords-rec arg specs body (rename 'let) rename))
	 (_ (syntax-violation 'let-keywords
			      "malformed let-keywords" form))))))

  (define-syntax let-keywords*
    (er-macro-transformer
     (lambda (form rename compare)
       (match form
	 ((_ arg specs . body)
	  (%let-keywords-rec arg specs body (rename 'let*) rename))
	 (_ (syntax-violation 'let-keywords*
			      "malformed let-keywords*" form))))))

  ;; like Gauche's :key
  ;; (define-with-key (name arg0 arg1 :key key1 (key2 #f) :allow-other-keys opt)
  ;;   body ...)
  ;; ->
  ;; (define (name arg0 arg1 . optional)
  ;;   (let-keywords* optional (key1
  ;;                            (key2 #f)
  ;;                            . opt)
  ;;     body ...))
  (define (%define name expr rename compare)
    (define (parse-lambda-args formals)
      (let loop ((formals formals) (args '()) (n 0))
	(match formals
	  (()      (values (reverse args) n 0 '()))
	  (((? keyword?) . _) (values (reverse args) n 1 formals))
	  ((x . y) (loop (cdr formals) (cons (car formals) args) (+ n 1)))
	  (x       (values (reverse (cons x args)) n 1 '())))))
    (define (extended-lambda garg kargs body)
      (define (collect-args xs r)
	(match xs
	  (() (values (reverse r) '()))
	  (((? keyword?) . _) (values (reverse r) xs))
	  ((var . rest) (collect-args rest (cons var r)))))
      (define (parse-kargs xs os ks r a)
	(match xs
	  (() (expand-opt os ks r a))
	  ((:optional . xs)
	   (unless (null? os) (too-many :optional))
	   (receive (os xs) (collect-args xs '()) (parse-kargs xs os ks r a)))
	  ((:key . xs)
	   (unless (null? ks) (too-many :key))
	   (receive (ks xs) (collect-args xs '()) (parse-kargs xs os ks r a)))
	  ((:rest . xs)
	   (when r (too-many :rest))
	   (receive (rs xs) (collect-args xs '())
	     (match rs
	       ((r) (parse-kargs xs os ks r a))
	       (_ 
		(syntax-violation 'define-with-key
		 ":rest keyword in the define-with-key form must be followed by exactly one argument" kargs)))))
	  ((:allow-other-keys . xs)
	   (when a (too-many :allow-other-keys))
	   (receive (a xs) (collect-args xs '())
	     (match a
	       (()   (parse-kargs xs os ks r #t))
	       ((av) (parse-kargs xs os ks r av))
	       (_ (syntax-violation 'define-with-key
		   ":allow-other-keys keyword in define-with-key form can be followed by zero or one argument" kargs)))))
	  (_ (syntax-violation 'define-with-key
	      "invalid define-with-key list:" kargs))))
      (define (too-many key)
	(syntax-violation 'define-with-key
	  (format "too many ~s keywords in define-with-key: ~s" key kargs)
	  name expr))
      (define (expand-opt os ks r a)
	(if (null? os)
	    (if r
		`(,(rename 'let) ((,r ,garg)) ,@(expand-key ks garg a))
		(expand-key ks garg a))
	    (let ((binds (map (match-lambda
			       ((? variable? o) o)
			       ((o init) `(,o ,init))
			       (_ (syntax-violation 'define-with-key
				    "illegal optional argument spec" kargs)))
			      os))
		  (rest (or r (gensym))))
	      `((,(rename 'let-optionals*) ,garg ,(append binds rest)
		 ,@(if (and (not r) (null? ks))
		       `((,(rename 'unless) (,(rename 'null?) ,rest)
			  (,(rename 'assertion-violation)
			   "too many argument for" ',(unwrap-syntax expr)))
			 (,(rename 'let) () ,@(expand-key ks rest a)))
		       (expand-key ks rest a)))))))
      (define (expand-key ks garg a)
	(if (null? ks)
	    body
	    (let ((args (map (match-lambda
			      ((? variable? o) o)
			      ((((? keyword? key) o) init) `(,o ,key, init))
			      ;; for compatibility
			      (( o (? keyword? key) init) `(,o ,key, init))
			      ((o init) `(,o ,init))
			      (_ (syntax-violation 'define-with-key
				   "illegal keyword argument spec" kargs)))
			     ks)))
	      `((,(rename 'let-keywords*) ,garg
		 ,(if a (append args a) args)
		 ,@body)))))
      (parse-kargs kargs '() '() #f #f))
    (define (construct formals body)
      (receive (args reqargs optarg kargs) (parse-lambda-args formals)
	;; we do not make 'lambda hygenic.
	(define (make-dot-pair args)
	  (let loop ((args args)
		     (r '()))
	    (cond ((and (pair? args) (null? (cdr args)))
		   (append! (reverse r) (car args)))
		  (else (loop (cdr args) (cons (car args) r))))))
	(if (null? kargs)
	    `(,(rename 'define) ,name
	      (lambda ,(if (zero? optarg) args (make-dot-pair args)) ,@body))
	    (let ((g (gensym)))
	      `(,(rename 'define) ,name
		(lambda ,(append args g)
		  ,@(extended-lambda g kargs body)))))))
    (match expr
      ((_ formals . body)
       (construct formals body))
      (_ (syntax-violation 'define-with-key
			   "malformed define-with-key form" expr))))

  (define-syntax define-with-key
    (er-macro-transformer
     (lambda (form rename compare)
       (match form
	 ((_ (name . args) body ...)
	  `(,(rename 'define-with-key) ,name
	    (,(rename 'lambda) ,args ,@body)))
	 ((_ name expr)
	  (unless (variable? name) 
	    (syntax-violation 'define-with-key 
			      "invalid define-with-key name" form name))
	  (if (and (pair? expr) (compare 'lambda (car expr)))
		(%define name expr rename compare)
	      `(,(rename 'define) ,name ,expr)))
	 (_
	  (syntax-violation 'define-with-key "malformed define-with-key"
			    form))))))
	 

  ;;(define-macro (let-keywords arg specs . body)
  ;;  (%let-keywords-rec arg specs body 'let))

  ;;(define-macro (let-keywords* arg specs . body)
  ;;  (%let-keywords-rec arg specs body 'let*))

  (define (%let-keywords-rec arg specs body %let rename)
    (define (triplet var&default)
      (or (and-let* ([ (list? var&default) ]
		     [var (unwrap-syntax (car var&default))]
		     [ (symbol? var) ])
	    (case (length var&default)
	      [(2) (values (car var&default)
			   (make-keyword var)
			   (cadr var&default))]
	      [(3) (values (car var&default)
			   (unwrap-syntax (cadr var&default))
			   (caddr var&default))]
	      [else #f]))
	  (and-let* ([var (unwrap-syntax var&default)]
		     [ (symbol? var) ])
	    (values var (make-keyword var) (undefined)))
	  (error 'let-keywords "bad binding form in let-keywords" var&default)))
    (define (process-specs specs)
      (let loop ((specs specs)
		 (vars '()) (keys '()) (defaults '()) (tmps '()))
	(define (finish restvar)
	  (values (reverse! vars)
		  (reverse! keys)
		  (reverse! defaults)
		  (reverse! tmps)
		  restvar))
	(cond [(null? specs) (finish #f)]
	      [(pair? specs)
	       (receive (var key default) (triplet (car specs))
		 (loop (cdr specs)
		       (cons var vars)
		       (cons key keys)
		       (cons default defaults)
		       (cons (gensym) tmps)))]
	      [else (finish (or specs #t))])))

    (let ((argvar (gensym "args")) (loop (gensym "loop"))
	  ;; we make a little bit hygine.
	  (_error (rename 'error))
	  (_undefined? (rename 'undefined?)))
      (receive (vars keys defaults tmps restvar) (process-specs specs)
	`(let ,loop ((,argvar ,arg)
		     ,@(if (boolean? restvar) '() `((,restvar '())))
		     ,@(map (cut list <> (undefined)) tmps))
	      (cond
	       [(null? ,argvar)
		(,%let ,(map (lambda (var tmp default)
			       `(,var (if (,_undefined? ,tmp) ,default ,tmp)))
			     vars tmps defaults)
		       ,@body)]
	       [(null? (cdr ,argvar))
		(,_error 'let-keywords "keyword list not even" ,argvar)]
	       [else
		(case (car ,argvar)
		  ,@(map (lambda (key)
			   `((,key)
			     (,loop (cddr ,argvar)
				    ,@(if (boolean? restvar)
					  '()
					  `(,restvar))
				    ,@(map (lambda (k t)
					     (if (eq? key k)
						 `(cadr ,argvar)
						 t))
					   keys tmps))))
			 keys)
		  (else
		   ,(cond [(eq? restvar #t)
			   `(,loop (cddr ,argvar) ,@tmps)]
			  [(eq? restvar #f)
			   `(begin
			      (,_error 'let-keywords "unknown keyword " (car ,argvar))
			      (,loop (cddr ,argvar) ,@tmps))]
			  [else
			   `(,loop
			     (cddr ,argvar)
			     (,(rename 'cons*) (car ,argvar) (cadr ,argvar) ,restvar)
			     ,@tmps)])))
		]))))
    )       


  (define-syntax begin0
    (syntax-rules ()
      ((begin0 exp exp2 ...)
       (receive r exp exp2 ... (apply values r)))))
  
  ;; from Gauche
  (define-syntax let1
    (syntax-rules ()
      ((let1 var exp . body)
       (let ((var exp)) . body))))

  (define-syntax check-arg
    (syntax-rules ()
      ((_ pred val proc)
       (if (pred val)
           val
           (assertion-violation
            'proc
            (format "invalid argument, assertion failed in expression ~s" (list 'pred 'val))
            (list pred val))))))
)
