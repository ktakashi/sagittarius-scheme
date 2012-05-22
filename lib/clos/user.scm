;; -*- mode:scheme; coding: utf-8; -*-
(library (clos user)
    (export make
	    initialize
	    write-object object-equal? object-apply

	    <top> <object> <class> <generic> <method> <next-method>
	    <slot-accessor>
	    ;; immediates
	    <boolean> <char> <eof-object> <undefined-object> <unknown>
	    ;; pair
	    <list> <pair> <null>
	    ;; number
	    <number> <complex> <real> <rational> <integer>
	    ;; collection
	    <collection> <sequence> <dictionary> <ordered-dictionary>
	    <string> <vector> <bytevector>
	    <hashtable> <tree-map>
	    <weak-vector> <weak-hashtable>
	    ;; symbol keyword
	    <symbol> <keyword> <gloc>
	    ;; io
	    <port> <codec> <transcoder>
	    ;; record
	    <record-type>
	    ;; procedure
	    <procedure>
	    ;; etc
	    <identifier>

	    slot-ref
	    slot-set!
	    slot-ref-using-accessor slot-set-using-accessor!
	    initialize-direct-slots
	    is-a?

	    define-class
	    define-method
	    define-generic)
    (import (rnrs) 
	    (sagittarius)
	    (clos core)
	    (only (sagittarius clos) %ensure-generic-function)
	    (sagittarius vm))

  ;; We provides CL like define-class
  ;; The syntax is (define-class ?name (?supers ...) ({slot-specifier}*))
  ;; no class-option. (maybe I will provide :metaclass in future)
  ;; slot-specifier must be like this:
  ;;  slot-specifier ::= slot-name | (slot-name [[slot-option]])
  ;;  slot-name      ::= symbol
  ;;  slot-option    ::= {:accessor   function-name}
  ;;                   | {:init-value   expr}
  ;;                   | {:init-keyword keyword}
  ;;  function-name  ::= symbol
  ;; slot-option's options names are taken from Gauche. I think it's clearer
  ;; than CL's initform etc.
  (define %get-default-metaclass
    (let ((generated-metas '()))
      (define (find-metaclass metasupers)
	(cond ((assoc metasupers generated-metas) => cdr)
	      (else (make-metaclass metasupers))))
      (define (make-metaclass metasupers)
	(let ((meta (make <class>
		      :definition-name (gensym "metaclass")
		      :direct-supers metasupers
		      :direct-slots '())))
	  (set! generated-metas (acons metasupers meta generated-metas))
	  meta))
      (lambda (supers)
	(if (null? supers)
	    <class>
	    (let* ((all-metas (map class-of supers))
		   (all-cpls  (apply append
				     (map (lambda (m)
					    (cdr (class-cpl m))) all-metas)))
		   (needed '()))
	      (for-each (lambda (m)
			  (when (and (not (memq m all-cpls))
				     (not (memq m needed)))
			    (set! needed (cons m needed)))) all-metas)
	      (if (null? (cdr needed))
		  (car needed)
		  (find-metaclass (reverse! needed))))))))

  (define-syntax define-class
    (lambda (x)
      (define (collect-accessor slot-defs)
	(let loop ((defs (unwrap-syntax slot-defs)) (r '()))
	  (syntax-case defs ()
	    (() r)
	    (((name . acc) . rest)
	     (identifier? #'name)
	     (let ((s (memq :accessor #'acc)))
	       (cond (s
		      (unless (and (pair? (cdr s))
				   (identifier? (cadr s)))
			(syntax-violation 'define-class
					  "malformed slot specifier"
					  (unwrap-syntax x)
					  (unwrap-syntax (car defs))))
		      (loop (cdr defs)
			    (acons (identifier->symbol #'name)
				   (identifier->symbol (cadr s)) r)))
		     (else (loop (cdr defs) r)))))
	    (((name) . rest)
	     (identifier? #'name)
	     (loop (cdr defs) r))
	    (_ (syntax-violation 'define-class
				 "malformed slot specifier"
				 (unwrap-syntax x)
				 (unwrap-syntax (car defs)))))))
      (define (build name supers slot-defs options)
	;; we creates generic accessor and the rest will be
	;; for generic make
	(let* ((accessors (collect-accessor slot-defs))
	       (metaclass (or (get-keyword :metaclass options #f)
			      #`(%get-default-metaclass 
				 (list #,@supers)))))
	  (define (process-slot-definition sdef)
	    (if (pair? sdef)
		(let loop ((opts (cdr sdef)) (r '()))
		  (cond ((null? opts) `(list ',(car sdef) ,@(reverse! r)))
			((not (and (pair? opts) (pair? (cdr opts))))
			 (syntax-violation 'define-class
					   "bad slot specification" sdef))
			(else
			 (case (car opts)
			   ((:init-form)
			    (loop (cddr opts)
				  `((lambda () ,(cadr opts)) :init-thunk ,@r)))
			   ((:accessor)
			    (loop (cddr opts) 
				  `(',(cadr opts) ,(car opts) ,@r)))
			   (else 
			    (loop (cddr opts)
				  (cons* (cadr opts) (car opts) r)))))))
		`'(,sdef)))
	  ;; TODO check if given name is already exists as generic
	  #`(begin
	      (define #,name
		(make #,metaclass
		  :definition-name (quote #,name)
		  :direct-supers   (list #,@supers)
		  :direct-slots    (list #,@(map process-slot-definition
						 slot-defs))))
	      #,@(if (null? accessors)
		     #`((undefined))
		     ;; build generic
		     (map (lambda (slot)
			    (let ((slot-name (car slot))
				  (accessor  (cdr slot))
				  (tmp  (gensym)))
			      #`(begin
				  (define-method #,accessor ((#,tmp #,name))
				    (slot-ref #,tmp (quote #,slot-name)))
				  ;; getter
				  (define-method #,accessor ((#,tmp #,name) obj)
				    (slot-set! #,tmp (quote #,slot-name) obj)))))
			  accessors)))))
      (syntax-case x ()
	((_ name () slot-defs . options)
	 #'(define-class name (<object>) slot-defs . options))
	((_ name (supers ...) slot-defs . options)
	 (or (null? #'slot-defs) (pair? #'slot-defs))
	 (build #'name #'(supers ...) #'slot-defs #'options))
	(_ (syntax-violation
	    'define-class
	    "malformed define-class" (unwrap-syntax x))))))

  (define-syntax define-method
    (lambda (x)
      (define (analyse args)
	(let loop ((ss args) (rs '()))
	  (cond ((null? ss)        (values (reverse! rs) #f))
		((not (pair? ss))  (values (reverse! rs) ss))
		(else (loop (cdr ss) (cons (car ss) rs))))))
      (define (build qualifier generic qargs opt body)
	;; ugly kludge
	(define (rewrite body)
	  (let loop ((body body))
	    (cond ((null? body) '())
		  ((pair? body)
		   (let ((a (rewrite (car body)))
			 (d (rewrite (cdr body))))
		     (if (and (eq? a (car body)) (eq? d (cdr body)))
			 body
			 (cons a d))))
		  ((and (identifier? body)
			(free-identifier=? body #'call-next-method))
		   'call-next-method)
		  (else body))))
	(let* ((specializers (map (lambda (s)
				    (if (pair? s) (cadr s) '<top>)) qargs))
	       (reqargs      (map (lambda (s) 
				    (if (pair? s) (car s) s)) qargs))
	       (lambda-list  (if opt `(,@reqargs . ,opt) reqargs))
	       (real-args    (if opt
				 `(call-next-method ,@reqargs . ,opt)
				 `(call-next-method ,@reqargs)))
	       (real-body    `(lambda ,real-args ,@(rewrite body)))
	       (gf           (gensym)))
	  ;; TODO if generic does not exists, make it
	  #`(let ((#,gf (%ensure-generic-function
			 '#,generic (vm-current-library))))
	      (add-method #,gf
			  (make <method>
			    :specializers  (list #,@specializers)
			    :qualifier     #,qualifier
			    :generic       #,generic
			    :lambda-list  '#,lambda-list
			    :procedure     #,real-body)))))
      (syntax-case x ()
	((_ qualifier generic args . body)
	 (keyword? #'qualifier)
	 (let-values (((qargs opt) (analyse #'args)))
	   (build #'qualifier #'generic qargs opt #'body)))
	((_ generic qualifier args . body)
	 (keyword? #'qualifier)
	 #'(define-method qualifier generic ,args . body))
	((_ generic args . body)
	 #'(define-method :primary generic args . body)))))

  (define-syntax define-generic
    (syntax-rules ()
      ((_ name)
       (define name (make <generic> :definition-name 'name)))))

)