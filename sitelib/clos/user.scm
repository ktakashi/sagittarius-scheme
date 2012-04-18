;; -*- mode:scheme; coding: utf-8; -*-
(library (clos user)
    (export make
	    initialize
	    write-object object-equal?

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
	    <identifier> <fasl-meta>

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
	    (match)
	    (shorten)
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
    (er-macro-transformer
     (lambda (form rename compare)
       (define (collect-accessor slot-defs)
	 (let loop ((defs (unwrap-syntax slot-defs))
		    (r '()))
	   (match defs
	     (() r)
	     ((((? symbol? name) . acc) . rest)
	      (let ((s (memq :accessor acc)))
		(cond (s
		       (unless (and (pair? (cdr s))
				    (symbol? (cadr s)))
			 (syntax-violation 'define-class
					   "malformed slot specifier"
					   (unwrap-syntax form)
					   (unwrap-syntax (car def))))
		       (loop (cdr defs)
			     (acons (caar defs) (cadr s) r)))
		      (else (loop (cdr defs) r)))))
	     (((? symbol? name) . rest)
	      (loop (cdr defs) r))
	     (_
	      ;; error
	      (syntax-violation 'define-class
				"malformed slot specifier"
				(unwrap-syntax form)
				(unwrap-syntax (car defs)))))))
       (define (build name supers slot-defs options)
	 ;; we creates generic accessor and the rest will be
	 ;; for generic make
	 (let* ((accessors (collect-accessor slot-defs))
		(_make     (rename 'make))  (_define (rename 'define))
		(_begin    (rename 'begin)) (_quote  (rename 'quote))
		(_list     (rename 'list))
		(_define-generic (rename 'define-generic))
		(_define-method  (rename 'define-method))
		(metaclass (or (get-keyword :metaclass options #f)
			       `(,(rename '%get-default-metaclass) 
				 (,_list ,@supers)))))
	   (define (process-slot-definition sdef)
	     (if (pair? sdef)
		 (let loop ((opts (cdr sdef)) (r '()))
		   (cond ((null? opts) `(,_list ',(car sdef) ,@(reverse! r)))
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
	   `(,_begin
	     (,_define ,name
		       (,_make ,metaclass
			       :definition-name (,_quote ,name)
			       :direct-supers   (,_list ,@supers)
			       :direct-slots    (,_list ,@(map process-slot-definition
							       slot-defs))))
	     ,@(if (null? accessors)
		   `((,(rename 'undefined)))
		   ;; build generic
		   (map (lambda (slot)
			  (let ((slot-name (car slot))
				(accessor  (cdr slot))
				(tmp  (gensym)))
			    `(,_begin
			      ;;(,_define-generic ,accessor)
			      ;; setter
			      (,_define-method ,accessor ((,tmp ,name))
					       (,(rename 'slot-ref) ,tmp (,_quote ,slot-name)))
			      ;; getter
			      (,_define-method ,accessor ((,tmp ,name) obj)
					       (,(rename 'slot-set!) ,tmp (,_quote ,slot-name)
						obj)))))
			accessors))
	     )))
       (match form
	 ((_ name () slot-defs . options)
	  `(,(rename 'define-class) ,name (<object>) ,slot-defs ,@options))
	 ((_ name (supers ...) slot-defs . options)
	  (unless (or (null? slot-defs) (pair? slot-defs))
	    (syntax-violation 'define-class
			      "malformed define-class" (unwrap-syntax form)))
	  (build name supers slot-defs options))
	 (_ (syntax-violation 
	     'define-class
	     "malformed define-class" (unwrap-syntax form)))))))

  (define-syntax define-generic
    (syntax-rules ()
      ((_ name)
       (define name (make <generic> :definition-name 'name)))))

  ;; I actually should use syntax-case but yeah, we have bugs in it...
  (define-syntax define-method
    (er-macro-transformer
     (lambda (form rename compare)
       (define (analyse args)
	 (let loop ((ss args) (rs '()))
	   (cond ((null? ss)        (values (reverse! rs) #f))
		 ((not (pair? ss))  (values (reverse! rs) ss))
		 (else (loop (cdr ss) (cons (car ss) rs))))))
       (define (build qualifier generic qargs opt body)
	 (let* ((specializers (map (^(s) (if (pair? s) (cadr s) '<top>)) qargs))
		(reqargs      (map (^(s) (if (pair? s) (car s) s)) qargs))
		(lambda-list  (if opt `(,@reqargs . ,opt) reqargs))
		(real-args    (if opt
				  `(call-next-method ,@reqargs . ,opt)
				  `(call-next-method ,@reqargs)))
		(real-body    `(,(rename 'lambda) ,real-args ,@body))
		(gf           (gensym)))
	   ;; TODO if generic does not exists, make it
	   `(,(rename 'let) ((,gf (,(rename '%ensure-generic-function)
				   ',generic (,(rename 'vm-current-library)))))
	     (,(rename 'add-method) ,gf
	       (,(rename 'make) ,(rename '<method>)
		:specializers  (,(rename 'list) ,@specializers)
		:qualifier     ,qualifier
		:generic       ,generic
		:lambda-list  ',lambda-list
		:procedure     ,real-body)))))

       (match form
	 ((_ (? keyword? qualifier) generic args . body)
	  (let-values (((qargs opt) (analyse args)))
	    (build qualifier generic qargs opt body)))
	 ((_ generic (? keyword? qualifier) args . body)
	  `(,(rename 'define-method) ,qualifier ,generic ,args . ,body))
	 ((_ generic args . body)
	  `(,(rename 'define-method) :primary ,generic ,args . ,body)))
       )))
    
)