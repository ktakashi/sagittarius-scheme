(library (clos user)
    (export make
	    initialize
	    write-object

	    <top> <object>

	    slot-ref
	    slot-set!
	    initialize-direct-slots

	    define-class
	    define-method
	    define-generic)
    (import (rnrs) 
	    (sagittarius)
	    (match)
	    (shorten)
	    (clos core))

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
  (define-syntax define-class
    (er-macro-transformer
     (lambda (form rename compare)
       (define (collect-accessor slot-defs)
	 (let loop ((defs slot-defs)
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
					   form
					   (car def)))
		       (loop (cdr defs)
			     (acons (caar defs) (cadr s) r)))
		      (else (loop (cdr defs) r)))))
	     (_
	      ;; error
	      (syntax-violation 'define-class
				"malformed slot specifier" form (car defs))))))
       (define (build name supers slot-defs)
	 ;; we creates generic accessor and the rest will be
	 ;; for generic make
	 (let ((accessors (collect-accessor slot-defs))
	       (_make     (rename 'make))  (_define (rename 'define))
	       (_begin    (rename 'begin))
	       (_define-generic (rename 'define-generic))
	       (_define-method  (rename 'define-method)))
	   ;; TODO check if given name is already exists as generic
	   `(,_begin
	     (,_define ,name
		 (,_make ,(rename '<class>)
			 :definition-name ',name
			 :direct-supers   (list ,@supers)
			 :direct-slots    ',slot-defs))
	     ,@(if (null? accessors)
		  `((,(rename 'undefined)))
		  ;; build generic
		  (map (lambda (slot)
			 (let ((slot-name (car slot))
			       (accessor  (cdr slot))
			       (tmp  (gensym)))
			   `(,_begin
			     (,_define-generic ,accessor)
			     (,_define-method ,accessor ((,tmp ,name))
				(,(rename 'slot-ref) ,tmp ',slot-name)))))
		       accessors))
		  )))
       (match form
	 ((_ name () slot-defs)
	  `(,(rename 'define-class) ,name (<object>) ,slot-defs))
	 ((_ name (supers ...) slot-defs)
	  (unless (or (null? slot-defs) (pair? slot-defs))
	    (syntax-violation 'define-class
			      "malformed define-class" form))
	  (build name supers slot-defs))
	 (_ (syntax-violation 'define-class
			      "malformed define-class" form)))))

    #;(syntax-rules ()
      ((_ name () slot-def ...)
       (define-class name (<object>) slot-def ...))
      ((_ name (super ...) slot-def ...)
       (define name
	 (make <class>
	   :definition-name 'name
	   :direct-supers   (list super ...)
	   :direct-slots   '(slot-def ...))))))

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
	 (let* ((specializers (map (^s (if (pair? s) (cadr s) '<top>)) qargs))
		(reqargs      (map (^s (if (pair? s) (car s) s)) qargs))
		(lambda-list  (if opt `(,@reqargs . ,opt) reqargs))
		(real-args    (if opt
				  `(call-next-method ,@reqargs . ,opt)
				  `(call-next-method ,@reqargs)))
		(real-body    `(,(rename 'lambda) ,real-args ,@body)))
	   ;; TODO if generic does not exists, make it
	   `(,(rename 'add-method) ,generic
	     (,(rename 'make) ,(rename '<method>)
	      :specializers  (,(rename 'list) ,@specializers)
	      :qualifier     ,qualifier
	      :generic       ,generic
	      :lambda-list  ',lambda-list
	      :procedure     ,real-body))))

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