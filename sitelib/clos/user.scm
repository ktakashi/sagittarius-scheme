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

  (define-syntax define-class
    (syntax-rules ()
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
				  `(call-next-method ,@reqargs ,opt)
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