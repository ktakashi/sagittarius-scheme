(library (clos core)
    (export slot-ref slot-set!
	    make initialize add-method
	    ;; <class>
	    class-of
	    class-direct-supers
	    class-direct-slots
	    class-cpl
	    ;; <generic>
	    generic-methods
	    ;; <methods>
	    method-specializers
	    method-procedure
	    ;; builtin class
	    <top> <object> <class> <generic> <method> <next-method>
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

	    ;; builtin generic
	    write-object allocate-instance
	    ;; helper
	    make-class make-method make-generic
	    initialize-direct-slots
	    ;; helper generics
	    compute-cpl
	    compute-slots)
    (import (rnrs)
	    (srfi :1 lists)
	    (srfi :26 cut)
	    (sagittarius)
	    (sagittarius clos))
  (define (initialize-direct-slots obj cls init-args)
    (let loop ((slots (slot-ref cls 'direct-slots)))
      (unless (null? slots)
	(let ((name (caar slots)))
	  (slot-set! obj name (getl init-args (make-keyword (caar slots))))
	  (loop (cdr slots))))))

  (define (getl initargs name . not-found)
    (cond ((find-tail (cut eq? name <>) initargs) => cadr)
	  ((pair? not-found) (car not-found))
	  (else (assertion-violation 'getl
				     "required argument could not be found"
				     initargs))))

  (define (%make class . initargs)
    (let ((obj (allocate-instance class initargs)))
      (initialize obj initargs)
      obj))

  (let ((body  (lambda (call-next-method class . initargs)
		 (let ((obj (allocate-instance class initargs)))
		   (initialize obj initargs)
		   obj))))
    (add-method make
		(%make <method>
		       :generic make
		       :specializers  (list <class>)
		       :lambda-list  '(class . initargs)
		       :procedure body)))

  (add-method initialize
	      (make <method>
		:specializers (list <class>)
		:generic initialize
		:lambda-list '(class . initargs)
		:procedure (lambda (call-next-method class initargs)
			     (call-next-method)
			     (slot-set! class 'name
					(getl initargs :definition-name #f))
			     (slot-set! class 'direct-supers
					(getl initargs :direct-supers '()))
			     (slot-set! class
					'direct-slots
					(map (lambda (s)
					       (if (pair? s) s (list s)))
					     (getl initargs :direct-slots '())))
			     (slot-set! class 'cpl   (compute-cpl class))
			     (slot-set! class 'slots (compute-slots class))
			     (slot-set! class 'getters-n-setters
					(compute-getters-and-setters
					 class
					 (slot-ref class 'slots)))
			     (slot-set! class 'nfields
					(length (slot-ref class 'slots))))))

  (add-method initialize
	      (make <method>
		:specializers (list <generic>)
		:generic initialize
		:lambda-list '(generic . initarg)
		:procedure (lambda (call-next-method generic initarg)
			     (call-next-method)
			     (slot-set! generic 'name
					(getl initarg :definition-name #f)))))

  ;; make generic accessor for <class> <generic> and <method>
  ;; <class>
  (define class-direct-supers
    (make <generic> :definition-name 'class-direct-supers))
  (define class-direct-slots
    (make <generic> :definition-name 'class-direct-slots))
  (define class-cpl (make <generic> :definition-name 'class-cpl))
  (define class-slots (make <generic> :definition-name 'class-slots))

  (add-method class-direct-supers
	      (make <method>
		:specializers (list <class>)
		:lambda-list '(class)
		:generic class-direct-supers
		:procedure (lambda (call-next-method class)
			     (slot-ref class 'direct-supers))))
  (add-method class-direct-slots
	      (make <method>
		:specializers (list <class>)
		:lambda-list '(class)
		:generic class-direct-slots
		:procedure (lambda (call-next-method class)
			     (slot-ref class 'direct-slots))))
  (add-method class-cpl
	      (make <method>
		:specializers (list <class>)
		:lambda-list '(class)
		:generic class-cpl
		:procedure (lambda (call-next-method class)
			     (slot-ref class 'cpl))))
  (add-method class-slots
	      (make <method>
		:specializers (list <class>)
		:lambda-list '(class)
		:generic class-slots
		:procedure (lambda (call-next-method class)
			     (slot-ref class 'slots))))
  ;; <generic>
  (define generic-methods (make <generic> :definition-name 'generic-methods))
  (add-method generic-methods
	      (make <method>
		:specializers (list <generic>)
		:lambda-list '(generic)
		:generic generic-methods
		:procedure (lambda (call-next-method gf)
			     (slot-ref gf 'methods))))

  ;; <method>
  (define method-specializers
    (make <generic> :definition-name 'method-specializers))
  (define method-procedure (make <generic> :definition-name 'method-procedure))
  (add-method method-specializers
	      (make <method>
		:specializers (list <method>)
		:lambda-list '(method)
		:generic method-specializers
		:procedure (lambda (call-next-method m)
			     (slot-ref m 'specializers))))
  (add-method method-procedure
	      (make <method>
		:specializers (list <method>)
		:lambda-list '(method)
		:generic method-procedure
		:procedure (lambda (call-next-method m)
			     (slot-ref m 'procedure))))
)
