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
  (define initialize (make-generic 'initialize))
  (define compute-cpl (make-generic 'compute-cpl))
  (define compute-slots (make-generic 'compute-slots))
  (define compute-getters-and-setters
    (make-generic 'compute-getters-and-setters))

  (define (initialize-direct-slots obj cls init-args)
    (let loop ((slots (slot-ref cls 'direct-slots)))
      (when (and (not (null? slots))
		 (pair? (car slots))) ;; avoid accident to car call-next-method
	(let ((name (caar slots)))
	  (slot-set! obj name (getl init-args (make-keyword (caar slots))))
	  (loop (cdr slots))))))

  (define (getl initargs name . not-found)
    (cond ((find-tail (cut eq? name <>) initargs) => cadr)
	  ((pair? not-found) (car not-found))
	  (else (assertion-violation 'getl
				     "required argument could not be found"
				     initargs))))

  (define (class-direct-supers c)
    (slot-ref c 'direct-supers))

  (define (class-direct-slots c)
    (slot-ref c 'direct-slots))
  
  (define (class-cpl c)
    (slot-ref c 'cpl))

  (define (class-slots c)
    (slot-ref c 'slots))

  (define (generic-methods gf)
    (slot-ref gf 'methods))

  (define (method-specializers m)
    (slot-ref m 'specializers))

  (define (method-procedure m)
    (slot-ref m 'procedure))

  (add-method compute-cpl
	      (make-method (list <class>)
		(lambda (class call-next-method)
		  (let ((cpl (compute-std-cpl class)))
		    cpl))))

  (add-method compute-slots
	      (make-method (list <class>)
		(lambda (class call-next-method)
		  (compute-std-slots class))))

  (add-method compute-getters-and-setters
	      (make-method (list <class> <list>)
		(lambda (class slots call-next-method)
		  (compute-std-getters-and-setters class slots))))

  (add-method initialize
	      (make-method (list <object> <list>)
		(lambda (object initargs call-next-method)
		  object)))

  (add-method initialize
	      (make-method (list <class> <list>)
		(lambda (class initargs call-next-method)
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
		  (slot-set! class 'nfields (length (slot-ref class 'slots))))))

  (add-method initialize
	      (make-method (list <method> <list>)
		(lambda (method initarg call-next-method)
		  (call-next-method)
		  (slot-set! method 'name
			     (getl initarg :definition-name #f))
		  (slot-set! method 'specializers
			     (getl initarg :specializers))
		  (slot-set! method 'procedure
			     (getl initarg :procedure)))))

  (add-method initialize
	      (make-method (list <generic> <list>)
		(lambda (generic initarg call-next-method)
		  (call-next-method)
		  (slot-set! generic 'name
			     (getl initarg :definition-name #f)))))

  (add-method initialize
	      (make-method (list <object> <list>)
		(lambda (object . initarg) object)))

  ;; add initial make
  (add-method make
	      (make-method (list <class>)
		(lambda (class . initargs)
		  (let ((obj (allocate-instance class initargs)))
		    (initialize obj initargs)
		    obj))))

  ;; convenient methods
  (define (make-class direct-supers direct-slots . name)
    (make <class>
      :direct-supers direct-supers
      :direct-slots direct-slots
      :definition-name (and (pair? name) (car name))))

)