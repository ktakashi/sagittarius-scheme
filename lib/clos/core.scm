(library (clos core)
    (export slot-ref slot-set! slot-bound? 
	    slot-ref-using-accessor slot-set-using-accessor!
	    slot-unbound slot-missing
	    make initialize 
	    make-method add-method remove-method
	    ;; <class>
	    class-of
	    class-direct-supers
	    class-direct-slots
	    class-cpl
	    class-slots
	    ;; <generic>
	    generic-methods
	    ;; <methods>
	    method-specializers
	    method-procedure
	    method-required
	    method-optional
	    method-qualifier
	    ;; slots
	    slot-definition-name
	    slot-definition-options
	    slot-definition-option
	    slot-definition-accessor
	    ;; builtin class
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
	    <record-type> <tuple>
	    ;; procedure
	    <procedure>
	    ;; etc
	    <identifier> <code-builder>


	    ;; builtin generic
	    write-object allocate-instance compute-applicable-methods
	    compute-apply-methods
	    compute-apply-generic compute-method-more-specific?
	    object-equal? object-apply |setter of object-apply|
	    ;; helper
	    initialize-direct-slots is-a?
	    ;; helper generics
	    compute-cpl
	    compute-slots
	    compute-getters-and-setters
	    compute-getter-and-setter

	    ;; ugly solution for macro expansion
	    call-next-method
	    ;; eql specializer
	    eql
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius clos))

  (define (call-next-method . args)
    (error 'call-next-method "this must not be called on toplevel"))

  (define (initialize-direct-slots obj cls init-args)
    (let loop ((slots (slot-ref cls 'direct-slots)))
      (unless (null? slots)
	(let ((name (caar slots)))
	  (slot-set! obj name (get-keyword (make-keyword (caar slots))
					   init-args))
	  (loop (cdr slots))))))

  ;; NOTE: generic method must have call-next-method as its first argument.
  ;;       but %make does not need this, so just a dummy
  (let ((%make (lambda (dummy class . initargs)
		 (let ((obj (allocate-instance class initargs)))
		   (initialize obj initargs)
		   obj)))
	(body  (lambda (call-next-method class . initargs)
		 (let ((obj (allocate-instance class initargs)))
		   (initialize obj initargs)
		   obj))))
    (add-method make
		(%make 'dummy ;; note above
		       <method>
		       :generic make
		       :specializers  (list <class>)
		       :lambda-list  '(class . initargs)
		       :procedure body)))

  (define (make-method specializers procedure :key (qualifier :primary)
		       (generic #f))
    (define (gen-lambda-list specializers procedure)
      (let ((rest (if (cdr (arity procedure)) (gensym) '()))
	    (reqs (map (lambda (s) (gensym)) specializers)))
	(if (null? reqs)
	    rest
	    (cons reqs rest))))
    (make <method>
      :specializers specializers
      :lambda-list (gen-lambda-list specializers procedure)
      :qualifier qualifier
      :generic generic
      :procedure procedure))

  (add-method initialize
	      (make <method>
		:specializers (list <class>)
		:generic initialize
		:lambda-list '(class . initargs)
		:procedure (lambda (call-next-method class initargs)
			     (call-next-method)
			     (slot-set! class 'name
					(get-keyword :definition-name
						     initargs #f))
			     (slot-set! class 'direct-supers
					(get-keyword :direct-supers
						     initargs '()))
			     (slot-set! class
					'direct-slots
					(map (lambda (s)
					       (if (pair? s) s (list s)))
					     (get-keyword :direct-slots
							  initargs '())))
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
					(get-keyword :definition-name 
						     initarg #f)))))

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
  (define method-required (make <generic> :definition-name 'method-required))
  (define method-optional (make <generic> :definition-name 'method-optional))
  (define method-qualifier (make <generic> :definition-name 'method-qualifier))
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
  (add-method method-required
	      (make <method>
		:specializers (list <method>)
		:lambda-list '(method)
		:generic method-required
		:procedure (lambda (call-next-method m)
			     (slot-ref m 'optional))))
  (add-method method-optional
	      (make <method>
		:specializers (list <method>)
		:lambda-list '(method)
		:generic method-optional
		:procedure (lambda (call-next-method m)
			     (slot-ref m 'optional))))

  (add-method method-qualifier
	      (make <method>
		:specializers (list <method>)
		:lambda-list '(method)
		:generic method-qualifier
		:procedure (lambda (call-next-method m)
			     (slot-ref m 'qualifier))))

  ;; low level slot APIs
  (define (slot-definition-name slot) (car slot))
  (define (slot-definition-options slot) (cdr slot))
  (define (slot-definition-option slot key . default)
    (apply get-keyword key (cdr slot) default))
  (define (slot-definition-accessor slot)
    (get-keyword :accessor (cdr slot) #f))

  ;; write-object for <class>
  (add-method write-object
	      (make <method>
		:specializers (list <class> <port>)
		:lambda-list '(c p)
		:generic write-object
		:procedure (lambda (call-next-method c p)
			     (format p "#<class ~a>" (slot-ref c 'name)))))

  ;; generic invocation
  (add-method compute-apply-generic
	      (make <method>
		:specializers (list <generic> <list>)
		:lambda-list '(g l)
		:generic compute-apply-generic
		:procedure 
		(lambda (call-next-method gf args)
		  (let ((methods (compute-applicable-methods gf args)))
		    (compute-apply-methods gf methods args)))))

  (define compute-applicable-methods 
    (make <generic> :definition-name 'compute-applicable-methods))
  (define (compute-around-methods around before primary after more-specific?)
    (let ((primary (list-sort more-specific? primary))
	  (around  (list-sort more-specific? around))
	  (before  (list-sort more-specific? before))
	  (after   (list-sort more-specific? after)))
      (%compute-around-methods around before primary after)))

  (add-method compute-applicable-methods
	      (make <method>
		:specializers (list <generic> <list>)
		:lambda-list '(g l)
		:generic compute-applicable-methods
		:procedure 
		(lambda (call-next-method gf args)
		  ;; To allow derived generic class have qualifiers
		  ;; we do the same things done in C here as well.
		  ;; FIXME duplicate code may introduce bugs!!
		  (let ((applicable (%compute-applicable-methods gf args))
			(more-specific?
			 (compute-method-more-specific? gf args)))
		    (let-values (((primary before after around)
				  (%sort-method-by-qualifier applicable)))
		      (if (and (null? before) (null?  after) (null? around))
			  (list-sort more-specific? primary)
			  (compute-around-methods around before primary after
						  more-specific?)))))))

  (add-method compute-apply-methods
	      (make <method>
		:specializers (list <generic> <top> <top>)
		:lambda-list '(g l a)
		:generic compute-apply-methods
		:procedure
		(lambda (call-next-method gf methods args)
		  (compute-apply-methods gf methods %make-next-method args))))

  (add-method compute-apply-methods
	      (make <method>
		:specializers (list <generic> <top> <top> <top>)
		:lambda-list '(g l m a)
		:generic compute-apply-methods
		:procedure
		(lambda (call-next-method gf methods build-next args)
		  (apply (build-next gf methods args) args))))

  ;; it's already documented so we can't remove this...
  (define compute-getters-and-setters
    (make <generic> :definition-name 'compute-getters-and-setters))
  (add-method compute-getters-and-setters
    (make <method>
      :specializers (list <class> <list>)
      :lambda-list '(class slots)
      :generic compute-getters-and-setters
      :procedure
      (lambda (call-next-method class slots)
	(let loop ((i 0) (slots slots) (r '()))
	  (if (null? slots)
	      (reverse! r)
	      (let* ((slot (car slots))
		     (accs (compute-getter-and-setter class slot))
		     (sac (apply %make-slot-accessor class (car slot) i accs)))
		(loop (+ i 1) (cdr slots) (cons sac r))))))))
)
