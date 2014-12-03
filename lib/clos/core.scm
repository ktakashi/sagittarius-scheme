(library (clos core)
    (export slot-ref slot-set! slot-bound? 
	    slot-ref-using-accessor slot-set-using-accessor!
	    slot-ref-using-class slot-set-using-class! slot-bound-using-class?
	    slot-unbound slot-missing
	    slot-exists-using-class? slot-exists?
	    make initialize 
	    make-method add-method remove-method
	    ;; <class>
	    class-of
	    class-name
	    class-direct-supers
	    class-direct-slots
	    class-cpl
	    class-slots
	    class-direct-subclasses
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
	    ;; procedure
	    <procedure>
	    ;; etc
	    <identifier> <code-builder> <time> <char-set> <comparator>


	    ;; builtin generic
	    write-object allocate-instance compute-applicable-methods
	    compute-apply-methods
	    compute-apply-generic compute-method-more-specific?
	    object-equal? object-apply |setter of object-apply|
	    object-compare
	    ;; helper
	    initialize-direct-slots is-a? subtype?
	    ;; helper generics
	    compute-cpl
	    compute-slots
	    compute-getters-and-setters
	    compute-getter-and-setter

	    ;; eql specializer
	    eql

	    ;; change-class
	    change-class
	    update-instance!
	    )
    (import (core)
	    (core base)
	    (sagittarius)
	    (sagittarius clos))

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
			     (let ((supers (get-keyword :direct-supers
							initargs '())))
			       (slot-set! class 'name
					  (get-keyword :definition-name
						       initargs #f))
			       (slot-set! class 'defined-library
					  (get-keyword :defined-library
						       initargs #f))
			       (slot-set! class 'direct-supers supers)

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
					  (length (slot-ref class 'slots)))
			       ;; sub classes
			       (for-each (lambda (super)
					   (%add-direct-subclass! super class))
					 supers)
			       (slot-set! class 'initargs initargs)))))

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
  (define class-name
    (make <generic> :definition-name 'class-name))
  (define class-direct-supers
    (make <generic> :definition-name 'class-direct-supers))
  (define class-direct-slots
    (make <generic> :definition-name 'class-direct-slots))
  (define class-cpl (make <generic> :definition-name 'class-cpl))
  (define class-slots (make <generic> :definition-name 'class-slots))
  (define class-direct-subclasses 
    (make <generic> :definition-name 'class-direct-subclasses))

  (add-method class-name
	      (make <method>
		:specializers (list <class>)
		:lambda-list '(class)
		:generic class-name
		:procedure (lambda (call-next-method class)
			     (slot-ref class 'name))))

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

  (add-method class-direct-subclasses
	      (make <method>
		:specializers (list <class>)
		:lambda-list '(class)
		:generic class-direct-subclasses
		:procedure (lambda (call-next-method class)
			     (slot-ref class 'direct-subclasses))))
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
		     (sac (apply %make-slot-accessor class slot i accs)))
		(loop (+ i 1) (cdr slots) (cons sac r))))))))

  ;; change-class stuff
  ;; it's defined in C
  ;;(define change-class (make <generic> :definition-name 'change-class))
  (define update-instance! (make <generic> :definition-name 'update-instance!))
  (define slot-exists-using-class?
    (make <generic> :definition-name 'slot-exists-using-class?))

  (define (slot-exists? obj slot) 
    (slot-exists-using-class? (class-of obj) obj slot))

  (add-method slot-exists-using-class?
    (make <method>
      :specializers (list <class> <top> <top>)
      :lambda-list '(class obj slot)
      :generic slot-exists-using-class?
      :procedure
      (lambda (call-next-method class obj slot)
	(not (not (assq slot (class-slots class)))))))

  (add-method change-class
    (make <method>
      :specializers (list <object> <class>)
      :lambda-list '(obj class . initargs)
      :generic change-class
      :procedure
      (lambda (call-next-method old new-class . initargs)
	(let ((new (allocate-instance new-class initargs))
	      (current-class (current-class-of old)))
	  (let loop ((slots (map slot-definition-name (class-slots new-class))))
	    (unless (null? slots)
	      (let ((slot (car slots)))
		(when (and (slot-exists-using-class? current-class old slot)
			   (slot-bound-using-class? current-class old slot))
		  (let ((v (slot-ref-using-class current-class old slot)))
		    (slot-set-using-class! new-class new slot v))))
	      (loop (cdr slots))))
	  (%swap-class-and-slots! new old)
	  (apply update-instance! new old initargs)
	  old))))

  (add-method update-instance!
    (make <method>
      :specializers (list <object> <object>)
      :lambda-list '(old new . rest)
      :generic update-instance!
      :procedure
      (lambda (call-next-method old new . initargs)
	(let* ((old-class (current-class-of old))
	       (existing-slots (remp (lambda (acc) 
				      (not (slot-exists-using-class?
					    old-class 
					    old (slot-ref acc 'name))))
				     (slot-ref (class-of new)
					       'getters-n-setters))))
	  (for-each 
	   (lambda (acc)
	     (unless (slot-bound-using-accessor? new acc)
	       (slot-initialize-using-accessor! new acc initargs)))
	   existing-slots)))))

)
