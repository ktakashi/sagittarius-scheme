;; -*- scheme -*-
#!core
(library (core record procedural)
    (export make-record-type-descriptor
	    record-type-descriptor?
	    make-record-constructor-descriptor
	    record-constructor
	    record-predicate
	    record-accessor
	    record-mutator
	    make-record-type
	    ;; for inspection
	    record?
	    record-rtd
	    record-type-name
	    record-type-parent
	    record-type-uid
	    record-type-generative?
	    record-type-sealed?
	    record-type-opaque?
	    record-type-field-names
	    record-field-mutable?

	    record-type-rcd
	    record-type-rtd
	    <record-type-descriptor>
	    <record-constructor-descriptor>
	    default-protocol)
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax)
	    (sagittarius)
	    (clos user) ;; we can use it here
	    (clos core)
	    (sagittarius clos)) ;; for <record-type-meta>

  (define-class <record-type-descriptor> ()
    ((name :init-keyword :name     :reader record-type-name)
     (parent :init-keyword :parent :reader record-type-parent)
     (uid :init-keyword :uid       :reader record-type-uid)
     (sealed? :init-keyword :sealed? :reader record-type-sealed?)
     (opaque? :init-keyword :opaque? :reader record-type-opaque?)
     (fields :init-keyword :fields :reader rtd-fields)
     ;; instanciate class
     (class :init-keyword :class :reader rtd-class)))
  (define-class <record-constructor-descriptor> ()
    ((rtd :init-keyword :rtd :reader rcd-rtd)
     (protocol :init-keyword :protocol :reader rcd-protocol)
     (parent :init-keyword :parent :reader rcd-parent)))

  (define-method compute-getter-and-setter ((c <record-type-meta>) slot)
    (let ((mutability (slot-definition-option slot :mutable #f))
	  (accessors (call-next-method)))
      (if mutability
	  accessors
	  (list (car accessors)
		(lambda (o v) (error 'record-accessor
				     "field is immutable"
				     (slot-definition-name slot) o))
		(caddr accessors)))))
  
  (define (make-record-type-descriptor name parent uid sealed? opaque? fields)
    (define (process-fields fields)
      ;; -> slot form
      (map (lambda (field)
	     (if (pair? field)
		 (let ((mutability (car field))
		       (name (cadr field)))
		   ;; TODO check mutability keyword
		   (list name :mutable (eq? mutability 'mutable)
			 :init-keyword (make-keyword name)))
		 (error 'make-record-type-descriptor "invalid field")))
	   (vector->list fields)))
    (let* ((p (and parent (slot-ref parent 'class)))
	   (type (make <record-type-meta>
		   :definition-name name
		   :direct-supers (or (and p (list p))
				      '())
		   :direct-slots (process-fields fields)
		   :defined-library (current-library)))
	   (rtd (make <record-type-descriptor>
		  :name name :parent parent :uid uid
		  :sealed? sealed? :opaque? opaque?
		  :fields fields :class type)))
      (slot-set! type 'rtd rtd)
      rtd))
  (define (record-type-descriptor? o) (is-a? o <record-type-descriptor>))
  
  (define (make-record-constructor-descriptor rtd parent protocol)
    (unless (record-type-descriptor? rtd)
      (assertion-violation 'make-record-constructor-descriptor
       (wrong-type-argument-message "record-type-descriptor" rtd 1)
       (list rtd parent protocol)))
    (and parent
         (or (record-constructor-descriptor? parent)
             (assertion-violation 'make-record-constructor-descriptor
	      (wrong-type-argument-message "record-constructor-descriptor or #f"
					   parent 2)
	      (list rtd parent protocol))))
    (and protocol
         (or (procedure? protocol)
             (assertion-violation 
	      'make-record-constructor-descriptor
	      (wrong-type-argument-message "procedure or #f" protocol 3)
	      (list rtd parent protocol))))
    (and parent
         (or (record-type-parent rtd)
             (assertion-violation
              'make-record-constructor-descriptor
              "mismatch between rtd and parent constructor descriptor"
              rtd parent protocol)))
    (and parent
	 (record-type-parent rtd)
         (or (eq? (rcd-rtd parent) (record-type-parent rtd))
             (assertion-violation
              'make-record-constructor-descriptor
              "mismatch between rtd and parent constructor descriptor"
              rtd parent protocol)))
    (and protocol
	 (record-type-parent rtd)
         (or parent
             (assertion-violation
              'make-record-constructor-descriptor
              "expected #f for protocol since no parent constructor descriptor is provided"
              rtd parent protocol)))
    (and parent
         (not (rcd-protocol parent))
         (or protocol
             (assertion-violation
              'make-record-constructor-descriptor
              "expected procedure for protocol since parent constructor descriptor have custom one"
              rtd parent protocol)))

    (let ((rcd (make <record-constructor-descriptor>
		 :rtd rtd 
		 :parent (or parent
			     (cond ((record-type-parent rtd)
				    => (lambda (rtd)
					 (make-record-constructor-descriptor
					  rtd #f #f)))
				   (else #f)))
		 :protocol (or protocol (default-protocol rtd)))))
      (slot-set! (slot-ref rtd 'class) 'rcd rcd)
      rcd))
  (define (record-constructor-descriptor? o) 
    (is-a? o <record-constructor-descriptor>))

  (define (make-record-type name rtd rcd) (slot-ref rtd 'class))
  
  (define (rtd-total-field-count rtd)
    (length (class-slots (slot-ref rtd 'class))))
  
  (define (record-type-rtd type) (slot-ref type 'rtd))
  (define (record-type-rcd type) (slot-ref type 'rcd))
  
  ;; well at least we need to list all slots
  ;; to make my life easier, the returned list structure is like this.
  ;; ((name . class) ...)
  (define (compute-all-slots class)
    ;; assume record only contains one super class
    ;; so the CPL is direct ref
    (let loop ((supers (class-cpl class)) (r '()))
      (if (null? supers)
	  r
	  (let ((names (map slot-definition-name 
			    (class-direct-slots (car supers)))))
	    (loop (cdr supers)
		  `(,@r ,@(map (lambda (n) (cons n (car supers))) names)))))))

  (define (record-constructor rcd)
    (unless (record-constructor-descriptor? rcd)
      (assertion-violation 'record-constructor
       (format "record-constructor-descriptor required but got ~s" rcd)
       rcd))
    (let ((rtd (rcd-rtd rcd)))
      (if (rcd-parent rcd)
	  (let ((class (slot-ref rtd 'class)))
	    (make-nested-conser rcd rtd (length (compute-all-slots class))))
	  (make-simple-conser rcd rtd (vector-length (rtd-fields rtd))))))
  
  (define (%make-record rtd field-values)
    (let* ((class (slot-ref rtd 'class))
	   ;; TODO create (kw v) list to make mutable/immutable thing
	   (tuple (make class))
	   (slot&class (compute-all-slots class)))
      (for-each (lambda (slot value)
		  (slot-set-using-class! (cdr slot) tuple (car slot) value))
		slot&class field-values)
      tuple))
  
  (define (make-nested-conser desc rtd argc)
    ((rcd-protocol desc)
     ((let loop ((desc desc))
	(cond ((rcd-parent desc)
	       => (lambda (parent)
		    (lambda extra-field-values
		      (lambda protocol-args
			(lambda this-field-values
			  (apply ((rcd-protocol parent)
				  (apply (loop parent)
					 (append this-field-values
						 extra-field-values)))
				 protocol-args))))))
	      (else
	       (lambda extra-field-values
		 (lambda this-field-values
		   (let ((field-values (append this-field-values
					       extra-field-values)))
		     (if (= (length field-values) argc)
			 (%make-record rtd field-values)
			 (assertion-violation "record constructor"
					      "wrong number of arguments"
					      field-values)))))))))))
  
  (define (make-simple-conser desc rtd argc)
    ((rcd-protocol desc)
     (lambda field-values
       (if (= (length field-values) argc)
	   (%make-record rtd field-values)
	   (assertion-violation "record constructor"
				"wrong number of arguments"
				field-values)))))
  
  (define (default-protocol rtd)
    (let ((parent (record-type-parent rtd)))
      (if parent
	  (let ((parent-field-count (rtd-total-field-count parent)))
	    (lambda (p)
	      (lambda field-values
		(receive (parent-field-values this-field-values)
		    (split-at field-values parent-field-count)
		  (let ((n (apply p parent-field-values)))
		    (apply n this-field-values))))))
	  (lambda (p)
	    (lambda field-values
	      (apply p field-values))))))

  (define (record-predicate rtd) (lambda (o) (is-a? o (slot-ref rtd 'class))))
  ;; TODO better error handling
  (define (searck-kth-slot class k)
    (let ((slots (class-direct-slots class)))
      (do ((i 0 (+ i 1)) (slots slots (cdr slots)))
	  ((= i k) (slot-definition-name (car slots))))))
  (define (record-accessor rtd k)
    (let ((name (searck-kth-slot (slot-ref rtd 'class) k)))
      (lambda (o) (slot-ref o name))))
      
  (define (record-mutator rtd k)
    (let ((name (searck-kth-slot (slot-ref rtd 'class) k)))
      (lambda (o v) (slot-set! o name v))))

  (define (record-rtd o)
    (unless (record? o)
      (assertion-violation 'record-rtd
			   (format "record required but got ~s" o) o))
    (record-type-rtd (class-of o)))


  (define (record-type-generative? rtd)
    (assertion-violation 'record-type-generative? "not yet"))
  ;; TODO check type
  (define (record-type-field-names rtd) (vector-map cadr (rtd-fields rtd)))
  (define (record-field-mutable? rtd k)
    (let ((fields (rtd-fields rtd)))
      (eq? (car (vector-ref fields k)) 'mutable)))

)