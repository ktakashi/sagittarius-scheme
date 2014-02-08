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
  #;
  (define-class <record-type-meta> (<class>)
    ((rtd :init-keyword :rtd)
     (rcd :init-keyword :rcd)))
  
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
  
  (define (make-record-constructor-descriptor rtd parent protocol)
    (let ((rcd (make <record-constructor-descriptor>
		 :rtd rtd :parent parent
		 :protocol (or protocol (default-protocol rtd)))))
      (slot-set! (slot-ref rtd 'class) 'rcd rcd)
      rcd))
  
  (define (make-record-type name rtd rcd) (slot-ref rtd 'class))
  
  (define (rtd-total-field-count rtd)
    (length (class-slots (slot-ref rtd 'class))))
  
  (define (record-type-rtd type) (slot-ref type 'rtd))
  (define (record-type-rcd type) (slot-ref type 'rcd))
  
  (define (record-constructor rcd)
    (let ((parent (rcd-parent rcd))
	  (rtd (rcd-rtd rcd)))
      (if parent
	  (let ((class (slot-ref rtd 'class)))
	    (make-nested-conser rcd rtd (length (class-slots class))))
	  (make-simple-conser rcd rtd (vector-length (rtd-fields rtd))))))
  
  (define (%make-record rtd field-values)
    (let* ((class (slot-ref rtd 'class))
	   ;; TODO create (kw v) list to make mutable/immutable thing
	   (tuple (make class)))
      (for-each (lambda (slot value)
		  (slot-set-using-class! class tuple
					 (slot-definition-name slot) value))
		(class-slots class) field-values)
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