;; -*- mode:scheme; coding: utf-8; -*-

;; helper library to be able to use define-generic and define-method
;; in the (clos user) library.
(library (clos helper)
    (export %make-setter-name %check-setter-name)
    (import (core) (core syntax) (sagittarius))
  ;;(define-syntax setter (syntax-rules ()))

  ;; never be symbol
  ;; NOTE: these will be unbound variable warning but for my laziness
  (define (%make-setter-name name)
    (string->symbol (format "setter of ~a" (syntax->datum name))))
  (define (%check-setter-name generic)
    (syntax-case generic ()
      ((?setter name)
       ;; not sure why the keyword literal doesn't work, could be a bug...
       (free-identifier=? #'setter #'?setter)
       #`(#,(%make-setter-name #'name) name))
      (n #'(n #f))))
)

(library (clos user)
    (export make
	    initialize
	    write-object object-equal? object-apply |setter of object-apply|

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

	    slot-ref
	    slot-set! slot-bound?
	    slot-ref-using-accessor slot-set-using-accessor!
	    slot-unbound slot-missing
	    initialize-direct-slots
	    is-a?

	    define-class
	    define-method
	    define-generic

	    eql
	    eqv? ;; for prefix or rename import

	    ;; class redefnition
	    redefine-class!
	    update-direct-subclass!
	    )
    (import (core)
	    (core base)
	    (core syntax)
	    (core exceptions)
	    (sagittarius)
	    (sagittarius vm)
	    (clos core)
	    (clos helper)
	    (only (sagittarius clos) %ensure-generic-function
		  %start-class-redefinition!
		  %end-class-redefinition!
		  %remove-direct-subclass!
		  %replace-class-binding!
		  %add-direct-subclass!))

  ;; We provides CL like define-class
  ;; The syntax is (define-class ?name (?supers ...) ({slot-specifier}*))
  ;; no class-option. (maybe I will provide :metaclass in future)
  ;; slot-specifier must be like this:
  ;;  slot-specifier ::= slot-name | (slot-name [[slot-option]])
  ;;  slot-name      ::= symbol
  ;;  slot-option    ::= {:accessor   function-name}
  ;;                   | {:init-value   expr}
  ;;                   | {:init-keyword keyword}
  ;;                   | {:reader function-name}
  ;;                   | {:writer function-name}
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
	(define (check defs target)
	  (unless (or (not target)
		      (and (pair? (cdr target)) (identifier? (cadr target))))
	    (syntax-violation 'define-class
			      "malformed slot specifier"
			      (unwrap-syntax x)
			      (unwrap-syntax (car defs)))))
	(let loop ((defs (unwrap-syntax slot-defs))
		   (ra '()) (rr '()) (rw '()))
	  (syntax-case defs ()
	    (() (values ra rr rw))
	    (((name . acc) . rest)
	     (identifier? #'name)
	     (let ((sa (memq :accessor #'acc))
		   (sr (memq :reader #'acc))
		   (sw (memq :writer #'acc)))
	       (check defs sa) (check defs sr) (check defs sw)
	       ;; I'm not sure how we should treat if everything is defined.
	       (loop (cdr defs)
		     (if sa
			 (acons (syntax->datum #'name)
				(syntax->datum (cadr sa)) ra)
			 ra)
		     (if sr
			 (acons (syntax->datum #'name)
				(syntax->datum (cadr sr)) rr)
			 rr)
		     (if sw
			 (acons (syntax->datum #'name)
				(syntax->datum (cadr sw)) rw)
			 rw))))
	    (((name) . rest) (identifier? #'name) (loop (cdr defs) ra rr rw))
	    ((name . rest) (identifier? #'name) (loop (cdr defs) ra rr rw))
	    (_ (syntax-violation 'define-class
				 "malformed slot specifier"
				 (unwrap-syntax x)
				 (unwrap-syntax (car defs)))))))
      (define (build name supers slot-defs options)
	;; we creates generic accessor and the rest will be
	;; for generic make
	(let-values (((accessors readers writers) (collect-accessor slot-defs))
		     ((metaclass) (or (get-keyword :metaclass options #f)
				    #`(%get-default-metaclass 
				       (list #,@supers)))))
	  (define (process-slot-definition sdef)
	    (if (pair? sdef)
		(let loop ((opts (cdr sdef)) (r '()))
		  (cond ((null? opts) #`(list '#,(car sdef) #,@(reverse! r)))
			((not (and (pair? opts) (pair? (cdr opts))))
			 (syntax-violation 'define-class
					   "bad slot specification"
					   (syntax->datum sdef)))
			(else
			 (case (car opts)
			   ((:init-form)
			    (loop (cddr opts)
				  #`((lambda () #,(cadr opts))
				     :init-thunk #,@r)))
			   ((:accessor :reader :writer)
			    (loop (cddr opts)
				  #`('#,(cadr opts) #,(car opts) #,@r)))
			   (else 
			    (loop (cddr opts)
				  (cons* (cadr opts) (car opts) r)))))))
		;; i think it will never reaches below though...
		#`'(#,sdef)))
	  ;; TODO check if given name is already exists as generic
	  #`(begin
	      (define #,name
		(let ((class 
		       (make #,metaclass
			 :definition-name (quote #,name)
			 :direct-supers   (list #,@supers)
			 :direct-slots    (list #,@(map process-slot-definition
							slot-defs))
			 :defined-library (current-library)
			 #,@options))
		      (c (find-binding (current-library) '#,name #f)))
		  ;; what shall we do with the drunken sailer...
		  ;; if the metaclass is not the same should we change the
		  ;; metaclass of the class?
		  ;; NB: PCL does
		  (and-let* (( c )
			     (cl (gloc-ref c))
			     ( (is-a? cl <class>) )) 
		    ;; (unless (eq? (class-of cl) #,metaclass) 
		    ;;   (change-class cl class))
		    (%redefine-class! cl class))
		  class))
	      #,@(if (null? accessors)
		     #`((undefined))
		     ;; build generic
		     (map (lambda (slot)
			    (let ((slot-name (car slot))
				  (accessor  (cdr slot))
				  (tmp  (gensym)))
			      #`(begin
				  ;; getter
				  (define-method #,accessor ((#,tmp #,name))
				    (slot-ref #,tmp (quote #,slot-name)))
				  ;; setter
				  (define-method #,accessor ((#,tmp #,name) obj)
				    (slot-set! #,tmp (quote #,slot-name)
					       obj))
				  ;; in case
				  (define-method (setter #,accessor)
				    ((#,tmp #,name) obj)
				    (slot-set! #,tmp (quote #,slot-name)
					       obj)))))
			  accessors))
	      #,@(if (null? readers)
		     #`((undefined))
		     (map (lambda (slot)
			    (let ((slot-name (car slot))
				  (reader (cdr slot))
				  (tmp (gensym)))
			      #`(define (#,reader #,tmp)
				  (slot-ref #,tmp (quote #,slot-name)))))
			  readers))
	      #,@(if (null? writers)
		     #`((undefined))
		     (map (lambda (slot)
			    (let ((slot-name (car slot))
				  (writer (cdr slot))
				  (tmp (gensym)))
			      #`(define (#,writer #,tmp obj)
				  (slot-set! #,tmp (quote #,slot-name) obj))))
			  writers)))))
      (syntax-case x ()
	((_ ?name () ?slot-defs . ?options)
	 #'(define-class ?name (<object>) ?slot-defs . ?options))
	((_ ?name (?supers ...) ?slot-defs . ?options)
	 (or (null? #'?slot-defs) (pair? #'?slot-defs))
	 (build #'?name #'(?supers ...) #'?slot-defs #'?options))
	(_ (syntax-violation
	    'define-class
	    "malformed define-class" (unwrap-syntax x))))))

  (define-syntax define-method
    (lambda (x)
      (define (analyse args)
	(let loop ((ss args) (rs '()))
	  (cond ((null? ss)          (values (reverse! rs) '() #f))
		((not (pair? ss))    (values (reverse! rs) ss #f))
		((keyword? (car ss)) (values (reverse! rs) (gensym) ss))
		(else (loop (cdr ss) (cons (car ss) rs))))))
      (define (build k qualifier generic qargs rest opts body)
	;; ugly kludge
	#;
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
		  ((and (identifier? body)
			(free-identifier=? body #'next-method?))
		   'next-method?)
		  (else body))))
	(define (parse-specializer s)
	  (syntax-case s (eqv?)
	    ((_ class) (identifier? #'class) #'class)
	    ((_ (eqv? v)) #'(eql v))
	    ((_ v) #'v)
	    (_ #'<top>)))
	(define (->s s) (datum->syntax k s))
	(define (smap1 p arg) (->s (map p arg)))
	(with-syntax (((specializers ...) (smap1 parse-specializer qargs))
		      ((reqargs ...) (smap1 (lambda (s) 
					      (if (pair? s) (car s) s)) qargs))
		      (rest      (->s rest))
		      (cm        (->s 'call-next-method))
		      (nm        (->s 'next-method?))
		      ((body ...) (->s body))
		      (?opts     (->s opts))
		      (gf        (->s (gensym))))
	  (with-syntax (((true-name getter-name) (%check-setter-name generic))
			(real-body (if opts
				       #'(lambda (cm reqargs ... . rest)
					   (define (nm)
					     (slot-ref cm 'next-method?))
					   (apply (lambda ?opts body ...) rest))
				       #'(lambda (cm reqargs ... . rest)
					   (define (nm)
					     (slot-ref cm 'next-method?))
					   body ...))))
	    #`(begin
		#,@(let* ((id #'true-name)
			  (lib (id-library id)))
		     (cond ((find-binding lib (syntax->datum id) #f) '())
			   (else
			    (%ensure-generic-function (syntax->datum id) lib)
			    #`((define-generic #,id)))))
		(let ((gf
		       ;; for cache perspective, we can't use library
		       ;; object directly...
		       (or (and-let* ((lib-name '#,(library-name 
						    (id-library #'true-name)))
				      (g (find-binding lib-name 'true-name #f))
				      (gf (gloc-ref g))
				      ( (is-a? gf <generic>) ))
			     gf)
			   (%ensure-generic-function 'true-name 
						     (current-library)))))
		  (add-method gf
			      (make <method>
				:specializers  (list specializers ...)
				:qualifier     #,qualifier
				:generic       true-name
				:lambda-list   '(reqargs ... . rest)
				:procedure     real-body))
		  #,@(if #'getter-name
			 #'((unless (has-setter? getter-name)
			      (set! (setter getter-name) gf)))
			 #'())
		  gf)))))
      (syntax-case x ()
	((k ?qualifier ?generic ?args . ?body)
	 (keyword? #'?qualifier)
	 (let-values (((qargs rest opt) (analyse #'?args)))
	   (build #'k #'?qualifier #'?generic qargs rest opt #'?body)))
	((_ ?generic ?qualifier ?args . ?body)
	 (keyword? #'?qualifier)
	 #'(define-method ?qualifier ?generic ?args . ?body))
	((_ ?generic ?args . ?body)
	 #'(define-method :primary ?generic ?args . ?body)))))

  (define-syntax define-generic
    (lambda (x)
      (define (generate-true-name k name)
	(datum->syntax k (%make-setter-name name)))
      (syntax-case x (setter)
	((k (setter name) . options)
	 (let ((class (get-keyword :class (syntax->datum #'options)
				   #'<generic>)))
	   (with-syntax ((true-name (generate-true-name #'k #'name))
			 (class-name (if (identifier? class)
					 class
					 (datum->syntax #'k class))))
	     ;; to avoid duplicated definition...
	     (%ensure-generic-function (syntax->datum #'true-name)
				       (current-library))
	     #'(begin
		 (define true-name (make class-name 
				     :definition-name 'true-name))
		 (set! (setter name) true-name)))))
	((k name . options)
	 (let ((class (get-keyword :class (syntax->datum #'options)
				   #'<generic>)))
	   ;; to avoid duplicated definition...
	   ;; FIXME this smells bugs
	   (%ensure-generic-function (syntax->datum #'name) (current-library))
	   (with-syntax ((class-name (if (identifier? class)
					 class
					 (datum->syntax #'k class))))
	     #'(define name (make class-name :definition-name 'name))))))))

  ;; class re-definition
  ;; basically the same as PCL's reinitialize-instance (I think..)
  ;; this procedure just locks the world
  (define (%redefine-class! old new)
    (define (warn e)
      (raise-continuable 
       (condition
	(make-warning)
	(make-who-condition 'redefine-class!)
	(make-message-condition 
	 (if (message-condition? e)
	     (condition-message e)
	     "failed to redefine a class"))
	(make-irritants-condition (list old new)))))
    ;; world lock
    (%start-class-redefinition! old)
    (guard (e (else (%end-class-redefinition! old #f)
		    (warn e)))
      (redefine-class! old new)
      (%end-class-redefinition! old new)))

  ;; reinitialize the class
  ;; remove old from new class's direct super's subclass
  (define-generic redefine-class!)
  (define-method redefine-class! ((old <class>) (new <class>))
    (for-each (lambda (sup) (%remove-direct-subclass! sup old))
	      (class-direct-supers old))
    ;; now for PCL the direct subclass will be updated
    ;; after this however the new class is already created
    ;; so that we need to do it manually here...
    (for-each (lambda (sub) (update-direct-subclass! sub old new))
	      (class-direct-subclasses old))
    )

  (define-generic update-direct-subclass!)
  (define-method update-direct-subclass! ((sub <class>)
					  (old <class>)
					  (new <class>))
    (define (new-supers supers)
      (map (lambda (s) (if (eq? s old) new s)) supers))
    (define (fixup-options supers initargs)
      (let ((old (get-keyword :direct-supers initargs)))
	(fold-right (lambda (e knil)
		      (cons (if (eq? e old) supers e) knil)) '() initargs)))

    ;; I believe at this point sub is not redefined so we can use
    ;; class-of to retrieve metaclass
    (let* ((metaclass (class-of sub))
	   (supers (new-supers (class-direct-supers sub)))
	   (new-sub (apply make metaclass
			   (fixup-options supers (slot-ref sub 'initargs)))))
      (%redefine-class! sub new-sub)
      ;; keep the direct subclass of the sub's supers
      (for-each (lambda (sup)
		  (%add-direct-subclass! sup sub))
		(class-direct-supers sub))
      ;; replace binding...
      (%replace-class-binding! sub new-sub)
      ))
)
