;; -*- mode: scheme; coding: utf-8; -*-
;; cgen cise.
#!compatible
(library (sagittarius cgen literal)
    (export <cgen-literal> cgen-c-name cgen-cexpr cgen-make-literal
	    cgen-cpred
	    cgen-literal-static?
	    define-cgen-literal cgen-literal)
    (import (except (rnrs) define)
	    (clos user)
	    (clos core)
	    (core base) ;; for print
	    (sagittarius)
	    (sagittarius vm) ;; for identifier accessor
	    (rename (sagittarius control) (define-with-key define))
	    (sagittarius cgen unit)
	    (shorten)
	    (util hashtables)
	    (srfi :2 and-let*)
	    (srfi :26 cut)
	    (srfi :42 eager-comprehensions))

  ;; utilities
  (define cgen-unique-name
    (let1 counter 0
      (lambda () (format "~5,'0d" (begin (set! counter (+ counter 1))
					 counter)))))

  (define-class <cgen-static-data-list> ()
    ((category :init-keyword :category)
     (c-type :init-keyword :c-type)
     (c-member-name)
     (count :init-value 0)
     (cpp-conditions)
     (init-thunks :init-value ())))

  (define-method initialize ((self <cgen-static-data-list>) initargs)
    (call-next-method)
    (slot-set! self 'c-member-name  (gensym "d"))
    (slot-set! self 'cpp-conditions (cgen-cpp-conditions)))

  (define (static-data-c-struct-name category)
    (case category
      ((constant) "sg__sc")
      ((runtime)  "sg__rc")
      (else (error "[cgen internal] invalid category" category))))

  (define (cgen-allocate-static-datum :optional (category 'runtime)
				                (c-type 'SgObject)
						(init-thunk #f))
    (define (ensure-static-data-list category c-type)
      (and-let* ((unit (cgen-current-unit)))
	(let* ((cppc (cgen-cpp-conditions))
	       (dl (find (^(dl) (and (eq? (slot-ref dl'c-type) c-type)
				     (eq? (slot-ref dl'category) category)
				     (equal? (slot-ref dl'cpp-conditions)
					     cppc)))
			 (slot-ref unit'static-data-list))))
	  (or dl
	      (let1 new (make <cgen-static-data-list>
			  :category category :c-type c-type)
		(slot-set! unit 'static-data-list
			   (append (slot-ref unit 'static-data-list)
				   (list new)))
		new)))))
    (let ((dl (ensure-static-data-list category c-type))
	  (value-type? (not init-thunk))
	  (ithunk (or init-thunk (if (eq? c-type 'SgObject)
				     "SG_UNBOUND" "NULL"))))
      (let1 count (slot-ref dl'count)
	(slot-set! dl 'init-thunks
		   (append (slot-ref dl 'init-thunks) (list ithunk)))
	(slot-set! dl 'count (+ (slot-ref dl 'count) 1))
	(if value-type?
	    (format "~a.~a[~a]"
		    (static-data-c-struct-name category)
		    (slot-ref dl 'c-member-name)
		    count)
	    (format "SG_OBJ(&~a.~a[~a]"
		    (static-data-c-struct-name category)
		    (slot-ref dl 'c-member-name)
		    count)))))

  (define-method cgen-emit-static-data ((unit <cgen-unit>))
    (define (emit-one-category category dls)
      (let1 dls (filter (^(dl) (eq? (slot-ref dl'category) category)) dls)
	(unless (null? dls)
	  (emit-struct-def category dls)
	  (print "{")
	  (dolist (dl dls) (emit-initializers dl))
	  (print "};"))))
    (define (emit-struct-def category dls)
      (let1 name (static-data-c-struct-name category)
	(format #t "static ~astruct ~aRec {~%"
		(if (eq? category 'constant) "const " "")
		name)
	(dolist (dl dls)
	  (for-each (cut print "#if " <>) (slot-ref dl 'cpp-conditions))
	  (format #t "  ~a ~a[~a];~%" (slot-ref dl'c-type)
		  (slot-ref dl'c-member-name) (slot-ref dl'count))
	  (for-each (cut print "#endif /* " <> "*/")
		    (slot-ref dl 'cpp-conditions)))
	(format #t "} ~a = " name)))
    (define (emit-initializers dl)
      (for-each (cut print "#if " <>) (slot-ref dl 'cpp-conditions))
      (print "  {  /* " (slot-ref dl'c-type)
	     " " (slot-ref dl'c-member-name) " */")
      (dolist (thunk (reverse (slot-ref dl'init-thunks)))
	(if (string? thunk)
	    (format #t "    ~a,~%" thunk)
	    (print "    " (thunk) (print ","))))
      (print "  },")
      (for-each (cut print "#endif /* " <> " */") (slot-ref dl'cpp-conditions)))
    (let ((dls (slot-ref unit 'static-data-list)))
      (when dls
	(emit-one-category 'constant dls)
	(emit-one-category 'runtime dls))))
	

  (define-class <cgen-literal> (<cgen-node>)
    ((scope  :init-keyword :scope :init-value 'local)
     (c-name :init-keyword :c-name)
     (value  :init-keyword :value :init-value #f)))

  (define-method initialize ((node <cgen-literal>) initargs)
    (slot-set! node 'c-name (format "sg_~a" (cgen-unique-name)))
    ;; if :c-name keyword is there, c-name must be overwritten
    (call-next-method)
    (when (slot-ref node 'c-name)
      (and-let* ((unit (cgen-current-unit)))
	(register-literal-value unit node)
	(cgen-unit-toplevel-nodes unit (append (cgen-unit-toplevel-nodes unit)
					       (list node))))))

  (define-method cgen-c-name ((node <cgen-literal>))
    (and-let* ((n (slot-ref node 'c-name)))
      (if (string? n) n (n))))
  (define-method cgen-cexpr ((node <cgen-literal>))
    (cgen-c-name node))
  (define-generic cgen-cpred)

  (define-method cgen-make-literal (value) 
    (or ;;(infer-literal-handler value)
	(error 'cgen-make-literal
	       "cannot make a static C data for Scheme value" value)))

  (define-method cgen-literal-static? (self) #t)

  (define-method cgen-emit-xtrn ((node <cgen-literal>))
    (when (and (slot-ref node 'extern?) (cgen-c-name node))
      (print "extern SgObject" (cgen-c-name node) ";")))

  (define-syntax define-cgen-literal
    (syntax-rules (make cexpr extern decl pred body init static)
      ((_ "methods" class scheme-class) #f)
      ((_ "methods" class scheme-class (make (arg) . ?body) . rest)
       (begin
	 (define-method cgen-make-literal ((arg scheme-class)) . ?body)
	 (define-cgen-literal "methods" class scheme-class . rest)))
      ((_ "methods" class scheme-class (cexpr (?self) . ?body) . rest)
       (begin
	 (define-method cgen-cexpr ((?self class)) . ?body)
	 (define-cgen-literal "methods" class scheme-class . rest)))
      ((_ "methods" class scheme-class (extern (?self) . ?body) . rest)
       (begin
	 (define-method cgen-emit-xtrn ((?self class)) . ?body)
	 (define-cgen-literal "methods" class scheme-class . rest)))
      ((_ "methods" class scheme-class (decl (?self) . ?body) . rest)
       (begin
	 (define-method cgen-emit-decl ((?self class)) . ?body)
	 (define-cgen-literal "methods" class scheme-class . rest)))
      ((_ "methods" class scheme-class (pred (?self) . ?body) . rest)
       (begin
	 (define-method cgen-cpred ((?self class)) . ?body)
	 (define-cgen-literal "methods" class scheme-class . rest)))
      ((_ "methods" class scheme-class (body (?self) . ?body) . rest)
       (begin
	 (define-method cgen-emit-body ((?self class)) . ?body)
	 (define-cgen-literal "methods" class scheme-class . rest)))
      ((_ "methods" class scheme-class (init (?self) . ?body) . rest)
       (begin
	 (define-method cgen-emit-init ((?self class)) . ?body)
	 (define-cgen-literal "methods" class scheme-class . rest)))
      ((_ "methods" class scheme-class (static (?self) . ?body) . rest)
       (begin
	 (define-method cgen-literal-static? ((?self class)) . ?body)
	 (define-cgen-literal "methods" class scheme-class . rest)))
      ((_ "methods" class scheme-class _ . rest)
       (syntax-violation 'define-cgen-literal
			 "Unrecognized method clause in define-cgen-literal"))
      ;; entry
      ((_ class scheme-class slots . methods)
       (begin
	 (define-class class (<cgen-literal>) slots)
	 (define-cgen-literal "methods" class scheme-class . methods)))
      ((_ . _)
       (syntax-violation 'define-cgen-literal
			 "malformed define-cgen-literal"))))

  (define (cgen-literal value)
    (or (and-let* ((unit (cgen-current-unit)))
	  (lookup-literal-value unit value))
	(cgen-make-literal value)))

  (define (get-literal-initializer value)
    (if (cgen-literal-static? value)
	(cgen-cexpr value)
	"SG_UNDEF"))

  (define (literal-value-hash literal)
    (define mask #x0fffffff)
    (define (rec val)
      (cond
       ((pair? val) (bitwise-and (+ (rec (car val)) (rec (cdr val))) mask))
       ((vector? val) (fold (^(v r) (bitwise-and (+ (rec v) r) mask) 0 val)))
       ((string? val) (bitwise-and (string-hash val) mask))
       ((identifier? val)
	(bitwise-and (+ (rec (id-name val)) (rec (id-library val))) mask))
       (else (eqv-hash val))))
    (rec literal))

  (define (literal-value=? x y)
    (define (rec x y)
      (cond 
       ((pair? x) (and (pair? y) (rec (car x) (car y)) (rec (cdr x) (cdr y))))
       ((vector? x)
	(and (vector? y)
	     (let1 len (vector-length x)
	       (and (= len (vector-length y))
		    (every?-ec (: i len)
			       (rec (vector-ref x i) (vector-ref y i)))))))
       ((string? x) (and (string? y) (string=? x y)))
       ((identifier? x)
	(and (identifier? y)
	     (eq? (id-name x) (id-name y))
	     (eq? (id-library x) (id-library y))))
       (else 
	(and (eq? (class-of x) (class-of y)) (eqv? x y)))))
    (rec x y))

  (define (ensure-literal-hash unit)
    (or (slot-ref unit 'literals)
	(let1 hash (make-hashtable literal-value-hash literal-value=?)
	  (slot-set! unit 'literals hash)
	  hash)))

  (define (register-literal-value unit obj)
    (let ((lh   (ensure-literal-hash unit))
	  (cppc (slot-ref obj 'cpp-conditions)))
      (hashtable-set! lh (cons cppc (slot-ref obj'value)) obj)))

  (define (lookup-literal-value unit val)
    (let ((lh   (ensure-literal-hash unit))
	  (cppc (cgen-cpp-conditions)))
      (hashtable-ref lh (cons cppc val) #f)))

  ;; primitive values
  (define-cgen-literal <cgen-scheme-boolean> <boolean>
    ()
    (make (value)
      (if value *cgen-scheme-true* *cgen-scheme-false*))
    (pred (self) (if (slot-ref self'value) "SG_TRUEP" "SG_FALSEP"))
    (cexpr (self)
      (if (slot-ref self 'value) "SG_TRUE" "SG_FALSE")))

  (define *cgen-scheme-true*
    (make <cgen-scheme-boolean> :c-name #f :value #t))
  (define *cgen-scheme-false*
    (make <cgen-scheme-boolean> :c-name #f :value #f))

  ;; character.
  (define-cgen-literal <cgen-scheme-char> <char>
    ()
    (make (value)
      (make <cgen-scheme-char> :c-name #f :value value))
    (pred (self) "SG_CHARP")
    (cexpr (self)
      (format "SG_MAKE_CHAR(~a)" (char->integer (slot-ref self'value)))))

  ;; ()
  (define-cgen-literal <cgen-scheme-null> <null>
    ()
    (make (value)
      (make <cgen-scheme-null> :c-name #f :value '()))
    (cexpr (self) "SG_NIL"))

  ;; fixnum
  (define-cgen-literal <cgen-scheme-integer> <integer>
    ((string-rep :init-keyword :string-rep :init-value #f))
    (make (value)
      (cond ((fixnum? value)
	     (make <cgen-scheme-integer> :c-name #f :value value))
	    ((< (- expt 2 31) value (- (expt 2 32)))
	     (make <cgen-scheme-integer>
	       :c-name (cgen-allocate-static-datum)
	       :value value))
	    (else 
	     (make <cgen-scheme-integer>
	       :value value
	       :c-name (cgen-allocate-static-datum)
	       :string-rep (cgen-literal (number->string value 16))))))
    (pred (self)
      (let ((value (slot-ref self'value)))
	(cond ((fixnum? value) "SG_INTP")
	      (else "Sg_IntegerP"))))
    (cexpr (self)
      (or (cgen-c-name self)
	  (if (positive? (slot-ref self'value))
	      (format "SG_MAKE_INT(~aU)" (slot-ref self'value))
	      (format "SG_MAKE_INT(~a)" (slot-ref self'value)))))
    (init (self)
      (let ((val (slot-ref self'value))
	    (cname (cgen-c-name self)))
	(cond ((< (- expt 2 31) val 0)
	       (print "  " cname " = Sg_MakeInteger("val");"))
	      ((<= 0 val (- expt 2 32))
	       (print "  " cname " = Sg_MakeIntegerU("val");"))
	      (else
	       (print "  " cname " = Sg_StringToNumber(SG_MAKE_STRING("
		      (cgen-cexpr (slot-ref self'string-rep)) "), 16 TRUE);")))
	))
    (static (self)
      (if (cgen-c-name self) #f #t)))

  ;; string
  (define-cgen-literal <cgen-scheme-string> <string>
    ()
    (make (value)
      (make <cgen-scheme-string>
	:c-name (cgen-allocate-static-datum)
	:value value))
    (init (self)
      (format #t "  ~a = SG_MAKE_STRING(\"~a\");~%"
	      (cgen-c-name self)
	      (slot-ref self 'value)))
    (static (self) #f))

  ;; symbol
  (define-cgen-literal <cgen-scheme-symbol> <symbol>
    ((symbol-name :init-keyword :symbol-name))
    (make (value)
      (make <cgen-scheme-symbol>
	:c-name (cgen-allocate-static-datum)
	:value value
	:symbol-name (cgen-literal (symbol->string value))))
    (pred (self) "SG_SYMBOLP")
    (init (self)
     (format #t "  ~a = Sg_Intern(~a); /* ~a */~%"
	     (cgen-c-name self)
	     (cgen-c-name (slot-ref self 'symbol-name))
	     (cgen-safe-comment (slot-ref self 'value))))
    (static (self) #f))

  (define-cgen-literal <cgen-scheme-keyword> <keyword>
    ((keyword-name :init-keyword :keyword-name))
    (make (value)
      (make <cgen-scheme-keyword> :value value
	    :c-name (cgen-allocate-static-datum)
	    :keyword-name (cgen-literal (keyword->string value))))
    (pred (self) "SG_KEYWORDP")
    (init (self)
      (print "  " (cgen-c-name self)
	     " = Sg_MakeKeyword(SG_STRING("
	     (cgen-cexpr (slot-ref self 'keyword-name))
	     ")); /* " (cgen-safe-comment (slot-ref self 'value)) " */"))
    (static (self) #f))
)
