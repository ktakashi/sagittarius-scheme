;; part of Sagittarius Scheme
(library (core inline)
    (export define-inliner ;; to add inliner into existing procedures
	    define-inline  ;; to define inliners *and* procedures.

	    ;; not export this for now.
	    ;;define-raw-inliner
	    )
    (import (core)
	    (core errors)
	    (core base)
	    (core syntax)
	    (sagittarius)
	    (sagittarius compiler)
	    (sagittarius compiler procedure)
	    (sagittarius compiler util)
	    (sagittarius vm))

  ;; trick to get pass1 from compiler
  (define pass1 (let ((lib (find-library '(sagittarius compiler) #f)))
		  (gloc-ref (find-binding lib 'pass1 #f))))

  (define-syntax define-raw-inliner
    (lambda (x)
      (syntax-case x ()
	((k name library inliner)
	 (with-syntax ((debug-name (datum->syntax #'k
				    (string->symbol (format "inliner/~a"
							    (datum name))))))
	   #'(define dummy
	       (let* ((proc (find-procedure 'name 
					    (if 'library 
						'library 
						(current-library))))
		      (orig (procedure-inliner proc))
		      (debug-name (lambda (form p1env)
				    (define (const-value expr)
				      (let ((iform (pass1 expr p1env)))
					;; $CONST = #($CONST value)
					(if (eqv? (vector-ref iform 0) $CONST)
					    (vector-ref iform 1)
					    (undefined))))
				    (let ((form2 (inliner form const-value)))
				      (if (undefined? form2)
					  (if orig
					      (orig form p1env)
					      ;; return undefined so that
					      ;; compiler just compiles to
					      ;; $call
					      form2)
					  ;; must return iform
					  (pass1 form2 p1env))))))
		 (when (integer? orig)
		   (error 'name "Can't overwrite insn inliner"))
		 (procedure-inliner-set! proc debug-name))))))))

  (define-syntax define-inliner
    (lambda (x)
      (define (parse patterns acc)
	(syntax-case patterns ()
	  (((p f t) rest ...)
	   (parse (cdr patterns) (cons (list #'p #'f #'t) acc)))
	  (((p t) rest ...)
	   (parse (cdr patterns) (cons (list #'p #'#t #'t) acc)))
	  (() (reverse! acc))))
      (syntax-case x ()
	((_ name lib pattern* ...)
	 (with-syntax ((((pattern fender template) ...) 
			(parse #'(pattern* ...) '())))
	   #'(define-raw-inliner name lib
	       (lambda (form const-value)
		 (syntax-case form ()
		   (pattern fender (syntax template)) ...
		   (_ (undefined))))))))))

  ;; for convenience
  ;; we define 2 things, one is a macro which is the real name
  ;; the other one is actual implementation. for debugging purpose
  ;; we use syntax-case to generate implementation name.
  (define-syntax define-inline
    (lambda (x)
      (define (actual-name name)
	(string->symbol (format "~~~a" (syntax->datum name))))
      (syntax-case x ()
	((me (name . formals) body ...)
	 (with-syntax ((%impl (datum->syntax #'me (actual-name #'name))))
	   #'(begin
	       (define-syntax name
		 (lambda (x)
		   (syntax-case x ()
		     ((_ args (... ...))
		      ;; compiler will inline this
		      #'((lambda formals body ...) args (... ...)))
		     (k (identifier? #'k) #'%impl))))
	       (define (%impl . formals) body ...)
	       ;; do we need this?
	       (define-raw-inliner %impl #f
		 (lambda (form const-value)
		   (syntax-case form ()
		     ((_ args (... ...))
		      #'((lambda formals body ...) args (... ...))))))))))))


  )
