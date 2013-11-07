;; part of Sagittarius Scheme
(library (core inline)
    (export define-inliner
	    ;; not export this for now.
	    ;;define-raw-inliner
	    )
    (import (core)
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
	((k name library proc)
	 (with-syntax ((debug-name (datum->syntax #'k
				    (string->symbol (format "inliner/~a"
							    (datum name))))))
	   #'(let ((debug-name (lambda (form p1env)
				 (define (const-value expr)
				   (let ((iform (pass1 expr p1env)))
				     ;; $CONST = #($CONST value)
				     (if (eqv? (vector-ref iform 0) $CONST)
					 (vector-ref iform 1)
					 (undefined))))
				 (let ((form2 (proc form const-value)))
				   (if (undefined? form2)
				       ;; return undefined so that compiler
				       ;; just compiles to $call
				       form2
				       ;; must return iform
				       (pass1 form2 p1env))))))
	       (procedure-inliner-set! (find-procedure 'name 'library)
				       debug-name)))))))

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

)