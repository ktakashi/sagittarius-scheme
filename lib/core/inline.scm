;; part of Sagittarius Scheme
(library (core inline)
    (export define-inliner
	    define-raw-inliner)
    (import (core)
	    (core base)
	    (core syntax)
	    (sagittarius)
	    (sagittarius compiler)
	    (sagittarius compiler procedure)
	    (sagittarius vm))

  ;; trick to get pass1 from compiler
  (define pass1 (let ((lib (find-library '(sagittarius compiler) #f)))
		  (gloc-ref (find-binding lib 'pass1 #f))))

  (define-syntax define-raw-inliner
    (lambda (x)
      (syntax-case x ()
	((k name library (form pass1*) body ...)
	 (with-syntax ((debug-name (datum->syntax #'k
				    (string->symbol (format "inliner/~a"
							    (datum name))))))
	   #'(let ((debug-name (lambda (form p1env)
				 (define (pass1* expr) (pass1 expr p1env))
				 body ...)))
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
	   #'(define-raw-inliner name lib (form pass1)
	       (syntax-case form ()
		 (pattern fender (pass1 (syntax template))) ...
		 (_ (undefined)))))))))

)