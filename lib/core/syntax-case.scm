;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
(library (core syntax-case)
    (export syntax-case syntax datum->syntax)
    (import null
	    (sagittarius)
	    (sagittarius vm)
	    (core base)
	    (core syntax helper))

  (define-syntax syntax-case
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((ref      (cadr form))
	     (keywords (caddr form))
	     (clauses (cdddr form)))
	 (expand form ref keywords clauses
		 rename compare generate-output)))))

  (define (generate-output ref rename compare 
			   r-form r-rename sids template)
    (define (inner)
      (let loop ((template template)
		 (ellipses '()))
	(cond ((variable? template)
	       (let ((sid (let loop ((sids sids))
			    (and (pair? sids)
				 (if (eq? (sid-name (car sids)) template)
				     (car sids)
				     (loop (cdr sids)))))))
		 (if sid
		     (begin
		       (add-control! sid ellipses)
		       (sid-expression sid))
		     `(,r-rename (,(rename 'quote) ,template)))))
	      ((zero-or-more? template rename compare)
	       (optimized-append rename compare
				 (let ((ellipsis (make-ellipsis '())))
				   (generate-ellipsis rename ellipsis
						      (loop (car template)
							    (cons ellipsis
								  ellipses))))
				 (loop (cddr template) ellipses)))
	      ((pair? template)
	       (optimized-cons rename compare
			       (loop (car template) ellipses)
			       (loop (cdr template) ellipses)))
	      ((vector? template)
	       (loop (vector->list template) ellipses))
	      (else
	       `(,(rename 'quote) ,template)))))
    ;; we need to reconstruct form for syntax-case
    ;; (fuga 1 2 3) => (list 'fuga 1 2 3)
    (define (re-construct-form)
      (let ((r-let (rename 'let)) (r-loop (rename 'loop))
	    (r-v (rename 'v))     (r-r (rename 'r))
	    (r-i (rename 'i))     (r-t (rename 't))
	    (r-length (rename 'length)) (r-+ (rename '+))
	    (r-vector-set! (rename 'vector-set!))
	    (r-vector->list (rename 'vector->list))
	    (r-list->vector (rename 'list->vector))
	    (r-make-vector (rename 'make-vector)))
	`(,r-let ,r-loop ((,r-v (,r-list->vector ,r-form))
			  (,r-r (,r-make-vector (,r-+ (,r-length ,r-form) 1)))
			  (,r-i 0))
		(,r-vector-set! ,r-r 0 (,(rename 'quote) ,(rename 'list)))
		(,(rename 'if) (,(rename '=) ,r-i (,(rename 'vector-length) ,r-v))
		 (,r-vector->list ,r-r)
		 (,r-let ((,r-t (,(rename 'vector-ref) ,r-v ,r-i)))
		   (,(rename 'cond)
		    ((,(rename 'pair?) ,r-t)
		     (,r-vector-set! ,r-r (,r-+ ,r-i 1)
				     (,r-loop (,r-list->vector ,r-t)
					      (,r-make-vector (,r-+ (,r-length ,r-t) 1))
					      0))
		     (,r-loop ,r-v ,r-r (,r-+ ,r-i 1)))
		    ((,(rename 'or)
		      (,(rename 'identifier?) ,r-t)
		      (,(rename 'symbol?) ,r-t))
		     (,r-vector-set! ,r-r (,r-+ ,r-i 1)
				     (quasiquote
				      (quote
				       (unquote ,r-t))))
		     (,r-loop ,r-v ,r-r (,r-+ ,r-i 1)))
		    (,(rename 'else)
		     (,r-vector-set! ,r-r (,r-+ ,r-i 1) ,r-t)
		     (,r-loop ,r-v ,r-r (,r-+ ,r-i 1)))))))
	))

    (let ((r (inner))
	  (form (re-construct-form))
	  (r-list (rename 'list))
	  (r-quote (rename 'quote)))
      `(,r-list
	(,r-quote ,(rename 'let)) (,r-list (,r-list (,r-rename (,r-quote ,(rename ref)))
						    ,form))
	,r)))

  (define (sexp-map f s)
    (cond ((null? s) '())
	  ((pair? s) (cons (sexp-map f (car s))
			   (sexp-map f (cdr s))))
	  ((vector? s)
	   (apply vector (sexp-map f (vector->list s))))
	  (else (f s))))

  (define (datum->syntax tid datum)
    (or (and (user-defined-syntax? tid)
	     (identifier? (syntax-proc tid)))
	(identifier? tid)
	(error 'datum-syntax "identifier required,  but got" tid))
    (let ((id (if (user-defined-syntax? tid)
		  (syntax-proc tid)
		  tid)))
      (sexp-map (lambda (leaf)
		  (cond ((symbol? leaf)
			 (make-identifier leaf
					  (id-env id)
					  (id-library id)))
			(else leaf)))
	      datum)))
  )