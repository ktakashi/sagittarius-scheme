;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
(library (core syntax-rules)
    (export syntax-rules 
	    er-macro-transformer
	    syntax-rules2)
    (import null
	    (sagittarius)
	    ;;(sagittarius vm)
	    (core base)
	    (core errors)
	    ;;(pp)
	    ;(core syntax pattern)
	    ;(core syntax template)
	    (core syntax helper))

  ;; from MIT scheme
  (define-syntax syntax-rules
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((keywords (cadr form))
	     (clauses (cddr form))
	     (r-form (rename 'form))
	     (r-rename (rename 'rename))
	     (r-compare (rename 'compare)))
	 `(,(rename 'er-macro-transformer)
	   (,(rename 'lambda)
	    (,r-form ,r-rename ,r-compare)
	    ,(expand form 'syntax-rules keywords clauses
		     rename compare generate-output)))))))

  (define (generate-output oform rename compare
			   r-form r-rename sids template)
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

  (define (generate-ellipsis rename ellipsis body)
    (let ((sids (ellipsis-sids ellipsis)))
      (if (pair? sids)
	  (let ((name (sid-name (car sids)))
		(expression (sid-expression (car sids))))
	    (cond ((and (null? (cdr sids))
			(eq? body name))
		   expression)
		  ((and (null? (cdr sids))
			(pair? body)
			(eq? (cadr body) name)
			(null? (cddr body)))
		   `(,(rename 'map) ,(car body) ,expression))
		  (else
		   `(,(rename 'map) (,(rename 'lambda)
				     ,(map sid-name sids)
				     ,body)
		     ,@(map sid-expression sids)))))
	  (assertion-violation 'syntax-rules "missing ellipsis in expansion." (list rename ellipsis body)))))

  )
