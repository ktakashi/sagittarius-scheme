;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
(library (core syntax-rules)
    (export syntax-rules)
    (import null
	    (sagittarius)
	    (core base)
	    (core syntax helper))

  ;; from MIT scheme
  (define-syntax syntax-rules
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((keywords (cadr form))
	     (clauses (cddr form)))
	 (expand form #f keywords clauses
		 rename compare generate-output)))))

  (define (generate-output ref rename compare 
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
  )
