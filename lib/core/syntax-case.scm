;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
(library (core syntax-case)
    (export syntax-case syntax)
    (import null
	    (sagittarius)
	    (core base)
	    (core syntax helper))

  (define-syntax syntax-case
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((ref      (cadr form))
	     (keywords (caddr form))
	     (clauses (cdddr form)))
	 (expand form ref keywords clauses
		 rename compare generate-match generate-output)))))

  (define (generate-match rename compare keywords r-rename r-compare
			  pattern expression)
    (letrec ((loop (lambda (pattern expression)
		     (cond ((variable? pattern)
			    (if (id-memq pattern keywords)
				(let ((temp (rename 'temp)))
				  `(,(rename 'let) ((,temp ,expression))
				    (,(rename 'if) (,(rename 'or)
						    (,(rename 'symbol?) ,temp)
						    (,(rename 'identifier?) ,temp))
				     (,r-compare ,temp
						 (,r-rename 
						  (,(rename 'quote) ,pattern)))
				     #f)))
				`#t))
			   ((and (zero-or-more? pattern rename compare)
				 (null? (cddr expression)))
			    #;`(,(rename 'if) (,(rename 'null?) ,expression)
			    #f
			    ,(do-list (car pattern) expression))
			    (do-list (car pattern) expression))
			   ((pair? pattern)
			    (let ((generate-pair
				   (lambda (expression)
				     (conjunction 
				      `(,(rename 'pair?) ,expression)
				      (conjunction 
				       (loop (car pattern)
					     `(,(rename 'car) ,expression))
				       (loop (cdr pattern)
					     `(,(rename 'cdr) ,expression)))))))
			      (if (variable? expression)
				  (generate-pair expression)
				  (let ((temp (rename 'temp)))
				    `(,(rename 'let) ((,temp ,expression))
				      ,(generate-pair temp))))))
			   ((vector? pattern)
			    (loop (vector->list pattern) expression))
			   ((null? pattern)
			    `(,(rename 'null?) ,expression))
			   (else
			    `(,(rename 'equal?) ,expression
			      (,(rename 'quote) ,pattern))))))
	     (do-list (lambda (pattern expression)
			(let ((r-loop (rename 'loop))
			      (r-l (rename 'l)))
			  `(,(rename 'letrec) 
			    ((,r-loop
			      (,(rename 'lambda) (,r-l)
			       (,(rename 'if) (,(rename 'null?) ,r-l)
				#t
				,(conjunction
				  `(,(rename 'pair?) ,r-l)
				  (conjunction
				   (loop pattern
					 `(,(rename 'car) ,r-l))
				   `(,r-loop (,(rename 'cdr) ,r-l))))))))
			    (,r-loop ,expression)))))
	     (conjunction (lambda (predicate consequent)
			    (cond ((eq? predicate #t) consequent)
				  ((eq? consequent #t) predicate)
				  (else `(,(rename 'if) ,predicate
					  ,consequent #f))))))
      (loop pattern expression)))

  (define (generate-output rename compare r-rename sids template)
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

  (define (add-control! sid ellipses)
    (let loop ((sid sid) (ellipses ellipses))
      (let ((control (sid-control sid)))
	(when control
	  (if (pair? ellipses)
	      (let ((sids (ellipsis-sids (car ellipses))))
		(cond ((not (memq control sids))
		       (set-ellipsis-sids! (car ellipses)
					   (cons control sids)))
		      ((not (eq? control (car sids)))
		       (error 'syntax-rules "illegal control/ellipsis combination" control sids))))
	      (error 'syntax-rules "missing ellipsis in expression" #f))
	  (loop control (cdr ellipses))))))

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
	  (error 'syntax-rules "missing ellipsis in expanstion."))))
  )