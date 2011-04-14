(library (core syntax template)
    (export generate-output)
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax pattern)
	    (core syntax helper)
	    (core misc)
	    (sagittarius)
	    (sagittarius vm))
  ;; from Ypsilon
  (define parse-ellipsis-splicing
    (lambda (form rename compare)
      (let loop ((len 2) (tail (cdddr form)))
	(cond ((and (pair? tail) (compare (car tail) (rename '...)))
	       (loop (+ len 1) (cdr tail)))
	      (else
	       (values (list-head form len) tail len))))))

  (define generate-output
    (lambda (template sids rename compare expr in-syntax? macro-expander)
      (let ((_cdr (rename 'cdr))       (_car (rename 'car))
	    (_quote (rename 'quote))   (_lambda (rename 'lambda))
	    (_null? (rename 'null?))   (_pair? (rename 'pair?))
	    (_map (rename 'map))       (_apply (rename 'apply))	  
	    (_list->vector (rename 'list->vector))
	    (_rename (rename 'rename)) (_compare (rename 'compare)))

	(define generate-ellipsis
	  (lambda (ellipsis body)
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
			   `(,_map ,(car body) ,expression))
			  (else
			   `(,_map (,_lambda
				    ,(map sid-name sids)
				    ,body)
				   ,@(map sid-expression sids)))))
		  (assertion-violation "syntax template" "missing ellipsis in expansion." 
				       (list template (unwrap-syntax body)))))))
	(define expand-variable
	  (lambda (tmpl ellipses)
	    (let ((sid (let lp ((sids sids))
			 (and (pair? sids)
			      (if (compare (sid-name (car sids)) tmpl)
				  (car sids)
				  (lp (cdr sids)))))))
	      (if sid
		  (begin
		    (add-control! sid ellipses)
		    (sid-expression sid))
		  `(,_rename (,_quote ,tmpl))))))

	(define syntax-expression?
	  (lambda (expr)
	    (and (pair? expr)
		 (compare (car expr) (rename 'syntax)))))

	(define loop
	  (lambda (tmpl ellipses)
	    (cond ((variable? tmpl)
		   (expand-variable tmpl ellipses))
		  ((ellipsis-quote? tmpl rename compare)
		   `(,_quote ,(if (pair? (cdr tmpl))
				  (if (pair? (cddr tmpl)) (cddr tmpl) (cadr tmpl))
				  (cdr tmpl))))
		  ;; (p ... ...)
		  ((ellipsis-splicing-pair? tmpl rename compare)
		   (receive (body tail len) (parse-ellipsis-splicing tmpl rename compare)
		     (optimized-append (apply optimized-append
					      rename compare (let ((ellipsis (make-ellipsis '())))
							       (generate-ellipsis ellipsis
										  (loop body
											(cons ellipsis
											      ellipses)))))
				       (loop tail ellipses))))
		  
		  ;; (p ...)
		  ((ellipsis-pair? tmpl rename compare)
		   (optimized-append rename compare
				     (let ((ellipsis (make-ellipsis '())))
				       (generate-ellipsis ellipsis
							  (loop (car tmpl)
								(cons ellipsis
								      ellipses))))
				     (loop (cddr tmpl) ellipses)))
		  ((pair? tmpl)
		   (cond ((and macro-expander
			       (variable? (car tmpl))
			       ;; ex) (with-syntax ((name (datum->syntax #'k 'a))) #`(lambda (name) args ...))
			       ;;    -> (syntax (syntax-case (datum->syntax #'k 'a) () (name (begin args ...))))
			       ;; but input expression is still the same, expanded expression can be used
			       ;; as a template.			  
			       (macro-expander tmpl expr sids in-syntax?)))
			 ((and macro-expander ; use this as a flag. not so good
			       (syntax-expression? tmpl))
			  (generate-output (cadr tmpl) sids rename compare
					   `(,_cdr ,expr) #t macro-expander))
			 ((and macro-expander
			       (not in-syntax?))
			  ;; here we need to expand macro
			  (let lp2 ((tmpl tmpl))
			    (cond ((variable? tmpl) tmpl)
				  ((and (pair? tmpl)
					(variable? (car tmpl))
					(macro-expander tmpl expr sids in-syntax?))
				   => (lambda (expanded)
					(lp2 expanded)))
				  ((syntax-expression? tmpl)
				   (generate-output (cadr tmpl) sids rename compare
						    `(,_cdr ,expr) #t macro-expander))
				  ((pair? tmpl)
				   (cons (car tmpl) #;(lp2 (car tmpl))
					 (lp2 (cdr tmpl))))
				  (else tmpl))))
			 (else
			  (optimized-cons rename compare
					  (loop (car tmpl) ellipses)
					  (loop (cdr tmpl) ellipses)))))
		  ((vector? tmpl)
		   `(,_list->vector ,(loop (vector->list tmpl) ellipses)))
		  ((null? tmpl)
		   `(,_quote ()))
		  (else 
		   `(,_quote ,tmpl)))))
	(loop template '()))))

)
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End: