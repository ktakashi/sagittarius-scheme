;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
(library (core syntax helper)
    (export <sid>
	    make-sid sid-name sid-expression
	    sid?
	    sid-control

	    <ellipsis>
	    make-ellipsis ellipsis? ellipsis-sids set-ellipsis-sids!

	    syntax
	    id-memq variable? zero-or-more? expand
	    optimized-cons optimized-append
	    parse-pattern)
    (import null
	    (sagittarius)
	    (core base)
	    (core misc)
	    (core struct))
  (define-struct <sid>
    (make-sid name expression control)
    sid?
    #f
    (name sid-name)
    (expression sid-expression)
    (control sid-control)
    (output-expression sid-output-expression set-sid-output-expression!))

  (define-struct <ellipsis>
    (make-ellipsis sids)
    ellipsis?
    #f
    (sids ellipsis-sids set-ellipsis-sids!))

  ;; TODO it's just a stub implementation
  (define-syntax syntax
    (er-macro-transformer
     (lambda (form rename compare)
       (or (= (length form) 2)
	   (error 'syntax "expected exactly one datum" form))
       (let ((r (let loop ((form (cadr form)))
		  (cond ((null? form) '())
			((pair? form) (cons
				       (loop (car form))
				       (loop (cdr form))))
			
			((variable? form)
			 (make-syntax-object form (rename form)))
			(else
			 form)))))
	 `',r))))

  (define (variable? o)
    (or (identifier? o)
	(symbol? o)))

  (define (id-memq id lst)
    (if (identifier? id)
	(memq (id-name id) lst)
	(memq id lst)))

  (define (zero-or-more? pattern rename compare)
    (and (pair? pattern)
	 (pair? (cdr pattern))
	 (compare (cadr pattern) (rename '...))))

  ;; common expander for both syntax-rules and syntax-case
  ;; syntax-rules
  ;;  (syntax-rules () ((match) (template)))
  ;; syntax- case
  ;;  (syntax-case x () ((match) (template)))
  ;; 
  ;; param:
  ;;    form   	   -- original form of macro itself
  ;;    ref    	   -- reference for target form(for syntax-case's 'x')
  ;;    keyword    -- keywords for macro(literals)
  ;;    clauses    -- patterns and templates
  ;;    rename     -- rename procedure from er-macro-transformer
  ;;    compare    -- compare procedure from er-macro-transformer
  ;;    gen-match  -- pattern match generator
  ;;    gen-output -- template generator
  (define (expand form ref keywords clauses
		  rename compare gen-match gen-output)
    (unless (unique-id-list? keywords)
      (error 'syntax-rules "duplicate literals" keywords))
    (let ((r-form (rename 'form))
	  (r-rename (rename 'rename))
	  (r-compare (rename 'compare)))
      `(,(rename 'er-macro-transformer)
	(,(rename 'lambda)
	 (,r-form ,r-rename ,r-compare)
	 ,(let loop ((clauses clauses))
	    (if (pair? clauses)
		(let ((pattern (caar clauses)))
		  (let ((sids (parse-pattern rename compare keywords
					     pattern r-form)))
		    `(,(rename 'if)
		      ,(gen-match rename compare keywords
				  r-rename r-compare
				  pattern r-form)
		      ,(gen-output rename compare r-rename
				   sids (cadar clauses))
		      ,(loop (cdr clauses)))))
		`(,(rename 'begin)
		  (,(rename 'error)
		   (,(rename 'quote) 'syntax-rules)
		   "no expansion for" (,(rename 'unwrap-syntax) ,r-form)))))))))

  (define (parse-pattern rename compare keywords pattern expression)
    (let loop ((pattern pattern)
	       (expression expression)
	       (sids '())
	       (control #f))
      (cond ((variable? pattern)
	     (if (id-memq pattern keywords)
		 sids
		 (cons (make-sid pattern expression control) sids)))
	    ((and (zero-or-more? pattern rename compare)
		  (null? (cddr pattern)))
	     (let ((variable (gensym "control")))
	       (loop (car pattern)
		     variable
		     sids
		     (make-sid variable expression control))))
	    ((pair? pattern)
	     (loop (car pattern)
		   `(,(rename 'car) ,expression)
		   (loop (cdr pattern)
			 `(,(rename 'cdr) ,expression)
			 sids
			 control)
		   control))
	    ((vector? pattern)
	     (loop (vector->list pattern)
		   expression
		   sids
		   control))
	    (else sids))))

  (define (optimized-cons rename compare a d)
    (cond ((and (pair? d)
		(compare (car d) (rename 'quote))
		(pair? (cdr d))
		(null? (cadr d))
		(null? (cddr d)))
	   `(,(rename 'list) ,a))
	  ((and (pair? d)
		(compare (car d) (rename 'list))
		(list? (cdr d)))
	   `(,(car d) ,a ,@(cdr d)))
	  (else
	   `(,(rename 'cons) ,a ,d))))

  (define (optimized-append rename compare x y)
    (if (and (pair? y)
	     (compare (car y) (rename 'quote))
	     (pair? (cdr y))
	     (null? (cadr y))
	     (null? (cddr y)))
	x
	`(,(rename 'append) ,x ,y)))

)