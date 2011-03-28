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
	    add-control! generate-ellipsis
	    parse-pattern)
    (import null
	    (sagittarius)
	    ;(sagittarius vm)
	    (core base)
	    (core misc)
	    (core struct))

  (define-struct <sid>
    (make-sid name expression control)
    sid?
    (lambda (i p)
      (format p "#<sid ~a ~a>" (sid-name i) (sid-control i)))
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
  #;(define-syntax syntax
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

  (define-syntax syntax
    (er-macro-transformer
     (lambda (form rename compare)
       (or (= (length form) 2)
	   (error 'syntax "expected exactly one datum" form))
       `(make-syntax-object (,(rename 'quote) ,(cadr form))))))
  
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
  ;;    who        -- caller
  ;;    keyword    -- keywords for macro(literals)
  ;;    clauses    -- patterns and templates
  ;;    rename     -- rename procedure from er-macro-transformer
  ;;    compare    -- compare procedure from er-macro-transformer
  ;;    gen-match  -- pattern match generator
  ;;    gen-output -- template generator
  (define (expand form who keywords clauses
		  rename compare gen-output)
    (unless (unique-id-list? keywords)
      (error who "duplicate literals" keywords))
    (let ((r-form (rename 'form))
	  (r-rename (rename 'rename))
	  (r-compare (rename 'compare)))
      (let loop ((clauses clauses))
	(if (pair? clauses)
	    (let ((pattern (caar clauses)))
	      (let ((sids (parse-pattern rename compare keywords
					 pattern r-form)))
		`(,(rename 'if)
		  ,(generate-match rename compare keywords
				   r-rename r-compare
				   pattern r-form)
		  ,(gen-output form rename compare
			       r-form r-rename sids (cadar clauses))
		  ,(loop (cdr clauses)))))
	    `(,(rename 'begin)
	      (,(rename 'error)
	       (,(rename 'quote) ,who)
	       "no expansion for" (,(rename 'unwrap-syntax) 
				   ,r-form)))))))

  (define (parse-pattern rename compare keywords pattern expression)
    (let loop ((pattern pattern)
	       (expression expression)
	       (sids '())
	       (control #f))
      (cond ((variable? pattern)
	     (if (id-memq pattern keywords)
		 sids
		 (cons (make-sid pattern expression control) sids)))
	    ;; (p ...)
	    ((and (zero-or-more? pattern rename compare)
		  (null? (cddr pattern)))
	     (let ((variable (gensym "control")))
	       (loop (car pattern)
		     variable
		     sids
		     (make-sid variable expression control))))
	    ;; (p ... (e ...))
	    #;((and (zero-or-more? pattern rename compare)
		  (not (null? (cddr pattern))))
	     (let ((variable (gensym "control")))
	       (loop (car pattern)
		     variable
		     (loop (cddr pattern)
			   `(,(rename 'cddr) ,expression)
			   sids
			   control)
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
				 (null? (cddr pattern))
				 (null? (cddr expression)))
			    #;`(,(rename 'if) (,(rename 'null?) ,expression)
			      #f
			      ,(do-list (car pattern) expression))
			    (do-list (car pattern) expression))
			   #;((and (zero-or-more? pattern rename compare)
				 (not (null? (cddr pattern)))
				 (null? (cddr expression)))
			    (let ((generate-pair
				   (lambda (expression)
				     (conjunction 
				      `(,(rename 'pair?) ,expression)
				      (conjunction 
				       (do-list (car pattern) expression)
				       (loop (cddr pattern)
					     `(,(rename 'cddr) ,expression)))))))
			      ;(print (caddr pattern)","expression)
			      (if (variable? expression)
				  (generate-pair expression)
				  (let ((temp (rename 'temp)))
				    `(,(rename 'let) ((,temp ,expression))
				      ,(generate-pair temp))))))
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

)