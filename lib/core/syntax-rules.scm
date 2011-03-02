;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
;; helper library
(library (core syntax-rules util)
    (export <sid>
	    make-sid sid-name sid-expression
	    sid?
	    sid-control
	    ;sid-output-expression set-sid-output-expression!

	    <ellipsis>
	    make-ellipsis ellipsis? ellipsis-sids set-ellipsis-sids!)
    (import null 
	    (sagittarius)
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
)

(library (core syntax-rules)
    (export syntax-rules)
    (import null
	    (sagittarius)
	    (core base)
	    (core syntax-rules util))

(define (duplicate? lst)
  (let loop ((lst lst))
    (and (pair? lst)
	 (or (memq (car lst) (cdr lst))
	     (loop (cdr lst))))))

(define (variable? o)
  (or (identifier? o)
      (symbol? o)))

(define (id-memq id lst)
  (if (identifier? id)
      (memq (id-name id) lst)
      (memq id lst)))

;; from MIT scheme
(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (form rename compare)
     (expand form rename compare))))

(define (expand form rename compare)
  ;; TODO user defined ellipses
  (let ((keywords (cadr form))
	(clauses (cddr form)))
    (when (duplicate? keywords)
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
		      ,(generate-match rename compare keywords
				       r-rename r-compare
				       pattern r-form)
		      ,(generate-output rename compare r-rename
					sids (cadar clauses))
		      ,(loop (cdr clauses)))))
		`(,(rename 'begin)
		  (,(rename 'error)
		   (,(rename 'quote) 'syntax-rules)
		   "no expansion for" (,(rename 'unwrap-syntax) ,r-form))))))))))

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

(define (zero-or-more? pattern rename compare)
  (and (pair? pattern)
       (pair? (cdr pattern))
       (compare (cadr pattern) (rename '...))))

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
