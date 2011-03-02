(import (sagittarius vm))

(define-syntax define-struct
  (er-macro-transformer
   (lambda (form rename compare)
     (define (create-accessor name fields)
       (define (get-accessor field type)
	 (if (pair? field)
	     (case type
	       ((get) 
		(or (>= (length field) 2)
		    (error 'define-struct "no getter for the struct" name))
		(cadr field))
	       ((set)
		(if (>= (length field) 3)
		    (caddr field)
		    (string->symbol (string-append (symbol->string (car field)) "-set!")))))
	     field))
       (let ((n (rename 'name))
	     (v (rename 'value)))
	 (let loop ((fields fields)
		    (r '()))
	   (if (null? fields)
	       r
	       (loop (cdr fields)
		     (cons 
		      `(,(rename 'define) (,(get-accessor (car fields) 'get) ,n)
			(,(rename 'generic-ref) ,n ',(if (pair? (car fields))
							 (caar fields)
							 (car fields))))
		      (cons
		       `(,(rename 'define) (,(get-accessor (car fields) 'set) ,n ,v)
			 (,(rename 'generic-set!) ,n ',(if (pair? (car fields))
							   (caar fields)
							   (car fields))
			  ,v))
		       r)))))))

     (let ((name (cadr form))
	   (constructor (caddr form))
	   (predicate (cadddr form))
	   (printer (car (cddddr form)))
	   (fields (cdr (cddddr form))))
       (let ((field-names (map (lambda (field)
				 (cond ((symbol? field) field)
				       ((pair? field) (car field))
				       (else
					(error 'define-struct
					       "invalid field name" field))))
			       fields))
	     (temp (rename 'temp))
	     (ins  (rename 'ins)))
	 `(,(rename 'begin)
	   ,@(create-accessor name fields)
	   (,(rename 'define) ,constructor
	    (,(rename 'let) ((,ins  (create-instance ,name)))
	      ,@(let loop ((params (cdr constructor))
			   (field-names field-names)
			   (r '()))
		  (if (null? params)
		      r
		      (loop (cdr params) (cdr field-names)
			    (cons `(,(rename 'generic-set!) ,ins ',(car field-names) ,(car params))
				  r))))
	      ,ins))
	   (,(rename 'define) ,name (,(rename 'make-generic) ',name ,printer ,(car constructor) 
				     ,@(map (lambda (x)
					      `(,(rename 'quote) ,x))
					    field-names)))
	   ))))))

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

(define (duplicate? lst)
  (let loop ((lst lst))
    (and (pair? lst)
	 (or (memq (car lst) (cdr lst))
	     (loop (cdr lst))))))

(define (variable? o)
  (or (identifier? o)
      (symbol? o)))

;; from MIT scheme
(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (form rename compare)
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
			"no expansion for" ,r-form)))))))))))

(define (parse-pattern rename compare keywords pattern expression)
  (let loop ((pattern pattern)
	     (expression expression)
	     (sids '())
	     (control #f))
    (cond ((variable? pattern)
	   (if (any (lambda (key) (compare pattern key)) keywords)
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
			  (if (any (lambda (key) (compare pattern key))
				   keywords)
			      (let ((temp (rename 'temp)))
				`(,(rename 'let) ((,temp ,expression))
				  (,(rename 'if)
				   (,(rename 'variable?) ,temp)
				   (,r-compare ,temp
					       (,r-rename 
						(,(rename 'quote) ,pattern)))
				   #f)))
			      `#t))
			 ((and (zero-or-more? pattern rename compare)
			       (null? (cddr expression)))
			  `(,(rename 'if) (,(rename 'null?) ,expression)
			    #f
			    ,(do-list (car pattern) expression)))
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
			`(,(rename 'letrec) ((,r-loop
					   (,(rename 'lambda) (,r-l)
					    (,(rename 'if)
					     (,(rename 'null?) ,r-l)
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

(define-syntax test
  (syntax-rules ()
      ((_ o1 o2 ...)
       (begin
	 (display o1)
	 (test o2 ...)))
      ((_ o)
       (begin (display o)(test)))
      ((_) (newline))))
(test 1 2 3)
(test 3 4 5)
(define-syntax hoge
  (syntax-rules ()
    ((_ r ...)
     (begin
       (list '(r (r ...)) ...)))))
(display (hoge 1 2 3))
