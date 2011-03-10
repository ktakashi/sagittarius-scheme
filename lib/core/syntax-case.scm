;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
(library (core syntax-case)
    (export syntax-case syntax datum->syntax)
    (import null
	    (sagittarius)
	    (sagittarius vm)
	    (sagittarius compiler)
	    (core base)
	    ;(core syntax match)
	    (core syntax helper))

  ;;
  ;; syntax-case
  ;;
  ;; deference between syntax-rules and syntax-case.
  ;; * syntax-case should not return er-macro-transformer because it's just a 
  ;;   closure.
  (define-syntax syntax-case
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((expr     (cadr form))
	     (keywords (caddr form))
	     (clauses  (cdddr form))
	     (r-form   (rename 'form)) (r-rename (rename 'rename))
	     (r-compare (rename 'compare)) (r-let    (rename 'let))
	     (r-dict   (rename 'dict)))
	 `(,r-let ((,r-dict (,(rename 'make-eq-hashtable))))
	    (,r-let ((,r-form (,(rename 'car) ,expr))
		     (,r-rename (lambda (s) (er-rename s (cdr ,expr) ,r-dict)))
		     (,r-compare (lambda (a b)
				   (or (eq? a b)
				       (cond ((and (symbol? a)
						   (identifier? b))
					      (eq? (rename a) b))
					     ((and (identifier? a)
						   (symbol? b))
					      (eq? a (rename b)))
					     (else #f))))))
		    ,(expand form 'syntax-case keywords clauses
			     rename compare generate-output)))))))

  (define (generate-output oform rename compare
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
		     (rename template))))
	      ((zero-or-more? template rename compare)
	       (cons
		(let ((ellipsis (make-ellipsis '())))
		  (generate-ellipsis rename ellipsis
				     (loop (car template)
					   (cons ellipsis
						 ellipses))))
		(loop (cddr template) ellipses)))
	      ((pair? template)
	       ;(if (compare (car template) 'syntax)
		   ;(expand-syntax template)
		   (cons (loop (car template) ellipses)
			 (loop (cdr template) ellipses)));)
	      ((vector? template)
	       (loop (vector->list template) ellipses))
	      (else
	       template))))
    (let ((r (inner)))
	  r))
#|
  (define (expand-syntax form)
    (smatch form
      ((- template)
       (process-template template 0 #f))))

  (define (process-template template dim ellipses-quoted?)
    (smatch template
      ((syntax ...)
       (if (not ellipses-quoted?)
	   (syntax-violation 'syntax "Invalid occurrence of ellipses in syntax template" template))
       (syntax-reflect template))
      ((? identifier? id)
       (let ((binding (binding id)))
	 (cond ((and binding
		     (eq? (binding-type binding) 'pattern-variable)
		     (binding-dimension binding))
		=> (lambda (pdim)
		     (if (<= pdim dim)
			 (begin
			   (check-binding-level id binding)
			   (register-use! id binding)
			   (binding-name binding))
			 (syntax-violation 'syntax "Template dimension error (too few ...'s?)" id))))
	       (else
		(syntax-reflect id)))))
      (((syntax ...) p)
       (process-template p dim #t))
      ((? (lambda (_) (not ellipses-quoted?))
	  (t (syntax ...) . tail))
       (let* ((head (segment-head template)) 
	      (vars
	       (map (lambda (mapping)
		      (let ((id      (car mapping))
			    (binding (cdr mapping)))
			(check-binding-level id binding)
			(register-use! id binding)
			(binding-name binding)))
		    (free-meta-variables head (+ dim 1) '() 0))))
	 (if (null? vars)
	     (syntax-violation 'syntax "Too many ...'s" template)
	     (let* ((x (process-template head (+ dim 1) ellipses-quoted?))
		    (gen (if (equal? (list x) vars)   ; +++
			     x                        ; +++
			     (if (= (length vars) 1) 
				 `(map (lambda ,vars ,x)
				       ,@vars)
				 `(if (= ,@(map (lambda (var) 
						  `(length ,var))
						vars))
				      (map (lambda ,vars ,x)
					   ,@vars)
				      (ex:syntax-violation 
				       'syntax 
				       "Pattern variables denoting lists of unequal length preceding ellipses"
				       ',(syntax->datum template) 
				       (list ,@vars))))))
		    (gen (if (> (segment-depth template) 1)
			     `(apply append ,gen)
			     gen)))
	       (if (null? (segment-tail template))   ; +++
		   gen                               ; +++
		   `(append ,gen ,(process-template (segment-tail template) dim ellipses-quoted?)))))))
      ((t1 . t2)
       `(cons ,(process-template t1 dim ellipses-quoted?)
	      ,(process-template t2 dim ellipses-quoted?)))
      (#(ts ___)
       `(list->vector ,(process-template ts dim ellipses-quoted?)))
      (other
       `(quote ,(expand other)))))
|#
  (define (generate-ellipsis rename ellipsis body)
    (let ((sids (ellipsis-sids ellipsis))
	  (r-map (rename 'map)))
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
		   `(,(rename 'quasiquote)
		     (,(rename 'unquote)
		      (,r-map ,(car body) ,expression))))
		  (else
		   `(,(rename 'quasiquote)
		     (,(rename 'unquote)
		      (,r-map (,(rename 'lambda)
			       ,(map sid-name sids)
			       ,body)
			      ,@(map sid-expression sids)))))))
	  (error 'syntax-case "missing ellipsis in expanstion."))))

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