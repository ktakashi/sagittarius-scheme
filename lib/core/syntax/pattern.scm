;; -*- scheme -*-
(library (core syntax pattern)
    (export collect-sids
	    generate-match)
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax helper)
	    (core syntax-case)
	    (sagittarius))

  ;; we use sid to keep pattern variable information.
  ;; it contains variable name, form expression which we need when generate
  ;; template matching, and control which is the name of template matching.
  ;; this sid stuffs are from MIT scheme
  (define (collect-sids pat expr lites rename compare)
    (define _car (rename 'car))
    (define _cdr (rename 'cdr))
    (define _cadr (rename 'cadr))
    (define _cddr (rename 'cddr))
    (define _let (rename 'let))
    (define _loop (rename 'loop))
    (define _pair? (rename 'pair?))
    (define _null? (rename 'null?))
    (define _lst (rename 'lst))
    (define _cond (rename 'cond))
    (define _else (rename 'else))
    (define _and (rename 'and))
    (define _syntax-violation (rename 'syntax-violation))
    (define (rec pat expr depth sids control)
      (cond ((compare pat (rename '_)) sids)
	    ((variable? pat)
	     (if (id-memq pat lites)
		 sids
		 (cons (make-sid pat expr depth control) sids)))
	    ;; (p ...)
	    ((ellipsis-pair? pat)
	     ;; correct?
	     (let ((var (gensym "control")))
	       ;; we want to assosiate a control to ellipsis
	       ;; (a p1 ... (b p2 ...)) in this case both p1 and p2 must have
	       ;; control. so we also need to check cddr of this pattern
	       (rec (cddr pat)
		    (if (null? (cddr pat))
			`(,_cddr ,expr) ;; '()
			`(,_let ,_loop ((,_lst (,_cdr ,expr)))
			   (,_cond ((,_null? ,_lst) ;; should not happend, i guess
				    ,expr
				    #;(,_syntax-violation "syntax template"
							"invalid syntax"
							,expr))
				   ((,_and (,_pair? (,_cdr ,_lst))
					   (,_pair? (,_cadr ,_lst)));; we need from here
				    (,_cdr ,_lst))
				   (,_else
				    (,_loop (,_cdr ,_lst))))))
		    depth
		    (rec (car pat)
			 var
			 (+ depth 1)
			 sids
			 (make-sid var (if (null? (cddr pat))
					   expr
					   `(,(rename 'list-head) ,expr (,(rename '-) (,(rename 'length) ,expr) 1)))
				   (+ depth 1) control))
		    control)))
	    ((pair? pat)
	     (rec (cdr pat) `(,_cdr ,expr) depth
		  (rec (car pat) `(,_car ,expr) depth sids control)
		  control))
	    ((vector? pat)
	     (rec (vector->list pat) expr depth sids control))
	    (else sids)))
    (rec pat expr 0 '() #f))
  
  ;; generate match
  (define (generate-match pattern literals rename compare expr)
    (let ((_if (rename 'if))    (_loop (rename 'loop))
	  (_let (rename 'let))  (_cdr (rename 'cdr))
	  (_car (rename 'car))  (_quote (rename 'syntax-quote))
	  (_lambda (rename 'lambda)) (_l (rename 'l))
	  (_letrec (rename 'letrec)) (_null? (rename 'null?))
	  (_pair? (rename 'pair?)) (_temp (rename 'temp))
	  (_vector? (rename 'vector?)) (_equal? (rename 'equal?))
	  (_or (rename 'or))    (_symbol? (rename 'symbol?))
	  (_list? (rename 'list?)) (_> (rename '>))
	  (_and (rename 'and))   (_n (rename 'n))
	  (_- (rename '-))      (_= (rename '=))
	  (_+ (rename '+))
	  (_identifier? (rename 'identifier?))
	  (_count-pair (rename '.count-pair)) ;; toplevel
	  (_rename (rename 'rename)) (_compare (rename 'compare)))
      (define (conjunction predicate consequent)
	(cond ((eq? predicate #t) consequent)
	      ((eq? consequent #t) predicate)
	      (else `(,_if ,predicate
			   ,consequent #f))))
      ;; match ellipsis
      (define (do-list pattern expr)
	`(,_letrec ((,_loop (,_lambda (,_l)
				(,_if (,_null? ,_l)
				      #t
				      (,_and (,_pair? ,_l)
					     ,(loop pattern `(,_car ,_l))
					     (,_loop (,_cdr ,_l)))))))
	     (,_loop ,expr)))
      (define (do-list-n pattern expr)
	`(,_letrec ((,_loop (,_lambda (,_l ,_n)
				(,_if (,_= ,_n 0)
				      #t
				      ,(conjunction
					`(,_pair? ,_l)
					(conjunction
					 (loop (car pattern)
					       `(,_car ,_l))
					 `(,_loop (,_cdr ,_l) (,_- ,_n 1)))))))
		    
		    (,_n (,_- (,_count-pair ,expr)
			      ,(count-pair (cddr pattern))))) ;; original count-pair is in macro.scm
	     (,_if (,_= ,_n 0)
		   ,(loop (cddr pattern) expr)
		   (,_and (,_> ,_n 0)
			  (,_loop ,expr ,_n)
			  ,(loop (cddr pattern) `(,(rename 'list-tail) ,expr ,_n))))))
      (define (loop pattern expr)
	(cond ((variable? pattern) ;; pattern variable
	       (if (id-memq pattern literals)
		   `(,_let ((,_temp ,expr))
		       (,_if (,_or (,_symbol? ,_temp)
				   (,_identifier? ,_temp))
			     (,_compare ,_temp (,_rename (,_quote ,pattern)))
			     #f))
		   `#t))
	      ;; TODO
	      ((ellipsis-pair? pattern)
	       (if (null? (cddr pattern))
		   `(,_if (,_list? ,expr)
			  ,(if (symbol? (car pattern))
			       #t
			       (do-list (car pattern) expr))
			  ,(do-list-n pattern expr))
		   (do-list-n pattern expr)))
	      ((pair? pattern)
	       (let ((generate-pair (lambda (expr)
				      (conjunction
				       `(,_pair? ,expr)
				       (conjunction
					(loop (car pattern)
					      `(,_car ,expr))
					(loop (cdr pattern)
					      `(,_cdr ,expr)))))))
		 (if (variable? expr)
		     (generate-pair expr)
		     `(,_let ((,_temp ,expr))
			 ,(generate-pair _temp)))))
	      ((vector? pattern)
	       `(,_if (,_vector? ,expr)
		     ,(loop (vector->list pattern) expr)
		     #f))
	      ((null? pattern)
	       `(,_null? ,expr))
	      (else
	       `(,_equal? ,expr (,_quote ,pattern)))))
      ;;(trace loop do-list do-list-n conjunction)
      (loop pattern expr)))
)