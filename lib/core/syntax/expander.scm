;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
(library (core syntax expander)
    (export expand-syntax-case)
    (import (core)
	    (sagittarius)
	    (sagittarius vm)
	    (core base)
	    (core errors)
	    (core syntax pattern)
	    (core syntax template)
	    (core syntax helper))

  (define expand-syntax-case
    (lambda (expr rename compare)
	(let ((literal (caddr expr))
	      (clauses (cdddr expr))
	      (_define (rename 'define)) (_er-rename (rename 'er-rename))
	      (_a (rename 'a))           (_b (rename 'b))
	      (_variable? (rename 'variable?)) (_eq? (rename 'eq?))
	      (_env (rename 'env))       (_cdr (rename 'cdr))
	      (_dict (rename 'dict))     (_compare (rename 'compare))
	      (_let (rename 'let))       (_loop (rename 'loop))
	      (_pair? (rename 'pair?))   (_rename (rename 'rename))
	      (_cond (rename 'cond))     (_else (rename 'else))
	      (_expr (rename 'expr))     (_cons (rename 'cons))
	      (_car (rename 'car))       (_variable? (rename 'variable?))
	      (_begin (rename 'begin))    (_and (rename 'and)))
	  ;; not use
	  #;(define expand-macro
	    (lambda (tmpl expr sids in-syntax?)
	      (let* ((t (rename (car tmpl)))
		     (m (find-binding (id-library t)
				      (id-name t))))
		(cond ((and (not (compare (car tmpl) (rename 'syntax-case)))
			    (not (compare (car tmpl) (rename 'syntax)))
			    (macro? m))
		       (generate-output (call-macro-expander m tmpl env)
					sids rename compare expr in-syntax? expand-macro))
		      ;; call-macro-expander doesn't know how to treat '...
		      ((compare (car tmpl) (rename 'syntax-case))
		       (expand-syntax-case tmpl env sids rename compare #f in-syntax?))
		      (else
		       #f)))))

	  (define loop 
	    (lambda (form clauses)
	      (if (pair? clauses)
		  (let ((pat (caar clauses))
			(template (cdar clauses)))
		    (check-pattern pat literal rename compare)
		    (let ((sids (collect-sids pat form literal rename compare))
			  (len  (length template)))
		      (when (or (> len 2)
				(< len 1))
			(syntax-violation 'syntax-case
					  "a clause must be either (<pattern> <output>) or (<pattern> <fender> <output>)"
					  clause))
		      (let ((fender (if (= len 2)
					(car template)
					#f))
			    (output (if (= len 2)
					(cadr template)
					(car template))))
			;; TODO
			;; we need to check template since we just output non syntax-expression
			;; but for now
			;;(check-template template sids rename compare)
			`(,(rename 'if)
			  (,_and ,(generate-match pat literal rename compare form)
				 ,(if fender
				      (cadr template)
				      #t))
			  ,(generate-output output sids rename compare form #t)
			  ,(loop form (cdr clauses))))))
		  `(,_begin
		    (,(rename 'syntax-violation)
		     (,(rename 'quote) syntax-case)
			   "invalid syntax"
			   (,(rename 'car) ,form))))))
	  (let* ((x   `(,(rename 'car) ,(cadr expr)))
		 (form x))
	    (if #t
		`(,_let ((,_env (,_cdr ,(cadr expr)))
			 (,_dict (,(rename 'make-eq-hashtable))))
			(,_define (,_rename ,_a) (,_er-rename ,_a ,_env ,_dict))
			(,_define (,_compare ,_a ,_b)
				  (,(rename 'or) (,_eq? ,_a ,_b)
				   (,(rename 'and)
				    (,_variable? ,_a)
				    (,_variable? ,_b)
				    (,_eq? (,_rename ,_a) (,_rename ,_b)))
				   #f))
			,(loop form clauses))
		`(,_begin ,(loop form clauses)))))))
)
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End: