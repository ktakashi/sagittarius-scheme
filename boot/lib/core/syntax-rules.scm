;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
;; TODO this implementation is REALLY slow.
(library (core syntax-rules)
    (export syntax-rules 
	    er-macro-transformer)
    (import (core)
	    (sagittarius)
	    (core base)
	    (core errors)
	    ;;(core syntax-case)
	    (core syntax helper)
	    (core syntax pattern)
	    (core syntax template))

  (define check-pattern
    (lambda (pat lites)

      (define check-duplicate-variable
	(lambda (pat lites)
	  (let loop ((lst pat) (pool '()))
	    (cond ((pair? lst)
		   (loop (cdr lst)
			 (loop (car lst) pool)))
		  ((ellipsis? lst) pool)
		  ((bar? lst) pool)
		  ((variable? lst)
		   (if (id-memq lst lites)
		       pool
		       (if (memq lst pool)
			   (assertion-violation "syntax pattern" "duplicate pattern variables" pat lst)
			   (cons lst pool))))
		  ((vector? lst)
		   (loop (vector->list lst) pool))
		  (else pool)))))

      (define check-misplaced-ellipsis
	(lambda (pat lites)
	  (let loop ((lst pat))
	    (cond ((ellipsis? lst)
		   (assertion-violation "syntax pattern" "improper use of ellipsis" pat))
		  ((ellipsis-pair? lst)
		   (and (variable? (car lst))
			(id-memq (car lst) lites)
			(assertion-violation "syntax pattern" "ellipsis following literal" pat lst))
		   (let loop ((lst (cddr lst)))
		     (and (pair? lst)
			  (if (ellipsis? (car lst))
			      (assertion-violation "syntax pattern" "ambiguous use of ellipsis" pat)
			      (loop (cdr lst))))))
		  ((pair? lst)
		   (or (loop (car lst)) (loop (cdr lst))))
		  ((vector? lst)
		   (loop (vector->list lst)))
		  (else #f)))))

      (check-misplaced-ellipsis pat lites)
      (check-duplicate-variable pat lites)))

  (define-syntax syntax-rules
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((literal (unwrap-syntax (cadr form)))
	     (clauses (cddr form)))
	 `(,(rename 'er-macro-transformer)
	   (,(rename 'lambda) (,(rename 'form) ,(rename 'rename) ,(rename 'compare))
	    ,(let loop ((clauses clauses))
	       (if (pair? clauses)
		   (let ((pat (caar clauses))
			 (template (cadar clauses)))
		     (check-pattern pat literal)
		     (let ((sids (collect-sids pat (rename 'form) literal rename compare)))
		       ;;(check-template template sids rename compare)
		       `(,(rename 'if)
			 ,(generate-match pat literal rename compare (rename 'form))
			 ,(generate-output template sids rename compare (rename 'form))
			 ,(loop (cdr clauses)))))
		   `(,(rename 'begin)
		     (,(rename 'syntax-violation)
		      (,(rename 'quote) syntax-rules)
		      "invalid syntax"
		      (,(rename 'unwrap-syntax) ,(rename 'form))))))))))))
  )
