;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
(library (core syntax-rules)
    (export syntax-rules 
	    er-macro-transformer
	    syntax-rules2)
    (import (core)
	    (sagittarius)
	    (core base)
	    (core errors)
	    (core syntax-case)
	    (core syntax pattern)
	    (core syntax template))

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
