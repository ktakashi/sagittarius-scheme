(import (core syntax pattern)
	(core syntax template)
	(core base)
	(pp)
	(sagittarius vm))

(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((literal (cadr form))
	   (clauses (cddr form)))
       (let ((r 
	      `(,(rename 'er-macro-transformer)
		(,(rename 'lambda) (,(rename 'form) ,(rename 'rename) ,(rename 'compare))
		 ,(let loop ((clauses clauses))
		    (if (pair? clauses)
			(let ((pat (caar clauses))
			      (template (cadar clauses)))
			  (check-pattern pat literal rename compare)
			  (let ((ranks (collect-vars-ranks pat literal 0 '() rename compare)))
			    (check-template template ranks rename compare)
			    `(,(rename 'if)
			      ,(generate-match pat literal rename compare (rename 'form))
			      ,(generate-output template ranks rename compare (rename 'form) #f)
			      ,(loop (cdr clauses)))))
			`(,(rename 'begin)
			  (,(rename 'syntax-violation)
			   (,(rename 'quote) syntax-rules)
			   "invalid syntax"
			   ,(rename 'form)))))))))
	 (pretty-print (unwrap-syntax r))
	 r)))))

(define-syntax check-pattern
  (syntax-rules (else)
    ((_ a b ... (else c d ...))
     (list 'a 'c))
    ((_ a b ...)
     (list 'a 'b ...))))

(print (check-pattern 'a 'b (else 'c 'd)))
(print (check-pattern 'a 'b 'c 'd))