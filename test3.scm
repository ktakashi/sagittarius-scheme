(import (core syntax pattern)
	(core base)
	(pp)
	(sagittarius vm))

(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((literal (cadr form))
	   (clauses (cddr form)))
       (let ((r 
	      `(er-macro-transformer
		(lambda (,(rename 'form) ,(rename 'rename) ,(rename 'compare))
		  (,(rename 'or)
		    ,@(map (lambda (clause)
			     (let ((pat (car clause))
				   (template (cadr clause)))
			       (check-pattern pat literal rename compare)
			       (let ((ranks (collect-vars-ranks pat literal 0 '() rename compare)))
				 (generate-match pat literal rename compare (rename 'form)))))
			   clauses))))))
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