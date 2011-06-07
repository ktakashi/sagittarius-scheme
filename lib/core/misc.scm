;; -*- scheme -*-
(library (core misc)
    (export unique-id-list?
	    define-macro)
    (import (core)
	    (sagittarius)
	    (core base))
  (define (unique-id-list? lst)
    (and (list? lst)
	 (not (let loop ((lst lst))
		(and (pair? lst)
		     (or (not (symbol? (car lst)))
			 (memq (car lst) (cdr lst))
			 (loop (cdr lst))))))))

  (define-syntax define-macro
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((name (cadr form))
	     (body (cddr form)))
	 (let ((_define-macro (rename 'define-macro))
	       (_lambda (rename 'lambda))
	       (_define-syntax (rename 'define-syntax))
	       (_car (rename 'car)) (_cdr (rename 'cdr))
	       (_er-macro-transformer (rename 'er-macro-transformer))
	       (_apply (rename 'apply)) (_form (rename 'form)))
	   (if (pair? name)
	       `(,_define-macro ,(car name) (,_lambda ,(cdr name) ,@body))
	       `(,_define-syntax ,name
		  (,_er-macro-transformer
		   (,_lambda (,_form rename compare)
		     (,_apply ,(car body) (,_cdr ,_form)))))))))))
)
