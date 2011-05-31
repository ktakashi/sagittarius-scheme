;; -*- scheme -*-
(library (sagittarius misc)
    (export output-port-width
	    define-macro
	    er-macro-transformer)
    (import (core)
	    (core base)
	    (sagittarius))

  (define *output-port-width* 79)
  ;; we don't provide specific port width from port.
  (define (output-port-width port . width)
    (if (null? width)
	*output-port-width*
	(set! *output-port-width* (car width))))

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
