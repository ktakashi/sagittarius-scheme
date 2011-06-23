;; -*- scheme -*-
(library (core misc)
    (export unique-id-list?
	    define-macro
	    define-optional)
    (import (core)
	    (sagittarius)
	    (core base)
	    (core syntax))

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


  (define-syntax define-optional
    (syntax-rules (optional)
      ((define-opt (name . bindings) . bodies)
       (define-opt :seek-optional bindings () ((name . bindings) . bodies)))

      ((define-opt :seek-optional ((optional . _opt-bindings))
	 (reqd ...) ((name . _bindings) . _bodies))
       (define (name reqd ... . _rest)
	 (letrec-syntax
	     ((handle-opts
	       (syntax-rules ()
		 ((_ rest bodies (var init))
		  (let ((var (if (null? rest) init
				 (if (null? (cdr rest)) (car rest)
				     (error 'name "extra rest" rest)))))
		    . bodies))
		 ((_ rest bodies var) (handle-opts rest bodies (var #f)))
		 ((_ rest bodies (var init) . other-vars)
		  (let ((var (if (null? rest) init (car rest)))
			(new-rest (if (null? rest) '() (cdr rest))))
		    (handle-opts new-rest bodies . other-vars)))
		 ((_ rest bodies var . other-vars)
		  (handle-opts rest bodies (var #f) . other-vars))
		 ((_ rest bodies)		; no optional args, unlikely
		  (let ((_ (or (null? rest) (error 'name "extra rest" rest))))
		    . bodies)))))
	   (handle-opts _rest _bodies . _opt-bindings))))

      ((define-opt :seek-optional (x . rest) (reqd ...) form)
       (define-opt :seek-optional rest (reqd ... x) form))

      ((define-opt :seek-optional not-a-pair reqd form)
       (define . form))			; No optional found, regular define

      ((define-opt name body)		; Just the definition for 'name',
       (define name body))		; for compatibilibility with define
      ))
  
)
