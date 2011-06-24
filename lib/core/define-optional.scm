;; -*- scheme -*-
(library (core define-optional)
    (export define-optional)
    (import (core)
	    (core syntax))
  (define-syntax define-optional
    (syntax-rules (optional)
      ((_ (name . bindings) . bodies)
       (define-optional :seek-optional bindings () ((name . bindings) . bodies)))

      ((_ :seek-optional ((optional . _opt-bindings))
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

      ((_ :seek-optional (x . rest) (reqd ...) form)
       (define-optional :seek-optional rest (reqd ... x) form))

      ((_ :seek-optional not-a-pair reqd form)
       (define . form))			; No optional found, regular define

      ((_ name body)		; Just the definition for 'name',
       (define name body))		; for compatibilibility with define
      ))
)