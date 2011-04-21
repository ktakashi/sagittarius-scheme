;; -*- scheme -*-
(library (sagittarius aspect)
    (export point-cut %insert-binding)
    (import (core)
	    (core errors)
	    (sagittarius)
	    (sagittarius vm))

  ;;
  ;; point-cut
  ;;   (point-cut (lib) name value)
  (define-syntax point-cut
    (er-macro-transformer
     (lambda (form rename compare)
       ;; TODO rewrite when match is ready
       (let ((lib   (find-library (cadr form) #f))
	     (name  (caddr form))
	     (value (cadddr form)))
	 (let ((bind (find-binding lib name)))
	   (or (procedure? bind)
	       (assertion-violation 'point-cut
				    "target binding is not procedure"
				    bind
				    form))
	   (or (eq? (car value) 'lambda)
	       (assertion-violation 'point-cut
				    "replacement value is not procedure"
				    value
				    form))
	   #;(or (equal? (arity bind) (arity value))
	       (assertion-violation 'point-cut
				    "argument count is not the same"
				    `((binding: ,bind) (replacement: ,value))
				    form))
	   (let ((org (string->symbol (format ".~s-org" name))))
	     `(begin
		(define ,org ,bind)
		(define ,name 
		  (lambda ,(cadr value)
		    (define (proceed)
		      (,org ,@(cadr value)))
		    ,@(cddr value)))
		(%insert-binding ,lib
				 ',name
				 ,name))))))))
)