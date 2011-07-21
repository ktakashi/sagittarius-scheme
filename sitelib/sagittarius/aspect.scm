;; -*- scheme -*-
(library (sagittarius aspect)
    (export point-cut %insert-binding gloc-set!)
    (import (core)
	    (core base)
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
	 (let ((gloc (find-binding lib name #f)))
	   (if gloc
	       (let ((bind (gloc-ref gloc)))
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
		 (let ((org (string->symbol (format ".~s-org" name))))
		   `(begin
		      (define ,org ,bind)
		      (define ,name 
			(lambda ,(cadr value)
			  (define (proceed)
			    (,org ,@(cadr value)))
			  ,@(cddr value)))
		      (gloc-set! ,gloc ,name))))
	       (assertion-violation 'point-cut
				    "unbound variable"
				    name)))))))
)