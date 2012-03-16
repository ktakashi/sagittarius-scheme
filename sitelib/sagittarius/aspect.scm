;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
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
       (define (parse-formal formal)
	 (let loop ((lst formal)
		    (r '()))
	   (cond ((null? lst) (values (reverse! r) '()))
		 ((pair? lst)
		  (loop (cdr lst) (cons (car lst) r)))
		 (else
		  (values (reverse! r) lst)))))
       (unless (= (length form) 4)
	 (syntax-violation 'point-cut
			   "malformed point-cut" form))
       (let ((lib (cadr form))
	     (name (caddr form))
	     (value (cadddr form)))
	 (let* ((lib   (find-library lib #f))
		 (gloc (find-binding lib name #f)))
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
		  (let* ((org (string->symbol (format ".~s-org" name)))
			 (formal (cadr value))
			 (body   (cddr value)))
		    (let-values (((req rest) (parse-formal formal)))
		      ;;(format #t "~a~%" (append '(list) args))
		      `(begin
			 (define ,org ,bind)
			 (define ,name 
			   (lambda ,formal
			     (define (proceed)
			       (apply ,org ,@req ,rest))
			     ,@body))
			 (gloc-set! ,gloc ,name)))))
	       (assertion-violation 'point-cut
				    "unbound variable"
				    name)))))))
)