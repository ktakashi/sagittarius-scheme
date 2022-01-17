;; -*- scheme -*-
(library (core misc)
    (export unique-id-list?
	    define-macro
	    define-vector-type)
    (import (core)
	    (core syntax)
	    (core base)
	    (sagittarius))

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

  ;; TODO we may want to export this from somewhere else
  ;; (it seems pretty handy to have this)
  (define-syntax define-vector-type
    (lambda (x)
      (define (order-args args fs)
	(map (lambda (a) 
	       (cond ((memp (lambda (f) (bound-identifier=? a f)) fs) => car)
		     (else
		      (syntax-violation 'define-vector-type "unknown tag" a))))
	     args))
      (define (generate-accessor k acc)
	;; starting from 1 because 0 is type tag
	(let loop ((r '()) (i 1) (acc acc))
	  (syntax-case acc ()
	    (((get set) rest ...)
	     (with-syntax ((n (datum->syntax k i)))
	       (loop (cons #'(begin (define (get o) (vector-ref o n))
				    (define (set o v) (vector-set! o n v)))
			   r)
		     (+ i 1)
		     #'(rest ...))))
	    (((name) rest ...)
	     (with-syntax ((n (datum->syntax k i)))
	       (loop (cons #'(define (name o) (vector-ref o n)) r)
		     (+ i 1)
		     #'(rest ...))))
	    (() r))))
      (syntax-case x ()
	((k type (ctr args ...) pred
	    (field accessor ...) ...)
	 (and (identifier? #'pred) (identifier? #'type) (identifier? #'ctr))
	 (with-syntax (((ordered-args ...)
			(order-args #'(args ...) #'(field ...)))
		       ((acc ...)
			(generate-accessor #'k #'((accessor ...) ...))))
	 #'(begin
	     (define (ctr args ...) (vector 'type ordered-args ...))
	     (define (pred o) 
	       (and (vector? o)
		    (= (vector-length o) (+ (length #'(field ...)) 1))
		    (eq? (vector-ref o 0) 'type)))
	     acc ...))))))
)
