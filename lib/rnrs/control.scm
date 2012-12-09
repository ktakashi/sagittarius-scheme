;; -*- scheme -*-
(library (rnrs control (6))
    (export when unless do case-lambda)
    (import (core)
	    (core base)
	    (only (sagittarius) reverse!)
	    (core syntax))

  (define-syntax case-lambda-aux
    (lambda (x)
      (define (construct args formals clause*)
	(define _car #'car)
	(define _cdr #'cdr)
	(define (parse-formals formal args inits)
	  (syntax-case formal ()
	    (() (reverse! inits))
	    ((a . d)
	     (with-syntax ((arg `(,_car ,args))
			   (args `(,_cdr ,args)))
	       (parse-formals #'d #'args
			      (cons (list #'a #'arg) inits))))
	    (v
	     (reverse! (cons (list #'v args) inits)))))
	(with-syntax ((((var init) ...) (parse-formals formals args'()))
		      ((clause ...) clause*))
	  #'(let ((var init) ...) clause ...)))
      (syntax-case x ()
	((_ args n)
	 #'(assertion-violation #f "unexpected number of arguments" args))
	((_ args n ((x ...) b ...) more ...)
	 (with-syntax ((let-clause (construct #'args #'(x ...) #'(b ...))))
	   #'(if (= n (length '(x ...)))
		 let-clause
		 (case-lambda-aux args n more ...))))
	((_ args n ((x1 x2 ... . r) b ...) more ...)
	 (with-syntax ((let-clause (construct #'args #'(x1 x2 ... . r) 
					      #'(b ...))))
	   #'(if (>= n (length '(x1 x2 ...)))
		 let-clause
		 (case-lambda-aux args n more ...))))
	((_ args n (r b ...) more ...)
	 #'(let ((r args)) b ...)))))

  (define-syntax case-lambda
    (syntax-rules ()
      ((_ (fmls b1 b2 ...))
       (lambda fmls b1 b2 ...))
      ((_ (fmls b1 b2 ...) ...)
       (lambda args
	 (let ((n (length args)))
	   (case-lambda-aux args n (fmls b1 b2 ...) ...))))))


  ;; implementation from srfi-16
;;   (define-syntax case-lambda
;;     (syntax-rules ()
;;       ((case-lambda (?a1 ?e1 ...) ?clause1 ...)
;;        (lambda args
;; 	 (let ((l (length args)))
;; 	   (case-lambda "CLAUSE" args l
;; 			(?a1 ?e1 ...)
;; 			?clause1 ...))))
;;       ((case-lambda "CLAUSE" ?args ?l
;; 		    ((?a1 ...) ?e1 ...)
;; 		    ?clause1 ...)
;;        (if (= ?l (length '(?a1 ...)))
;; 	   (apply (lambda (?a1 ...) ?e1 ...) ?args)
;; 	   (case-lambda "CLAUSE" ?args ?l
;; 			?clause1 ...)))
;;       ((case-lambda "CLAUSE" ?args ?l
;; 		    ((?a1 . ?ar) ?e1 ...)
;; 		    ?clause1 ...)
;;        (case-lambda "IMPROPER" ?args ?l 1 (?a1 . ?ar) (?ar ?e1 ...)
;; 		    ?clause1 ...))
;;       ((case-lambda "CLAUSE" ?args ?l
;; 		    (?a1 ?e1 ...)
;; 		    ?clause1 ...)
;;        (let ((?a1 ?args))
;; 	 ?e1 ...))
;;       ((case-lambda "CLAUSE" ?args ?l)
;;        (syntax-violation 'case-lambda
;; 			 "wrong number of arguments to case-lambda."))
;;       ((case-lambda "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
;; 		    ?clause1 ...)
;;        (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...)
;; 		    ?clause1 ...))
;;       ((case-lambda "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...)
;; 		    ?clause1 ...)
;;        (if (>= ?l ?k)
;; 	   (apply (lambda ?al ?e1 ...) ?args)
;; 	   (case-lambda "CLAUSE" ?args ?l
;; 			?clause1 ...)))))
  ;; implementation from R6RS
;;   (define-syntax case-lambda-help
;;     (syntax-rules ()
;;       ((_ args n)
;;        (assertion-violation #f "unexpected number of arguments"))
;;       ((_ args n ((x ...) b1 b2 ...) more ...)
;;        (if (= n (length '(x ...)))
;; 	   (apply (lambda (x ...) b1 b2 ...) args)
;; 	   (case-lambda-help args n more ...)))
;;       ((_ args n ((x1 x2 ... . r) b1 b2 ...) more ...)
;;        (if (>= n (length '(x1 x2 ...)))
;; 	   (apply (lambda (x1 x2 ... . r) b1 b2 ...)
;; 		  args)
;; 	   (case-lambda-help args n more ...)))
;;       ((_ args n (r b1 b2 ...) more ...)
;;        (apply (lambda r b1 b2 ...) args))))
;; 
;;   (define-syntax case-lambda
;;     (syntax-rules ()
;;       ((_ (fmls b1 b2 ...))
;;        (lambda fmls b1 b2 ...))
;;       ((_ (fmls b1 b2 ...) ...)
;;        (lambda args
;; 	 (let ((n (length args)))
;; 	   (case-lambda-help args n
;; 			     (fmls b1 b2 ...) ...))))))

) ; [end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
