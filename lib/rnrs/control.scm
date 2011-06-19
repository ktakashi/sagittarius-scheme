;; -*- scheme -*-
(library (rnrs control (6))
    (export when unless do case-lambda)
    (import (core)
	    (core base)
	    (core syntax))
  ;; implementation from srfi-16
  (define-syntax case-lambda
    (syntax-rules ()
      ((case-lambda
	(?a1 ?e1 ...)
	?clause1 ...)
       (lambda args
	 (let ((l (length args)))
	   (case-lambda "CLAUSE" args l
			(?a1 ?e1 ...)
			?clause1 ...))))
      ((case-lambda "CLAUSE" ?args ?l
		    ((?a1 ...) ?e1 ...)
		    ?clause1 ...)
       (if (= ?l (length '(?a1 ...)))
	   (apply (lambda (?a1 ...) ?e1 ...) ?args)
	   (case-lambda "CLAUSE" ?args ?l
			?clause1 ...)))
      ((case-lambda "CLAUSE" ?args ?l
		    ((?a1 . ?ar) ?e1 ...)
		    ?clause1 ...)
       (case-lambda "IMPROPER" ?args ?l 1 (?a1 . ?ar) (?ar ?e1 ...)
		    ?clause1 ...))
      ((case-lambda "CLAUSE" ?args ?l
		    (?a1 ?e1 ...)
		    ?clause1 ...)
       (let ((?a1 ?args))
	 ?e1 ...))
      ((case-lambda "CLAUSE" ?args ?l)
       (syntax-violation 'case-lambda
			 "wrong number of arguments to case-lambda."))
      ((case-lambda "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
		    ?clause1 ...)
       (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...)
		    ?clause1 ...))
      ((case-lambda "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...)
		    ?clause1 ...)
       (if (>= ?l ?k)
	   (apply (lambda ?al ?e1 ...) ?args)
	   (case-lambda "CLAUSE" ?args ?l
			?clause1 ...)))))
  ;; implementation from R6RS
  #;(define-syntax case-lambda-help
    (syntax-rules ()
      ((_ args n)
       (assertion-violation #f "unexpected number of arguments"))
      ((_ args n ((x ...) b1 b2 ...) more ...)
       (if (= n (length '(x ...)))
	   (apply (lambda (x ...) b1 b2 ...) args)
	   (case-lambda-help args n more ...)))
      ((_ args n ((x1 x2 ... . r) b1 b2 ...) more ...)
       (if (>= n (length '(x1 x2 ...)))
	   (apply (lambda (x1 x2 ... . r) b1 b2 ...)
		  args)
	   (case-lambda-help args n more ...)))
      ((_ args n (r b1 b2 ...) more ...)
       (apply (lambda r b1 b2 ...) args))))

  #;(define-syntax case-lambda
    (syntax-rules ()
      ((_ (fmls b1 b2 ...))
       (lambda fmls b1 b2 ...))
      ((_ (fmls b1 b2 ...) ...)
       (lambda args
	 (let ((n (length args)))
	   (case-lambda-help args n
			     (fmls b1 b2 ...) ...))))))

) ; [end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
