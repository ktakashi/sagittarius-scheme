;; -*- scheme -*-
(library (rnrs control (6))
    (export when unless do case-lambda)
    (import (core)
	    (core syntax-rules))
  ;; implementation from R6RS
  (define-syntax case-lambda
    (syntax-rules ()
      ((_ (fmls b1 b2 ...))
       (lambda fmls b1 b2 ...))
      ((_ (fmls b1 b2 ...) ...)
       (lambda args
	 (let ((n (length args)))
	   (case-lambda-help args n
			     (fmls b1 b2 ...) ...))))))
  
  (define-syntax case-lambda-help
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

) ; [end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
