;; -*- scheme -*-
(library (trace)
    (export trace
	    trace:trace-procedure
	    trace:indent)
    (import (slib trace))
  ;; TODO add more macros from slib
  (define-syntax trace
    (er-macro-transformer
     (lambda (form rename compare)
       (unless (>= (length form) 2)
	 (assertion-violation 'trace
			      (format "at least 2 arguments required, but got ~a" (length form))
			      form))
       (let ((xs (cdr form)))
	 (if (null? xs)
	     `(begin (set! trace:indent 0)
		     ,@(map (lambda (x)
			      `(set! ,x (trace:trace-procedure 'trace ,x ',x)))
			    (map car *traced-procedures*))
		     (map car *traced-procedures*))
	     `(begin ,@(map (lambda (x)
			      `(set! ,x (trace:trace-procedure 'trace ,x ',x))) xs)))))))
)