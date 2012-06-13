;; -*- scheme -*-
(library (core exceptions)
    (export with-exception-handler
	    guard
	    raise
	    raise-continuable)
    (import (core)
	    (core errors)
	    (sagittarius))

  (define-syntax guard
    (lambda (x)
      (syntax-case x (else)
	((_ (var clause ... (else e1 e2 ...)) b1 b2 ...)
	 #'((call/cc
	     (lambda (guard-k)
	       (with-exception-handler
		(lambda (condition)
		  ((call/cc
		    (lambda (handler-k)
		      (guard-k
		       (lambda ()
			 (let ((var condition))
			   (cond clause ... 
				 (else e1 e2 ...)))))))))
		(lambda ()
		  ;; Sagittarius prefer receive
		  (receive args
		      (let () b1 b2 ...)
		    (guard-k (lambda () (apply values args))))))))))
	((_ (var clause ...) b1 b2 ...)
	 #'((call/cc
	     (lambda (guard-k)
	       (with-exception-handler
		(lambda (condition)
		  ((call/cc
		    (lambda (handler-k)
		      (guard-k
		       (lambda ()
			 (let ((var condition))
			   (cond clause ...
				 (else 
				  (handler-k 
				   (lambda () 
				     (raise-continuable condition))))))))))))
		(lambda ()
		  (receive args
		      (let () b1 b2 ...)
		    (guard-k (lambda () (apply values args)))))))))))))

)