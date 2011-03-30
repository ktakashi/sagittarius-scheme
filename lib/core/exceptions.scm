;; -*- scheme -*-
(library (core exceptions)
    (export with-exception-handler
	    guard
	    raise
	    raise-continuable)
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax-rules)
	    (sagittarius))

  (define-syntax guard
    (syntax-rules (else)
      ((_ (var clause ... (else e1 e2 ...)) b1 b2 ...)
       ((call/cc
	 (lambda (guard-k)
	   (with-exception-handler
	    (lambda (condition)
	      ((call/cc
		(lambda (handler-k)
		  (guard-k
		   (lambda ()
		     (let ((var condition))
		       (cond clause ... (else e1 e2 ...)))))))))
	    (lambda ()
	      (call-with-values
		  (lambda () b1 b2 ...)
		(lambda args (guard-k (lambda () (apply values args)))))))))))
      ((_ (var clause ...) b1 b2 ...)
       ((call/cc
	 (lambda (guard-k)
	   (with-exception-handler
	    (lambda (condition)
	      ((call/cc
		(lambda (handler-k)
		  (guard-k
		   (lambda ()
		     (let ((var condition))
		       (cond clause ...
			     (else (handler-k (lambda () (raise-continuable condition))))))))))))
	    (lambda ()
	      (call-with-values
		  (lambda () b1 b2 ...)
		(lambda args (guard-k (lambda () (apply values args)))))))))))))
)