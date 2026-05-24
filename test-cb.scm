(import (rnrs)
	(core conditions)
	(sagittarius)
	(rename (srfi :64)
		(test-equal s64:test-equal)
		(test-assert s64:test-assert)
		(test-error s64:test-error)))

(define-syntax test-equal
  (syntax-rules ()
    ((_ name expected expr)
     (s64:test-equal name expected (call/prompt (lambda () expr))))
    ((_ expected expr)
     (test-equal 'expr expected expr))))

(define-syntax test-assert
  (syntax-rules ()
    ((_ name expr)
     (s64:test-assert name (call/prompt (lambda () expr))))
    ((_ expr)
     (test-assert 'expr expr))))

(define-syntax test-error
  (syntax-rules ()
    ((_ pred expr)
     (s64:test-error pred (call/prompt (lambda () expr))))
    ((_ expr)
     (test-error values expr))))

(test-begin "continuation barrier")

(define-syntax with-cc-variants
  (lambda (x)
    (syntax-case x ()
      ((k expr ...)
       (with-syntax ((call/cc (datum->syntax #'k 'call/cc))
		     (call-with-current-continuation
		      (datum->syntax #'k 'call-with-current-continuation)))
	 #'(begin
	     (define (a-test call/cc call-with-current-continuation) expr ...)
	     (a-test call/cc call-with-current-continuation)
	     (a-test call/delim-cc call-with-delimited-current-continuation)))))))
	     
(with-cc-variants
 (test-equal 103 (call-with-continuation-barrier
		  (lambda ()
		    (call/cc
		     (lambda (k)
		       (+ 100 (k 103)))))))

 (test-equal 104 (call/cc
		  (lambda (k)
		    (call-with-continuation-barrier
		     (lambda ()
		       (+ 100 (k 104)))))))
 
 (test-equal 112 (call-with-current-continuation
		  (lambda (k)
		    (call-with-continuation-barrier
		     (lambda ()
		       (call-with-continuation-prompt
			(lambda ()
			  (k 112))))))))

 (test-equal 'ok
	     (call/cc
	      (lambda (k)
		(call-with-continuation-barrier
		 (lambda ()
		   (k 'ok))))))
 )

(test-equal '((1 3 5) . 11)
	    (let ([res '()])
              (define put!
		(lambda (obj)
		  (set! res (cons obj res))))
              (define result
		(lambda ()
		  (reverse res)))
              (define val
		(call-with-continuation-prompt
		 (lambda ()
		   (+ 1
                      (call-with-composable-continuation
                       (lambda (k)
			 (call-with-continuation-barrier
			  (lambda ()
			    (dynamic-wind
				(lambda () (put! 1))
				(lambda ()
				  (put! (k 2))
				  10)
				(lambda () (put! 5)))))))))))
              (cons (result) val)))

(print "error cases")
(test-error continuation-violation?
	    (call-with-continuation-barrier
	     (lambda ()
	       (call/comp values))))

(with-cc-variants
 (test-assert (continuation?
	       (call-with-continuation-barrier
		(lambda ()
		  (call/cc values)))))
 (test-error continuation-violation?
	     ((call-with-continuation-barrier
	       (lambda ()
		 (call/cc values))))))

(test-end)
