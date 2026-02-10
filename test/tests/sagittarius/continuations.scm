(import (rnrs)
	(sagittarius continuations)
	(srfi :64))

(test-begin "Continuations")

(test-assert "continuation? (call/cc)"
	     (continuation? (call/cc (lambda (k) k))))
(test-assert "continuation? (call/comp)"
	     (continuation?
	      (call/prompt
	       (lambda ()
		 (call/comp (lambda (k) k))))))
(test-assert "continuation? (call/delimited-cc)"
	     (continuation?
	      (call/prompt
	       (lambda ()
		 (call/delim-cc (lambda (k) k))))))

(test-assert "continuation? (symbol)" (not (continuation? 'a)))
(test-assert "continuation? (symbol)" (not (continuation? (lambda args args))))

(test-assert (continuation-prompt-tag? (default-continuation-prompt-tag)))
(test-assert (continuation-prompt-tag? (make-continuation-prompt-tag)))

(let ((p1 (make-continuation-prompt-tag 'p1)))
  (define (check)
    (let ((k (call-with-continuation-prompt
	      (lambda ()
		((call/delim-cc
		  (lambda (k) (lambda () k))
		  p1)))
	      p1
	      (lambda (x) (display x) (newline) x))))
      (call-with-continuation-prompt
       (lambda () (k (lambda () 'invoked)))
       p1
       (lambda (x) 'outer x))))
  (test-equal 'invoked (check)))

(test-end)
