(define (test)
  (lambda (a)
    (lambda b
      (cond ((null? b)
	     => (lambda (t)
		  (or a
		      t)))
	    (else
	     (display 'hoge))))))
(display (((test) #f)))
  