(define prob
  (lambda (form rename compare)
    (if (if (pair? form)
	    (let ((temp (cdr form)))
	      (if (pair? temp)
		  (null? (cdr temp))
		  #f))
	    #f)
	(list (rename 'begin)
	      (list (rename 'display)
		    (list (rename 'quote)
			  (car (cdr form))))
	      (list (rename 'newline)))
	(begin (error 'syntax-rules "no expansion for" (unwrap-syntax (car form)))))))

(display (prob '(test a) (lambda (s) s) (lambda (a b) #t)))

#|
(display 1.539989614439558e-36)(newline)
(display 5.447603722011605e-270)(newline)
(let ((a 5.447603722011605e-270)
      (b 1.539989614439558e-36))
  (write/ss a)(newline)
  (write/ss b)(newline))

(display (bytevector-ieee-double-ref #vu8(1 2 3 4 5 6 7 8) 0 'little))
|#