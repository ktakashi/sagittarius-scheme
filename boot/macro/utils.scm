;; This needs to be moved to C++
(define (syntax-violation who message form . maybe-subform)
  (newline)
  (display "Syntax violation: ")
  (let ((who (if who
		 who
		 (cond ((identifier? form)
			(syntax->datum form))
		       ((and (list? form)
			     (identifier? (car form)))
			(syntax->datum (car form)))
		       (else ""))))
	(subform (cond ((null? maybe-subform) #f)
		       ((and (pair? maybe-subform)
			     (null? (cdr maybe-subform)))
			(car maybe-subform))
		       (else (assertion-violation 'syntax-violation
						  "Invalid subform in syntax violation"
						  maybe-subform)))))
    (display who)
    (newline)
    (newline)
    (display message)
    (newline)
    (newline)
    (if subform
	(begin (display "Subform: ")
	       (pretty-print (syntax-debug subform))
	       (newline)))
    (display "Form: ")
    (pretty-print (syntax-debug form))
    (newline)
    (display "Trace: ")
    (newline)
    (newline)
    (for-each (lambda (exp)
		(display "  ")
		(pretty-print (syntax-debug exp))
		(newline))
	      *trace*)
    (error 'syntax-violation "Integrate with host error handling here")))


(define reverse!
  (lambda (lst)
    (if (pair? lst)
	(let loop ((first lst)
		   (result '()))
	  (if (pair? first)
	      (let ((next (cdr first)))
		(set-cdr! first result)
		(loop next first))
	      result))
	lst)))
(define last-pair
  (lambda (l)
    (or (pair? l)
	(error "pair required: " l))
    (let loop ((l l))
      (let ((cd (cdr l)))
	(if (pair? cd)
	    (loop (cdr l))
	    l)))))
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End
