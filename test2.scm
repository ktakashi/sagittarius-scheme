#|
(import (sagittarius vm profiler))
(import (core syntax pattern)
	(core base)
	(pp)
	(sagittarius vm))

(profiler-start)
(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((literal (cadr form))
	   (clauses (cddr form)))
       (let ((r 
	      `(er-macro-transformer
		(lambda (,(rename 'form) rename compare)
		  ,@(map (lambda (clause)
			  (let ((pat (car clause))
				(template (cadr clause)))
			    (check-pattern pat literal rename compare)
			    (let ((ranks (collect-vars-ranks pat literal 0 '() rename compare)))
			      (print ranks)
			      (generate-match pat literal rename compare (rename 'form)))))
			clauses)))))
	 (pretty-print (unwrap-syntax r))
	 r)))))

(define-syntax check-pattern
  (syntax-rules ()
    ((_ a b ... (c d ...))
     (list 'a 'c))
    ((_ a b c d)
     (list 'a 'b 'c))))

(print (check-pattern 'a 'b '(c d)))
(profiler-stop)

(profiler-show #f 'time 50)
|#
(let loop ((i 0))
  (unless (= i 10)
    (loop (+ i 1))))