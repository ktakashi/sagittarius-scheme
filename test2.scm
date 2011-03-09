(import (rnrs) (sagittarius vm))

(define-syntax fuga
  (lambda (x)
    (syntax-case x ()
      ((_ r ...)
       (begin
	 (display x)(newline)
	 (syntax
	  (list '(r (r ...)) ...)))))))

(define-syntax hoge
  (syntax-rules ()
    ((_ r ...)
     (list '(r (r ...)) ...))))

;(compile-p2 '(syntax (list 1 2 3)))
;(compile-p2 '(fuga 1 2 3))

#;(define-syntax hoge
  (lambda (x)
    (let ((dict (make-eq-hashtable)))
      (let ((form (car x))
	    (rename (lambda (s) (er-rename s (cdr x) dict)))
	    (compare (lambda (a b)
		       (or (eq? a b)
			   (cond ((and (symbol? a) (identifier? b))
				  (eq? (rename a) b))
				 ((and (identifier? a) (symbol? b)) 
				  (eq? a (rename b)))
				 (else #f))))))
	(if (if (pair? form)
		(letrec ((loop (lambda (l)
				 (if (null? l)
				     #t
				     (if (pair? l)
					 (loop (cdr l)) #f)))))
		  (loop (cdr form)))
		#f)
	    (begin
	      (display x)(newline)
	      (list 'syntax (cons 'list (map (lambda (control5) (list 'quote (list control5 (cdr form)))) (cdr form)))))
	    (begin (error 'syntax-case no expansion for (unwrap-syntax (car form)))))))))
;(display (hoge 1 2 3))

;(display (syntax (list 1 2 3)))  ;; -> (1 2 3) ... wrong...
;(newline)
(display (fuga 1 2 3))(newline)
#;(display (unwrap-syntax (%macroexpand
(fuga 1 2 3)
)))


#;(define-syntax show-vars
  (lambda (x)
    (syntax-case x ()
      [(_) (syntax 'shown)]
      [(_ e1 e2 ...) 
       (syntax (begin (display 'e1) (display "->") (display e1) (newline) (show-vars e2 ...)))])))
#;(display
(let ((i 0) (j 1) (k 2)) (show-vars i j k))
)
;(newline)

;(display (syntax (let ((it c)) (if it b a))))

#;(display (unwrap-syntax (%macroexpand

(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      [(k c b ...)
       (with-syntax 
	([it (datum->syntax (syntax k) 'it)])
	(syntax (let ((it c))
		  (if it b ...))))])))
)))
