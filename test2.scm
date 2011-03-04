(import (rnrs) (sagittarius vm))

(define-syntax fuga
  (lambda (x)
    (syntax-case x ()
      ((_ r ...)
       (begin
	 (syntax
	  (list '(r (r ...)) ...)))))))
;(compile-p2 '(syntax (list 1 2 3)))
;(compile-p2 '(fuga 1 2 3))

(display (syntax (list 1 2 3)))  ;; -> (1 2 3) ... wrong...
(newline)
(display (fuga 1 2 3))(newline)

(define-syntax show-vars
  (lambda (x)
    (syntax-case x ()
      [(_) (syntax 'shown)]
      [(_ e1 e2 ...) 
       (syntax (begin (display 'e1) (display "->") (display e1) (newline) (show-vars e2 ...)))])))
(display
(let ((i 0) (j 1) (k 2)) (show-vars i j k))
)
(newline)

(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      [(k c b ...)
       (with-syntax 
	([it (datum->syntax (syntax k) 'it)])
	(syntax (let ((it c))
		  (if it b ...))))])))
(display with-syntax)(newline)
(compile-p2
'(let ((i 4))
  (aif (memv i '(2 4 6 8))
       (display (car it))
       #f)))
