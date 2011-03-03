(import (rnrs))

(define-syntax hoge
  (syntax-rules ()
    ((_ r ...)
     (list '(r (r ...)) ...))))

(define-syntax fuga
  (lambda (x)
    (syntax-case x ()
      ((_ r ...)
       (begin
	 (display x)(newline)
	 (syntax
	  (list '(r (r ...)) ...)))))))
(display (hoge 1 2 3))(newline)
(display (%macroexpand (fuga 1 2 3)))
