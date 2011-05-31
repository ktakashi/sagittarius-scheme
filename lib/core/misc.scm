;; -*- scheme -*-
(library (core misc)
    (export unique-id-list?
	    #;safe-length)
    (import (core)
	    (sagittarius)
	    (core base))
  (define (unique-id-list? lst)
    (and (list? lst)
	 (not (let loop ((lst lst))
		(and (pair? lst)
		     (or (not (symbol? (car lst)))
			 (memq (car lst) (cdr lst))
			 (loop (cdr lst))))))))

  #;(define safe-length
    (lambda (lst)
      (let loop ((lst lst) (n 0))
	(if (pair? lst)
	    (loop (cdr lst) (+ n 1))
	    (or (and (null? lst) n) -1)))))
)
