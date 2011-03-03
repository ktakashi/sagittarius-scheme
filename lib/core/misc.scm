;; -*- scheme -*-
(library (core misc)
    (export unique-id-list?)
    (import null
	    (sagittarius)
	    (core base))
  (define (unique-id-list? lst)
    (and (list? lst)
	 (not (let loop ((lst lst))
		(and (pair? lst)
		     (or (not (symbol? (car lst)))
			 (memq (car lst) (cdr lst))
			 (loop (cdr lst))))))))
)