;; -*- scheme -*-
(library (core with-syntax)
    (export with-syntax)
    (import null 
	    (sagittarius)
	    (core syntax-case))
  (define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
        ((_ () e1 e2 ...)
	 (syntax (begin e1 e2 ...)))
        ((_ ((out in)) e1 e2 ...)
	 (syntax (syntax-case in ()
		   (out (begin e1 e2 ...)))))
        ((_ ((out in) ...) e1 e2 ...)
	 (syntax (syntax-case (list in ...) ()
		   ((out ...) (begin e1 e2 ...))))))))
)