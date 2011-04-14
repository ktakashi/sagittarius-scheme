;; -*- scheme -*-
(library (core with-syntax)
    (export with-syntax)
    (import null 
	    (sagittarius)
	    (core syntax helper)
	    (core syntax-case))

  #;(define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
        ((_ ((p e0) ...) e1 e2 ...)
         (if (backtrace)
             (syntax (syntax-case (list e0 ...) ()
                       ((p ...) (let () e1 e2 ...))
                       (_ (syntax-violation
                           'with-syntax
                           "value does not match to pattern"
                           '((p e0) ...)))))
             (syntax (syntax-case (list e0 ...) ()
                       ((p ...) (let () e1 e2 ...)))))))))

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