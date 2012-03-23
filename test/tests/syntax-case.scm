;; -*- mode: scheme; coding: utf-8; -*-
(library (issue :10)
    (export loop-10)
    (import (rnrs))
  (define-syntax loop-10
    (lambda (x)
      (syntax-case x ()
        [(k e ...)
         (with-syntax
             ([break (datum->syntax #'k 'break)])
           #'(call-with-current-continuation
              (lambda (break)
                (let f () e ... (f)))))])))
)
(import (rnrs)
	(issue :10)
	(srfi :64 testing))

;; from mosh issue 138
(define-syntax doit
  (lambda (stx)
    (syntax-case stx ()
      ((_ a b c)
       (for-all identifier? #'(a b c))
       #'(begin 'yes))
      (_
       #'(begin 'no)))))

;; issue 7
;; the same as r6rs test suites' syntax-case.sls 
(define-syntax loop
  (lambda (x)
    (syntax-case x ()
      [(k e ...)
       (with-syntax
	   ([break (datum->syntax #'k 'break)])
	 #'(call-with-current-continuation
	    (lambda (break)
	      (let f () e ... (f)))))])))
;; because of the defferent behaviour of macro expander
;; when the expression is in macro such as test-equal,
;; we can not test properly. so put it outside of macro
;; and check the return value.
(define (loop-test)
  (let ((n 3) (ls '()))
    (loop
     (if (= n 0) (break ls))
     (set! ls (cons 'a ls))
     (set! n (- n 1)))))

(test-begin "syntax-case issues")
(test-equal "doit yes"
	    'yes
	    (doit x y z))
(test-equal "doit no"
	    'no
	    (doit x 1 z))

(test-equal "loop"
	    '(a a a)
	    (loop-test))

(test-equal "loop-10" '(a a a)
	    (let ((n 3) (ls '()))
	      (loop-10
	       (if (= n 0) (break ls))
	       (set! ls (cons 'a ls))
	       (set! n (- n 1)))))

(test-end)