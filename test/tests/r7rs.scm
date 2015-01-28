(import (scheme base) (scheme eval) (srfi 64))

(test-begin "R7RS extra")

;; #95 removing import from (scheme base)
(test-assert "overwrite import syntax" 
	     (eval '(define-library (lib)
		      (export foo)
		      (import (scheme base))
		     (begin
		       (define-syntax import (syntax-rules ()))
		       (define-syntax foo
			 (syntax-rules (import)
			   ((_ import) 'ng)
			   ((_ _) 'ok))))
		     )
		   (environment '(sagittarius))))

;; (lib) is there
(import (lib))
(test-equal "syntax-rules literal comparison" 'ok (foo import))
(let ((r (foo import)))
  (test-equal "syntax-rules literal comparison (symbol)" 'ok r))

;; the same test as contrib tests on r7rs-tests.scm 
;; but wrapped by syntax-case (test-equal in srfi-64)
;; http://saito.hatenablog.jp/entry/2014/03/24/070839
(let ()
  (define-syntax bar
    (syntax-rules ()
      ((_ m body)
       (let ((m 1))
	 (body)))))

  (define-syntax foo
    (syntax-rules ()
      ((_ m body)
       (let ((n 2))
	 (let-syntax ((%body
		       (syntax-rules ()
			 ((_) body))))
	   (bar m %body))))))

  (let ((n 3))
    (test-equal 3 (foo n (values n)))))

(test-end)