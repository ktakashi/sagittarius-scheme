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
;; disabled for pattern-match-lambda. this is written in
;; portable R7RS macro and the test case is for affinity
;; between R6RS and R7RS. latter case is much lower priority
;; for me.

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
    (test-equal "should be 3" 3 (foo n (values n)))))

;; extracted from
;; https://github.com/SaitoAtsushi/pattern-match-lambda
(let ()
  (define-syntax if-identifier
    (syntax-rules ()
      ((_ condition seq alt)
       (let-syntax ((foo (syntax-rules () ((_) seq))))
	 (let-syntax ((test (syntax-rules ()
			      ((_ condition) (foo))
			      ((_ foo) alt))))
	   (test foo))))))
  
  (define-syntax %if-match
    (syntax-rules ()
      ((_  (p . r) e seq alt)
       (let ((temp e))
	 (if (pair? temp)
	     (%if-match p (car temp)
			(%if-match r (cdr temp) seq alt)
			alt)
	     (alt))))
      ((_ () e seq alt)
       (if (null? e) seq (alt)))
      ((_ p e seq alt)
       (if-identifier p
		      (if (equal? 'p e) seq (alt))
		      (if (equal? p e) seq (alt))))))

  (define-syntax if-match
    (syntax-rules ()
      ((_ pattern lst seq alt)
       (let ((alt-thunk (lambda() alt)))
	 (%if-match pattern lst seq alt-thunk)))))

  (define foo
    (lambda lst
      (if-match (1 2 3) lst 'ok 'ng)))
  (test-equal "identifier renaming" 'ok (foo 1 2 3))
  )
(test-end)
