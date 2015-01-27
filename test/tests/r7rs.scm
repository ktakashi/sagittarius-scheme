(import (scheme base) (scheme eval) (srfi 64))

(test-begin "R7RS extra")

;; #95 removing import from (scheme base)
;; combining syntax-case based macro and er based macro
;; doesn't work with this case... kinda bug?
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

(test-end)