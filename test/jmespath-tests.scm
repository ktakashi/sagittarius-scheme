(import (rnrs)
	(text json)
	(text json pointer)
	(text json jmespath)
	(util file)
	(except (chibi test) test-error))

(define-syntax test-error
  (syntax-rules ()
    ((_ type? name expr)
     (test-assert name (guard (e ((type? e))) expr)))))
	

(test-begin "JMESPath")

(define files
  (find-files "./jmespath.test/tests" :pattern "\\.json$"))

(define given-pointer (json-pointer "/given"))
(define cases-pointer (json-pointer "/cases"))
(define comment-pointer (json-pointer "/comment"))

(define expr-pointer (json-pointer "/expression"))
(define result-pointer (json-pointer "/result"))
(define error-pointer (json-pointer "/error"))
(define bench-pointer (json-pointer "/bench"))

(define (test-file file)
  (define source (call-with-input-file file json-read))
  (define (run-test source)
    (define (get-description input fallback)
      (let ((comment (comment-pointer input)))
	(if (json-pointer-not-found? comment)
	    fallback
	    (list comment fallback))))
    (define given (given-pointer source))
    (define cases (cases-pointer source))
    (define comment (get-description source file))
    (define (test-case case-expr)
      (define expr (expr-pointer case-expr))
      (define bench (bench-pointer case-expr))
      (define err (error-pointer case-expr))
      (define comment (get-description case-expr expr))
      (cond ((not (json-pointer-not-found? bench))
	     (case (string->symbol bench)
	       ((full) (test-assert comment ((jmespath expr) given)))
	       ((parse) (test-assert comment (jmespath expr)))))
	    ((not (json-pointer-not-found? err))
	     (let ((type (string->symbol err)))
	       (cond ((eq? type 'syntax)
		      (test-error jmespath-parse-error? expr (jmespath expr)))
		     ((memq type '(unknown-function invalid-value))
		      (test-error jmespath-compile-error? expr (jmespath expr)))
		     ;; we don't distinguish arity, type, and value error.
		     (else (test-error jmespath-runtime-error? type ((jmespath expr) given))))))
	    (else
	     (let ((result (result-pointer case-expr)))
	       (test-equal equal? comment result ((jmespath expr) given))))))
    (for-each test-case cases))
  (test-group (path-basename file) (for-each run-test source)))

(for-each test-file files)

(test-end)
(test-exit)
