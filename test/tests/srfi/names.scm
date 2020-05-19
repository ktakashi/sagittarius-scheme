(import (rnrs)
	(rnrs eval)
	(util file)
	(util port)
	(sagittarius)
	(srfi :64))

(test-begin "SRFI library names")

(define srfi-data (string-append (current-directory) "/test/data/srfi-data.scm"))

(define (test-srfi-name data)
  (define number (cadr (assq 'number data)))
  (define library-name (cond ((assq 'library-name data) => cadr)
			     (else #f)))
  (define (supported? n)
    (guard (e (else #f))
      (eval `(import (srfi ,n)) (environment '(sagittarius)))))
  (when (and library-name (supported? number))
    ;; check if we support srfi itself
    (let ((name (read (open-string-input-port
		       (format "(srfi :~a ~a)" number library-name)))))
      (test-assert name (eval `(import ,name) (environment '(sagittarius)))))))

(call-with-input-file srfi-data
  (lambda (p)
    (port-for-each test-srfi-name (lambda () (read p)))))

(test-end)
