(import (rnrs)
	(text json)
	(text json schema)
	(text json validator)
	(util file)
	(srfi :133)
	(chibi test))

;; This test requires external git repository, so runs only on CI
;; environment using separated CMake command.

;; TODO make http server with port 1234 to retrieve remote schema.

(define data-directory "JSON-Schema-Test-Suite/tests/draft7/")
(define files
  (find-files data-directory :recursive #f :pattern "\\.json$"))

;; FIXME copy&paste...
(define (key=? key) (lambda (e) (and (pair? e) (string=? (car e) key) e)))
(define value-of
  (case-lambda
   ((key schema) (value-of key schema #f))
   ((key schema default)
    (cond ((vector-any (key=? key) schema) => cdr)
	  (else default)))))

(define (run-schema-tests file)
  (define (run-schema-test json)
    (define description (value-of "description" json))
    (define schema (value-of "schema" json))
    (define tests (value-of "tests" json))
    (define (run-validator validator test)
      (define test-description (value-of "description" test))
      (define data (value-of "data" test))
      (define valid? (value-of "valid" test))
      (test-equal eqv? test-description valid? (validate-json validator data)))
    (test-group description
      (let ((validator (json-schema->json-validator schema)))
	(for-each (lambda (test) (run-validator validator test)) tests))))
  (test-group file
    (for-each run-schema-test (call-with-input-file file json-read))))

(test-begin "JSON Schema test")

(for-each run-schema-tests files)

(test-end)
