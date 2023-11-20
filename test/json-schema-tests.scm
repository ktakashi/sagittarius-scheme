#!read-macro=sagittarius/regex
(import (rnrs)
	(text json)
	(text json schema)
	(text json validator)
	(util file)
	(net server)
	(sagittarius socket)
	(sagittarius regex)
	(util file)
	(srfi :1)
	(srfi :39)
	(srfi :133)
	(chibi test))

;; This test requires external git repository, so runs only on CI
;; environment using separated CMake command.
(define remote-directory "JSON-Schema-Test-Suite/remotes")
(define (http-handler server socket)
  (define in/out (transcoded-port (socket-port socket) (native-transcoder)))
  (define (get-content path)
    (let ((file (if (string=? path "/folderInteger.json")
		    (string-append remote-directory "/folder" path)
		    (string-append remote-directory path))))
      (and (file-exists? file)
	   (file->string file))))
  (let ((line (get-line in/out)))
    (cond ((#/GET (.+?) HTTP/ line) =>
	   (lambda (m)
	     (cond ((get-content (m 1)) =>
		    (lambda (content)
		      (put-string in/out "HTTP/1.1 200 OK\r\n")
		      (put-string in/out "Content-Length: ")
		      (put-string in/out (number->string (string-length content)))
		      (put-string in/out "\r\n")
		      (put-string in/out "Content-Type: application/json\r\n\r\n")
		      (put-string in/out content)))
		   (else (put-string in/out "HTTP/1.1 404 Not Found\r\n\r\n")))))
	  (else (put-string in/out "HTTP/1.1 404 Not Found\r\n\r\n")))))
(define server (make-simple-server "1234" http-handler))

(define data-directories
  '("JSON-Schema-Test-Suite/tests/draft7/"
    "JSON-Schema-Test-Suite/tests/draft2019-09/"
    "JSON-Schema-Test-Suite/tests/draft2020-12/"
    ))
(define core-files
  (append-map (lambda (dir) (find-files dir :recursive #f :pattern "\\.json$"))
	      data-directories))
(define format-files
  (append-map (lambda (dir)
		(find-files (build-path* dir "optional" "format")
			    :recursive #f :pattern "\\.json$"))
	      data-directories))

;; the *will not be supported/fixed* test names
(define *known-incompatible-tests* '())
(define *ignoring-scenarios*
  '())

(define *ignoring-files*
  '(
    ;; We don't support custom meta schema, too much hastle
    "JSON-Schema-Test-Suite/tests/draft2019-09/vocabulary.json"
    "JSON-Schema-Test-Suite/tests/draft2020-12/vocabulary.json"

    ;; We will support idn-hostname later, maybe...
    "JSON-Schema-Test-Suite/tests/draft7/optional/format/idn-hostname.json"
    "JSON-Schema-Test-Suite/tests/draft2019-09/optional/format/idn-hostname.json"
    "JSON-Schema-Test-Suite/tests/draft2020-12/optional/format/idn-hostname.json"
    ))

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
      (unless (member test-description *known-incompatible-tests*)
	(test-equal eqv? test-description valid? (validate-json validator data))))
    (unless (member description *ignoring-scenarios*)
      (test-group description
	(let ((validator (json-schema->json-validator schema)))
	  (for-each (lambda (test) (run-validator validator test)) tests)))))
  (unless (member file *ignoring-files*)
    (test-group file
      (for-each run-schema-test (call-with-input-file file json-read)))))

(test-begin "JSON Schema test")

(server-start! server :background #t)
(parameterize ((*json-schema:resolve-external-schema?* #t))
  ;; Seems format must only be optional on core tests
  (parameterize ((*json-schema:validate-format?* #f))
    (for-each run-schema-tests core-files))
  (for-each run-schema-tests format-files)
  )
(server-stop! server)

(newline)
(display "##############################################") (newline)
(display "These test files are skipped") (newline)
(for-each (lambda (test) (display "- ") (display test) (newline))
	  *ignoring-files*)
(display "These test scenarios are skipped") (newline)
(for-each (lambda (test) (display "- ") (display test) (newline))
	  *ignoring-scenarios*)
(display "These test cases are skipped") (newline)
(for-each (lambda (test) (display "- ") (display test) (newline))
	  *known-incompatible-tests*)
(display "##############################################") (newline)
(newline)

(test-end)
(test-exit)

