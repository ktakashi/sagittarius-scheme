;; all tests for extensions

(add-load-path ".")
(add-load-path "../lib")
(add-load-path "../sitelib")
(cond-expand
 (sagittarius.os.windows
  (add-dynamic-load-path "../build/modules"))
 (else
  (add-dynamic-load-path "../build")))

(import (rnrs) (util file) (core errors) (srfi :39 parameters)
	(srfi :64 testing))

(define-constant resource-file ".sagittarius-exttestrc")
(define search-path #f)
(if (file-exists? resource-file)
    (call-with-input-file resource-file
      (lambda (p)
	(let ((line (get-line p)))
	  (unless (eof-object? line)
	    (set! search-path line)
	    (do ((path (get-line p) (get-line p)))
		((eof-object? path))
	      (add-load-path path))))))
    ;; to avoid to use installed time library. for ext/thread
    (add-load-path "./time"))
(let* ((files (find-files (or search-path ".") :pattern "^test.scm$"))
       (thunks (map (lambda (file) (lambda () (load file))) files)))
  (for-each (lambda (file thunk)
	      (parameterize ((test-runner-current (test-runner-create)))
		(guard (e (#t (print (describe-condition e))))
		  (thunk)))
	      (newline))
	    files thunks))