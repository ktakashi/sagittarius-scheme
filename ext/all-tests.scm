;; all tests for extensions
(add-load-path ".")
(add-load-path "../lib")
(add-load-path "../sitelib")
(cond-expand
 (sagittarius.os.windows
  (add-dynamic-load-path "../build/modules"))
 (else
  (add-dynamic-load-path "../build")))

(import (rnrs) (util file) (core errors))
;; to avoid to use installed time library. for ext/thread
(add-load-path "./time")
(let* ((files (find-files "." :pattern "^test.scm"))
       (thunks (map (lambda (file) (lambda () (load file))) files)))
  (for-each (lambda (file thunk)
	      (print file)
	      (guard (e (#t
			 (print (describe-condition e))))
		(thunk))
	      (newline))
	    files thunks))