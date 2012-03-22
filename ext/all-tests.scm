;; all tests for extensions
(add-load-path ".")
(cond-expand
 (sagittarius.windows
  (add-dynamic-load-path "../build/modules"))
 (else
  (add-dynamic-load-path "../build")))

(import (rnrs) (util file))
(let* ((files (find-files "." :pattern "^test.scm"))
       (thunks (map (lambda (file) (lambda () (load file))) files)))
  (for-each (lambda (file thunk)
	      (print file)
	      (guard (e (#t
			 (print (describe-condition e))))
		(thunk))
	      (newline))
	    files thunks))