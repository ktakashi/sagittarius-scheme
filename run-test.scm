(add-load-path "./lib")
(add-load-path "./sitelib")
(import (srfi :0)
	(srfi :37)
	(rnrs)
	(sagittarius))

;; r7rs tests also use dynamic modules
(cond-expand
       (sagittarius.os.windows
	(add-dynamic-load-path "./build/modules"))
       (else
	(add-dynamic-load-path "./build")))

(let ((args (command-line)))
  (let-values (((test) (args-fold (cdr args)
				  '()
				  (lambda (option name arg . seed)
				    (assertion-violation 'run-test
							 "Unrecognized option"
							 name))
				  (lambda (operand test)
				    (values (cons operand test)))
				  '())))
    (define (r6rs-test)
      ;; for R6RS test suites
      (print "testing R6RS test suite")
      (flush-output-port (current-output-port))
      (add-load-path "./test/r6rs-test-suite")
      (load "./test/r6rs-test-suite/tests/r6rs/run.sps")
      (flush-output-port (current-output-port)))
    (define (r7rs-test)
      ;; for R7RS test
      ;; prepare for process-context
      ;; R7RS now depends alot of extension libraries...
      (setenv "R7RS_TEST" "OK")
      (print "testing R7RS tests")
      (flush-output-port (current-output-port))
      (add-load-path "./test/r7rs-tests")
      ;; for srfi 19
      (add-load-path "./ext/time")
      (load "./test/r7rs-tests/tests/r7rs/run.scm")
      (flush-output-port (current-output-port)))

    (define (sitelib-test)
      ;; for sitelib
      (print "testing sitelib")
      (flush-output-port (current-output-port))
      (add-load-path "./test")
      ;; for (math)
      (add-load-path "./ext/crypto")
      (load "./test/tests.scm")
      (flush-output-port (current-output-port)))
    (define (ext-test)
      ;; for extensions
      (print "testing extensions")
      (flush-output-port (current-output-port))
      (set-current-directory "ext")
      (add-load-path "../lib")
      (add-load-path "../sitelib")
      (cond-expand
       (sagittarius.os.windows
	;; all-tests adds dynamic-load-path however it's for non windows environment
	(add-dynamic-load-path "../build/modules"))
       (else
	#t))
      (load "./all-tests.scm")
      (set-current-directory "..")
      (flush-output-port (current-output-port)))

    (if (null? test)
	(begin
	  (r6rs-test)
	  (r7rs-test)
	  (sitelib-test)
	  (ext-test))
	(for-each (lambda (test)
		    (case (string->symbol test)
		      ((r6rs) (r6rs-test))
		      ((r7rs) (r7rs-test))
		      ((sitelib) (sitelib-test))
		      ((ext) (ext-test))
		      (else
		       (error 'run-test
			      "unknown test" test))))
		  (reverse! test)))))
	  