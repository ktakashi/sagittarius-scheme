(add-load-path "./lib")
(add-load-path "./sitelib")
(add-load-path "./ext/time")
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

;; load test resource
(define (load-test-resource resource)
  (if (file-exists? resource)
      (call-with-input-file resource
        (lambda (p) 
          (do ((line (get-line p) (get-line p)))
              ((eof-object? line) #t)
            (add-load-path line))))
      #f))
(or (load-test-resource ".sagittarius-r6rstestrc")
    (add-load-path "./test/r6rs-test-suite"))

(or (load-test-resource ".sagittarius-r7rstestrc")
    (begin (add-load-path "./test/r7rs-tests")
           (add-load-path "./ext/crypto")))

(define (main args)  
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
      (load "./test/r6rs-test-suite/tests/r6rs/run.sps")
      (flush-output-port (current-output-port)))
    (define (r7rs-test)
      ;; for R7RS test
      ;; prepare for process-context
      ;; R7RS now depends alot of extension libraries...
      (setenv "R7RS_TEST" "OK")
      (print "testing R7RS tests")
      (flush-output-port (current-output-port))
      (load "./test/r7rs-tests/tests/r7rs/run.scm")
      (flush-output-port (current-output-port))
      (load "./test/r7rs-tests/r7rs-tests.scm")
      (flush-output-port (current-output-port)))

    (define (sitelib-test)
      ;; for sitelib
      (print "testing sitelib")
      (flush-output-port (current-output-port))
      (load "./test/tests.scm")
      (flush-output-port (current-output-port)))
    (define (ext-test)
      ;; for extensions
      (print "testing extensions")
      (flush-output-port (current-output-port))
      (set-current-directory "ext")
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
	  