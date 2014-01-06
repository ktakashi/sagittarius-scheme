(add-load-path "./lib")
(add-load-path "./sitelib")
(add-load-path "./ext/time")
#!read-macro=sagittarius/regex
(import (srfi :0)
	(srfi :13)
	(srfi :37)
	(srfi :39)
	(rnrs)
	(sagittarius)
	(sagittarius io)
	(sagittarius regex))

;; r7rs tests also use dynamic modules
(cond-expand
 (sagittarius.os.windows
  (add-dynamic-load-path "./build/modules"))
 (else
  (add-dynamic-load-path "./build")))

;; define build directory fullpath
(define build-directory-path
  (string-append (current-directory) "/build"))

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

(define (%call-test-case thunk error-regex)
  (define no-error? #t)
  (define stdout (current-output-port))
  (define buffer (open-output-string))
  (define (error-check-port)
    (define (write! str start count)
      (when no-error?
	(put-string buffer str start count)
	(when (string-contains str "\n")
	  (let ((str (extract-output-string buffer)))
	    (when (error-regex str)
	      (set! no-error? #f)))))
      (put-string stdout str start count)
      count)
    (make-custom-textual-output-port "error check port" write! #f #f #f))
  (with-output-to-port (error-check-port) thunk)
  (flush-output-port stdout)
  no-error?)

(define-syntax call-test-case
  (syntax-rules ()
    ((_ expr regex)
     (%call-test-case (lambda () expr) regex))))

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
      (call-test-case (load "./test/r6rs-test-suite/tests/r6rs/run.sps")
		      #/tests failed/))
    (define (r7rs-test)
      ;; for R7RS test
      ;; prepare for process-context
      ;; R7RS now depends alot of extension libraries...
      (setenv "R7RS_TEST" "OK")
      (print "testing R7RS tests")
      (flush-output-port (current-output-port))
      (and (call-test-case (load "./test/r7rs-tests/tests/r7rs/run.scm")
			   #/tests failed/)
	   (call-test-case (load "./test/r7rs-tests/r7rs-tests.scm")
			   #/failure/)))

    (define (sitelib-test :optional (multithread? #t))
      ;; for sitelib
      (print "testing sitelib")
      (flush-output-port (current-output-port))
      (load "./test/tests.scm")
      (call-test-case (run-sitelib-tests multithread?)
		      #/FAIL/))
    (define (ext-test)
      ;; for extensions
      (print "testing extensions")
      (flush-output-port (current-output-port))
      (parameterize ((current-directory "ext"))
	(call-test-case (load "./all-tests.scm")
			#/FAIL/)))

    (let ((r (if (null? test)
		 (list (r6rs-test)
		       (r7rs-test)
		       (sitelib-test)
		       (ext-test))
		 (map (lambda (test)
			(case (string->symbol test)
			  ((r6rs) (r6rs-test))
			  ((r7rs) (r7rs-test))
			  ((sitelib sitelib-m) (sitelib-test))
			  ((sitelib-s) (sitelib-test #f))
			  ((ext) (ext-test))
			  (else
			   (error 'run-test
				  "unknown test" test))))
		      (reverse! test))
		 )))
      (if (for-all (lambda (b) b) r)
	  0
	  1))))
