;; -*- scheme -*-

;; pre setup for testing
(define-constant config-file ".sagittarius-sitetestrc")
(define config #f)
(if (file-exists? config-file)
    (call-with-input-file config-file
      ;; the first line is path
      (lambda (p)
	(let ((line (get-line p)))
	  (unless (eof-object? line)
	    (set! config line)
	    (do ((path (get-line p) (get-line p)))
		((eof-object? path) line)
	      (add-load-path path))))))
    (begin
      (add-load-path "./test")
      ;; now we are using multithreading test
      (add-load-path "./ext/threads")
      (add-load-path "./ext/crypto")
      (add-load-path "./ext/socket")
      #f))

(import (rnrs)
	(clos user)
	(core errors)
	;;(srfi :18 multithreading)
	(sagittarius threads)
	(sagittarius control)
	;; for resetting
	(srfi :64 testing)
	(util file)
	(sagittarius io)
	(scheme load)
	;; well, using this before testing huh?
	(util concurrent)
	(sagittarius vm)
	(pp))


(define-constant max-promise (cpu-count))

(define tests-executor (make-thread-pool-executor max-promise))
(define (make-promise proc)
  (let ((f (make-executor-future proc)))
    (execute-future! tests-executor f)
    f))

(cond-expand
 (sagittarius.os.windows
  (define-constant path "test\\tests"))
 (else
  (define-constant path "test/tests")))

(define (debug . args)
  (for-each (lambda (arg) (display arg (current-error-port))) args)
  (newline (current-error-port)))

(define (clear-bindings library)
  ;; dont use this casually
  (let ((table (library-table library)))
    (hashtable-clear! table))
  ;; must be '() otherwise would die
  (library-imported-set! library '())
  (eval '(import (only (sagittarius) import library define-library))
	library))


(define (run-sitelib-tests :key (multithread? #t)
			   (pattern ".scm$"))
  ;; FIXME this is also in ext/all-tests.scm
  (define (test-on-test-end-detail runner)
    (define (%test-write-result1 pair port)
      (display "  " port)
      (display (car pair) port)
      (display ": " port)
      (write (cdr pair) port)
      (newline port))
    (let ((log  (test-runner-aux-value runner))
	  (kind (test-result-ref runner 'result-kind)))
      (when (memq kind '(xpass fail))
	(let* ((results (test-result-alist runner))
	       (source-file (assq 'source-file results))
	       (source-line (assq 'source-line results))
	       (test-name (assq 'test-name results)))
	  (when (or source-file source-line)
	    (if source-file (display (cdr source-file)))
	    (display ":")
	    (if source-line (display (cdr source-line)))
	    (display ":"))
	  (display (if (eq? kind 'xpass) "XPASS" "FAIL"))
	  (when test-name
	    (display " ")(display (cdr test-name)))
	  (newline))
	(let ((expected (test-result-ref runner 'expected-value))
	      (actual   (test-result-ref runner 'actual-value))
	      (exerr (test-result-ref runner 'expected-error))
	      (acerr (test-result-ref runner 'actual-error)))
	  (if exerr
	      (begin
		(display #\tab)(display "expected error: ")
		(display exerr)(newline)
		(display #\tab)(display "  actual error: ")
		(display acerr)(newline))
	      (begin
		(display #\tab)(display "expected value: ")
		(display expected)(newline)
		(display #\tab)(display "  actual value: ")
		(display actual)(newline)))))
      (when (output-port? log)
	(display "Test end:" log)
	(newline log)
	(let loop ((list (test-result-alist runner)))
	  (when (pair? list)
	    (let ((pair (car list)))
	      ;; Write out properties not written out by on-test-begin.
	      (when (not (memq (car pair) '(test-name 
					    source-file 
					    source-line 
					    source-form)))
		(%test-write-result1 pair log))
	      (loop (cdr list))))))))

  (define (test-runner-detail)
    (let ((runner (test-runner-simple)))
      (test-runner-on-test-end! runner test-on-test-end-detail)
      runner))
  (define-syntax with-detailed-runner
    (syntax-rules ()
      ((_ exprs ...)
       (parameterize ((test-runner-factory test-runner-detail))
	 (parameterize ((test-runner-current (test-runner-create)))
	   exprs ...)))))


  (define (run-tests files)
    ;; might use this in future
    ;; well not a big deal to rewrite but keep it for my sake.
    ;;   (define storage '())
    ;;   (define (push-to-storage thread file)
    ;;     (cond ((assq thread storage) =>
    ;; 	   (lambda (slot)
    ;; 	     (set-cdr! slot (cons file (cdr slot)))))
    ;; 	  (else 
    ;; 	   (set! storage (acons thread (list file) storage)))))
    (define timeout-value (list 'timeout))
    (define (print-results futures)
      (for-each (lambda (f)
		  (guard (e (else (report-error e)))
		    ;; there seems a bug on concurrent test
		    ;; https://github.com/okuoku/yunibase/issues/11
		    ;; it's better to fix it but we don't know what the cuase
		    ;; is yet. so for the time being, wait only certain amout
		    ;; of time. In my experience, 2min would be long enough
		    ;; even those long ones (e.g. RSA key generations)
		    (let ((r (future-get (cdr f) 120 timeout-value)))
		      (cond ((eq? r timeout-value)
			     (print "FAIL: Execution timeout on " (car f))
			     (future-cancel (cdr f)))
			    (else
			     (print r))))))
		(reverse! futures)))
    (let loop ((files files) (futures '()))
      (cond ((null? files) 
	     (print-results futures)
	     (shutdown-executor! tests-executor))
	    ((executor-available? tests-executor)
	     (let ((file (car files)))
	       (loop (cdr files) 
		     (acons file
			    (make-promise
			     (lambda ()
			       (with-detailed-runner
				(with-output-to-string 
				  (lambda ()
				    ;; (push-to-storage (current-thread) file)
				    (clear-bindings (current-library))
				    (load file)
				    (test-runner-reset (test-runner-get)))))))
			    futures))))
	    (else
	     (print-results futures)
	     (loop files '())))))

  (let ((files (find-files (or config path) :pattern pattern)))
    (if (and multithread? 
	     (or (> (cpu-count) 1)
		 (begin 
		   (print "Only one CPU, disabling multithreading tests")
		   #f)))
	(run-tests files)
	(with-detailed-runner
	 (let ((thunks (map (^f
			     (^()
			       (load f (environment '(rnrs) '(sagittarius)))
			       (test-runner-reset (test-runner-get))))
			    files)))
	   (for-each (lambda (file thunk) (thunk) (print))
		     files thunks))))))
