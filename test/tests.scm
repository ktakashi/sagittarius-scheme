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
    (hashtable-clear! table)))

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
  (define (print-results futures)
    (for-each (lambda (f)
		(guard (e ((uncaught-exception? e)
			   (print (describe-condition 
				   (uncaught-exception-reason e))))
			  (else (print e)))
		  (print (future-get f))))
	      (reverse! futures)))
  (let loop ((files files) (futures '()))
    (cond ((null? files) (print-results futures))
	  ((executor-available? tests-executor)
	   (let ((file (car files)))
	     (loop (cdr files) 
		   (cons (make-promise
			  (lambda ()
			    (with-output-to-string 
			      (lambda ()
				;; (push-to-storage (current-thread) file)
				(clear-bindings (current-library))
				(load file)
				(test-runner-reset (test-runner-get))))))
			 futures))))
	  (else
	   (print-results futures)
	   (loop files '())))))

(define (run-sitelib-tests :optional (multithread? #t))
  (let ((files (find-files (or config path) :pattern ".scm$")))
    (if (and multithread? 
	     (or (> (cpu-count) 1)
		 (begin (print "Only one CPU, disabling multithreading tests")
			#f)))
	(run-tests files)
	(let ((thunks (map (^f
			     (^()
			       (load f (environment '(rnrs) '(sagittarius)))
			       (test-runner-reset (test-runner-get))))
			   files)))
	  (for-each (lambda (file thunk)
		      (thunk)
		      #;
		      (guard (e (#t (print (describe-condition e))))
			)
		      (print))
		    files thunks)))))
