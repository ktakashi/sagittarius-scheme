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
	;; child thread can not access to default parameter value,
	;; so parent must import this.
	(srfi :64 testing)
	;; so is rfc uuid...
	(rfc uuid)
	;; so is text sre
	(text sre)
	;; so is rfc tls
	(rfc tls)
	;; so is rfc http
	(rfc http)
	(util file)
	(sagittarius io)
	(scheme load)
	;; well, using this before testing huh?
	(util concurrent)
	(sagittarius vm)
	)

;; ;; simple future
;; (define-class <promise> ()
;;   ((future :init-keyword :future)
;;    (thread :init-keyword :thread :accessor promise-thread)))
;;  
;; (define-class <future> ()
;;   ((promise :accessor future-promise)))
;; 
;; (define-method promise-specific ((p <promise>) o)
;;   (thread-specific-set! (promise-thread p) o))
;; (define-method promise-specific ((p <promise>))
;;   (thread-specific (promise-thread p)))
;; 
;;  
;; (define (make-promise proc . args)
;;   (let* ((thunk (lambda () (apply proc args)))
;;   (thread (make-thread thunk))
;;   (future (make <future> :thunk thunk))
;;   (promise (make <promise> :future future :thread thread)))
;;     (future-promise future promise)
;;     (thread-start! thread)
;;     promise))
;;  
;; (define-method get-future ((p <promise>))
;;   (slot-ref p 'future))
;;  
;; (define-method get ((f <future>))
;;   (let ((promise (future-promise f)))
;;     (guard (e (#t e))
;;       (thread-join! (promise-thread promise)))))
;;  
;; (define-method finished? ((f <future>))
;;   (let ((promise (future-promise f)))
;;     (eq? (thread-state (promise-thread promise)) 'terminated)))
;; 
;; ;; Do we even want to know if the promise is finished or not?
;; ;; I don't think this does not affect any performance.
;; (define (get-finished-future promises)
;;   (let loop ((p promises)
;; 	     (r '()))
;;     (if (null? p)
;; 	(reverse! r)
;; 	(let ((f (get-future (car p))))
;; 	  (cond ((finished?  f)
;; 		 (let ((result (get f)))
;; 		   (cond ((uncaught-exception? result)
;; 			  (print "FAILED WITH ERROR:")
;; 			  (print (promise-specific (car p)))
;; 			  (print (describe-condition
;; 				  (uncaught-exception-reason result))))
;; 			 (else
;; 			  (print (get f)))))
;; 		 (loop (cdr p) r))
;; 		(else
;; 		 (loop (cdr p) (cons (car p) r))))))))

(define-constant max-promise 5)

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

;; parameters problem for (rfc http). it's required in (net oauth) and
;; the library is also tested. so these 2 dependency causes parameters
;; problem like (srfi :64 testing) therefore we need to import it here.
;; it's a bit awkward solution.
(import (rfc http))

(define (debug . args)
  (for-each (lambda (arg) (display arg (current-error-port))) args)
  (newline (current-error-port)))

(define (clear-bindings library)
  ;; dont use this casually
  (let ((table (library-table library)))
    (hashtable-clear! table)))

(define (run-tests files)
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
				(clear-bindings (current-library))
				(load file)
				(test-runner-reset (test-runner-get))))))
			 futures))))
	  (else
	   (print-results futures)
	   (loop files '())))))

(define (run-sitelib-tests :optional (multithread? #t))
  (let ((files (find-files (or config path) :pattern ".scm$")))
    (if multithread?
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
