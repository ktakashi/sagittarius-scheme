;; -*- scheme -*-

#|
;; multithread version does not work correctly yet.
;; load with multithread is really buggy.
(import (clos user)
	(core errors)
	(srfi :18 multithreading)
	(util file)
	(sagittarius io))

;; simple future
(define-class <promise> ()
  ((future :init-keyword :future)
   (thread :init-keyword :thread :accessor promise-thread)))
 
(define-class <future> ()
  ((promise :accessor future-promise)))
 
(define (make-promise proc . args)
  (let* ((thunk (lambda () (apply proc args)))
  (thread (make-thread thunk))
  (future (make <future> :thunk thunk))
  (promise (make <promise> :future future :thread thread)))
    (future-promise future promise)
    (thread-start! thread)
    promise))
 
(define-method get-future ((p <promise>))
  (slot-ref p 'future))
 
(define-method get ((f <future>))
  (let ((promise (future-promise f)))
    (guard (e (#t e))
      (thread-join! (promise-thread promise)))))
 
(let* ((files (find-files "test/tests" :pattern ".scm"))
	 (promises (map (lambda (file)
			  (make-promise 
			   (lambda (f)
			     (with-output-to-string
			       (lambda ()
				 (load f)))) file))
			files)))
    (for-each (lambda (file p)
		(let ((f (get-future p)))
		  (print file)
		  (let ((result (get f)))
		    (cond ((uncaught-exception? result)
			   (print (uncaught-exception-reason result)))
			  (else
			   (print (get f)))))))
	      files promises))
|#

(import (rnrs) (util file) (core errors) (scheme load))
(let* ((files (find-files "test/tests" :pattern ".scm"))
       (thunks (map (lambda (file)
		      (lambda () (load file (environment '(rnrs)
							 '(sagittarius)))))
		    files)))
  (for-each (lambda (file thunk)
	      (print file)
	      (guard (e (#t
			 (print (describe-condition e))))
		(thunk))
	      (print))
	    files thunks))
