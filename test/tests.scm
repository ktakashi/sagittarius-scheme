;; -*- scheme -*-

#|
;; multithread version does not work correctly yet.
;; load with multithread is really buggy.
;; at least it's working with separated test, ex) run-test.scm sitelib
;; but it can not handle the whole test yet.
(import (rnrs)
	(clos user)
	(core errors)
	;;(srfi :18 multithreading)
	(sagittarius threads)
	(srfi :64 testing)
	(util file)
	(sagittarius io))

;; simple future
(define-class <promise> ()
  ((future :init-keyword :future)
   (thread :init-keyword :thread :accessor promise-thread)))
 
(define-class <future> ()
  ((promise :accessor future-promise)))

(define-method promise-specific ((p <promise>) o)
  (thread-specific-set! (promise-thread p) o))
(define-method promise-specific ((p <promise>))
  (thread-specific (promise-thread p)))

 
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
 
(define-method finished? ((f <future>))
  (let ((promise (future-promise f)))
    (eq? (thread-state (promise-thread promise)) 'terminated)))

(define (get-finished-future promises)
  (let loop ((p promises)
	     (r '()))
    (if (null? p)
	(reverse! r)
	(let ((f (get-future (car p))))
	  (cond ((finished?  f)
		 (let ((result (get f)))
		   (cond ((uncaught-exception? result)
			  (print "FAILED WITH ERROR:")
			  (print (promise-specific (car p)))
			  (print (describe-condition
				  (uncaught-exception-reason result))))
			 (else
			  (print (get f)))))
		 (loop (cdr p) r))
		(else
		 (loop (cdr p) (cons (car p) r))))))))

;; ugly dependency resolver for test cases.
;; TODO this needs to be removed.
;;      I think the reason why it needs here is because of the reading cache
;;      or creating a new library. it does not lock any thing, so the timing
;;      problem occures when child threads import the same library.
(import (packrat))

(let* ((files (find-files "test/tests" :pattern ".scm"))
       (promises (map (lambda (file)
			(let ((p (make-promise 
				  (lambda (f)
				    (with-output-to-string
				      (lambda ()
					(load f)))) file)))
			  (promise-specific p file)
			  p))
		      files)))
  (let loop ((r (get-finished-future promises)))
    (unless (null? r)
      (loop (get-finished-future r)))))
|#

(import (rnrs) (util file) (core errors) (scheme load))
(cond-expand
 (sagittarius.os.windows
  (define-constant path "test\\tests"))
 (else
  (define-constant path "test/tests")))

(let* ((files (find-files path :pattern ".scm" :all #f))
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

