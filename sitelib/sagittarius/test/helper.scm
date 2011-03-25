;; -*- scheme -*-
(library (sagittarius test helper)
    (export current-reporter
	    push-report
	    push-error
	    push-success
	    show-report
	    generate-reporter)
    (import (rnrs)
	    (core))
  (define current-reporter
    (let ((dict (make-eqv-hashtable)))
      (lambda (id . rep)
	(if (null? rep)
	    (hashtable-ref dict id '())
	    (hashtable-set! dict id (car rep))))))

  (define (push-report report)
    ;; todo get thread-id
    (let ((reporter (current-reporter 0)))
      ((reporter 'push) report)))

  (define (push-error err)
    (let ((reporter (current-reporter 0)))
      ((reporter 'error) err)))

  (define (push-success err)
    (let ((reporter (current-reporter 0)))
      ((reporter 'success) err)))

  (define (show-report)
    (let ((reporter (current-reporter 0)))
      ((reporter 'show))))

  (define (generate-reporter)
    (define report (make-eqv-hashtable))
    (define (push key r)
      (let ((org (hashtable-ref report key '())))
	(hashtable-set! report key (cons r org))))
    (define (show)
      (let ((rep (hashtable-ref report 'push '()))
	    (err (hashtable-ref report 'error '()))
	    (suc (hashtable-ref report 'success '())))
	(for-each (lambda (r)
		    (display r)(newline))
		  (reverse rep))
	(format #t "test finished: success ~a, error ~a~%" (length suc) (length err))
	(for-each (lambda (r)
		    (display r)(newline))
		  (reverse err))))
    (lambda (x)
      (case x
	((push error success)
	 (lambda (r)
	   (push x r)))
	((show)
	 (lambda ()
	   (show))))))
)