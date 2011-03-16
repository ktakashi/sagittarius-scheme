;; -*- scheme -*-
(library (sagittarius test helper)
    (export current-reporter
	    push-report
	    show-report
	    generate-reporter)
    (import (rnrs))

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

  (define (show-report)
    (let ((reporter (current-reporter 0)))
      ((reporter 'show))))

  (define (generate-reporter)
    (define report '())
    (define (push r)
      (set! report (cons r report)))
    (define (show)
      (for-each (lambda (r)
		  (display r)(newline))
		(reverse report)))
    (lambda (x)
      (case x
	((push)
	 (lambda (r)
	   (push r)))
	((show)
	 (lambda ()
	   (show))))))
)