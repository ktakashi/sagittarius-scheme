;; -*- mode: scheme; coding: utf-8; -*-
(library (sagittarius interactive)
    (export read-eval-print-loop
	    current-printer
	    current-exception-printer
	    current-evaluator
	    current-prompter
	    default-exception-printer
	    default-evaluator
	    default-printer
	    default-prompter)
    (import null
	    (core base)
	    (core errors)
	    (sagittarius))

  (define (default-exception-printer c . out)
    (report-error c))

  (define current-exception-printer
    (make-parameter default-exception-printer
		    (lambda (x)
		      (cond ((not x) values)
			    ((procedure? x) x)
			    (else
			     (assertion-violation
			      'current-exception-printer
			      (format "expected procedure or #f, but got ~s" x)))))))

  (define (default-evaluator form env)
    (eval form env))

  (define current-evaluator
    (make-parameter default-evaluator
		    (lambda (x)
		      (cond ((not x) values)
			    ((procedure? x) x)
			    (else
			     (assertion-violation
			      'current-evaluator
			      (format "expected procedure or #f, but got ~s" x)))))))

  (define (default-printer . args)
    (for-each (lambda (arg)
		(write/ss arg)(newline))
	      args))

  (define current-printer
    (make-parameter default-printer
		    (lambda (x)
		      (cond ((not x) values)
			    ((procedure? x) x)
			    (else
			     (assertion-violation
			      'current-printer
			      (format "expected procedure or #f, but got ~s" x)))))))

  (define (default-prompter)
    (display "sash> "))

  (define current-prompter
    (make-parameter default-prompter
		    (lambda (x)
		      (cond ((not x) values)
			    ((procedure? x) x)
			    (else
			     (assertion-violation
			      'current-prompter
			      (format "expected procedure or #f, but got ~s" x)))))))

  (define (read-eval-print-loop)
    ;; initialize env
    (define interactive-environment
      (environment 'null ;; for syntax import.
		   '(core base)
		   '(sagittarius)
		   '(rnrs)))

    (let ((plugged (getenv "EMACS")))
      (let loop ()
	(call-with-current-continuation
	 (lambda (continue)
	   (with-error-handler
	     (lambda (c)
	       (flush-output-port (current-output-port))
	       ((current-exception-printer) c)
	       (and (serious-condition? c) (continue)))
	     (lambda ()
	       ((current-prompter))
	       (flush-output-port (current-output-port))
	       (let ((form (read/ss (current-input-port))))
		 (and (eof-object? form) (exit 0))
		 (and plugged #;(format #t "~%")(flush-output-port (current-output-port)))
		 (receive ans ((current-evaluator) form interactive-environment)
		   (apply (current-printer) ans)
		   (flush-output-port (current-output-port))))))))
	(loop))))
)