;; -*- mode: scheme; coding: utf-8; -*-
(library (sagittarius interactive)
    (export read-eval-print-loop
	    current-printer
	    current-reader
	    current-exception-printer
	    current-evaluator
	    current-prompter
	    default-exception-printer
	    default-evaluator
	    default-printer
	    default-reader
	    default-prompter
	    interactive-environment)
    (import null
	    (core base)
	    (core errors)
	    (sagittarius))

  (define-constant +resource-file+
    (let ((home (or (getenv "HOME") ;; this is the strongest
		    (getenv "USERPROFILE") ;; for windows
		    )))
      (build-path home ".sashrc")))

  (define (default-exception-printer c . out)
    (report-error c))

  (define current-exception-printer
    (make-parameter
     default-exception-printer
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
    (make-parameter 
     default-evaluator
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
    (make-parameter
     default-printer
     (lambda (x)
       (cond ((not x) values)
	     ((procedure? x) x)
	     (else
	      (assertion-violation
	       'current-printer
	       (format "expected procedure or #f, but got ~s" x)))))))

  (define (default-reader in)
    (read/ss in))

  (define current-reader
    (make-parameter 
     default-reader
     (lambda (x)
       (cond ((not x) values)
	     ((procedure? x) x)
	     (else
	      (assertion-violation
	       'current-reader
	       (format "expected procedure or #f, but got ~s" x)))))))

  (define (default-prompter)
    (display "sash> "))

  (define current-prompter
    (make-parameter 
     default-prompter
     (lambda (x)
       (cond ((not x) values)
	     ((procedure? x) x)
	     (else
	      (assertion-violation
	       'current-prompter
	       (format "expected procedure or #f, but got ~s" x)))))))

  ;; initialise env
  (define interactive-environment
    (let ((env #f))
      (lambda ()
	(unless env (set! env (environment 'null ;; for syntax import.
					   '(core base)
					   '(sagittarius)
					   '(rnrs))))
	env)))

  (define (read-eval-print-loop)
    (let ((plugged (getenv "EMACS")))
      ;; load resource file
      (when (file-exists? +resource-file+)
	(call-with-port
	 (open-file-input-port +resource-file+ #f 'block (native-transcoder))
	 (lambda (p)
	   (let loop ((form (read/ss p)))
	     (unless (eof-object? form)
	       ((current-evaluator) form (interactive-environment))
	       (loop (read/ss p)))))))
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
	       (let ((form ((current-reader) (current-input-port))))
		 (and (eof-object? form) (exit 0))
		 (and plugged (flush-output-port (current-output-port)))
		 (receive ans ((current-evaluator) form 
			       (interactive-environment))
		   (apply (current-printer) ans)
		   (flush-output-port (current-output-port))))))))
	(loop))))
)
