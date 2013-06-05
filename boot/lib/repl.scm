;; -*- mode: scheme; coding: utf-8; -*-
(library (sagittarius interactive)
    (export read-eval-print-loop
	    current-printer
	    current-reader
	    current-exception-printer
	    current-evaluator
	    current-prompter
	    current-exit
	    default-exception-printer
	    default-evaluator
	    default-printer
	    default-reader
	    default-prompter
	    default-exit)
    (import null
	    (core base)
	    (core errors)
	    (sagittarius)
	    (sagittarius vm)
	    ;; This is not a builtin library
	    ;; but REPL is not working without (rnrs) anyway
	    (sagittarius parameters))

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
    (for-each (lambda (arg) (write/ss arg)(newline)) args))

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

  (define (default-reader in) (read/ss in))

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

  (define (default-prompter) (display "sash> "))

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

  (define (default-exit) (exit 0))

  (define current-exit
    (make-parameter 
     default-exit
     (lambda (x)
       (cond ((not x) values)
	     ((procedure? x) x)
	     (else
	      (assertion-violation
	       'current-exit
	       (format "expected procedure or #f, but got ~s" x)))))))
  ;; initialise env
  (define (read-eval-print-loop :optional (load-resouce #t))
    (define interactive-environment
      (let ((env (find-library 'user #f)))
	(eval '(import (rnrs)) env)
	env))
    (let ((plugged (getenv "EMACS")))
      ;; load resource file
      (when (and load-resouce (file-exists? +resource-file+))
	(call-with-port
	 (open-file-input-port +resource-file+ #f 'block (native-transcoder))
	 (lambda (p)
	   (let loop ((form (read/ss p)))
	     (unless (eof-object? form)
	       ((current-evaluator) form interactive-environment)
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
		 (cond ((eof-object? form) ((current-exit)))
		       (else
			(and plugged (flush-output-port (current-output-port)))
			(receive ans ((current-evaluator) form
				      interactive-environment)
			  (apply (current-printer) ans)
			  (flush-output-port (current-output-port))))))))))
	(loop))))
)
