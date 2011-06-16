(import (core)
	(core base)
	(core errors)
	(sagittarius))

(define (default-reader)
  (read (current-input-port)))

(define (default-evaluator exp env)
  (eval exp env))

(define (default-printer . args)
  (for-each (lambda (arg)
	      (write/ss arg)(newline))
	    args))

(define (default-prompter)
  (display "sash> ")
  (flush-output-port (current-output-port)))

(define (repl reader evaluator printer prompter)
  (let loop1 ()
    (call-with-current-continuation
     (lambda (continue)
       (with-error-handler
	(lambda (e) 
	  (print "error:")
	  (display (describe-condition e))
	  (and (serious-condition? e) (continue)))
	(lambda ()
	  (prompter)
	  (let ((exp (reader)))
	    (and (eof-object? exp) (exit 0))
	    (receive result (evaluator exp (environment '(rnrs)))
	      (apply printer result)))))))
     (loop1)))

(repl default-reader default-evaluator default-printer default-prompter)