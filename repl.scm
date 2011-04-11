(import (rnrs)
	(core errors))
;; dummy
(define-syntax environment
  (syntax-rules ()
    ((_ _)
     '())))

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
    (print "loop1 start")
    (and
     (with-exception-handler
       (lambda (e) 
	 (print "error:")
	 (display (describe-condition e)) #t)
       (lambda ()
	 (prompter)
         (let loop2 ((exp (reader)))
           (if (eof-object? exp)
               #f
               (begin
                 (receive result (evaluator exp (environment '(rnrs)))
                   (apply printer result)
		   (prompter))
                 (loop2 (reader)))))))
     (loop1))))

(repl default-reader default-evaluator default-printer default-prompter)