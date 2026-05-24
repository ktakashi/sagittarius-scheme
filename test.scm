(import (srfi :226 control)
	(only (sagittarius) current-exception-handlers))

(print
 (guard (c [(eqv? c 42) (print 'here c) c])
   (+ 1
      (call-with-continuation-prompt
       (lambda ()
	 (raise 42))))))

