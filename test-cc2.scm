(import (rnrs)
	(sagittarius)
	(sagittarius vm)
	(srfi :39)
	(rename (sagittarius continuations)
		(call-with-delimited-current-continuation
		 call-with-non-composable-continuation)))
;;(print-stack-frames)

(define (with-output-to-string thunk)
  (let-values (((out e) (open-string-output-port)))
    (parameterize ((current-output-port out))
      (reset (thunk))
      (e))))
(print (with-output-to-string
         (lambda ()
           (define k1 #f)
           (define done #f)
           (call/cc
            (lambda (k0)
              (reset
               (display "[r01]")
               (shift k (set! k1 k))
               (display "[r02]")
               (unless done
                 (set! done #t)
                 (k0))
               (display "[r03]"))))
           (k1))))
