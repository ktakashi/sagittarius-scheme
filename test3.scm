
(print "\"")

(import (core syntax-rules)
	(sagittarius vm))
(define-syntax test
  (syntax-rules (else)
    ((_ (var clause ... (else hoge ...)))
     (for-each (lambda (c h)
		 (display c)
		 (display h))
	       (list clause ...)
	       (list hoge ...)))))
(test (hoge 'a 'b  (else 'e 'f)))

;(import (core exceptions))
;(display guard)(newline)

#;(print (unwrap-syntax (%macroexpand 
(guard (con
        ((error? con)
         (if (message-condition? con)
             (display (condition-message con))
             (display "an error has occurred"))
         'error)
        ((violation? con)
         (if (message-condition? con)
             (display (condition-message con))
             (display "the program has a bug"))
         'violation))
  (raise
   (condition
    (make-error)
    (make-message-condition "I am an error"))))
)))
