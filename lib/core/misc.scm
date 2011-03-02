;; -*- scheme -*-
(library (core misc)
    (export call-with-values)
    (import null
	    (sagittarius)
	    (core syntax-rules)
	    (core base))
  (define call-with-values
    (lambda (producer consumer)
      (receive vals (producer) (apply consumer vals))))
)