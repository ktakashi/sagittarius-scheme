;; -*- scheme -*-

(add-load-path "./socket")
(library (socket test)
    (export run-socket-test)
    (import (srfi :64 testing)
	    (srfi :13 strings)
	    (rnrs)
	    (sagittarius socket))

  (define (run-socket-test)
    
    )
)