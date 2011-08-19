;; -*- scheme -*-
(library (tests sagittarius)
    (export run-sagittarius-tests)
    (import (rnrs)
	    (sagittarius)
	    (srfi :64))

  (define (run-sagittarius-tests)
    (test-equal "bytevector->integer"
		#x12345678
		(bytevector->integer #vu8(#x12 #x34 #x56 #x78)))

    (test-equal "integer->bytevector"
		#vu8(#x12 #x34 #x56 #x78)
		(integer->bytevector #x12345678))
    )
)
