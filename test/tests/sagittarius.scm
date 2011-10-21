;; -*- scheme -*-
#!compatible
(library (tests sagittarius)
    (export run-sagittarius-tests)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius vm)
	    (srfi :64))

  (define (run-sagittarius-tests)
    (test-equal "bytevector->integer"
		#x12345678
		(bytevector->integer #vu8(#x12 #x34 #x56 #x78)))

    (test-equal "integer->bytevector"
		#vu8(#x12 #x34 #x56 #x78)
		(integer->bytevector #x12345678))

    (test-assert "load test"
		 (begin
		   (load "r6rs-hash.scm")
		   (not (vm-r6rs-mode?))))

    )
)
