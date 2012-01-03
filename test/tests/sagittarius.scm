;; -*- scheme -*-
#!compatible
(library (tests sagittarius)
    (export run-sagittarius-tests)
    (import (rnrs)
	    (rnrs mutable-pairs)
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

    ;;(test-assert "literal list" (eq? '(a b c) '(a b c)))
    ;;(test-assert "literal list" (eq? '(a b . c) '(a b . c)))
    ;;(test-assert "literal vector" (eq? #(a b c) #(a b c)))
    (test-assert "literal bytevector" (eq? #vu8(1 2 3) #vu8(1 2 3)))
#| macro copies all pair and vector. so this test does not pass.
    (test-error "literal list set!"
		(lambda (e) (assertion-violation? e))
		(set-car! '(a b c) 'e))
    (test-error "literal list set!" 
		(lambda (e) (assertion-violation? e))
		(set-car! '(a b . c) 'e))
    (test-error "literal vector set!" 
		(lambda (e) (assertion-violation? e))
		(vector-set! '#(a b c) 0 'e))
|#
    (test-error "literal bytevector u8 set!"
		(lambda (e) (assertion-violation? e))
		(bytevector-u8-set! #vu8(1 2 3) 0 4))
    (test-error "literal bytevector s8 set!"
		(lambda (e) (assertion-violation? e))
		(bytevector-s8-set! #vu8(1 2 3) 0 4))
    (test-error "literal bytevector native u16 set!"
		(lambda (e) (assertion-violation? e))
		(bytevector-u16-native-set! #vu8(1 2 3 4) 1 5))
    (test-error "literal bytevector u16 set!"
		(lambda (e) (assertion-violation? e))
		(bytevector-u16-set! #vu8(1 2 3 4) 1 5))
    (test-error "literal bytevector native s16 set!"
		(lambda (e) (assertion-violation? e))
		(bytevector-s16-native-set! #vu8(1 2 3 4) 1 5))
    (test-error "literal bytevector s16 set!"
		(lambda (e) (assertion-violation? e))
		(bytevector-s16-set! #vu8(1 2 3 4) 1 5))

    )
)
