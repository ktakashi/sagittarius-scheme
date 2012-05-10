;; -*- scheme -*-
#!compatible

(import (rnrs)
	(rnrs mutable-pairs)
	(sagittarius)
	(sagittarius vm)
	(srfi :64 testing))

(define-syntax define-lambda
  (syntax-rules ()
    ((_ name formals body ...)
     (define name (lambda formals body ...)))))
(define-lambda f (t rest) `(t ,t))

(test-begin "sagittarius specific")
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

(test-equal "`(t ,t)" (f 'a 'b) '(t a))

(let ((l1 '(a b c))
      (l2 '(a b . c))
      (v  '#(a b c)))
  (test-error "literal list set!"
	      (lambda (e) (assertion-violation? e))
	      (set-car! l1 'e))
  (test-error "literal list set!" 
	      (lambda (e) (assertion-violation? e))
	      (set-car! l2 'e))
  (test-error "literal vector set!" 
	      (lambda (e) (assertion-violation? e))
	      (vector-set! v 0 'e)))

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

;; Issue 12
(test-equal "\\x0; test" #vu8(0) (string->utf8 "\x0;"))

;; bytevector output-port
(let-values (((out getter) (open-bytevector-output-port)))
  (put-bytevector out #vu8(1 2))
  (test-equal "port-position(binary port)" 2 (port-position out))
  (let ((save (port-position out)))
    (set-port-position! out 0)
    (put-bytevector out #vu8(1 2 3 4 5))
    (set-port-position! out save)
    (put-bytevector out #vu8(1 2 3 4 5)))
  (test-equal "getter" #vu8(1 2 1 2 3 4 5) (getter)))

(let-values (((out getter) (open-string-output-port)))
  (put-string out "12")
  (test-equal "port-position(binary port)" 2 (port-position out))
  (let ((save (port-position out)))
    (set-port-position! out 0)
    (put-string out "12345")
    (set-port-position! out save)
    (put-string out "12345"))
  (test-equal "getter" "1212345" (getter)))

;; issue 15
(test-equal "heavy call" 1 ((let ((c (lambda (n) n))) c) 1))
(test-end)
