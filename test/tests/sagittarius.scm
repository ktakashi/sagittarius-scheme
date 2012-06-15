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
;; make result bignum
(test-equal "bytevector->integer"
	    #x1234567890
	    (bytevector->integer #vu8(#x12 #x34 #x56 #x78 #x90)))
(test-equal "bytevector->integer"
	    #x1234567890abcdef1234567890abcdef
	    (bytevector->integer #vu8(#x12 #x34 #x56 #x78 #x90 #xab #xcd #xef
				      #x12 #x34 #x56 #x78 #x90 #xab #xcd #xef)))

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
      (l3 '((a b) . c))
      (v  '#(a b c)))
  (test-error "literal list set!"
	      (lambda (e) (assertion-violation? e))
	      (set-car! l1 'e))
  (test-error "literal list set!" 
	      (lambda (e) (assertion-violation? e))
	      (set-car! l2 'e))
  (test-error "literal list set!" 
	      (lambda (e) (assertion-violation? e))
	      (set-car! (car l3) 'e))
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


(library (inner)
    (export (rename (fuga buzz) (car first)))
    (import (rnrs))
  (define fuga 'fuga))

(library (test)
    (export)
    (import (rnrs) (sagittarius) (inner))
  (define test '*test*)
  (define test1 'oops)
  
  (export test1 buzz first)
  (export test (rename cdr test3) (rename car test2))
  )
(import (test))
(test-equal "multi export syntax" car first)
(test-equal "multi export syntax" car test2)
(test-equal "multi export syntax" buzz 'fuga)
(test-equal "multi export syntax" test1 'oops)
(test-equal "multi export syntax" test '*test*)

;; issue 16
(define (make-test-binary-port out)
  (define (write! bv start count)
    (put-bytevector out bv start (+ start count))
    count)
  (make-custom-binary-output-port "test port" write! #f #f #f))

(test-equal "custom binary output port"
	    (string->utf8 "test")
	    (call-with-bytevector-output-port
	     (lambda (out)
	       (let* ((bin (make-test-binary-port out))
		      (tin (transcoded-port bin (native-transcoder))))
		 (display "test" tin)))))

;; custom codec test
;; This must be run on UTF-8 file λ
(import (encoding sjis) (encoding euc-jp))
;; sjis
(let ((tr (make-transcoder (sjis-codec) 'lf))
      (file  (string-append (current-directory)
			    "/test/data/sjis.txt")))
  ;; read
  (test-equal "read from sjis file" "あいうえお"
	      (call-with-input-file file
		get-line
		:transcoder tr))
  ;; write
  ;; bytevector contains \n as well
  (let ((bv (call-with-input-file file get-bytevector-all :transcoder #f)))
    (let-values (((out getter) (open-bytevector-output-port tr)))
      (put-string out "あいうえお\n")
      (test-equal "write sjis" bv (getter))))
  )

;; euc-jp
(let ((tr (make-transcoder (euc-jp-codec) 'lf))
      (file  (string-append (current-directory)
			    "/test/data/euc-jp.txt")))
  ;; read
  (test-equal "read from euc-jp file" "あいうえお"
	      (call-with-input-file file
		get-line
		:transcoder tr))
  ;; write
  ;; bytevector contains \n as well
  (let ((bv (call-with-input-file file get-bytevector-all :transcoder #f)))
    (let-values (((out getter) (open-bytevector-output-port tr)))
      (put-string out "あいうえお\n")
      (test-equal "write euc-jp" bv (getter))))
  )

;; some illegal character reading
(test-error "ascii 0 read" (read (open-string-input-port "\x0;")))
(test-error "ascii 1 read" (read (open-string-input-port "\x1;")))
(test-error "ascii 2 read" (read (open-string-input-port "\x2;")))
(test-error "ascii 3 read" (read (open-string-input-port "\x3;")))

;; Textual port buffer problem
(define tr (make-transcoder (utf-8-codec)))
(call-with-port
 (open-bytevector-input-port (string->utf8 "xyzzy") tr)
 (lambda (in)
   (test-equal "first get char" #\x (get-char in))
   (test-equal "first lookahead char" #\y (lookahead-char in))))
(call-with-port
 (open-bytevector-input-port (string->utf8 "abcdef") tr)
 (lambda (in)
   (test-equal "second get char" #\a (get-char in))
   ;; actually, this is the only problem
   (test-equal "sedond lookahead char" #\b (lookahead-char in))))

(test-end)
