;; -*- scheme -*-
#!compatible

(import (rnrs)
	(rnrs mutable-pairs)
	(rename (rnrs eval) (eval r6rs:eval))
	(sagittarius)
	(sagittarius vm)
	(encoding decoder)
	(srfi :1)
	(srfi :64 testing))

(define-syntax define-lambda
  (syntax-rules ()
    ((_ name formals body ...)
     (define name (lambda formals body ...)))))
(define-lambda f (t rest) `(t ,t))

(test-begin "Sagittarius specific")
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


;; with start and end
(test-equal "bytevector->integer (start end)"
	    #x0203
	    (bytevector->integer #vu8(1 2 3 4) 1 3))
(test-equal "bytevector->integer (bignum start end)"
	    #x02030405060708090a
	    (bytevector->integer #vu8(1 2 3 4 5 6 7 8 9 #xa #xb) 1 10))
(test-equal "integer->bytevector"
	    #vu8(#x12 #x34 #x56 #x78)
	    (integer->bytevector #x12345678))
(test-equal "integer->bytevector (with optional)"
	    #vu8(#x00 #x12 #x34 #x56 #x78)
	    (integer->bytevector #x12345678 5))
(test-equal "integer->bytevector (with optional)"
	    #vu8(#x34 #x56 #x78)
	    (integer->bytevector #x12345678 3))


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
    (export ok)
    (import (rnrs) (sagittarius) (inner))
  (define test '*test*)
  (define test1 'oops)
  
  (export test1 buzz first)
  (export test (rename cdr test3) (rename car test2))

  (define variable 1)
  (export variable)

  (define (ok) (ok-aux))
  (define-syntax ok-aux
    (syntax-rules ()
      ((_) 'ok)))
  )
(import (test))
(test-equal "multi export syntax" car first)
(test-equal "multi export syntax" car test2)
(test-equal "multi export syntax" buzz 'fuga)
(test-equal "multi export syntax" test1 'oops)
(test-equal "multi export syntax" test '*test*)
;; this must be syntax error
;;(test-error "immutable varialbe" (set! variable 2))

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
(test-assert "flush" 
	     (call-with-bytevector-output-port
	      (lambda (out)
		(let* ((bin (make-test-binary-port out))
		       (tin (transcoded-port bin (native-transcoder))))
		  (flush-output-port tin)
		  'ok))))


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

;; issue 19
(test-assert "bytevector set! (minus values)"
	     (bytevector-s32-native-set! (make-bytevector 4) 0 -1))
(test-assert "bytevector set! (minus values)"
	     (bytevector-s32-set! (make-bytevector 4) 0 -1 'little))
(test-assert "bytevector set! (minus values)"
	     (bytevector-s64-native-set! (make-bytevector 8) 0 -1))
(test-assert "bytevector set! (minus values)"
	     (bytevector-s64-set! (make-bytevector 8) 0 -1 'little))

;;-----------------------------------------------------------------------
;; Al Petrofsky's finding
;; http://groups.google.com/groups?dq=&hl=ja&selm=87g00y4b6l.fsf%40radish.petrofsky.org

(test-equal "Al's call/cc test" 1 (call/cc (lambda (c) (0 (c 1)))))

;; syntax-case stuff
(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      ((aif expr then else)
       (with-syntax ((it (datum->syntax #'aif 'it)))
	 #'(let ((it expr))
	     (if it then else)))))))
(test-equal "aif"
	    1
	    (aif (assq 'a '((a . 1) (b . -2)))
		 (cdr it)
		 it))
(test-equal "aif (with local it)"
	    1
	    (let ((it #f))
	      (aif (assq 'a '((a . 1) (b . -2)))
		   (cdr it)
		   it)))

;; pattern variable resolution
(define-syntax patvar-inner
  (syntax-rules ()
    ((_ r e) 
     (if (= r 1) e #f))))
(define-syntax patvar
  (syntax-rules ()
    ((_ expr)
     (let ((r 1))
       (patvar-inner r expr)))))

(test-equal "pattern variable resolution" 2 
	    (let ((r 2)) (patvar r)))


;; optional argument for utf8->string, string->utf8 (R7RS)
(let ((bv #vu8(#x31 #x32 #x33 #x34 #x35 #x36)))
  (test-equal "utf8->string (with start)" "23456" (utf8->string bv 1))
  (test-equal "utf8->string (with start and end)" "234" (utf8->string bv 1 4)))

(let ((s "123456"))
  (test-equal "string->utf8 (with start)"
	      #vu8(#x32 #x33 #x34 #x35 #x36) (string->utf8 s 1))
  (test-equal "string->utf8 (with start and end)"
	      #vu8(#x32 #x33 #x34) (string->utf8 s 1 4)))

;;; equal? for record
(let ()
  (define-record-type (pare kons pare?)
    (fields (mutable x kar set-kar!)
	    (immutable y kdr)))
  (define a (kons (vector 1 2 3) '(a b c)))
  (define b (kons (vector 1 2 3) '(a b c)))
  (test-assert "equal? (record)" (equal? a b)))

;; vector-append
(let ((a '#(1 2 3))
      (b '#(4 5 6)))
  (test-equal "vector-append (0)" '#() (vector-append))
  (test-equal "vector-append (1)" '#(1 2 3) (vector-append a))
  ;; newly created check
  (test-assert "vector-append (eq?)" (not (eq? a (vector-append a))))
  (test-equal "vector-append (2)" '#(1 2 3 4 5 6) (vector-append a b)))

;; vector-concatenate
(test-equal "vector-append (0)" '#() (vector-concatenate '()))
(test-equal "vector-append (1)" '#(1 2 3) (vector-concatenate '(#(1 2 3))))
;; newly created check
(let ((a '(#(1 2 3))))
  (test-assert "vector-append (eq?)" (not (eq? a (vector-concatenate a)))))
(test-equal "vector-append (2)" '#(1 2 3 4 5 6)
	    (vector-concatenate '(#(1 2 3) #(4 5 6))))

;; bytevector-append
(let ((a #vu8(1 2 3))
      (b #vu8(4 5 6)))
  (test-equal "bytevector-append (0)" #vu8() (bytevector-append))
  (test-equal "bytevector-append (1)" #vu8(1 2 3) (bytevector-append a))
  ;; newly created check
  (test-assert "bytevector-append (eq?)" (not (eq? a (bytevector-append a))))
  (test-equal "bytevector-append (2)" #vu8(1 2 3 4 5 6)
	      (bytevector-append a b)))

;; bytevector-concatenate
(test-equal "bytevector-append (0)" #vu8() (bytevector-concatenate '()))
(test-equal "bytevector-append (1)" #vu8(1 2 3)
	    (bytevector-concatenate '(#vu8(1 2 3))))
;; newly created check
(let ((a '(#vu8(1 2 3))))
  (test-assert "bytevector-append (eq?)" 
	       (not (eq? a (bytevector-concatenate a)))))
(test-equal "bytevector-append (2)" #vu8(1 2 3 4 5 6)
	    (bytevector-concatenate '(#vu8(1 2 3) #vu8(4 5 6))))

;; eqv?
;; R6RS 11.5 6th item
(test-assert "eqv? 0.0 -0.0" (not (eqv? 0.0 -0.0)))

;; SRFI 61 cond
(test-equal "SRFI-61 cond"
	    3
	    (cond ((+ 1 2) number? => (lambda (x) x))))

;; Issue 22
(test-equal "read-delimited-list from custom texutal port"
	    '(1)
	    (let ()
	      (define (make-custom-input-port in)
		(define (read! s start count)
		  (get-string-n! in s start count))
		(define (close) (close-input-port in))
		(make-custom-textual-input-port "test" read! #f #f close))
	      (read-delimited-list 
	       #\) (make-custom-input-port (open-string-input-port "1)")))))

;; list->string has optional arguments start and end
(test-equal "list->string with start"
	    "bcdef" (list->string '(#\a #\b #\c #\d #\e #\f) 1))
(test-equal "list->string with start and end"
	    "bc" (list->string '(#\a #\b #\c #\d #\e #\f) 1 3))
(test-equal "list->string no length" "" (list->string '(#\a) 0 0))
;; list->string only checks given range is char or not
(test-equal "list->string with start (check)"
	    "bcdef" (list->string '(hoge #\b #\c #\d #\e #\f) 1))
(test-equal "list->string with start and end (check)"
	    "bc" (list->string '(hoge #\b #\c hoge hoge hoge) 1 3))
;; error case
(test-error "list->string range error" (lambda (e) e)
	    (list->string '(#\a) -1))
(test-error "list->string range error" (lambda (e) e)
	    (list->string '(#\a) 2 1))
(test-error "list->string range error" (lambda (e) e)
	    (list->string '(#\a) 0 2))

;; mod-inverse
(let ((ps
       '("c6c93915786185fa7ee88f1983cc8d238cd6c5f7a36416c8be317c3df475277784ae1b87b263b88a84d5bacaf63798474ffe490fa412cb437abe5908efbe41b1"
	 "f2d6323e96c9ad655ab520dccbc5bdf3312dcf4e32858650caa21d7e8c7ed6d13d8bbe166e0ac7cb787ef38bec6c55529f3f93b0d7c9e5ceb5188571699619bf"
	 "e50fce1d57633704798f7b2465ddccebf6e5c9f22a8e3017a39f8de7cb3b78285003dca54bf9c7a2c478add7cfd7cf678b831be1db331f2f3961435c6937a545"
	 "a9782bf45cdb460875a56c89b580df3f959f33e07ea43ec166241c5add827303815ab0131b7e98430038aed9e136b83e1a82d099bb40a26ac9497ef3abb58dfd"
	 "d265038c4fee2f3f87c8a2e15c1fa67dfac4ad5eb78bec468d9df27ffe3224581a2a189f87946a012a228f579abfb0d183e99cd831341af9b750b4582236e15d"
	 "ca911176fce31e4332ec9ada6fa268f6ea1a9a71c81599a77797d74d5c7c48491fafce22428c516d7318c36907aa76df89e92be5ab66b42b25be777640ecc76d"
	 "eb97f1e80a81d9b725dd5708fe7d65ab5339d7a339c703ee73de339fb0f10a4d76bd827536b9f6da49507ee12ca37b8157f8103f3d12a9eb9468576d9b2ef59f"))
      (qs
       '("bc5e04097e88241c2e9f145a829c158bacb17756b0c6aba175318c4b0b799067a83509dc45fb34c82aa7d3caacc80f1d0013c9bdd24bd52f31f04edfa169ef75"
	 "da554d942ebe105e7a60070bfcaf3953f29ecfd6493aac69c6427a00be66c978515e7222180cc84606bcf7348c8aba0f9b05870cf2ab1c3669199c4316d40669"
	 "ceb5591d98f1e1bfe3095f21a7e7c47d18bfcfbb8e0a1971a13941bd4cc2c861c2ef4b85cdf52b6aaeeb20264456b3c3c2a7f6a52b21eb91276acb3caa3603d1"
	 "d47c206d19142ad870648eb09ca183cf4875f8009d91fcc0e085ac65455caf17ee5e91f2ccb564a88a8d13100faf1c95c6481c1b2e3fb6483f1bcdb2894356ad"
	 "cc36f153789677c45232afdaed78f2a20658f53fcbaa0626f64d0fa29a6f70516420999fee96dca6d232c644b09d1e27cdc0215fcbc4c36a5c493f2e1fed7bb1"
	 "e63821b08b4bcc12e80a3e019f4f424c20aa72b426fc912bb2157569f9ee4422f970bbc4bf75ac05e77e48d436ce980e0646c2ba3eafb9e98aff77e19b59257f"
	 "d8e26d53f31a647889ce845e892b076e578f0a68565005d5d23ed8a4ff8370cbb12cb41854badfe17053db1a94e754ea241ede1d879bff36b75f5fa96eb64927")))
  (for-each (lambda (p q)
	      (let* ((p (string->number p 16))
		     (q (string->number q 16))
		     (phi (* (- p 1) (- q 1))))
		(test-assert "mod-inverse" (mod-inverse #x10001 phi)))) ps qs)


  (test-equal "mod-inverse (recursion)"
	      #xa3a790f0b7d2bea3a81dc676032cf99c23c28bee
	      (mod-inverse #x1a1eb1e6b8f115eee3dc1334afc7de2f7efbd568
			   #xde09f1902cf484f232fee5d27262372d1c6072d7))

  )
;; mod-expt
;; TODO bignum
(test-equal "3 ^  5 mod 10" 3 (mod-expt 3  5 10))
(test-equal "3 ^ -5 mod 10" 7 (mod-expt 3 -5 10))
(test-equal "3 ^ -3 mod 10" 3 (mod-expt 3 -3 10))

(test-equal "bignums(1)"
	    144823644014482364401448236440
	    (mod-expt 123456789012345678901234567890
		      987654321098765432109876543210987654
		      147258369014725836901472583690))

;; macro problems
(library (settable-variable)
  (export define-settable)
  (import (rnrs))
  
  (define-syntax define-settable
    (syntax-rules ()
      ((_ var val)
       (begin
         (define dummy val)
         (define (set-dummy! x) (set! dummy x))
         (define (dummy-ref) dummy)
         (define-syntax var
           (make-variable-transformer
            (lambda(x)
              (syntax-case x (set!)
                ((set! _ a) #'(set-dummy! a))
                (_ #'(dummy-ref))))))))))
  )
(library (macro problem test)
  (export var1 var2)
  (import (rnrs) (settable-variable))
  
  (define-settable var1 #f)
  (define-settable var2 #f)
  )
(import (macro problem test))

(test-assert "set! var1" (set! var1 1))
(test-assert "set! var2" (set! var2 2))
(test-equal "var1" 1 var1)
(test-equal "var2" 2 var2)

;; macro renaming
(define-syntax renaming-test
  (syntax-rules ()
    ((_ var val)
     (begin
       (define dummy val)
       (define (var) dummy)))))
 
(renaming-test a 'a)
(renaming-test b 'b)
(test-equal "rename a" 'a (a))
(test-equal "rename b" 'b (b))
(test-error "unbound dummy" violation? dummy)

;; Issue 26
(test-assert "cond-features" (constant-literal? (cond-features)))

;; Issue 27
;; macro will re-construct constant literal non constant,
;; so we need to keep it as binded variables.
(let ((a '(a b c))
      (b '(c d)))
  (test-error "append! with literal" assertion-violation? 
	      (append! a '(c d)))
  (test-error "append! with literal" 
	      assertion-violation?
	      (let ((a (list 'a 'b 'c)))
		(append! a b)
		(set-cdr! (last-pair a) 'f)))

  (test-error "reverse! with literal" assertion-violation? 
	      (reverse! a)))

;; Issue 28
(let ((a #(1 2 3))
      (b #(1 2 3)))
  (test-error "vector-reverse!" assertion-violation? (vector-reverse! a)))

;; since 0.3.7 values are not first class object
(test-equal "values (over pre allocated)"
	    (iota 1000 1)
	    (receive x (apply values (iota 1000 1)) x))
(test-equal "values (over pre allocated)"
	    (iota 2000 1)
	    (receive x (apply values (iota 2000 1)) x))

;; issue 51
(define (nothing))
(test-assert "binary-port?"
	     (let ((cbi (make-custom-binary-input-port "id" nothing #f #f #f)))
	       (binary-port? cbi)))
(test-assert "textual-port?"
	     (let ((cti (make-custom-textual-input-port "id" nothing #f #f #f)))
	       (textual-port? cti)))

;; issue 58
(test-assert "library version 0"
	     (r6rs:eval '(import (rnrs (0))) (environment '(sagittarius))))
(test-assert "library version full spec (1)"
	     (r6rs:eval '(import (rnrs (and 1 1)))
			(environment '(sagittarius))))
(test-assert "library version full spec (2)"
	     (r6rs:eval '(import (rnrs (or (1 (>= 1)) (2))))
			(environment '(sagittarius))))

;; issue 59
(test-equal "inlined append" '(c d e) (let ((a '(a b c d e)))
					(cond ((memq 'c a) => append))))

;; issue 60
;; For now we only throw &i/o-write
;; The behaviour is taken from CLisp and SBCL (stack overflow detection)
(test-error "deeply nested list stack overflow detection"
	    i/o-error?
	    (let loop ((cnt 0) (ls '()))
	      (if (< cnt 1000000)
		  (loop (+ cnt 1) (list ls))
		  (string-length (call-with-string-output-port
				  (lambda (p) (display ls p)))))))

;; issue 63
;; for keyword ignored it's library reference
(library (issue-63 aux)
    (export problem)
    (import (rnrs))
  (define (problem) #t))

(library (issue-63)
    (export problem)
    (import (rnrs) (for (prefix (issue-63 aux) aux:) expand run))
  (define (problem) (aux:problem)))
(test-assert "issue 63" (r6rs:eval '(import (issue-63)) 
				   (environment '(sagittarius))))

;; issue 64
(test-assert "issue 64" (let () (define inner) inner))

;; issue 66
;; the issue was only valid in R6RS mode
#!r6rs
(let ((env (environment '(rnrs))))
  (test-assert "issue 66 (define)" 
	       (r6rs:eval
		'(let-syntax ((def (syntax-rules () ((_) (lambda () #t)))))
		   (define prob (def))) env))
  (test-assert "issue 66 (run)" (r6rs:eval '(prob) env)))
#!compatible

;; issue 67
(let* ((in  (open-bytevector-input-port (string->utf8 "hello")))
       (tp (transcoded-port in (make-transcoder (lookup-decoder "sjis")))))
  (test-assert "custom codec(0)" (port-closed? in))
  (test-equal "custom codec(1)" "hello" (get-string-all tp))
  (test-assert "custom codec(2)" (port-closed? in)))

(let ()
  (define (error-codec)
    (define (getc port mode check-bom? data)
      (get-u8 port)
      (error 'error-codec "always error"))
    
    (define (putc port c mode data)
      (error 'error-codec "always error"))
    (make-codec 'error-codec getc putc #f))
  (test-assert 
   "error closing"
   (let* ((bin (open-bytevector-input-port #vu8(1 2 3 4)))
	  (tin (transcoded-port bin (make-transcoder (error-codec)))))
     (guard (e ((error? e) (port-closed? bin))
	       (else #f))
       (get-char tin)))))

;; issue 68
(let ()
  (define-syntax foo
    (lambda (x)
      (syntax-case x ()
	((_)
	 (with-syntax ((set (let ((type #'bytevector-u8-set!))
			      #`(#,type bv 0 1))))
	   #'(let ((bv (make-bytevector 1)))
	       set
	       bv))))))
  (test-equal "issue 68" #vu8(1) (foo)))

;; issue 1
(let ()
  (define-syntax let/scope
    (lambda(x)
      (syntax-case x ()
	((k scope-name body ...)
	 #'(let-syntax
	       ((scope-name
		 (lambda(x)
		   (syntax-case x ()
		     ((_ b (... ...))
		      #`(begin
			  #,@(datum->syntax #'k
				(syntax->datum #'(b (... ...))))))))))
	     body ...)))))

  (let ((x 1))
    (let/scope d1
      (let ((x 2))
	(let/scope d2
	  (let ((x 3))
	    (test-equal "issue 1 (bending scope)"
			'(1 2 3)
			(list (d1 x) (d2 x) x)))))))
)
;; issue 46
(let ()
  (define (issue-46)
    (define-syntax define-inline
      (syntax-rules ()
	((_ (?name ?arg ...) ?form0 ?form ...)
	 (define-syntax ?name
	   (syntax-rules ()
	     ((_ ?arg ...)
	      (begin ?form0 ?form ...)))))))
    (let ((a 1))
      (define-inline (ret-a) a)
      (ret-a)))
  (test-equal "issue 46" 1 (issue-46)))

;; issue 69
(test-equal "issue 69" 
	    '(hoge . bar) (unwrap-syntax
			   (let ((bar 'bar))
			     #`(hoge . #,bar))))

(let ()
  (define-syntax expand-it
    (lambda (x)
      (define (gen-return var) (with-syntax ((a var)) #'a))
      (syntax-case x ()
	((_ (v init) expr ...)
	 (with-syntax ((r (gen-return #'v)))
	   #'(let ((v init))
	       (when (= v r)
		 expr ...)))))))
  
  (test-equal "the same input form in differenct syntax"
	      'ok (expand-it (v 1) 'ok))
)

;; issue 71
(let ((iv #x3000000000000000)
      (bv (make-bytevector 8)))
  (bytevector-u64-set! bv 0 iv (endianness big))
  (test-equal "issue 71" iv (bytevector->integer bv)))

;; issue 72
(let ()
  (define p2 (cons 4 5))
  (define-syntax p2.car
    (make-variable-transformer
     (lambda (x)
       (syntax-case x (set!)
	 [(set! _ e) #'(set! p2 (cons e (cdr p2)))]
	 [(_ . rest) #'((car p2) . rest)]
	 [_  #'(car p2)]))))
  (set! p2.car 15)
  (test-equal "local variable transformer" 15 p2.car)
)

;; issue 73
(let ()
  (define (read! . args) 0)
  (define cbin (make-custom-binary-input-port "cbin" read! #f #f #f))
  (define ctin (make-custom-textual-input-port "ctin" read! #f #f #f))
  (test-assert "custom binary port-eof?" (port-eof? cbin))
  (test-assert "custom textual port-eof?" (port-eof? ctin)))

;; issue 74
(let ()
  (define-syntax foo
    (lambda (x)
      (define-syntax m1
	(syntax-rules ()
	  ((_ ?x ?x2)
	   (free-identifier=? ?x ?x2))))

      (define-syntax m2
	(syntax-rules ()
	  ((_ ?atom ?atom2)
	   (if (m1 ?atom ?atom2)
	       #''ok
	       #''ng))))

      (define (fuga id1 id2) (m2 id1 id2))
      (syntax-case x ()
	((_ bar boo) (fuga #'bar #'boo)))))

  (test-equal "issue 74" 'ok (foo a a)))

;; macro scope
(let ()
  (define-syntax define-inline
    (syntax-rules ()
      ((_ (?name ?args ...) ?form0 ?forms ...)
       (define-syntax ?name
	 (syntax-rules ()
	   ((_ ?args ...)
	    (begin ?form0 ?forms ...)))))))
  (define (inlined) 'ng)

  (let ()
    (define (boo)
      (define-inline (inlined) 'ok)
      (define (inner) (inlined))
      (inner))
    (test-equal "macro scope (inlined)" 'ok (boo)))
)

;; issue 76
(library (free)
    (export (rename (mod %))
	    compare=?)
    (import (rnrs))

  (define-syntax compare=?
    (syntax-rules ()
      ((_ a b)
       (free-identifier=? #'a #'b))))

)
(import (for (rnrs) (meta -1) run)
	(for (free) (meta -1) run))
(test-assert "issue 76" (compare=? % mod))

;; issue 77
(test-assert "(eqv? 0.0+0.0i 0.0-0.0i)" (not (eqv? 0.0+0.0i 0.0-0.0i)))
(test-assert "(eqv? 1.0+0.0i 1.0-0.0i)" (not (eqv? 1.0+0.0i 1.0-0.0i)))
(test-assert "(eqv? 1.0+0i 1.0-0i)" (eqv? 1.0+0i 1.0-0i))

;; to use SRFI-42 in R6RS mode
#!r6rs
(import (srfi :42))
(test-assert "not keyword" (symbol? ':foo))
(test-equal "R6RS mode SRFI-42" '(0 1 2 3 4 5 6 7 8 9) (list-ec (: x 10) x))

;; issue 80
(test-equal "bignum sqrt" 1377769.8968554218 (sqrt 1898249888681))
;; issue 82
(test-equal "bit count (1)" 0 (bitwise-bit-count 0))
(test-equal "bit count (2)" 0 (fxbit-count 0))

;; bytevector-fill!
(let ((bv (make-bytevector 5 10)))
  (test-equal "bytevector-fill!"
	      #vu8(2 2 2 2 2) 
	      (begin (bytevector-fill! bv 2) bv))
  (test-equal "bytevector-fill! (0->)"
	      #vu8(5 5 5 5 5) 
	      (begin (bytevector-fill! bv 5 0) bv))
  (test-equal "bytevector-fill! (0->3)"
	      #vu8(0 0 0 5 5) 
	      (begin (bytevector-fill! bv 0 0 3) bv))
  (test-equal "bytevector-fill! (3->4)"
	      #vu8(0 0 0 2 5) 
	      (begin (bytevector-fill! bv 2 3 4) bv))
)
;; enbugged case
(let ()
  (define-syntax define-inline
    (syntax-rules ()
      ((_ (name arg ... . rest) body ...)
       (define-syntax name
	 (syntax-rules ()
	   ((_ arg ... . rest)
	    (begin body ...)))))))

  (define (foo v)
    (define-inline (bar args) args)
    (if v
	(syntax-case v ()
	  (var (bar v)))
	(bar #t)))

  (test-assert "enbugged" (foo #f))
  (test-equal "enbugged" '(a b c) (foo '(a b c))))

;; issue 93
(let ()
  (define-syntax prob
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
	 ((_ vals ...)
	  #'(begin vals ...))))))
  
  (define-syntax define-thing
    (lambda (x)
      (syntax-case x ()
	((k)
	 (with-syntax ((thing (datum->syntax #'k 'thing)))
	   #'(define (thing r) r))))))

  (let ((a 1))
    (define-thing)
    (test-equal "issue 93" 1 (prob (thing a)))))

;; issue 94
(let ()
  (define-syntax define-inline
    (syntax-rules ()
      ((_ (name . args) body ...)
       (define-syntax name
	 (syntax-rules ()
	   ((_ . args)
	    (begin body ...)))))))
  (define (puts args) args)
  (define-inline (print args) args)
  (test-equal "issue 94 (def->macro)" "abc" (print "abc"))
  )
(test-equal "define-syntax after define" 'ok (ok))

(let ()
  (define-syntax define-inline
    (syntax-rules ()
      ((_ (name . args) body ...)
       (define-syntax name
	 (syntax-rules ()
	   ((_ . args)
	    (begin body ...)))))))
  (define-inline (print args) args)
  (define (puts args) args)
  (test-equal "issue 94 (macro->def)" "abc" (print "abc")))

;; issue 101
(test-equal "(atan 0)" 0 (atan 0))
(test-equal "(atan 0.0)" 0.0 (atan 0.0))
(test-error "(atan 0+i)" values (atan 0+i))

;; issue 102
(test-equal "string-scan" '(#f #f) 
	    (receive v (string-scan "abcd1234pqrs" "1ZZZ" 'both) v))

;; issue 103
;; bytevector
(let ()
  (define bv (open-output-bytevector))
  (put-bytevector bv #vu8(1 2 3 4))
  (test-equal "get-output-bytevector (before)"
	      #vu8(1 2 3 4) (get-output-bytevector bv))
  (test-equal "get-output-bytevector (after)" 
	      #vu8(1 2 3 4) (get-output-bytevector bv)))

;; related
(let-values (((port getter) (open-bytevector-output-port)))
  (put-bytevector port #vu8(1 2 3 4))
  (test-equal "getter bv (before)" #vu8(1 2 3 4) (getter))
  (test-equal "getter bv (after)"  #vu8() (getter)))


;; string
(let ()
  (define str (open-output-string))
  (write 1234 str)
  (test-equal "get-output-string (before)" "1234" (get-output-string str))
  (test-equal "get-output-string (after)"  "1234" (get-output-string str)))

;; related
(let-values (((port getter) (open-string-output-port)))
  (write 1234 port)
  (test-equal "getter (before)" "1234" (getter))
  (test-equal "getter (after)" "" (getter)))

;; (/ 1 -0.0)
(test-equal "(/ 1 -0.0)" -inf.0 (/ 1 -0.0))
(test-equal "(/ -1 -0.0)" +inf.0 (/ -1 -0.0))

;; issue 112
(test-assert "#x800000" (positive? #x800000))

;; issue 120
(test-error "list-sort" (list-sort #f '()))
(test-error "list-sort" (list-sort #f '(a)))

;; issue 121
(test-error "make-bytevector" (make-bytevector -1))

;; log handling bignum
(test-equal "(log (expt 2 2048))" 1419.565425786768 (log (expt 2 2048)))
(test-equal "(log (expt 2 2048))" 1419.565425786768+3.141592653589793i
	    (log (- (expt 2 2048))))

;; issue 125
(test-equal "(sqrt (- (expt 2 2048)))" (- (expt 2 2048))
	    (* (sqrt (- (expt 2 2048))) (sqrt (- (expt 2 2048)))))

;; issue 126
(test-assert "(- 0 bignum)" (negative? (- 0 (expt 2 2048))))

;; issue 129
(test-assert "cond-expand" 
	     (r6rs:eval '(cond-expand ((and (library (rnrs)) 
				       (or sagittarius something)) 
				  #t))
		   '(sagittarius)))

;; issue 132
;; to avoid unbound variable on R6RS mode...
#!compatible
(test-assert "compiler error"
	     (r6rs:eval '(define (parse-string input-string)
			   (define (state0 c) (state13 (s1)))
			   (define (state11 c) )
			   (define (state12 c) (state11 (s2)))
			   (define (state13 c) (case c ((#\\) (state12 (s3)))))
			   (define (state29 c) (state0 (s4)) (state29 (s5)))
			   'a)
			(environment '(rnrs))))

;; issue 141
(define binary-file (string-append (current-directory) "/test/data/5bytes"))
(let ((in (open-file-input-port binary-file (file-options no-fail)
				(buffer-mode block))))
  (get-u8 in) ;; discard first byte
  (set-port-position! in 0)
  (test-equal "issue 141 set-port-position!" #vu8(1 2 3 4 5)
	      (get-bytevector-all in)))

;; issue 142
(test-assert "Issue 142 always negative (- 0 <bignum>)"
	     (positive? (- 0 (- (expt 2 64)))))

;; issue 143
(let ((s (number->string (least-fixnum))))
  (test-assert "Issue 143 fixnum boundary" (fixnum? (read (open-string-input-port s)))))

;; issue 144
(test-assert "Issue 144 compiler error" (negative? -8388609))

;; issue 145
(test-equal "Issue 145 bitwise-xor with -1" -18446744073709551617
	     (bitwise-xor (expt 2 64) -1))

;; issue 146
(test-assert "Issue 146 0<<65 is not fixnum" 
	     (fixnum? (bitwise-arithmetic-shift 0 65)))
(test-assert "Issue 146 0<<65 is not 0" (zero? (bitwise-arithmetic-shift 0 65)))

;; issue 147
(test-equal "Issue 147 fixnum div0(1)" 18 (fxdiv0 -222 -12))
(test-equal "Issue 147 fixnum div0(2)"(div0 28665 -6) (fxdiv0 28665 -6))
(test-equal "Issue 147 fixnum mod0(1)" -6 (fxmod0 -222 -12))
(test-equal "Issue 147 fixnum mod0(2)"(mod0 28665 -6) (fxmod0 28665 -6))

;; issue 149
(test-assert "Issue 149 (fxbit-set? 1 64) returns #t"
	     (not (fxbit-set? 1 64)))
(test-assert "Issue 149 -1 is all bits set"
	     (fxbit-set? -1 100))
(test-error "fxbit-set? second argument must be non negative" 
	    assertion-violation?
	    (fxbit-set? -1 -1))

;; issue 154
(let ()
  (define save #f)
  (let* ([p (make-custom-binary-input/output-port
	     "custom in"
	     (lambda (bv start end)
	       (bytevector-u8-set! bv start 7)
	       (set! save bv)
	       1)
	     (lambda (bv start end)
	       1)
	     #f #f #f)])
    (put-u8 p 10)
    (flush-output-port p)
    (get-u8 p)
    (close-port p))
  (test-assert "DO NOT PASS STACK ALLOCATED OBJECT TO SCHEME"
	       (call-with-string-output-port
		(lambda (out) (display save out) (newline out))))
  )

;; transcoded textual port also supports the port-position things
(let* ((bv (string->utf8 "あいうえお"))
       (in (transcoded-port (open-bytevector-input-port bv)
			    (native-transcoder))))
  (test-equal "peek-char" #\あ (peek-char in))
  ;; must be the begging
  (test-equal "port-position" 0 (port-position in))
  ;; In UTF-8, Japanese hiragana is 3 bytes
  ;; but put the position 2 byte
  (test-assert "set-port-position!" (set-port-position! in 2))
  (test-equal "get-char invalid" #\xFFFD (get-char in))
  (test-equal "port-position (2)" 3 (port-position in))
  (test-equal "get-char valid" #\い (get-char in))
)

;; issue 156
(let ()
  (define (open-test-port sink)
    (define (write! str start count)
      (put-string sink str start count)
      count)
    (make-custom-textual-output-port "test-runner-port" write! #f #f #f))
  (test-equal "issue 156 (write to custom textual port)"
	      "ok"
	      (call-with-string-output-port
	       (lambda (out)
		 (let ((cp (open-test-port out)))
		   ;; point is the auto convertion on display/write
		   (display "ok" cp)))))
)

;; integer->bytevector issue
;; we introduced sinteger->bytevector for this
(test-equal "#x-deadbeaf" #vu8(255 33 82 65 81)
	    (sinteger->bytevector #x-deadbeaf))
(test-equal "#x-80" #vu8(128) (sinteger->bytevector #x-80))
(test-equal "#x80" #vu8(00 128) (sinteger->bytevector #x80))
(test-equal "#x-8080" #vu8(255 127 128) (sinteger->bytevector #x-8080))
(test-equal "#x-7F80" #vu8(128 128) (sinteger->bytevector #x-7F80))
;; bignums (32 bit)
(test-equal "#x-80808080" #vu8(255 127 127 127 128)
	    (sinteger->bytevector #x-80808080))
(test-equal "#x-7F808080" #vu8(128 127 127 128)
	    (sinteger->bytevector #x-7F808080))

(test-error "negative value for integer->bytevector" condition?
	    (integer->bytevector -1))
(test-error "#x-deadbeaf" condition? (uinteger->bytevector #x-deadbeaf))
(test-error "#x-80" condition? (uinteger->bytevector #x-80))

(test-equal "#x-80" #x-80 (bytevector->sinteger #vu8(128)))
(test-equal "#x80" #x80 (bytevector->sinteger #vu8(0 128)))
(test-equal "#x-80" #x-80 (bytevector->sinteger #vu8(#xFF #xFF #xFF 128)))

(test-equal "#x80" #x80 (bytevector->uinteger #vu8(128)))
(test-equal "#x80" #x80 (bytevector->uinteger #vu8(0 128)))
(test-equal "#xFFFFFF80" #xFFFFFF80 
	    (bytevector->uinteger #vu8(#xFF #xFF #xFF #x80)))

(test-end)
