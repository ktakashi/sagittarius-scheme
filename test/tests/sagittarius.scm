;; -*- scheme -*-
#!compatible

(import (except (rnrs) equal?)
	(rnrs mutable-pairs)
	(rnrs mutable-strings)
	(rename (rnrs eval) (eval r6rs:eval))
	(rnrs r5rs)
	(sagittarius)
	(sagittarius vm)
	(sagittarius conditions)
	(encoding decoder)
	(core) ;; for simple-condition? and equal?
	(srfi :1)
	(srfi :19)
	(srfi :39)
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
(test-equal "integer->bytevector (0)" #vu8(0) (integer->bytevector 0))

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
(test-error "ascii 0 read" condition? (read (open-string-input-port "\x0;")))
(test-error "ascii 1 read" condition? (read (open-string-input-port "\x1;")))
(test-error "ascii 2 read" condition? (read (open-string-input-port "\x2;")))
(test-error "ascii 3 read" condition? (read (open-string-input-port "\x3;")))

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

;; call #6
(let ()
  (define-syntax wrap
    (syntax-rules ()
      ((_)
       (aif 'ok it #f))))
  (test-equal "wrapped it" 'ok (wrap)))

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
  (test-assert "equal? (record)" (equal? a b))

  (let ((a1 (kons 1 2))
	(b1 (kons 1 2)))
    (set-kar! a1 a1)
    (set-kar! b1 b1)
    (test-assert "eqv? (record)" (not (eqv? a1 b1)))))

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

  (test-equal "mod-inverse (32bit long environment)"
	      84793287459004005994083570264676611930995373170935977255695558296701128546491
	      (mod-inverse
	       59791678501913488631701617161572303141620876383029885416585973023996318696896
	       115792089210356248762697446949407573530086143415290314195533631308867097853951)
	      )
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

(test-equal "bignums(4096)"
	    #x440de3080bd660cb54783479497fdbf1e107cf52ea957f95517773e5d003636e2815ad1cae60b1ae7760a172a2ae4d486af54f80d6e296227e074ed636460ff15b505fa2ec3fa2f63c2a7ce5eb8345b31fbb904a05285cfbe3debe7d8bf4b6b9e42e02b1f1b78e9b4a46fb82f5d30658cc94ac13c36e0f781e65e58c9aed719672869734ebefbe9d5d9c215223560aa2ba0102c876da1dc24280e53cd54878284c8c5d40015f83324984691a7780bae0019d292fc8145c989c26eafef3efcc800d85fd7278956aad02f73e0b62d8e90bef329442a8efe07fbd9f5036ee7e4d94df159ce5215c616e088409540684a1fefacced9eb14b87bb6c5396ffd455fb08c861bc7c358540502d584cec1e8289980002c6b354012129ff5b48d9dc6a3c98f5f90454aa729d58ad6764264c610eb3ff7a2ba406fbec534c1ae2c874a92fc22a6e26dfde3c8856292f1b4bcb9b29a61e748baa4265abd65f2a1b1f8b244b3067b5f3d8c14be54dbd76637d0b5aca7f7ca6ffe3a1e989012d281737f45b9dc94fe4ffa6f0cebe3757366efa69f5b319a6623832adcca3e85a3fcd730282a0125dd1ccdff95710ac34d14f86ad2f718c48752693ea4295c9f70f6111ed1833ac5dc61d3c061d0b7906d3e7612352fbf3d3827412f8b513cc27b0648c7ceb5a3ee2e72d6a55ddad2d55fa2a5d58248902760a65b9c01b85a43c9ab4aabc6d2141
	    (mod-expt #x2d2875f005db8c7ef05939e2c4a0e004d0484c2695399d31451732a7d3ec5237c916e0f79480bb8cd66dc3a51d99f7b21d473123e264a58ff7cdf434f99bdf363d22bc2adb35e3c227f071e55916078c0c2fc05badb4868772e079b812fff3360bf2492bc39a90eb01a719532a197901426ed2af7a4a4d65ff902ce0cc4902b21fd981d6e2c97c73586ee6df462ef14d274c414cf5a48a20fcb0a5a4e73a028b0f8a2f671620e3fda15a600cfb12d41bf0957a88cd812675e3949f34ef49382905ce89d10ae321724463913b66ea5bd20af66ddf74a3600bfa9db9ff4b45413d47a9f2a9a2f9539f6a747183bfc499e5aab66c853c0481ad45ca5663b285895c0f598b8ddcad057be03d28f63bc6d20426d542ff43da2158ddc1117ffc721e6917f36d2696dc1e31a53cad6f7d8e114c783f1d52ddf2bd1e50f3a4cbc0740d3658848a8b8207d154d3afff34d45cf8e8a8a448dd491b93bf5da47d0f14d55aafdef63f18d6255361349a78d5f5dc0b171eaed74b5c33ab79ef403d6fb7b68ff93ffaf68f288b5d842958ebea3303042e22cf2f999d7a631a023f7ec2ad29bc0450080f1d72af5d7a24b5ae6c88822e5ff442f2a42d18ebc99ffc9a06925f5380216c2d7ad249f1305a321fb4229f2eb2cb590bb8e93b674c1296e5438567a89ee264ee7c5d034c0a7137ed14663d572b1583e1129f386e0428f4d42c1bc9e311
		      #x10001
		      #xd2de09e51c3f01ff3fd33e378843f0201bac1a76d758ac3d41663b308c28e59bc6b3a1323de9a37a720c54e7d16f3bcf926fedd0e5405e595d71537728c837ed1c0d857defc7a7dd5698c6b43af0d149d96fcdb847f156d738f665f8e6437e0558e3e03a048791ee6d4bf390a6df4775bacc6b4b38c707cb4462a68ccc652d74f2838ac27cce3ecc322a31e87d5043ed8d4b9dbe92597b5968e468a7506ff4208161b1688689e5be65864a847198904b16c5738e66ba3b1deeca681b427cef473a1f8c6c4941659acd040f8593362b17293688f1d63574be41a8b6c8d8c688e231a2b0852ec458f3cc7cc306dd0a4358ee82671f955d12dfde93b6337d7f0a4e2e7722d70f5ed4d5680d710e5306bff6adb7c52aed8ac89b25aec9d125398f96f9a8a59465c46c9afddcb0c3f7f0199ee234bee840213fbe19542b7a3bf54b2e84985d9a0352e9ca7b826df0f93253a1acd46e02b6339d4eb91e0792ea96d6192b8067d11667f4f1130cba6a2fb01f180d5b606c868c7f4d987d06b104db44042727243261b8cee3efdad884e36b0adb8e6a8240a794c219b021436de68ed321548306413029ace2364f679ddfd87e94b81caa7a05136dbd9585126e0af62f93b87bf6b1c54d69c8da6bd130582d01cc3a68691bbffe9c205228dfb63a87aaa9f5c378331160e93ad3f449ff0fd15fd29b9237eabdc175dd49addb5f658a0881))

(let ()
  ;; naive expt
  (define (expt-ref b e)
    (do ((i 1 (+ i 1)) (r b (* r b)))
	((= i e) r)))
  ;; testing number must be bignum for both 32 and 64 bit environment
  (test-equal "bignum expt (8)"
	      (expt-ref #x123456789ABCDEF12 8)
	      (expt #x123456789ABCDEF12 8))
  (test-equal "bignum expt (511)"
	      (expt-ref #x123456789ABCDEF12 511)
	      (expt #x123456789ABCDEF12 511))
  ;; starting exponent 512, using sliding window internally
  (test-equal "bignum expt (512)"
	      (expt-ref #x123456789ABCDEF12 512)
	      (expt #x123456789ABCDEF12 512))
  (test-equal "bignum expt (1000)"
	      (expt-ref #x123456789ABCDEF12 1000)
	      (expt #x123456789ABCDEF12 1000))

)

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

;; similar but different, reported by Atsushi Saito
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
	  (let/scope d1
	    (test-equal "let/scope in global bindings" 
			'(1 2 3) (list (d2 (d1 x)) (d2 x) x))))))))


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
;; this should not raise an error
;; (test-error "(atan 0+i)" values (atan 0+i))

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
			(environment '(sagittarius))))

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

;; issue 226
(test-equal "Issue 226 negative vs negative bitwise-xor"
	    4951760157141521099596496895
	    (bitwise-xor (expt -2 91) (- (expt -2 91) 1)))

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
(test-error "#x-80 (error)" condition? (uinteger->bytevector #x-80))

(test-equal "#x-80 (1 byte)" #x-80 (bytevector->sinteger #vu8(128)))
(test-equal "#x80" #x80 (bytevector->sinteger #vu8(0 128)))
(test-equal "#x-80 (3 bytes)" #x-80 (bytevector->sinteger #vu8(#xFF #xFF 128)))
(test-equal "#x-80 (4 bytes)" #x-80 (bytevector->sinteger #vu8(#xFF #xFF #xFF 128)))
(test-equal "#x-80 (5 bytes)" #x-80 (bytevector->sinteger #vu8(#xFF #xFF #xFF #xFF #x80)))

(test-equal "#x80" #x80 (bytevector->uinteger #vu8(128)))
(test-equal "#x80" #x80 (bytevector->uinteger #vu8(0 128)))
(test-equal "#xFFFFFF80" #xFFFFFF80 
	    (bytevector->uinteger #vu8(#xFF #xFF #xFF #x80)))

;; issue 162
(let*-values (((port extract) (open-bytevector-output-port))
              ((out) (transcoded-port port (native-transcoder))))
  (put-string out "hello")
  (test-equal "extract it " #vu8(104 101 108 108 111) (extract)))

;; issue 163 
(test-error "get-output-bytevector with non transcoded port"
	    condition?
	    (call-with-string-output-port 
	     (lambda (out) 
	       (put-string out "hello")
	       (get-output-bytevector out))))

(test-error "extract-output-bytevector with non transcoded port"
	    condition?
	    (call-with-string-output-port 
	     (lambda (out) 
	       (put-string out "hello")
	       (extract-output-bytevector out))))

;; issue 164
(test-equal "hex escape on R6RS mode" (string->symbol "foo bar")
	    (read (open-string-input-port "#!r6rs foo\\x20;bar")))
;; reset VM mode (not needed anymore, call #20)
;; #!compatible

;; some of the compiler optimisation stuff
(test-error "obvious error (1)" condition? ((lambda () (vector-length 'a) 'ng)))
(test-error "obvious error (2)" condition? ((lambda () (car 'a) 'ng)))

;; visibility check
(let ()
  (define-syntax renaming-test
  (syntax-rules ()
    ((_ var val)
     (begin
       (define dummy val)
       (define (var) dummy)))))
  (define dummy #f)
  (renaming-test a 'a)
  (test-equal "renaming-test" '(a #f) (list (a) dummy)))

;; invalid internal define
(test-error "(define dummy (begin (define ok? #f) 'ok))"
	    condition? (r6rs:eval '(define dummy (begin (define ok? #f) 'ok))
				  (environment '(rnrs))))

;; let-optionals* doesn't allow internal define
(test-assert "let-optionals* with internal define"
	     (r6rs:eval '(begin
			   (define (foo . opts) 
			     (let-optionals* opts ((a #t))
			       (define bar 'ok)
			       a))
			   (foo))
			(environment '(rnrs) '(sagittarius))))

;; local macro refered in global macro
(let ()
  (let-syntax ((a (syntax-rules () ((a) 'foo))))
    (define-syntax b (syntax-rules () ((_) (a)))))
  (test-equal "local macro refered in global macro"
	      'foo (b)))

(let ()
  (letrec-syntax ((not-visible (syntax-rules () ((_) 'foo)))))
  (test-error "local macro invalid scope" condition? (not-visible)))

(let ()
  (letrec-syntax ((not-visible (syntax-rules () ((_) 'foo)))))
  (define (b) (not-visible))
  (test-error "local macro invalid scope(2)" condition? (b)))

;; call #14
(let ()
  (define-syntax bind-to-zero
    (syntax-rules ()
      ((bind-to-zero id) (define id (zero)))))
  (bind-to-zero x)
  (define-syntax zero (syntax-rules () ((_) 0)))
  (test-equal "zero after definition" 0 x))

;; call #15
(test-equal "local macro compilation" 'ok
	    (let ()
	      (define-syntax foo
		(syntax-rules ()
		  ((_ ?atom ?stx ...)
		   (or (free-identifier=? ?atom ?stx) ...))))

	      (define-syntax bar
		(syntax-rules ()
		  ((_ ?atom ((?s ...) ?e ...) ... )
		   (cond ((foo ?atom (syntax ?s) ...) ?e ...) ... ))))
	      'ok))

;; call #16
(test-equal "expanded keyword arguments"
	    `(:a ,(undefined) :b ,(undefined) :c ,(undefined))
	    (let ()
	      (define-syntax define-key
		(lambda (x)
		  (define (key&name sym) (list (make-keyword sym) sym))
		  (define (params slots)
		    (let loop ((slots slots) (r '()))
		      (syntax-case slots ()
			(() (reverse! r))
			((name . rest)
			 (loop (cdr slots) 
			       (cons (key&name (syntax->datum #'name)) r))))))
		  (syntax-case x ()
		    ((k name (p ...) body ...)
		     (with-syntax ((((keys names) ...) 
				    (datum->syntax #'k (params #'(p ...)))))
		       #'(define (name :key names ...)
			   (apply append! (list `(keys ,names) ...))))))))
	      (define-key test (a b c) 'ignore)
	      (test)))

;; call #19
(test-equal "escape vertical" "|\\||" 
	    (call-with-string-output-port 
	     (lambda (out) (write '|\|| out))))

;; extra arguments for open-bytevector-input-port
(test-equal "open-bytevector-input-port(start 1)" "ello world!"
	    (get-string-all
	     (open-bytevector-input-port (string->utf8 "hello world!")
					 (native-transcoder)
					 1)))
(test-equal "open-bytevector-input-port(start 1, end 4)" "ell"
	    (get-string-all
	     (open-bytevector-input-port (string->utf8 "hello world!")
					 (native-transcoder)
					 1 4)))
(test-error "open-bytevector-input-port(error)" condition?
	    (open-bytevector-input-port (string->utf8 "hello world!")
					 (native-transcoder)
					 -1))
(test-error "open-bytevector-input-port(error)" condition?
	    (open-bytevector-input-port (string->utf8 "hello world!")
					 (native-transcoder)
					 13))
(test-error "open-bytevector-input-port(error)" condition?
	    (open-bytevector-input-port (string->utf8 "hello world!")
					 (native-transcoder)
					 0 13))

;; extra arguments for open-string-input-port
(test-equal "open-string-input-port(start 1)" "ello world!"
	    (get-string-all (open-string-input-port "hello world!" 1)))
(test-equal "open-string-input-port(start 1, end 4)" "ell"
	    (get-string-all (open-string-input-port "hello world!" 1 4)))
(test-error "open-string-input-port(error)" condition?
	    (open-string-input-port "hello world!" -1))
(test-error "open-string-input-port(error)" condition?
	    (open-string-input-port "hello world!" 13))
(test-error "open-string-input-port(error)" condition?
	    (open-string-input-port "hello world!" 0 13))

;; file option append
(let ()
  (define file "example.txt")
  (when (file-exists? file) (delete-file file))
  (call-with-output-file file (lambda (out) (put-string out "hello")))
  ;; bit awkward though
  (call-with-port (open-file-output-port file (file-options no-truncate append))
    (lambda (out) (put-bytevector out (string->utf8 " world!"))))
  (test-equal "file-options append" "hello world!"
	      (call-with-input-file file get-string-all)))
(let ()
  (define file "example.txt")
  (when (file-exists? file) (delete-file file))
  (call-with-output-file file (lambda (out) (put-string out "hello")))
  ;; bit awkward though
  (call-with-port (open-file-input/output-port file 
					       (file-options no-truncate append))
    (lambda (out) (put-bytevector out (string->utf8 " world!"))))
  (test-equal "file-options append(in/out)" "hello world!"
	      (call-with-input-file file get-string-all)))

;; extended set-port-position!
;; TODO must be tested all port type not only bytevector output port
(test-equal "set-port-position! (end)"
	    "hehehe"
	    (utf8->string
	     (let-values (((out extract) (open-bytevector-output-port)))
	       (put-bytevector out (string->utf8 "hello"))
	       (set-port-position! out -3 'end)
	       (put-bytevector out (string->utf8 "hehe"))
	       (extract))))
(test-equal "set-port-position! (current)"
	    "hehehe"
	    (utf8->string
	     (let-values (((out extract) (open-bytevector-output-port)))
	       (put-bytevector out (string->utf8 "hello"))
	       (set-port-position! out -3 'current)
	       (put-bytevector out (string->utf8 "hehe"))
	       (extract))))

(test-error "set-port-position! error (begin)"
	    condition?
	    (let-values (((out extract) (open-bytevector-output-port)))
	       (put-bytevector out (string->utf8 "hello"))
	       (set-port-position! out -3 'begin)
	       (put-bytevector out (string->utf8 "hehe"))
	       (extract)))
(test-equal "set-port-position! overvlow (end)"
	    "hello\x0;\x0;\x0;hehe"
	    (utf8->string
	     (let-values (((out extract) (open-bytevector-output-port)))
	       (put-bytevector out (string->utf8 "hello"))
	       (set-port-position! out 3 'end)
	       (put-bytevector out (string->utf8 "hehe"))
	       (extract))))
(test-equal "set-port-position! overflow (current)"
	    "hello\x0;\x0;\x0;hehe"
	    (utf8->string
	     (let-values (((out extract) (open-bytevector-output-port)))
	       (put-bytevector out (string->utf8 "hello"))
	       (set-port-position! out 3 'current)
	       (put-bytevector out (string->utf8 "hehe"))
	       (extract))))

(let ()
  (define file "example.txt")
  (when (file-exists? file) (delete-file file))
  (call-with-output-file file (lambda (out) (put-string out "hello world!")))
  (test-equal "set-port-position! (file end)" "world!"
	      (call-with-input-file file
		(lambda (in)
		  (set-port-position! in -6 'end)
		  (get-string-all in))))
  (test-equal "set-port-position! (file current)" "hello world!"
	      (call-with-input-file file
		(lambda (in)
		  (get-string-n in 5)
		  (set-port-position! in -5 'current)
		  (get-string-all in)))))

;; call #27 bytevector-input-port with set-port-position! overflowed
(test-equal "set-port-position! overflow"
	    (eof-object)
	    (call-with-port (open-bytevector-input-port 
			     (string->utf8 "hello world"))
	      (lambda (in)
		(set-port-position! in 100)
		(get-bytevector-all in))))

;; call #28
(let ((bv (call-with-bytevector-output-port
	   (lambda (out)
	     (set-port-position! out 100)
	     (set-port-position! out 0)
	     (put-bytevector out (string->utf8 "hello")))))
      (exptected (make-bytevector 100 0)))
  (bytevector-copy! (string->utf8 "hello") 0 exptected 0 5)
  (test-equal "length" 100 (bytevector-length bv))
  (test-equal "result" exptected bv))

(let ((bv (string->utf8 (call-with-string-output-port
			 (lambda (out)
			   (set-port-position! out 100)
			   (set-port-position! out 0)
			   (put-string out "hello")))))
      ;; well 32 is space...
      (exptected (make-bytevector 100 32)))
  (bytevector-copy! (string->utf8 "hello") 0 exptected 0 5)
  (test-equal "length" 100 (bytevector-length bv))
  (test-equal "result" exptected bv))

;; call #38
(test-equal "letrec* evaluation order"
	    "one-1one-2one-3two-1two-2two-3"
	    (call-with-string-output-port
	     (lambda (out)
	       (let ()
		 (letrec* ((one1 (begin (display "one-1" out) '11))
			   (one2 (begin (display "one-2" out) '12))
			   (one3 (begin (display "one-3" out) '13))
			   (two1 (begin (display "two-1" out) '11))
			   (two2 (begin (display "two-2" out) '12))
			   (two3 (begin (display "two-3" out) '13)))
		  (test-equal "don't optimise this" 
			      '(11 11) (list one1 two1)))))))

;; call #44
(test-equal "half value digit-value"
	    15/2 (digit-value (integer->char #x0F31)))

;; call #46
;; this needs to be defined *outside* of let,
;; otherwise compiler would optimise...
(define (normal-call k l)
  ((k (let loop ((l l) (r '())) 
	(if (null? l) r (loop (cdr l) (cons (car l) r)))))
   'ok))

(test-equal "normal-call"
	    '(ok 3 2 1)
	    (normal-call (lambda (l) (lambda (v) (cons v l))) '(1 2 3)))

;; call #49
(test-equal "bignum subtraction"
	    #xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff00000000000000000000000000000000
	    (- (expt 2 384) (expt 2 128)))

;; call #50
(let ()
  (define key #vu8(79 191 14 146 23 193 17 228 162 104 37 249 166 72 46 205))

(let ((ht (make-hashtable (lambda (bv)
                            (bytevector-uint-ref bv 0 (endianness big) 16))
                          equal?)))
  (test-assert "overflowing value set" (hashtable-set! ht key #t))
  (test-assert "overflowed value ref" (hashtable-ref ht key #f)))
)

;; call #52
;; eval can accept bindings in Sagittarius (not R6RS compliant)
(test-error "let-syntax scope" condition?
	    (eval '(begin
		     (define-syntax let-syntax
		       (syntax-rules ()
			 ((_ ((var trans) ...) expr ...)
			  (let ()
			    (rnrs:let-syntax ((var trans) ...) expr ...)))))
		     (let-syntax ((var (syntax-rules ())))
		       (define bar 'bar)
		       'ok)
		     bar)
		  (environment '(rename (rnrs) (let-syntax rnrs:let-syntax)))))
;; call #53
(test-equal "escaped symbol"
	    "|'\\x0a;|"
	    (call-with-string-output-port
	     (lambda (out)
	       (write
		(read (open-string-input-port
		       (call-with-string-output-port
			(lambda (out)
			  (display "|'\n|" out)))))
		out))))

;; call #56
(let ()
  (define strings "#!fold-case ABC ABC ΓΛΏΣΣΑ ΜΈΛΟΣ \
                   #!no-fold-case ABC ABC ΓΛΏΣΣΑ ΜΈΛΟΣ")
  
  (test-equal "#!fold-case"
	      "abcabcγλώσσαμέλοσABCABCΓΛΏΣΣΑΜΈΛΟΣ"
	      (call-with-port (open-string-input-port strings)
		(lambda (in)
		  (call-with-string-output-port
		   (lambda (out)
		     (let loop ((s (read in)))
		       (unless (eof-object? s)
			 (write s out)
			 (loop (read in))))))))))

;; call #66
(test-assert "toplevel macro expansion in library"
	     (r6rs:eval '(library (issue-66)
			     (export make-foo foo? foo-foo foo-bar)
			     (import (srfi :9))
			   (define-record-type <foo>
			     (make-foo foo bar)
			     foo?
			     (foo foo-foo)
			     (bar foo-bar)))
			;; need 'library syntax defined in (sagittarius)
			(environment '(sagittarius))))

;; call #68
(let ()
  (define (foo :key bar :allow-other-keys rest) rest)
  (test-equal ":allow-other-keys order"
	      '(:a :b :c :d) (foo :a :b :c :d)))

;; call #72
(test-assert "no export in define-library"
	     (r6rs:eval '(define-library (foo)) (environment '(sagittarius))))

;; call #76
(test-assert "parameter with transcoder" (make-parameter (native-transcoder)))
(test-assert "parameter with codec" (make-parameter (utf-8-codec)))

;; call #77
;; test from Mosh
(let ([only-once #t]
      [v0 (vector 1 2 3 4 5 6)]
      [cl '()]
      [old-v1 #f])
  (let ([v1 (vector-map
	     (lambda (e)
	      (call/cc
	       (lambda (c)
		(set! cl (cons c cl))
		(* e e))))
	     v0)])
    (when only-once
      (set! only-once #f)
      (set! old-v1 v1)
      ((car (reverse cl)) 'x))
    (test-equal '#(1 2 3 4 5 6) v0)
    (test-equal '#(1 4 9 16 25 36) old-v1)
    (test-equal '#(x 4 9 16 25 36) v1)))

;; call #78...
(test-equal "real-part 123" 123 (real-part 123))
(test-equal "real-part 1.23" 1.23 (real-part 1.23))
(test-equal "real-part 1/23" 1/23 (real-part 1/23))

;; call #79
(test-assert "(apply = 1+1i 1+1i '())" (apply = 1+1i 1+1i '()))

;; call #80
(test-equal "vector-reverse!" 
	    #(1 2 3 7 6 5 4 8 9 10)
	    (vector-reverse! (vector 1 2 3 4 5 6 7 8 9 10) 3 7))
(test-equal "bytevector-reverse!" 
	    #vu8(1 2 3 7 6 5 4 8 9 10)
	    (bytevector-reverse! (u8-list->bytevector 
				  '(1 2 3 4 5 6 7 8 9 10))
				  3 7))

;; nan thing
(test-assert "min with nan (0)" (nan? (min -inf.0 +nan.0)))
(test-assert "min with nan (1)" (nan? (min +nan.0 -inf.0)))
(test-assert "min with nan (2)" (nan? (min -inf.0 +nan.0 1)))
(test-assert "min with nan (3)" (nan? (min 1 +nan.0 -inf.0)))

(test-assert "max with nan (0)" (nan? (max +inf.0 +nan.0)))
(test-assert "max with nan (1)" (nan? (max +nan.0 +inf.0)))
(test-assert "max with nan (2)" (nan? (max +inf.0 +nan.0 1)))
(test-assert "max with nan (3)" (nan? (max 1 +nan.0 +inf.0)))

;; call #82
(let ()
  (define (make-custom)
    (define (read! s str count) count)
    (define (close) #t)
    (make-custom-textual-input-port "custom" read! #f #f close))
  (test-equal "get-string-n with 0"
	      ""
	      (call-with-port (make-custom)
		(lambda (in)
		  (get-string-n in 0)))))

(let ((file "get-string-n-0.tmp"))
  ;; make it non empty file
  (call-with-port (open-file-output-port file 
					 (file-options no-fail) 
					 (buffer-mode block)
					 (native-transcoder)) 
    (lambda (out) (put-string out "hoge")))
  (test-equal "get-string-n with 0 (file)"
	      ""
	      (call-with-input-file file
		(lambda (in) (get-string-n in 0))))
  (test-equal "get-string-n! with 0 (file)"
	      0
	      (call-with-input-file file
		(lambda (in) 
		  (let ((s (make-string 1 #\space)))
		    (get-string-n! in s 0 0)))))

  ;; was i/o error
  (test-equal "get-bytevector-n!"
	      #vu8(0)
	      (call-with-port (open-file-input-port file 
						    (file-options no-fail)
						    'none)
		(lambda (in)
		  (lookahead-u8 in)
		  (let ((bv (make-bytevector 1 0)))
		    (get-bytevector-n! in bv 0 0)
		    bv))))
  ;; was eof
  (test-equal "get-bytevector-n! reading count"
	      0
	      (call-with-port (open-file-input-port file 
						    (file-options no-fail)
						    'none)
		(lambda (in)
		  (let ((bv (make-bytevector 1 0)))
		    (get-bytevector-n! in bv 0 0)))))

  (delete-file file))

(let ((file "get-string-n-1.tmp"))
  ;; make empty file
  (call-with-port (open-file-output-port file 
					 (file-options no-fail) 
					 (buffer-mode block)
					 (native-transcoder)) 
    (lambda (out) #t))
  (test-equal "get-string-n with 0 (file)"
	      ""
	      (call-with-input-file file
		(lambda (in) (get-string-n in 0))))

  (test-equal "get-string-n! with 0 (file)"
	      0
	      (call-with-input-file file
		(lambda (in) 
		  (let ((s (make-string 1 #\space)))
		    (get-string-n! in s 0 0)))))
  (delete-file file))

;; U+180E is not whitespace...
(test-assert "Unicode 6.3.0 or later" (not (char-whitespace? #\x180E)))
(test-error "Unicode 6.3.0 or later..."
	    i/o-error?
	    (let ()
	      (define s (list->string '(#\" #\\ #\x180e #\")))
	      (let ((in (open-string-input-port s)))
		(do ((x (get-datum in) (get-datum in))
		     (results '() (cons x results)))
		    ((eof-object? x)
		     (reverse results))))))

;; read-line
(let ()
  (define (->port s) (open-string-input-port s))
  (test-equal "read-line (1)" "abc" (read-line (->port "abc\rdef")))
  (test-equal "read-line (2)" "abc" (read-line (->port "abc\ndef")))
  (test-equal "read-line (3)" "abc" (read-line (->port "abc\r\ndef"))))

(let ()
  (define (->port s) (open-string-input-port s))
  (test-equal "get-line (1)" "abc\rdef" (get-line (->port "abc\rdef")))
  (test-equal "get-line (2)" "abc"      (get-line (->port "abc\ndef")))
  (test-equal "get-line (3)" "abc\r"    (get-line (->port "abc\r\ndef"))))

;;; get-bytevector-until
(let ((in (open-bytevector-input-port (string->utf8 "hello\n"))))
  (test-equal "get-bytevector-until(1)" (string->utf8 "hello")
	      (get-bytevector-until in #vu8(#x0A)))
  (test-assert "get-bytevector-until(2)" 
	       (eof-object? (get-bytevector-until in #vu8(#x0A)))))

;; hashtable SEGV
(let ((ht (make-hashtable eqv-hash =)))
  (hashtable-set! ht 0 1)
  (test-assert "SEGV case" (hashtable-set! ht 0 1)))

;; #97
(test-equal "sint-list->bytevector (with 0)"
	    #vu8(0)
	    (sint-list->bytevector '(0) (endianness little) 1))


;; found in nausicaa-oopp
;; template variable should be renamed properly
;; this caused infnite loop before
(define-syntax %define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))

(define-syntax define-with-caller
  (lambda (stx)
    (syntax-case stx ()
      ((_ (?who (?caller-arg ...) ((?func-arg ?func-default) ...))
	  ?body0 ?body ...)
       (with-syntax (((FUNCTION) (generate-temporaries #'(#f))))
	 #'(begin
	     (%define-inline (?who ?caller-arg ...)
	       (FUNCTION ?caller-arg ... ?func-default ...))
	     (define (FUNCTION ?caller-arg ... ?func-arg ...)
	       (let-syntax ((?who (identifier-syntax FUNCTION)))
		 ?body0 ?body ...))))))))


(define (make-tagged-variable-transformer tag-id src-var-id)
  (make-variable-transformer
   (lambda (stx)
     (with-syntax ((TAG		tag-id)
		   (SRC-VAR	src-var-id))
       (syntax-case stx (set!)

	 ;;Syntax to reference the value of the binding.
	 (??var
	  (identifier? #'??var)
	  src-var-id)

	 ;;Syntax to apply a method or reference a field of the tag
	 ;;of ?VAR.
	 ((??var . ??stuff)
	  #'(TAG dispatch: SRC-VAR (??var . ??stuff)))

	 (_
	  (syntax-violation '?var
	    "invalid tagged-variable syntax use" stx)))))))

(define-with-caller (parse-let-bindings (bindings-stx top-id synner)
					((vars			'())
					 (tags			'())
					 (syntax-bindings	'())))

  (syntax-case bindings-stx ()
    (()
     (list (reverse vars) (reverse tags) (reverse syntax-bindings)))
    (((?var ?tag) . ?other-bindings)
     (let ((tag-id #'?tag))
       (parse-let-bindings #'?other-bindings top-id synner
	   (cons #'?var vars)
	   (cons tag-id tags)
	   (if (free-identifier=? tag-id top-id)
	       syntax-bindings
	       (cons #'(?var (make-tagged-variable-transformer #'?tag #'?var))
		     syntax-bindings)))))
    (_
     (synner "invalid bindings syntax" bindings-stx))))

(define-syntax with-tags
  (lambda (x)
    (define (synner msg . irr) (error 'with-tags msg irr))
    (syntax-case x ()
      ((_ (?var ...) ?body0 ?body ...)
       (with-syntax
	   ((((VAR ...) (TAG ...) (SYNTAX-BINDING ...))
	     (parse-let-bindings #'(?var ...) #'<top> synner)))
	 #`(let-syntax (SYNTAX-BINDING ...) ?body0 ?body ...)))
      (_
       (synner "syntax error")))))

(define-syntax <beta>
  (lambda (x)
    (syntax-case x (dispatch:)
      ((_ dispatch: src (var . stuff)) #'src))))

(define (beta-def-ref o)
  (with-tags ((o <beta>))
    (list (o d) (o e) (o f))))

(test-equal "template variable boundness" '(a a a) (beta-def-ref 'a))

(let ()
  (define-syntax wrap
    (syntax-rules ()
      ((_ name)
       (define (name o)
	 (with-tags ((o <beta>))
	   (list (o d) (o e) (o f)))))))
  (wrap beta-def-ref)
  (test-equal "template variable boundness (wrapped)" '(a a a) 
	      (beta-def-ref 'a)))

;; call #108
(test-equal "compilation error due to the incorrect check of $label"
	    '#(11)
	    (let () '#(11)))

;; call #113
(test-equal "complex number subtraction (fixnum)" 0-i (- 0 0+i))
(test-equal "complex number subtraction (fixnum)" 1-i (- 1 0+i))
(test-equal "complex number subtraction (bignum)" #xFFFFFFFFFFFFFFFF-i
	    (- #xFFFFFFFFFFFFFFFF 0+i))
(test-equal "complex number subtraction (rational)" 1/3-i (- 1/3 0+i))
(test-equal "complex number subtraction (flonum)" 0.0-1.0i (- 0.0 0+i))

;; call #121
(test-equal "quotient" 5.0 (quotient 17.0 3.0))

;; call #122
(test-assert "(log -0.0)"
	     (let* ((r (log -0.0))
		    (real (real-part r))
		    (imag (imag-part r)))
	       (and (infinite? real)
		    (<= 3.141592 imag 3.141593))))

;; modulo should return 0.0
(test-eqv "(modulo -15.0 -3.0" 0.0 (modulo -15.0 -3.0))
(test-eqv "(modulo -15.0 3.0"  0.0 (modulo -15.0 3.0))

;; escape thing on escaped symbol
(test-equal "|\\x07;\\x08;\\x09;\\x0a;\\x0d;\\|\"\\|"
	    '|\x07;\x08;\x09;\x0a;\x0d;\|"\\|
	    (read
	     (open-string-input-port
	      (string #\| #\\ #\a #\\ #\b #\\ #\t #\\ #\n #\\ #\r #\\ #\| #\\ #\" #\\ #\\ #\|))))

(test-equal "write identifier with null"
	    ;; this is slightly depending on the mode.
	    "|\\x00;\\x01;\\x02;\\x09;\\x0d;A\\x0a;\\x09;~\\x7f;|"
	    (call-with-string-output-port
	     (lambda (out)
	       (write '|\x000;\x01;\x2;\t\r\x41;\n\t\x7e;\x7f;| out))))

;; call #124,
(test-error "(exact +inf.0)" assertion-violation? (exact +inf.0)) 
(test-error "(exact -inf.0)" assertion-violation? (exact -inf.0)) 
(test-error "(exact +nan.0)" assertion-violation? (exact +nan.0)) 

;; this caused SEGV/panic
(let ()
  (define bv
    #vu8(117 23 184 61 117 118 125 80 30 220 41 170 238 203 62 205 103 193 58 55 7 64 159 106 80 198 235 228 192 213 160 39 217 235 60 177 25 138 65 4 2 48 45 158 18 95 130 1 203 149 190 171 171 84 99 30 52 26 239 216 228 105 228 91 61 191 192 37 189 158 5 184 230 132 49 146 54 243 198 53 47 105 66 31 79 104 39 210 24 110 243 204 8 253 40 143 161 147 68 153 97 178 201 108 202 216 53 134 90 135 10 105 147 61 198 147 243 124 235 134 252 194 247 192 44 145 108 195 5 103 200 13 5 198 141 32 174 44 57 122 126 27 78 157 247 17 74 7 151 144 175 58 224 22 251 180 80 37 176 247 105 59 76 129 169 218 81 212 146 128 61 110 162 175 146 209 91 229 206 123 59 164 115 238 196 106 255 168 116 185 244 43 205 15 208 245 77 238 21 136 118 212 65 98 198 67 214 5 191 185 82 239 223 184 55 162 168 190 3 156 152 77 56 95 49 227 84 105 241 2 217 60 90 40 69 86 234 215 26 57 35 141 214 99 131 204 251 86 140 18 7 144 188 225 124 147 149 183 216 221 149 22 157 240 62 124 73 74 14 107 94 109 135 97 90 215 39 224 191 10 112 102 11 132 224 117 192 199 249 139 92 81 57 42 97 164 34 208 77 62 50 255 162 161 235 53 222 75 203 244 131 190 84 186 15 120 4 9 4 251 93 95 224 197 221 62 37 88 6 36 81 50 86 147 230 213 79 9 98 191 111 8 71 114 56 14 19 108 40 157 72 47 193 51 100 57 129 82 233 12 234 120 85 38 122 39 42 9 179 221 230 51 19 28 11 38 28 98 23 96 76 49 140 99 37 7 232 173 37 102 173 192 206 204 89 26 158 187 110 61 23 177 106 167 183 48 207 218 0 182 27 84 112 197 208 151 137 219 108 33 201 122 113 116 178 32 93 14 194 79 178 113 123 133 238 27 91 68 147 142 228 10 31 72 148 89 20 203 237 175 240 149 109 142 53 40 150 116 97 2 230 227 246 165 223 89 114 143 255 88 87 66 200 94 35 60 184 237 88 255 81 131 116 9 17 162 249 220 122 200 101 246 10 119 58 217 67 45 246 3 163 108 27 246 172 178 39 48 220 129 156 51 181 87 248 125 181 182 189 144 94 28 105 234 46 11 126 13 167 1 122 119 71 128 223 170 144 6 43 36 0 21 224 103 25 43 124 241 89 202 129 68 66 112 109 222 82 159 194 65 11 213 254 235 235 148 163 94 116 90 47 24 36 169 36 155 125 255 0 101 253 222 69 248 38 196 113 210 118 51 6 117 111 169 130 95 143 168 103 18 88 174 51 12 72 61 104 207 225 211 132 217 161 242 9 172 10 24 117 198 154 199 74 169 211 125 6 211 51 188 43 198 60 2 55 0 108 209 172 3 69 167 8 77 69 6 205 96 238 108 121 186 190 91 142 221 55 81 138 71))

  (test-assert "bytevector->string more than pre defined buffer"
	       (utf16->string bv 'little)))
;; cpu-count
(test-assert "cpu-count" (>= (cpu-count) 1))

;; stack-trace condition
;; we may remove &stack-trace in the future.
;; (test-assert "compound condition"
;; 	     (guard (e (else (stack-trace-condition? e)))
;; 	       (error 'dummy "it's me")))

(test-assert "simple condition"
	     (guard (e (else (simple-condition? e)))
	       (raise (make-who-condition 'who))))

;; call #144
(test-assert "2.225073858507201e-308" (string->number "2.225073858507201e-308"))

(let ()
  (define (string-copy! src spos dst dpos size)
    (do ((i 0 (+ i 1)) (spos spos (+ spos 1)) (dpos dpos (+ dpos 1)))
	((= i size) size)
      (string-set! dst dpos (string-ref src spos))))
  (define (make-input s)
    (define pos 0)
    (define str-len (string-length s))
    (define (read! out start size)
      (if (>= pos str-len)
	  0
	  (let ((size (min (- str-len pos) size)))
	    (string-copy! s pos out start size)
	    (set! pos (+ pos size))
	    size)))
    (define (close) #f)
    (make-custom-textual-input-port "input" read! #f #f close))
  (let ((in (make-input "abcde")))
    (test-equal "get-line with custom port"
		"abcde"
		(get-line in))))

;; call #149
(test-equal "closure cache" #t (eval '(run) (environment '(inlined-cache))))
(test-assert "not cachable"
	     (not (eval '(cachable? *bar2*)
			(environment '(sagittarius) '(closure-cache)))))

;; test from Gauche
(let ([a 1] [b 2])
  (let-syntax ([foo (er-macro-transformer
                     (lambda (f r c)
                       (r '(cons (list a b) `#(,a ,b)))))])
    (let ([a -1] [b -2] [list *])
      (test-equal "list arg for rename procedure"
		  '((1 2) . #(1 2))
		  (foo)))))

;; use input
(let* ((e '(("a" "b") #("c") a))
       (v (eval `(let-syntax ((foo (lambda (x)
				     (syntax-case x ()
				       ((_ (a v c)) #'#(a v c))))))
		   (foo ,e))
		(environment '(rnrs)))))
  (test-assert "no reconstruct after macro expansion"
	       (and (eq? (vector-ref v 0) (car e))
		    (eq? (vector-ref v 1) (cadr e)))))

;; call #161
(test-equal "(- 0 1/3)" -1/3 (- 0 1/3))

;; radix optimisation for bignum
(define (radix-test radix)
  (define (do-test bases)
    (define exponent 1000)
    (for-each
     (lambda (base)
       (test-equal (format "fast number->string on radix ~a(~a)" radix base)
		   (expt base exponent) 
		   (string->number (number->string
				    (expt base exponent) radix)
				   radix))
       (test-equal (format "fast number->string on radix ~a(~a)(neg)"
			   radix base)
		   (- (expt base 1000) )
		   (string->number (number->string
				    (- (expt base exponent)) radix)
				   radix)))
     bases))
  (do-test '(2 3 5 7)))

(for-each radix-test (iota 35 2))

;; er-macro-transformer comparison procedure
(let ()
  (define-syntax foo
    (er-macro-transformer
     (lambda (f r c)
       (c f (r f)))))
  (test-assert "er compare vector" (foo #(a b c))))


;; titlecase with special casing

(let ()
  (define Floo "\xFB02;oo")
  (define Floo-bar "\xFB02;oo bar")
  (define Baffle "Ba\xFB04;e")
  (define LJUBLJANA "\x01C7;ub\x01C7;ana")
  (define Ljubljana "\x01C8;ub\x01C9;ana")
  (define ljubljana "\x01C9;ub\x01C9;ana")

  (define-syntax test
    (syntax-rules ()
      ((_ expect expr)
       (test-equal expect expect expr))))

  (test "\x01C5;" (string-titlecase/special-casing "\x01C5;"))
  (test "\x01C5;" (string-titlecase/special-casing "\x01C4;"))
  (test "Ss" (string-titlecase/special-casing "\x00DF;"))
  (test "Xi\x0307;" (string-titlecase/special-casing "x\x0130;"))
  (test "\x1F88;" (string-titlecase/special-casing "\x1F80;"))
  (test "Bar Baz" (string-titlecase/special-casing "bAr baZ"))
  (test "Floo" (string-titlecase/special-casing "floo"))
  (test "Floo" (string-titlecase/special-casing "FLOO"))
  (test "Floo" (string-titlecase/special-casing Floo))
  (test "Floo Bar" (string-titlecase/special-casing"floo bar"))
  (test "Floo Bar" (string-titlecase/special-casing "FLOO BAR"))
  (test "Floo Bar" (string-titlecase/special-casing Floo-bar))
  (test Baffle (string-titlecase/special-casing Baffle))
  (test Ljubljana (string-titlecase/special-casing LJUBLJANA))
  (test Ljubljana (string-titlecase/special-casing Ljubljana))
  (test Ljubljana (string-titlecase/special-casing ljubljana)))

(let ((v (vector 0 3 5 2 1 0)))
  (test-error "vector-sort! (err1)" assertion-violation? (vector-sort! < v -1))
  (test-error "vector-sort! (err2)" assertion-violation? (vector-sort! < v 10))
  (test-error "vector-sort! (err3)" assertion-violation? (vector-sort! < v 5 2))
  (test-error "vector-sort! (err4)" assertion-violation? 
	      (vector-sort! < v 0 -2))

  (test-assert "vector-sort! start" (vector-sort! < v 1))
  (test-equal "vector-sort! (start)" '#(0 0 1 2 3 5) v))

(let ((v (vector 0 3 5 2 1 0)))
  (test-assert "vector-sort! start end" (vector-sort! < v 1 5))
  (test-equal "vector-sort! start end" '#(0 1 2 3 5 0) v))

(let ((v (vector 0 3 5 2 1 0)))
  (test-assert "vector-sort! start end (same)" (vector-sort! < v 0 0))
  (test-equal "vector-sort! start end (same)" '#(0 3 5 2 1 0) v)
  (test-assert "vector-sort! start end (same)" (vector-sort! < v 6 6))
  (test-equal "vector-sort! start end (same)" '#(0 3 5 2 1 0) v))

(let ((v (vector -3 3 -1 1)))
  (define (abs< a b) (< (abs a) (abs b)))
  (test-assert "vector-sort! stable" (vector-sort! abs< v))
  (test-equal "vector-sort! stable" '#(-1 1 -3 3) v))
(let ((v (vector -3 3 -1 1)))
  (define (abs< a b) (< (abs a) (abs b)))
  (test-assert "vector-sort! stable" (vector-sort! abs< v 2))
  (test-equal "vector-sort! stable" '#(-3 3 -1 1) v))

(let ((v (vector 0 3 5 2 1 0)))
  (test-error "vector-sort (err1)" assertion-violation? (vector-sort < v -1))
  (test-error "vector-sort (err2)" assertion-violation? (vector-sort < v 10))
  (test-error "vector-sort (err3)" assertion-violation? (vector-sort < v 5 2))
  (test-error "vector-sort (err4)" assertion-violation? 
	      (vector-sort < v 0 -2))

  (test-equal "vector-sort (start)" '#(0 0 1 2 3 5) (vector-sort < v 1))

  (test-equal "vector-sort start end" '#(0 1 2 3 5 0) (vector-sort < v 1 5))

  (test-equal "vector-sort start end (same)" '#(0 3 5 2 1 0)
	      (vector-sort < v 0 0))
  (test-equal "vector-sort start end (same)" '#(0 3 5 2 1 0) 
	      (vector-sort < v 6 6)))

(let ((v (vector -3 3 -1 1)))
  (define (abs< a b) (< (abs a) (abs b)))
  (test-equal "vector-sort stable" '#(-1 1 -3 3) (vector-sort abs< v))
  (test-equal "vector-sort stable" '#(-3 3 -1 1) (vector-sort abs< v 2)))

;; asin and acos
(test-eqv "(asin 0)" 0 (asin 0))
(test-eqv "(acos 1)" 0 (acos 1))

;; symbol comparison
(let ()
  (define-syntax test
    (syntax-rules ()
      ((_ expr expected)
       (test-equal 'expr expected expr))))

  (test (symbol<? 'z 'z) #f)
  (test (symbol<? 'z '\xDF;) #t)
  (test (symbol<? '\xDF; 'z) #f)
  (test (symbol<? 'z 'zz) #t)
  (test (symbol<? 'z 'Z) #f)
  (test (symbol<=? 'z '\xDF;) #t)
  (test (symbol<=? '\xDF; 'z) #f)
  (test (symbol<=? 'z 'zz) #t)
  (test (symbol<=? 'z 'Z) #f)
  (test (symbol<=? 'z 'z) #t)

  (test (symbol<? 'z 'z) #f)
  (test (symbol>? 'z '\xDF;) #f)
  (test (symbol>? '\xDF; 'z) #t)
  (test (symbol>? 'z 'zz) #f)
  (test (symbol>? 'z 'Z) #t)
  (test (symbol>=? 'z '\xDF;) #f)
  (test (symbol>=? '\xDF; 'z) #t)
  (test (symbol>=? 'z 'zz) #f)
  (test (symbol>=? 'z 'Z) #t)
  (test (symbol>=? 'z 'z) #t)
  )

;; issue #168
(let ()
  (define s (string #\a))
  (define sm (string->symbol s))
  (test-error "modifying string from symbol->string"
	      assertion-violation?
	      (string-set! (symbol->string sm) 0 #\b)))


;; test for SRFI-61
;; from example
(test-equal '(#\c)
	    (let ((in (open-string-input-port "c")))
	      (cond ((read-char in) char? =>
		     (lambda (c) (list c))))))
;; multiple values
(test-equal '(1 2)
	    (cond ((values 1 2) (lambda (a b) #t) =>
		   (lambda (a b) (list a b)))))

;; character difference between R6RS and R7RS
;; NB: we are using #!r6rs directives so the evaluation must be done
;;     on top level (otherwise the VM mode will be changed before eval)
(define r6rs-chars
  '("#\\nul"
    "#\\alarm"
    "#\\backspace"
    "#\\tab"  
    ("#\\linefeed" . "#\\newline")
    "#\\newline"
    "#\\vtab"
    "#\\page"
    "#\\return"
    "#\\esc"
    "#\\space"
    "#\\delete"))
(define r7rs-chars
  '("#\\null"
    "#\\alarm"
    "#\\backspace"
    "#\\tab"  
    "#\\newline"
    "#\\return"
    "#\\escape"
    "#\\space"
    "#\\delete"))
(define (check who chars)
  (define (check1 c)
    (let ((r (if (pair? c) (car c) c))
	  (w (if (pair? c) (cdr c) c)))
      (test-equal (format "~a ~s" who c)
		  w
		  (call-with-port (open-string-input-port r)
		    (lambda (in) 
		      (call-with-string-output-port
		       (lambda (out)
			 (write (read in) out))))))))
  (for-each check1 chars))
#!r6rs (check 'r6rs r6rs-chars)
#!r7rs (check 'r7rs r7rs-chars)

(define (test-directives alist)
  (for-each (lambda (p)
	      (test-equal (car p) (cdr p)
			  (find-default-directive-by-path (car p))))
	    alist))

(test-directives '(("foo/foo.sld" . r7rs)
		   ("foo/foo.sls" . r6rs)
		   ("foo/foo.ss" . r6rs)
		   ("foo/foo.scm" . compatible)))

;; string literal and immutable string
(test-assert "literal-string? (1)" (literal-string? "abc"))
(test-assert "literal-string? (2)"
	     ;; "abc" is interend above but this is not a literal string
	     (not (literal-string? (string #\a #\b #\c))))
(test-assert "istring? (1)" (istring? "abc"))
(test-assert "istring? (2)" (not (istring? (string #\a #\b))))
(test-assert "istring? (3)" (istring? (string->istring (string #\a #\b))))

(define (test-istring es s . opt)
  (let ((is (apply string->istring s opt)))
    (test-equal (format "string->istring ~s" `(string->istring s ,@opt)) es is)
    (test-error (format "string->istring ~s" `(string->istring s ,@opt))
		assertion-violation? (string-set! is 0 #\b))))
(test-istring "abc" (string #\a #\b #\c))
(test-istring "bc" (string #\a #\b #\c) 1)
(test-istring "ab" (string #\a #\b #\c) 0 2)
(test-istring "bc" (string #\a #\b #\c #\d) 1 3)

;; issue 190
(test-error "non continuable condition"
	    condition?
	    (with-exception-handler
	     (lambda (k) #t)
	     (lambda ()
	       (with-exception-handler
		(lambda (k) #t)
		(lambda ()
		  (raise 'a))))))

;; issue 189
(test-equal "with-exception-handler (1)" 'ok
	    (with-exception-handler
	     (lambda (k) 'ok)
	     (lambda ()
	       (guard (con ((error? con) 'ng) (#f #f))
		 (raise-continuable 'test)))))

(test-error "with-exception-handler (2)" non-continuable-violation?
	    (with-exception-handler
	     (lambda (k) #f)
	     (lambda ()
	       (guard (con ((error? con) 'ng) (#f #f))
		 (raise 'test)))))

(test-error "with-exception-handler (3)" non-continuable-violation?
	    (with-exception-handler
	     (lambda (k) #f)
	     (lambda ()
	       (guard (con (#f #f))
		 (error 'who "msg")))))

(test-error "toplevel check"
	    (eval '(let () (export foo) 'ng)
		  (environment '(rnrs) '(sagittarius))))

#!compatible
;; better variable checks
(define (test-compile-error-expr expr)
  (test-error expr condition? (eval expr (environment '(rnrs)))))

(test-compile-error-expr '(let ((a 1) (a 1)) a))
(test-compile-error-expr '(letrec ((a 1) (a 1)) a))
(test-compile-error-expr '(letrec* ((a 1) (a 1)) a))
(test-compile-error-expr '(let () (define a 1) (define a 1) a))
(test-compile-error-expr '(lambda (a a) a))
(test-compile-error-expr '(lambda (a :key (b 1) b) a))
(test-compile-error-expr '(do ((i 0) (i 1)) (#t #f)))
(test-compile-error-expr
 '(let () (define a 1) (define-syntax a (lambda (x) x)) a))
(test-compile-error-expr
 '(let-syntax ((a (lambda (x) x)) (a (lambda (x) x))) 'ok))
(test-compile-error-expr
 '(letrec-syntax ((a (lambda (x) x)) (a (lambda (x) x))) 'ok))

(test-compile-error-expr '(let ((1 1) (a 1)) a))
(test-compile-error-expr '(letrec ((1 1) (a 1)) a))
(test-compile-error-expr '(letrec* ((1 1) (a 1)) a))
(test-compile-error-expr '(lambda (a 1) a))

(define-syntax test-i/o-error
  (syntax-rules ()
    ((_ expr) (test-error i/o-error? expr))))
(test-i/o-error (copy-file "doesn't-exist" "doesn't-exist-as-well"))
(test-i/o-error (file-stat-ctime "doesn't-exist"))
(test-i/o-error (file-stat-mtime "doesn't-exist"))
(test-i/o-error (file-stat-atime "doesn't-exist"))
(cond-expand
 ((not darwin)
  ;; This test case doesn't work on Mac, which creates a symlink against
  ;; a file with empty name... nice...
  (test-i/o-error (create-symbolic-link "" "no:such:path")))
 (else 'ignore))
(test-i/o-error (rename-file "doesn't-exist" "no:such:path"))
(test-i/o-error (change-file-mode "doesn't-exist" #o666))
(test-i/o-error (delete-directory "doesn't-exist"))
(test-i/o-error (create-directory "doesn't/exist"))

(let ((file "timestamp.tmp"))
  (when (file-exists? file) (delete-file file))
  (call-with-output-file file (lambda (out) (put-string out "test")))
  (let ((mtime (file-stat-mtime file))
	(atime (file-stat-atime file))
	(1sec (expt 10 9)))
    (test-assert (change-file-timestamps! file
					 (make-time time-utc (- atime 1sec) 0)
					 (make-time time-utc (- mtime 1sec) 0)))
    (test-equal (- mtime 1sec) (file-stat-mtime file))
    (test-equal (- atime 1sec) (file-stat-atime file))

    (test-assert (change-file-timestamps! file #f #f))
    (test-equal (- mtime 1sec) (file-stat-mtime file))
    (test-equal (- atime 1sec) (file-stat-atime file))

    (test-assert (change-file-timestamps! file #t #t))
    (test-assert (<= mtime (file-stat-mtime file)))
    (test-assert (<= atime (file-stat-mtime file)))))

;; fxrotate-bit-field
(test-equal 6 (fxrotate-bit-field 6 1 2 1))

;; flfinite?
(test-assert (not (flfinite? +nan.0)))
(test-assert (not (flinfinite? +nan.0)))
;; (fl/ 0.0)
(test-equal +inf.0 (fl/ 0.0))
(test-equal -inf.0 (fl/ -0.0))

(test-assert (not (= (inexact 100000000000000000000001)
		     (inexact 99999999999999999999999))))
(let ((ten11 (inexact (expt 10 11)))
      (ten12 (inexact (expt 10 12)))
      (one   (inexact 1)))
  (define (do-it x y z)
    (let ((x (exact x))
	  (y (exact y))
	  (z (exact z)))
      (inexact (+ (* x y) z))))
  (test-assert (not (= (do-it ten11 ten12 one) (do-it ten11 ten12 (fl- one))))))

;; Should we add this as a test?
;; (let ()
;;   (define-syntax define-single-valued-token
;;     (syntax-rules ()
;;       ((_ name)
;;        (define-record-type name
;; 	 (fields value)))))
;;   (define-single-valued-token t)
;;   
;;   (define v (make-t 'v))
;;   (test-error undefined-violation? (t-value v)))

;; ÿ thing
(define tmp "issue_256.tmp")
(when (file-exists? tmp) (delete-file tmp))
(call-with-output-file tmp (lambda (out) (write "\xff;" out)))
(let ((expected #vu8(#x22 #xc3 #xbf #x22)))
  (test-equal "ÿ is not \xff;" expected
	      (call-with-input-file tmp get-bytevector-all :transcoder #f)))

;; special case of (expt -0.0 x)
(test-equal -0.0 (expt -0.0 3))
(test-equal 0.0 (expt -0.0 4))

(test-equal 0.0 (abs -0.0))

(let ()
  (define ((*n n) v) (* n v))
  (test-equal '(2 4 6 8) (map (*n 2) '(1 2 3 4))))

(test-error "any1 is not unbound"
	    syntax-violation?
 (r6rs:eval '(define-syntax foo
	       (lambda (x)
		 (syntax-case x ()
		   ((_ n)
		    #'(define-syntax n
			(lambda (xx)
			  (syntax-case xx ()
			    ((_ v (... ...))
			     #'(lambda (xxx)
				 (syntax-case xxx ()
				   ((_ k ((... ...) ...))
				    (print '(v (... ...))
					     '(k ((... ...) ...))))))))))))))
	    (environment '(rnrs))))

(test-assert
 (r6rs:eval
  '(library (macro-macro)
       (export define-define)
       (import (rnrs))
     (define-syntax define-define
       (lambda (x)
	 (syntax-case x ()
	   ((_ name)
	    #'(define-syntax name
		(lambda (xx)
		  (syntax-case xx ()
		    ((_)
		     #'(syntax-rules ()
			 ((_) 'name)))))))))))
  (environment '(sagittarius))))
(test-assert
 (r6rs:eval
  '(library (definer)
       (export definer)
       (import (rnrs)
	       (macro-macro))
     (define-define bar)
     (define-syntax definer (bar)))
  (environment '(sagittarius))))
(test-equal 'bar (r6rs:eval '(definer) (environment '(definer))))

;; Missing type check
(test-error (bytevector->u8-list '(1)))
;; This caused SEGV
(test-error (bytevector->u8-list '(1 1)))

;; Enbugged, by previous release
;; (* x y) with both x and y are bignum and size difference
;; (6 words I confirmed)
(test-equal #x1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
	    (* #x100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
	   #x10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))

(let ()
  (define (foo (bar integer?)) bar)
  (define (bar (buz 'a)) buz)
  (define (bah (boo (or 'a integer?))) boo)
  (define (zzz (da (and integer? exact?))) da)
  (define (xyzzy (z (lambda (x) #t))) z)
  (test-equal "Typed lambda" 1 (foo 1))
  (test-error "Typed lambda" (foo 'a))
  (test-equal "Typed lambda" 'a (bar 'a))
  (test-error "Typed lambda" (bar 1))
  (test-error "Typed lambda" (bar 'b))
  (test-equal "Typed lambda" 1 (bah 1))
  (test-equal "Typed lambda" 'a (bah 'a))
  (test-error "Typed lambda" (bah 'b))
  (test-equal "Typed lambda" 1 (zzz 1))
  (test-error "Typed lambda" (zzz 1.0))
  (test-equal "Typed lambda" 1 (xyzzy 1))
  (test-equal "Typed lambda" 'a (xyzzy 'a)))

(let ()
  (define (foo :key ((bar integer?) 1)) bar)
  (define (bar :optional ((buz 'a) 'a)) buz)
  (define (bah :optional ((boo (or 'a integer?)) 1)
	       :key ((bla (or (and integer? exact?) symbol?)) 'ok))
    (list boo bla))
  (test-equal "Typed lambda keyword" 1 (foo))
  (test-equal "Typed lambda keyword" 2 (foo :bar 2))
  (test-error "Typed lambda keyword" (foo :bar 'a))
  (test-equal "Typed lambda keyword" 'a (bar 'a))
  (test-error "Typed lambda keyword" 'a (bar 'b))
  (test-equal "Typed lambda keyword" '(1 ok) (bah))
  (test-error "Typed lambda keyword" (bah 'b))
  (test-equal "Typed lambda keyword" '(a ok) (bah 'a))
  (test-equal "Typed lambda keyword" '(a bien) (bah 'a :bla 'bien)))

(define (test-read/write-invariance sym str)
  (test-equal (string-append "Read/write invariance of " str)
	      sym
	      (read (open-string-input-port
		     (let-values (((out e) (open-string-output-port)))
		       (write (read (open-string-input-port str)) out)
		       (e))))))
(test-read/write-invariance ', @ ", @")
(test-read/write-invariance '#, @ "#, @")

(test-equal 0 (mod-expt 0 2 #xFFFFFFFFFFFFFFFFFFFFFFFF))

(define-syntax test-cond-expand-version
  (lambda (x)
    (define (generate-version generator)
      (eval (syntax->datum generator)
	    (current-library)))
    (syntax-case x (version)
      ((k (version (cmp generator)) body ...)
       (with-syntax ((v (generate-version #'generator)))
	 #'(cond-expand
	    ((and cond-expand.version (version (cmp v))) body ...)
	    (else #f)))))))

(define (build-version)
  (string-append (sagittarius-version) ".1"))

(test-equal "version < (sagittarius-version).1"
 "ok" (test-cond-expand-version (version (< (build-version))) "ok"))
(test-assert "version > (sagittarius-version)"
 (not (test-cond-expand-version (version (> (sagittarius-version))) "nok")))
(test-equal "version <= (sagittarius-version).1"
 "ok" (test-cond-expand-version (version (<= (build-version))) "ok"))
(test-equal "version >= (sagittarius-version)" "ok"
 (test-cond-expand-version (version (>= (sagittarius-version))) "ok"))
(test-equal "version = (sagittarius-version)" "ok"
 (test-cond-expand-version (version (= (sagittarius-version))) "ok"))

;; compiler/macro expander
(let ()
  (define-syntax foo
    (lambda (x)
      (syntax-case x ()
	((_ a) #'a))))
  (let ((r (equal? '#1=(1 2 #1#) (foo '#2=(1 2 #2#)))))
    ;; if we inline the above expression, then get infinte loop.
    ;; this is because quasiquote doesn't handle cyclic list.
    ;; Should we do it? But it's rather weird?
    (test-assert "cyclic list expansion w/o identifier" r))

  (let ((r (equal? (foo '#3=(a 2 . #3#)) '#4=(a 2 . #4#))))
    (test-assert "cyclic without identifier" r))
  )

(test-end)
