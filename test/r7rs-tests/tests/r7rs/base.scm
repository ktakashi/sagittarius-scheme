;; -*- mode: scheme; coding: utf-8; -*-
#!r7rs
(define-library (tests r7rs base)
  (import (scheme base)
	  (tests r7rs test))
  (export run-r7rs-base-tests)
  (begin
    (define reverse-subtrace (lambda (x y) (- y x)))
    (define add4 (let ((x 4)) (lambda (y) (+ x y))))

    (define-syntax test-exactness
      (syntax-rules ()
	((_ expect exact? test)
	 (let ((tmp test))
	   (test-equal expect tmp)
	   (test-true (exact? tmp))))))

    (define radix
      (make-parameter
       10
       (lambda (x)
	 (if (and (integer? x) (<= 2 x 16))
	     x
	     (error "invalid radix")))))
    (define (f n) (number->string n (radix)))

    (define-syntax be-like-begin
      (syntax-rules ()
	((be-like-begin name)
	 (define-syntax name
	   (syntax-rules ()
	     ((name expr (... ...))
	      (begin expr (... ...))))))))
    (be-like-begin sequence)

    (define add3 (lambda (x) (+ x 3)))
    (define first car)

    (define-record-type <pare>
      (kons x y)
      pare?
      (x kar set-kar!)
      (y kdr))

    (define gen-counter
      (lambda ()
	(let ((n 0))
	  (lambda () (set! n (+ n 1)) n))))

    (define gen-loser
      (lambda ()
	(let ((n 0))
	  (lambda () (set! n (+ n 1)) 27))))

    (define list-length
      (lambda (obj)
	(call-with-current-continuation
	 (lambda (return)
	   (letrec ((r
		     (lambda (obj)
		       (cond ((null? obj) 0)
			     ((pair? obj)
			      (+ (r (cdr obj)) 1))
			     (else (return #f))))))
	     (r obj))))))

    (define (run-r7rs-base-tests)
      ;; 4.1.2
      (test-equal 'a (quote a))
      (test-equal '#(a b c) (quote #(a b c)))
      (test-equal '(+ 1 2) (quote (+ 1 2)))
      (test-equal "abc" '"abc")
      (test-equal '145932 145932)
      (test-equal '#t #t)

      ;; 4.1.3
      (test-equal 7 (+ 3 4))
      (test-equal 12 ((if #f + *) 3 4))

      ;; 4.1.4
      (test-true (procedure? (lambda (x) (+ x x))))
      (test-equal 8 ((lambda (x) (+ x x)) 4))
      (test-equal 3 (reverse-subtrace 7 10))
      (test-equal 10 (add4 6))
      (test-equal '(3 4 5 6) ((lambda x x) 3 4 5 6))
      (test-equal '(5 6) ((lambda (x y . z) z) 3 4 5 6))

      ;; 4.1.5
      (test-equal 'yes (if (> 3 2) 'yes 'no))
      (test-equal 'no (if (> 2 3) 'yes 'no))
      (test-equal 1 (if (> 3 2) (- 3 2) (+ 3 2)))

      ;; 4.1.6
      (let ((x 2))
	;;(define x 2)
	(test-unspecified (set! x 4))
	(test-equal 5 (+ x 1)))

      ;; 4.2.1
      (test-equal 'greater (cond ((> 3 2) 'greater)
				 ((< 3 2) 'less)))
      (test-equal 'equal (cond ((> 3 3) 'greater)
			       ((< 3 3) 'less)
			       (else 'equal)))
      (test-equal '2 (cond ((assv 'b '((a 1) (b 2))) => cadr)
			   (else #f)))

      (test-equal 'composite (case (* 2 3)
			       ((2 3 5 7) 'prime)
			       ((1 4 6 8 9) 'composite)))
      (test-unspecified (case (car '(c d))
			  ((a) 'a)
			  ((b) 'b)))
      (test-equal 'c (case (car '(c d))
		       ((a e i o u) 'vowel)
		       ((w y) 'semivowel)
		       (else => (lambda (x) x))))

      (test-true (and (= 2 2) (> 2 1)))
      (test-false (and (= 2 2) (< 2 1)))
      (test-equal '(f g) (and 1 2 'c '(f g)))
      (test-true (and))

      (test-true (or (= 2 2) (> 2 1)))
      (test-true (or (= 2 2) (< 2 1)))
      (test-false (or #f #f #f))
      (test-equal '(b c) (or (memq 'b '(a b c)) (/ 3 0)))

      ;; 4.2.2
      (test-equal 6 (let ((x 2) (y 3)) (* x y)))
      (test-equal 35 (let ((x 2) (y 3))
		       (let ((x 7)
			     (z (+ x y)))
			 (* z x))))

      (test-equal 70 (let ((x 2) (y 3))
		       (let* ((x 7)
			      (z (+ x y)))
			 (* z x))))

      (test-true (letrec ((even?
			   (lambda (n)
			     (if (zero? n)
				 #t
				 (odd? (- n 1)))))
			  (odd?
			   (lambda (n)
			     (if (zero? n)
				 #f
				 (even? (- n 1))))))
		   (even? 88)))

      (test-equal 5 (letrec* ((p
			       (lambda (x)
				 (+ 1 (q (- x 1)))))
			      (q
			       (lambda (y)
				 (if (zero? y)
				     0
				     (+ 1 (p (- y 1))))))
			      (x (p 5))
			      (y x))
			     y))

      (test-equal 35 (let-values (((root rem) (exact-integer-sqrt 32)))
		       (* root rem)))

      (test-equal '(x y x y) (let ((a 'a) (b 'b) (x 'x) (y 'y))
			       (let*-values (((a b) (values x y))
					     ((x y) (values a b)))
				 (list a b x y))))

      ;; 4.2.3
      (let ((x 0))
	;;(define x 0)
	(test-equal 6 (and (= x 0)
			   (begin (set! x 5)
				  (+ x 1)))))
      

      ;; 4.2.4
      (test-equal #(0 1 2 3 4) (do ((vec (make-vector 5))
				    (i 0 (+ i 1)))
				   ((= i 5) vec)
				 (vector-set! vec i i)))
      (test-equal 25 (let ((x '(1 3 5 7 9)))
		       (do ((x x (cdr x))
			    (sum 0 (+ sum (car x))))
			   ((null? x) sum))))

      (test-equal '((6 1 3) (-5 -2))
		  (let loop ((numbers '(3 -2 1 6 -5))
			     (nonneg '())
			     (neg '()))
		    (cond ((null? numbers) (list nonneg neg))
			  ((>= (car numbers) 0)
			   (loop (cdr numbers)
				 (cons (car numbers) nonneg)
				 neg))
			  ((< (car numbers) 0)
			   (loop (cdr numbers)
				 nonneg
				 (cons (car numbers) neg))))))

      ;; 4.2.6
      (test-equal "12" (f 12))
      (test-equal "1100" (parameterize ((radix 2)) (f 12)))
      (test-equal "12" (f 12))
      (test-unspecified (radix 16))
      (test-error (parameterize ((radix 0)) (f 12)))

      ;; 4.2.8
      (test-equal '(list 3 4) `(list ,(+ 1 2) 4))
      (test-equal '(list a (quote a)) (let ((name 'a)) `(list ,name ',name)))
      (test-equal '(a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
      (test-equal '((foo 7) . cons)
		  `(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))

      (test-equal '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
		  `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
      (test-equal '(a `(b ,x ,'y d) e)
		  (let ((name1 'x)
			(name2 'y))
		    `(a `(b ,,name1 ,',name2 d) e)))

      (test-equal '(list 3 4) (quasiquote (list (unquote (+ 1 2)) 4)))
      (test-equal '`(list ,(+ 1 2) 4) '(quasiquote (list (unquote (+ 1 2)) 4)))

      ;; 4.3.1
      (test-equal 'now (let-syntax ((when (syntax-rules ()
					    ((when test stmt1 stmt2 ...)
					     (if test
						 (begin stmt1
							stmt2 ...))))))
			 (let ((if #t))
			   (when if (set! if 'now))
			   if)))
      (test-equal 'outer (let ((x 'outer))
			   (let-syntax ((m (syntax-rules () ((m) x))))
			     (let ((x 'inner))
			       (m)))))

      #| ;; somehow this test does not pass. maybe rewrite test framework for r7rs?
      (test-equal 7 (letrec-syntax
      ((my-or (syntax-rules ()
      ((my-or) #f)
      ((my-or e) e)
      ((my-or e1 e2 ...)
      (let ((temp e1))
      (if temp
      temp
      (my-or e2 ...)))))))
      (let ((x #f)
      (y 7)
      (temp 8)
      (let odd?)
      (if even?))
      (my-or x
      (let temp)
      (if y)
      y))))
      |#  
      ;; 4.3.2
      (test-equal 4 (sequence 1 2 3 4))
      (test-equal 'ok (let ((=> #f)) (cond (#t => 'ok))))

      ;; 5.2.1
      (test-equal 6 (add3 3))
      (test-equal 1 (first '(1 2)))

      ;; 5.2.2
      (test-equal 45 (let ((x 5))
		       (define foo (lambda (y) (bar x y)))
		       (define bar (lambda (a b) (+ (* a b) a)))
		       (foo (+ x 3))))

      ;; 5.2.3
      (test-equal 3 (let ()
		      (define-values (x y) (values 1 2))
		      (+ x y)))

      ;; 5.3
      (test-equal '(2 1)
		  (let ((x 1) (y 2))
		    (define-syntax swap!
		      (syntax-rules ()
			((swap! a b)
			 (let ((tmp a))
			   (set! a b)
			   (set! b tmp)))))
		    (swap! x y)
		    (list x y)))
      
      ;; 5.4
      (test-true (pare? (kons 1 2)))
      (test-false (pare? (cons 1 2)))
      (test-equal 1 (kar (kons 1 2)))
      (test-equal 2 (kdr (kons 1 2)))
      (test-equal 3 (let ((k (kons 1 2)))
		      (set-kar! k 3)
		      (kar k)))

      ;; 6.1
      (test-true (eqv? 'a 'a))
      (test-false (eqv? 'a 'b))
      (test-true (eqv? 2 2))
      (test-true (eqv? '() '()))
      (test-true (eqv? 100000000 100000000))
      (test-false (eqv? (cons 1 2) (cons 1 2)))
      (test-false (eqv? (lambda () 1) (lambda () 2)))
      (test-false (eqv? #f '()))

      (test-true (let ((g (gen-counter))) (eqv? g g)))
      (test-false (eqv? (gen-counter) (gen-counter)))

      (test-true (let ((g (gen-loser))) (eqv? g g)))

      (test-false (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
			   (g (lambda () (if (eqv? f g) 'g 'both))))
		    (eqv? f g)))

      (test-true (let ((x '(a)))
		   (eqv? x x)))

      (test-true (eq? 'a 'a))
      (test-false (eq? (list 'a) (list 'a)))
      (test-true (eq? '() '()))
      (test-true (let ((x '(a)))
		   (eq? x x)))
      (test-true (let ((x '#()))
		   (eq? x x)))
      (test-true (let ((p (lambda (x) x)))
		   (eq? p p)))

      (test-true (equal? 'a 'a))
      (test-true (equal? '(a) '(a)))
      (test-true (equal? '(a (b) c) '(a (b) c)))
      (test-true (equal? "abc" "abc"))
      (test-true (equal? 2 2))
      (test-true (equal? (make-vector 5 'a) (make-vector 5 'a)))
      
      ;; 6.2.6
      (test-true (complex? 3+4i))
      (test-true (complex? 3))
      (test-true (real? 3))
      (test-true (real? -2.5+0i))
      (test-false (real? -2.5+0.0i))
      (test-true (real? #e1e10))
      (test-true (real? +inf.0))
      (test-false (rational? -inf.0))
      (test-true (rational? 6/10))
      (test-true (rational? 6/3))
      (test-true (integer? 3+0i))
      (test-true (integer? 3.0))
      (test-true (integer? 8/4))
      
      (test-false (exact? 3.0))
      (test-true (exact? #e3.0))
      (test-true (inexact? 3.))

      (test-true (exact-integer? 32))
      (test-false (exact-integer? 32.0))
      (test-false (exact-integer? 32/5))

      (test-exactness 4 exact? (max 3 4))
      (test-exactness 4.0 inexact? (max 3.9 4))

      (test-equal 7 (+ 3 4))
      (test-equal 3 (+ 3))
      (test-equal 0 (+))
      (test-equal 4 (* 4))
      (test-equal 1 (*))

      (test-equal  -1 (- 3 4))
      (test-equal  -6 (- 3 4 5))
      (test-equal  -3 (- 3))
      (test-equal  3/20 (/ 3 4 5))
      (test-equal  1/3 (/ 3))

      (test-equal 7 (abs -7))

      (test-exactness 1 exact? (modulo 13 4))
      (test-exactness 1 exact? (remainder 13 4))
      (test-exactness 3 exact? (modulo -13 4))
      (test-exactness -1 exact? (remainder -13 4))
      (test-exactness -3 exact? (modulo 13 -4))
      (test-exactness 1 exact? (remainder 13 -4))
      (test-exactness -1 exact? (modulo -13 -4))
      (test-exactness -1 exact? (remainder -13 -4))
      (test-exactness -1.0 inexact? (remainder -13 -4.0))

      (test-exactness 4 exact? (gcd 32 -36))
      (test-exactness 0 exact? (gcd))
      (test-exactness 288 exact? (lcm 32 -36))
      (test-exactness 288.0 inexact? (lcm 32.0 -36))
      (test-exactness 1 exact? (lcm))

      (test-equal 3 (numerator (/ 6 4)))
      (test-equal 2 (denominator (/ 6 4)))
      (test-equal 2.0 (denominator (exact->inexact (/ 6 4))))

      (test-exactness -5.0 inexact? (floor -4.3))
      (test-exactness -4.0 inexact? (ceiling -4.3))
      (test-exactness -4.0 inexact? (truncate -4.3))
      (test-exactness -4.0 inexact? (round -4.3))
      (test-exactness 3.0 inexact? (floor 3.5))
      (test-exactness 4.0 inexact? (ceiling 3.5))
      (test-exactness 3.0 inexact? (truncate 3.5))
      (test-exactness 4.0 inexact? (round 3.5)) ; inexact
      (test-exactness 4 exact? (round 7/2)) ; exact
      (test-exactness 7 exact? (round 7))

      (test-exactness 1/3 exact? (rationalize (inexact->exact .3) 1/10))
      (test-exactness #i1/3 inexact? (rationalize .3 1/10))

      (test-values (exact-integer-sqrt 4) 2 0)
      (test-values (exact-integer-sqrt 5) 2 1)

      ;; 6.2.7
      (let ((radixes '(2 8 10 16)))
	(define (test num)
	  (for-each 
	   (lambda (r)
	     (test-equal num
			 (string->number
			  (number->string num r) r))) radixes))
	(test 1234))

      (test-equal 100 (string->number "100"))
      (test-equal 256 (string->number "100" 16))
      (test-equal 100.0 (string->number "1e2"))

      ;; 6.3
      (test-false (not #t))
      (test-false (not 3))
      (test-false (not (list 3)))
      (test-true (not #f))
      (test-false (not '()))
      (test-false (not (list)))
      (test-false (not 'nil))

      (test-true (boolean? #f))
      (test-false (boolean? 0))
      (test-false (boolean? '()))

      ;; 6.4
      (let ()
	(define x (list 'a 'b 'c))
	(define y x)
	(test-true (list? y))
	(test-equal '(a b c) y)
	(test-unspecified (set-cdr! x 4))
	(test-equal '(a . 4) x)
	(test-true (eqv? x y))
	(test-equal '(a . 4) y)
	(test-false (list? y))
	(test-unspecified (set-cdr! x x))
	(test-false (list? x)))

      (test-true (pair? '(a . b)))
      (test-true (pair? '(a b c)))
      (test-false (pair? '()))
      (test-false (pair? '#(a b)))

      (test-equal '(a) (cons 'a '()))
      (test-equal '((a) b c d) (cons '(a) '(b c d)))
      (test-equal '("a" b c) (cons "a" '(b c)))
      (test-equal '(a . 3) (cons 'a 3))
      (test-equal '((a b) . c) (cons '(a b) 'c))

      (test-equal 'a (car '(a b c)))
      (test-equal '(a) (car '((a) b c d)))
      (test-equal 1 (car '(1 . 2)))
      (test-error (car '()))

      (test-equal '(b c d) (cdr '((a) b c d)))
      (test-equal 2 (cdr '(1 . 2)))
      (test-error (car '()))

      (let ()
	(define (f) (list 'not-a-constant-list))
	(define (g) '(constant-list))
	(test-error (set-cdr! (g) 3))
	(test-unspecified (set-car! (f) 3)))

      (test-true (null? '()))
      (test-true (null? (list)))
      (test-false (null? '(a)))

      (test-true (list? '(a b c)))
      (test-true (list? '()))
      (test-false (list? '(a . b)))
      (test-false (let ((x (list 'a)))
		    (set-cdr! x x)
		    (list? x)))

      (test-equal '(3 3) (make-list 2 3))

      (test-equal '(a 7 c) (list 'a (+ 3 4) 'c))
      (test-equal '() (list))

      (test-equal 3 (length '(a b c)))
      (test-equal 3 (length '(a (b) (c d e))))
      (test-equal 0 (length '()))

      (test-equal '(x y) (append '(x) '(y)))
      (test-equal '(a b c d) (append '(a) '(b c d)))
      (test-equal '(a (b) (c)) (append '(a (b)) '((c))))
      (test-equal '(a b c . d) (append '(a b) '(c . d)))
      (test-equal 'a (append '() 'a))

      (test-equal '(c b a) (reverse '(a b c)))
      (test-equal '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

      (test-equal 'c (list-ref '(a b c d) 2))
      (test-equal 'c (list-ref '(a b c d)
			       (inexact->exact (round 1.8))))

      (test-equal '(one two three) (let ((ls (list 'one 'two 'five!)))
				     (list-set! ls 2 'three)
				     ls))
      ;; macro rewrites lists
      (let ((x '(0 1 2)))
	(test-error (list-set! x 1 "oops")))

      (test-equal '(a b c) (memq 'a '(a b c)))
      (test-equal '(b c) (memq 'b '(a b c)))
      (test-false (memq 'a '(b c d)))
      (test-false (memq (list 'a) '(b (a) c)))
      (test-equal '((a) c) (member (list 'a)
				   '(b (a) c)))
      #| ;; string-ci=? is in (scheme char)
      (test-equal '("b" "c") (member "B"
      '("a" "b" "c")
      string-ci=?))
      |#
      (test-equal '(101 102) (memv 101 '(100 101 102)))

      (let ((e '((a 1) (b 2) (c 3))))
	(test-equal '(a 1) (assq 'a e))
	(test-equal '(b 2) (assq 'b e))
	(test-false (assq 'd e)))
      (test-false (assq (list 'a) '(((a)) ((b)) ((c)))))
      (test-equal '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))
      (test-equal '(2 4) (assoc 2.0 '((1 1) (2 4) (3 9)) =))
      (test-equal '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

      ;; 6.5
      (test-true (symbol? 'foo))
      (test-true (symbol? (car '(a b))))
      (test-false (symbol? "bar"))
      (test-true (symbol? 'nil))
      (test-false (symbol? '()))
      (test-false (symbol? #f))

      (test-equal "flying-fish" (symbol->string 'flying-fish))
      (test-equal "Martin" (symbol->string 'Martin))
      (test-equal "Malvina" (symbol->string
			     (string->symbol "Malvina")))
      (test-equal 'mISSISSIppi (string->symbol "mISSISSIppi"))
      (test-true (eq? 'bitBlt (string->symbol "bitBlt")))
      (test-true (eq? 'JollyWog
		      (string->symbol
		       (symbol->string 'JollyWog))))
      (test-true (string=? "K. Harper, M.D."
			   (symbol->string
			    (string->symbol "K. Harper, M.D."))))

      ;; 6.7
      (let ()
	(define (f) (make-string 3 #\*))
	(define (g) "***")
	(test-error (string-set! (g) 0 #\?))
	(test-unspecified (string-set! (f) 0 #\?))
	(test-error (string-set! (symbol->string 'immutable) 0 #\?)))

      ;; 6.8
      (test-equal 8 (vector-ref '#(1 1 2 3 5 8 13 21) 5))
      (test-equal '#(0 ("Sue" "Sue") "Anna")
		  (let ((vec (vector 0 '(2 2 2 2) "Anna")))
		    (vector-set! vec 1 '("Sue" "Sue"))
		    vec))
      ;; macro rewrites vector so it needs to be out side of macro
      (let ((v '#(0 1 2)))
	(test-error (vector-set! v 1 "doe")))

      (test-equal '(dah dah didah) (vector->list '#(dah dah didah)))
      (test-equal '#(dididit dah) (list->vector '(dididit dah)))

      (test-equal '#(#\A #\B #\C) (string->vector "ABC"))
      (test-equal "123" (vector->string '#(#\1 #\2 #\3)))

      ;; 6.9
      (test-equal "A" (utf8->string #u8(#x41)))
      (test-equal #u8(#xCE #xBB) (string->utf8 "λ"))

      ;; 6.10
      (test-true (procedure? car))
      (test-false (procedure? 'car))
      (test-true (procedure? (lambda (x) (* x x))))
      (test-false (procedure? '(lambda (x) (* x x))))
      (test-true (call-with-current-continuation procedure?))

      (test-equal 7 (apply + (list 3 4)))

      (test-equal '(b e h) (map cadr '((a b) (d e) (g h))))
      (test-equal '(1 4 27 256 3125)
		  (map (lambda (n) (expt n n))
		       '(1 2 3 4 5)))
      (test-equal '(5 7 9) (map + '(1 2 3) '(4 5 6 7)))
      (test-alts (let ((count 0))
		   (map (lambda (ignored)
			  (set! count (+ count 1))
			  count)
			'(a b))) '(1 2) '(2 1))

      (test-equal "IBM" (string-map
			 (lambda (c)
			   (integer->char (+ 1 (char->integer c))))
			 "HAL"))

      (test-equal '#(b e h) (vector-map cadr '#((a b) (d e) (g h))))
      (test-equal '#(1 4 27 256 3125) (vector-map (lambda (n) (expt n n))
						  '#(1 2 3 4 5)))
      (test-equal '#(5 7 9) (vector-map + '#(1 2 3) '#(4 5 6 7)))
      (test-alts (let ((count 0))
		   (vector-map
		    (lambda (ignored)
		      (set! count (+ count 1))
		      count)
		    '#(a b))) '#(1 2) '#(2 1))

      (test-equal '#(0 1 4 9 16) (let ((v (make-vector 5)))
				   (for-each (lambda (i)
					       (vector-set! v i (* i i)))
					     '(0 1 2 3 4))
				   v))
      (test-equal '(101 100 99 98 97)
		  (let ((v '()))
		    (string-for-each
		     (lambda (c) (set! v (cons (char->integer c) v)))
		     "abcde")
		     v))

      (test-equal '(0 1 4 9 16) (let ((v (make-list 5)))
				  (vector-for-each
				   (lambda (i) (list-set! v i (* i i)))
				   '#(0 1 2 3 4))
				  v))
      (test-equal -3 (call-with-current-continuation
		      (lambda (exit)
			(for-each (lambda (x)
				    (if (negative? x)
					(exit x)))
				  '(54 0 37 -3 245 19))
			#t)))
      (test-equal 4 (list-length '(1 2 3 4)))
      (test-false (list-length '(a b . c)))

      (test-equal 5 (call-with-values (lambda () (values 4 5))
		      (lambda (a b) b)))
      (test-equal -1 (call-with-values * -))

      (test-equal '(connect talk1 disconnect connect talk2 disconnect)
		  (let ((path '())
			(c #f))
		    (let ((add (lambda (s)
				 (set! path (cons s path)))))
		      (dynamic-wind
			  (lambda () (add 'connect))
			  (lambda ()
			    (add (call-with-current-continuation
				  (lambda (c0)
				    (set! c c0)
				    'talk1))))
			  (lambda () (add 'disconnect)))
		      (if (< (length path) 4)
			  (c 'talk2)
			  (reverse path)))))
      ;; 6.11
      )
    )
)