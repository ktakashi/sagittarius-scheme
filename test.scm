;(import (sagittarius compiler))
(import (rnrs))

(define v #(1 #f #f))
(vector? v)
(vector-ref v 0)
(vector-set! v 1 2)

(display v)(newline)

;(if (null? '()) #t #f)

(define-syntax print
  (syntax-rules ()
    ((_)
     (newline))
    ((_ o)
     (begin
       (display o)
       (print)))
    ((_ o1 o2 ...)
     (begin
       (display o1)
       (print o2 ...)))))

(print (list-transpose+ '(a b c) '(c d e)))


;(print 1 2 3)
;(print (apply map list '((a b c))))

#;(define (fib n)
    (if (< n 2)
	n
	(+ (fib (- n 1)) (fib (- n 2)))))
;(display (fib 30))(newline)

(define (tarai x y z)
  (if (<= x y) y
      (tarai (tarai (- x 1) y z)
	     (tarai (- y 1) z x)
	     (tarai (- z 1) x y))))
(print (tarai 10 5 0))

(let ((a (lambda ()
	   (let loop ((i 0))
	     (let ((a (car '(1 2))))
	       (let ((b (cdr '(1 . 2))))
		 (unless (>= i 1)
		   (print (+ a b i))
		   ;(display (+ a b i))(newline)
		   (loop (+ i 1)))))))))
  (if (null? a)
      '()
      (a)))

(define-syntax define-enum
  (er-macro-transformer
   (lambda (form rename compare)
     (define make-tag-list
       (lambda (name tags)
	 `(define-constant ,name ',tags)))
     (define make-enum
       (lambda (name vals)
	 (let ((len (length vals)))
	   (let loop ((i 0)
		      (vals vals)
		      (r '())
		      (tags '()))
	     (if (= i len)
		 (cons (make-tag-list name (reverse tags)) (reverse r))
		 (begin
		   (loop (+ i 1)
			 (cdr vals)
			 (cons `(define-constant ,(car vals) ,i) r)
			 (cons (cons (car vals) i) tags))))))))
     (let ((name (cadr form))
	   (vals (cddr form)))
       `(begin
	  ,@(make-enum name vals))))))

(define-enum .test
  $val1
  $val2)
(print .test)
(print $val1)
(print 10000000000)
(print (/ 1 2))
(print (+ 10000000000 10000000000))

(define v #(1 2 3))
(define v2 #(1 2 3))
(print (vector? v))
(print v)
(print (eqv? v v2))

(print (list 1 2 3))

(define l '(#0=(1 2 3) . #0#))
(display 'hoge)(newline)
(write :keyword)(newline)
(display :keyword)(newline)

(define s '|sy m|)

(write s)(newline)
(print s)

(define l1 '#0=(1 2 #0#))
(define l2 '#0=(1 2 #0#))
(print (eq? l1 l2))
(print (eqv? l1 l2))
(print (equal? l1 l2))
(write/ss l1)(newline)
(print (add-load-path "."))

(print (eq? :keyword :keyword))

(print '(file-options no-create))
(print #x10)
(print #e1)
(print 1.0)
(print 1+1i)
(print #e1+1.0i)
(print #i1+1i)
(print #vu8(1 2 3))

(define cont #f)
(print (+ 5 (call/cc (lambda (c) (begin (set! cont c) 10)))))
(print (cont 10))
(print (cont 20))
(print (cont 5))

(define a (gensym "v."))
(print (eq? a '|v.0|))

(define bv (make-bytevector 2))
(print bv)
(bytevector-u8-set! bv 0 255)
(bytevector-s8-set! bv 1 -127)
(print bv)
(print (bytevector-u8-ref bv 0))
(print (bytevector-u8-ref bv 1))
(print (bytevector-s8-ref bv 0))
(print (bytevector-s8-ref bv 1))

(print (bytevector-u16-ref #vu8(#xFF #xFF) 0 'little))
(print (bytevector-u32-ref #vu8(#xFF #xFF #xFF #xFF) 0 'little))
(print (bytevector-u64-ref #vu8(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF) 0 'little))
(print (native-endianness))
(print 1.539989614439558e-36)
(print (bytevector-ieee-single-ref #vu8(1 2 3 4) 0 'little))
(print (bytevector-ieee-double-ref #vu8(1 2 3 4 5 6 7 8) 0 'little))