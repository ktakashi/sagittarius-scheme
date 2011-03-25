;(import (sagittarius compiler))
(import (rnrs)
	(sagittarius test))

(print "test start")
(run-test

 (define v (vector 1 #f #f))
 (assert-true? (vector? v))
 (assert-equal? 1 (vector-ref v 0))
 (vector-set! v 1 2)
 (assert-equal? '#(1 2 #f) (vector-copy v))
 (vector-fill! v 'filled)
 (assert-equal? '#(filled filled filled) v)

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
 ;(assert-equal? '(($val1 . 0) ($val2 . 1)) .test)
 (assert-equal? 0 $val1)
 (assert-true? (number? 10000000000))
 (assert-true? (real? 10000000000))
 (assert-true? (real? (/ 1 2)))
 (assert-equal? 20000000000 (+ 10000000000 10000000000))

 (define l1 '#0=(1 2 #0#))
 (define l2 '#1=(1 2 #1#))
 (assert-false? (eq? l1 l2))
 (assert-false? (eqv? l1 l2))
 (assert-true? (equal? l1 l2))
 (write/ss l1)(newline)
 (print (add-load-path "."))

 (assert-true? (keyword? :keyword))
 (assert-true? (eqv? :keyword :keyword))

 (assert-equal? 16 #x10)
 (assert-equal? 1 #e1)

 (assert-true? (inexact? #i1))
 (assert-true? (inexact? 1.0))
 (assert-equal? 1.0 #i1)
 (assert-true? (number? 1+1i))
 (assert-true? (complex? 1+1i))
 (assert-true? (exact? 1+1i))
 (assert-equal? 2+2i (+ 1+1i 1+1i))
 (assert-true? (complex? #e1+1.0i))
 (assert-true? (exact? #e1+1.0i))
 (assert-equal? 1+1i #e1+1.0i)
 (assert-true? (complex? #i1+1i))
 (assert-true? (inexact? #i1+1i))
 (assert-equal? 1.0+1.0i #i1+1i)

 (define a (gensym "v."))
 (assert-false? (eq? a '|v.0|))

 (assert-true? (bytevector? #vu8(1 2 3)))
 (assert-true? (bytevector=? #vu8(1 2 3) #vu8(1 2 3)))

 (define bv (make-bytevector 2))
 (bytevector-u8-set! bv 0 255)
 (bytevector-s8-set! bv 1 -127)

 (assert-equal? 255 (bytevector-u8-ref bv 0))
 (assert-equal? 129 (bytevector-u8-ref bv 1))
 (assert-equal? -1 (bytevector-s8-ref bv 0))
 (assert-equal? -127 (bytevector-s8-ref bv 1))

 (assert-equal? #xFFAA (bytevector-u16-ref #vu8(#xAA #xFF) 0 'little))
 (assert-equal? #xAAFF (bytevector-u16-ref #vu8(#xAA #xFF) 0 'big))
 (assert-equal? #xDDCCBBAA (bytevector-u32-ref #vu8(#xAA #xBB #xCC #xDD) 0 'little))
 (assert-equal? #xAABBCCDD (bytevector-u32-ref #vu8(#xAA #xBB #xCC #xDD) 0 'big))

 (assert-equal? #xFFFFFFFFFFFFFFFF (bytevector-u64-ref #vu8(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF) 0 'little))
 (assert-equal? 1.539989614439558e-36 (bytevector-ieee-single-ref #vu8(1 2 3 4) 0 'little))
 (assert-equal? 5.447603722011605e-270 (bytevector-ieee-double-ref #vu8(1 2 3 4 5 6 7 8) 0 'little))

 (assert-equal? "abc" (utf8->string #vu8(97 98 99)))
 (assert-equal? #vu8(97 98 99) (string->utf8 "abc"))

 ;; bitwise
 (assert-equal? 32 (bitwise-arithmetic-shift 1 5))
 (assert-equal? 32 (bitwise-arithmetic-shift-left 1 5))
 (assert-equal? 1 (bitwise-arithmetic-shift-right 32 5))

 (assert-equal? #x200000000 (bitwise-arithmetic-shift 1 33))
 (assert-equal? #x200000000 (bitwise-arithmetic-shift-left 1 33))
 (assert-equal? 1 (bitwise-arithmetic-shift-right #x200000000 33))

 (assert-equal? 3 (bitwise-bit-count 7))
 (assert-equal? 1 (bitwise-bit-count #x200000000))
 (assert-equal? 1 (bitwise-length 1))
 (assert-equal? 34 (bitwise-length #x200000000))

 (assert-equal? 1 (bitwise-and 1 1))
 (assert-equal? #x200000000 (bitwise-and #x200000000 #x200000000))
 (assert-equal? 0 (bitwise-and #x100000000 #x200000000))

 (assert-equal? 3 (bitwise-ior 1 2))
 (assert-equal? 2 (bitwise-ior 2 2))
 (assert-equal? #x200000000 (bitwise-ior #x200000000 #x200000000))
 (assert-equal? #x300000000 (bitwise-ior #x100000000 #x200000000))

 (assert-equal? 0 (bitwise-xor 1 1))
 (assert-equal? 1 (bitwise-xor 1 0))
 (assert-equal? 1 (bitwise-xor 0 1))
 (assert-equal? 0 (bitwise-xor 0 0))
 (assert-equal? 0 (bitwise-xor #x100000000 #x100000000))
 (assert-equal? #x100000000 (bitwise-xor #x100000000 0))
 (assert-equal? #x100000000 (bitwise-xor 0 #x100000000))

 (assert-equal? 2 (bitwise-if 1 2 3))
 (assert-equal? 1 (bitwise-first-bit-set 2))
 (assert-equal? 0 (bitwise-first-bit-set 7))
 
 (assert-equal? 0 (bitwise-bit-field 1 2 3))
 (assert-equal? 1 (bitwise-bit-field 7 2 3))

 ;; base arith
 (assert-equal? 5 (max 5 4 3 2 1))
 (assert-equal? 1 (min 5 4 3 2 1))

 (assert-equal? 500 (abs -500))

 (assert-equal? 2.0 (round 1.6))
 (assert-equal? 2.0 (round 1.5))
 (assert-equal? 1.0 (round 1.4))
 (assert-equal? 2.0 (round 2.5))

 (assert-equal? 2.0 (ceiling 1.1))
 (assert-equal? 1.0 (ceiling 1.0))

 (assert-equal? 2.0 (floor 2.0))
 (assert-equal? 1.0 (floor 1.9))

 (assert-equal? 1.0 (truncate 1.9))

 (assert-equal? 15 (gcd 15 30 45))
 (assert-equal? 40 (lcm 4 5 8))

 (assert-equal? 12 (div 123 10))
 (assert-equal? 3 (mod 123 10))

 (assert-equal? -12 (div 123 -10))
 (assert-equal? 3 (mod 123 -10))

 (assert-equal? -13 (div -123 10))
 (assert-equal? 7 (mod -123 10))

 (assert-equal? 13 (div -123 -10))
 (assert-equal? 7 (mod -123 -10))

 (assert-equal? 15 (div 123/7 10/9))
 (assert-equal? 19/21 (mod 123/7 10/9))
 ;; 123/7 = 10/9 * 15 + 19/21

 (assert-equal? 3.0 (div 14.625 3.75))
 (assert-equal? 3.375 (mod 14.625 3.75))
 ;; 14.625 = 3.75 * 3.0 + 3.375

 (assert-equal? 12 (div0 123 10))
 (assert-equal? 3(mod0 123 10))

 (assert-equal? 13 (div0 127 10))
 (assert-equal? -3 (mod0 127 10))

 (assert-equal? -13 (div0 127 -10))
 (assert-equal? -3 (mod0 127 -10))

 (assert-equal? -13 (div0 -127 10))
 (assert-equal? 3 (mod0 -127 10))

 (assert-equal? 13 (div0 -127 -10))
 (assert-equal? 3 (mod0 -127 -10))

 (assert-equal? 1.9477340410546757 (exp 2/3))
 (assert-equal? 81 (expt 3 4))

 (assert-equal? 3.0 (log 8 2))
 (assert-equal? 6.907755278982137 (log 1000))

 (assert-equal? 1+1i (make-rectangular 1 1))
 (assert-equal? 1.0+1.0i (make-rectangular 1.0 1.0))

 (assert-equal? 1.4142135623730951 (sqrt 2))
 (assert-equal? 0.0+1.4142135623730951i (sqrt -2))
 (assert-equal? 16 (sqrt 256))
 (assert-equal? 16.0 (sqrt 256.0))
 (assert-equal? 9/13 (sqrt 81/169))

 (print 782763574)
 (receive (r i) (exact-integer-sqrt 782763574)
   (assert-equal? 27977 r)
   (assert-equal? 51045 i))

 #;(let ((in (open-file-input-port "test.scm" (file-options no-truncate) 'block (native-transcoder))))
   (assert-true? (port? in))
   (assert-true? (input-port? in))
   (close-input-port in)))
