;(import (sagittarius compiler))
(import (rnrs)
	(sagittarius test))

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

 #;(let ((in (open-file-input-port "test.scm" (file-options no-truncate) 'block (native-transcoder))))
   (assert-true? (port? in))
   (assert-true? (input-port? in))
   (close-input-port in)))
