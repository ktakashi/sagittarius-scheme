(import (rnrs) (srfi :64)
	(clos user)
	(sagittarius object)
	(binary data))

(test-begin "Binary data read/write")

;;  utilities
(define-syntax test-set
  (syntax-rules ()
    ((_ set conv v)
     (test-equal 'set (conv v) (call-with-bytevector-output-port 
			 (lambda (out) (set out v)))))
    ((_ set conv size v endian)
     (test-equal '(set conv) (conv v size)
		 (call-with-bytevector-output-port 
		  (lambda (out) (set out v endian)))))))

(define-syntax test-get
  (syntax-rules ()
    ((_ get conv v)
     (test-equal 'get v 
		 (get (open-bytevector-input-port (conv v)))))
    ((_ get conv size v endian)
     (test-equal '(get conv) v 
		 (get (open-bytevector-input-port (conv v size)) endian)))))
(define-syntax test-range-big
  (syntax-rules ()
    ((_ get set size conv min max)
     (begin
       (test-set set conv size min (endianness big))
       (test-set set conv size max (endianness big))
       (test-get get conv size min (endianness big))
       (test-get get conv size max (endianness big))))))

(define-syntax test-range-little
  (syntax-rules ()
    ((_ get set size conv min max)
     (begin
       (test-set set conv size min (endianness little))
       (test-set set conv size max (endianness little))
       (test-get get conv size min (endianness little))
       (test-get get conv size max (endianness little))))))

(define-syntax test-range-native
  (syntax-rules ()
    ((_ get set size conv min max)
     (begin
       (test-set set conv size min 'native)
       (test-set set conv size max 'native)
       (test-get get conv size min 'native)
       (test-get get conv size max 'native)))))

;; s8 doesn't take endiannness
(test-set put-s8 sinteger->bytevector -128)
(test-set put-s8 sinteger->bytevector 127)
(test-get get-s8 sinteger->bytevector -128)
(test-get get-s8 sinteger->bytevector 127)

(test-range-big get-u16 put-u16 2 uinteger->bytevector 0       #xFFFF)
(test-range-big get-u16 put-u16 2 uinteger->bytevector #x1234  #x7890)
(test-range-big get-s16 put-s16 2 sinteger->bytevector #x-8000 #x7FFF)
(test-range-big get-s16 put-s16 2 sinteger->bytevector #x-1234 #x7890)
(test-range-big get-u32 put-u32 4 uinteger->bytevector 0                   #xFFFFFFFF)
(test-range-big get-u32 put-u32 4 uinteger->bytevector #x12345678          #x78901234)
(test-range-big get-s32 put-s32 4 sinteger->bytevector #x-80000000         #x7FFFFFFF)
(test-range-big get-s32 put-s32 4 sinteger->bytevector #x-12345678         #x78901234)
(test-range-big get-u64 put-u64 8 uinteger->bytevector 0                   #xFFFFFFFF)
(test-range-big get-s64 put-s64 8 sinteger->bytevector #x-8000000000000000 #x7FFFFFFFFFFFFFFF)

(define (uint->bv-little i size)
  (let ((bv (make-bytevector size)))
    (bytevector-uint-set! bv 0 i (endianness little) size)
    bv))
(define (sint->bv-little i size)
  (let ((bv (make-bytevector size)))
    (bytevector-sint-set! bv 0 i (endianness little) size)
    bv))

(test-range-little get-u16 put-u16 2 uint->bv-little 0       #xFFFF)
(test-range-little get-u16 put-u16 2 uint->bv-little #x1234  #x7890)
(test-range-little get-s16 put-s16 2 sint->bv-little #x-8000 #x7FFF)
(test-range-little get-s16 put-s16 2 sint->bv-little #x-1234 #x7890)
(test-range-little get-u32 put-u32 4 uint->bv-little 0                   #xFFFFFFFF)
(test-range-little get-u32 put-u32 4 uint->bv-little #x12345678          #x78901234)
(test-range-little get-s32 put-s32 4 sint->bv-little #x-80000000         #x7FFFFFFF)
(test-range-little get-s32 put-s32 4 sint->bv-little #x-12345678         #x78901234)
(test-range-little get-u64 put-u64 8 uint->bv-little 0                   #xFFFFFFFF)
(test-range-little get-s64 put-s64 8 sint->bv-little #x-8000000000000000 #x7FFFFFFFFFFFFFFF)

;; if it's built-in, then we can accept native
(define (uint->bv-native i size)
  (let ((bv (make-bytevector size)))
    (bytevector-uint-set! bv 0 i (endianness native) size)
    bv))
(define (sint->bv-native i size)
  (let ((bv (make-bytevector size)))
    (bytevector-sint-set! bv 0 i (endianness native) size)
    bv))

(test-range-native get-u16 put-u16 2 uint->bv-native 0       #xFFFF)
(test-range-native get-u16 put-u16 2 uint->bv-native #x1234  #x7890)
(test-range-native get-s16 put-s16 2 sint->bv-native #x-8000 #x7FFF)
(test-range-native get-s16 put-s16 2 sint->bv-native #x-1234 #x7890)
(test-range-native get-u32 put-u32 4 uint->bv-native 0                   #xFFFFFFFF)
(test-range-native get-u32 put-u32 4 uint->bv-native #x12345678          #x78901234)
(test-range-native get-s32 put-s32 4 sint->bv-native #x-80000000         #x7FFFFFFF)
(test-range-native get-s32 put-s32 4 sint->bv-native #x-12345678         #x78901234)


;; kind of test for get-u*
(let ()
  (define (get-u24 in endian) (get-u* in 3 endian))
  (define (put-u24 in n endian) (put-u* in n 3 endian))
  (define (get-s24 in endian) (get-s* in 3 endian))
  (define (put-s24 in n endian) (put-s* in n 3 endian))
  
  (test-range-big get-u24 put-u24 3 uinteger->bytevector 0 #xFFFFFF)
  (test-range-big get-s24 put-s24 3 sinteger->bytevector #x-800000 #x7FFFFF))

(define-simple-datum-define define-simple simple-read simple-write)

(define-simple <simple1> ()
  (a)
  (lambda (in)    (get-u8 in))
  (lambda (out a) (put-u8 out a)))

(define-simple <simple2> (<simple1>)
  (b)
  (lambda (in)    (get-u8 in))
  (lambda (out b) (put-u8 out b)))

(define-simple <simple3> ()
  (a b (c 0))
  (lambda (in)
    (values (get-u8 in) (get-u8 in) (get-u8 in)))
  (lambda (out a b c)
    (put-u8 out a)
    (put-u8 out b)
    (put-u8 out c)))

(let* ((in (open-bytevector-input-port #vu8(1)))
       (s1 (simple-read <simple1> in)))
  (test-assert "read" (is-a? s1 <simple1>))
  (test-equal "simple1 a" 1 (slot-ref s1 'a))
  ;; write
  (test-equal "simple1 write" #vu8(1)
	      (call-with-bytevector-output-port
	       (lambda (out) (simple-write <simple1> s1 out)))))

(let* ((in (open-bytevector-input-port #vu8(1 2)))
       (s2 (simple-read <simple2> in)))
  (test-assert "read" (is-a? s2 <simple2>))
  (test-equal "simple2 a" 1 (slot-ref s2 'a))
  (test-equal "simple2 b" 2 (slot-ref s2 'b))
  ;; write
  (test-equal "simple1 write" #vu8(1 2)
	      (call-with-bytevector-output-port
	       (lambda (out) (simple-write <simple2> s2 out)))))

(let* ((in (open-bytevector-input-port #vu8(1 2 3)))
       (s3 (simple-read <simple3> in)))
  (test-assert "read" (is-a? s3 <simple3>))
  (test-equal "simple3 a" 1 (slot-ref s3 'a))
  (test-equal "simple3 b" 2 (slot-ref s3 'b))
  (test-equal "simple3 c" 3 (slot-ref s3 'c))
  ;; write
  (test-equal "simple3 write" #vu8(1 2 3)
	      (call-with-bytevector-output-port
	       (lambda (out) (simple-write <simple3> s3 out)))))
;; default values
(let ((s3 (make <simple3>)))
  (test-assert "simple3 default a" (not (slot-ref s3 'a)))
  (test-assert "simple3 default b" (not (slot-ref s3 'b)))
  (test-equal  "simple3 default c" 0 (slot-ref s3 'c)))

(define-composite-data-define define-composite simple-read simple-write)

(define-composite <composite1> ()
  ((a <simple1>)
   (b <simple3>)
   (c <simple1>)))

(let* ((in (open-bytevector-input-port #vu8(1 1 2 3 1)))
       (c1 (simple-read <composite1> in)))
  (test-assert "read" (is-a? c1 <composite1>))
  (test-assert "composite1 a" (is-a? (~ c1 'a) <simple1>))
  (test-equal "composite1 a a" 1 (~ c1 'a 'a))
  (test-assert "composite1 b" (is-a? (~ c1 'b) <simple3>))
  (test-equal "composite1 b a" 1 (~ c1 'b 'a))
  (test-equal "composite1 b b" 2 (~ c1 'b 'b))
  (test-equal "composite1 b c" 3 (~ c1 'b 'c))
  (test-assert "composite1 c" (is-a? (~ c1 'c) <simple1>))
  (test-equal "composite1 c a" 1 (~ c1 'c 'a))
  ;; write
  (test-equal "composite1 write" #vu8(1 1 2 3 1)
	      (call-with-bytevector-output-port
	       (lambda (out) (simple-write <composite1> c1 out)))))

(define-composite <composite2> ()
  ((c1 <composite1>)
   (a  <simple1>)))

(let* ((in (open-bytevector-input-port #vu8(1 1 2 3 1 4)))
       (c2 (simple-read <composite2> in)))
  (test-assert "read" (is-a? c2 <composite2>))
  (let ((c1 (~ c2 'c1)))
    (test-assert "composite2 c1" (is-a? c1 <composite1>))
    (test-assert "composite2 c1 a" (is-a? (~ c1 'a) <simple1>))
    (test-equal "composite2 c1 a a" 1 (~ c1 'a 'a))
    (test-assert "composite2 c1 b" (is-a? (~ c1 'b) <simple3>))
    (test-equal "composite2 c1 b a" 1 (~ c1 'b 'a))
    (test-equal "composite2 c1 b b" 2 (~ c1 'b 'b))
    (test-equal "composite2 c1 b c" 3 (~ c1 'b 'c))
    (test-assert "composite2 c1 c" (is-a? (~ c1 'c) <simple1>))
    (test-equal "composite2 c1 c a" 1 (~ c1 'c 'a)))

  (test-assert "composite2 a" (is-a? (~ c2 'a) <simple1>))
  (test-equal "composite2 a a" 4 (~ c2 'a 'a))
  ;; write
  (test-equal "composite2 write" #vu8(1 1 2 3 1 4)
	      (call-with-bytevector-output-port
	       (lambda (out) (simple-write <composite2> c2 out)))))

(define-composite <composite3> (<composite1>)
  ((d  <simple1>)))

(let* ((in (open-bytevector-input-port #vu8(1 1 2 3 1 4)))
       (c3 (simple-read <composite3> in)))
  (test-assert "read" (is-a? c3 <composite3>))
  (let ((c1 c3))
    (test-assert "composite3 c1" (is-a? c1 <composite1>))
    (test-assert "composite3 c1 a" (is-a? (~ c1 'a) <simple1>))
    (test-equal "composite3 c1 a a" 1 (~ c1 'a 'a))
    (test-assert "composite3 c1 b" (is-a? (~ c1 'b) <simple3>))
    (test-equal "composite3 c1 b a" 1 (~ c1 'b 'a))
    (test-equal "composite3 c1 b b" 2 (~ c1 'b 'b))
    (test-equal "composite3 c1 b c" 3 (~ c1 'b 'c))
    (test-assert "composite3 c1 c" (is-a? (~ c1 'c) <simple1>))
    (test-equal "composite3 c1 c a" 1 (~ c1 'c 'a)))

  (test-assert "composite3 a" (is-a? (~ c3 'd) <simple1>))
  (test-equal "composite3 a a" 4 (~ c3 'd 'a))
  ;; write
  (test-equal "composite3 write" #vu8(1 1 2 3 1 4)
	      (call-with-bytevector-output-port
	       (lambda (out) (simple-write <composite3> c3 out)))))

(test-end)
