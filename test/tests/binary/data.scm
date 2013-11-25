(import (rnrs) (srfi :64)
	(clos user)
	(sagittarius object)
	(binary data))

(test-begin "Binary data read/write")

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
