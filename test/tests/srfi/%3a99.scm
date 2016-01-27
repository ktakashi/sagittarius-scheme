;; to avoid reading :point as keyword...
#!r6rs
(import (rnrs)
	(srfi :99)
	(srfi :64))

(test-begin "SRFI-99 - ERR5RS")

;; from example
(define :point (make-rtd 'point '#((mutable x) (mutable y))))

(define make-point (rtd-constructor :point))

(define point? (rtd-predicate :point))
(define point-x (rtd-accessor :point 'x))
(define point-y (rtd-accessor :point 'y))
(define point-x-set! (rtd-mutator :point 'x))
(define point-y-set! (rtd-mutator :point 'y))

(define p1 (make-point 1 2))
(test-assert (point? p1))
(test-equal 1 (point-x p1))
(test-equal 2 (point-y p1))
(test-assert (point-x-set! p1 5))
(test-equal 5 (point-x p1))

(define :point2
  (make-rtd 'point2 '#((mutable x) (mutable y)) :point))

(define make-point2
  (rtd-constructor :point2))
(define point2? (rtd-predicate :point2))
(define point2-xx (rtd-accessor :point2 'x))
(define point2-yy (rtd-accessor :point2 'y))

(define p2 (make-point2 1 2 3 4))
(test-assert (point? p2))
(test-equal 1 (point-x p2))
(test-equal 2 (point-y p2))
(test-equal 3 (point2-xx p2))
(test-equal 4 (point2-yy p2))

(define make-point/abs
  (let ((maker (rtd-constructor :point)))
    (lambda (x y)
      (maker (abs x) (abs y)))))

(test-equal 1 (point-x (make-point/abs -1 -2)))
(test-equal 2 (point-y (make-point/abs -1 -2)))

(define :cpoint
  (make-rtd 'cpoint '#((mutable rgb)) :point))

(define make-cpoint
  (let ((maker (rtd-constructor :cpoint)))
    (lambda (x y c)
      (maker x y (color->rgb c)))))

(define make-cpoint/abs
  (let ((maker (rtd-constructor :cpoint)))
    (lambda (x y c)
      (maker (abs x) (abs y) (color->rgb c)))))

(define cpoint-rgb
  (rtd-accessor :cpoint 'rgb))

(define (color->rgb c)
  (cons 'rgb c))

(test-equal '(rgb . red) (cpoint-rgb (make-cpoint -1 -3 'red)))
(test-equal -1 (point-x (make-cpoint -1 -3 'red)))
(test-equal 1  (point-x (make-cpoint/abs -1 -3 'red)))

(test-end)
