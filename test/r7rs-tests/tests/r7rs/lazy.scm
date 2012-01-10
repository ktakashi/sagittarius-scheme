;; -*- mode: scheme; coding: utf-8; -*-
#!r7rs
(define-library (tests r7rs lazy)
  (import (scheme base)
	  (scheme lazy)
	  (tests r7rs test))
  (export run-r7rs-lazy-tests)
  (begin
    (define integers
      (letrec ((next
		(lambda (n)
		  (delay (cons n (next (+ n 1)))))))
	(next 0)))
    (define head
      (lambda (stream) (car (force stream))))
    (define tail
      (lambda (stream) (cdr (force stream))))

    (define (stream-filter p? s)
      (lazy
       (if (null? (force s))
	   (delay '())
	   (let ((h (car (force s)))
		 (t (cdr (force s))))
	     (if (p? h)
		 (delay (cons h (stream-filter p? t)))
		 (stream-filter p? t))))))

    (define count 0)
    (define x 5)
    (define p
      (delay (begin (set! count (+ count 1))
		    (if (> count x)
			count
			(force p)))))

    (define (run-r7rs-lazy-tests)
      ;; 4.2.5
      (test-equal 3 (force (delay (+ 1 2))))
      (test-equal '(3 3) (let ((p (delay (+ 1 2))))
			   (list (force p) (force p))))

      (test-equal 2 (head (tail (tail integers))))

      (test-equal 5 (head (tail (tail (stream-filter odd? integers)))))

      (test-equal 6 (force p))
      (test-equal 6 (begin (set! x 10) (force p)))
      )
    ) 
)