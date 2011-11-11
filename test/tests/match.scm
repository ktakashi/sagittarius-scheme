;; -*- coding: utf-8 -*-
#!compatible
(library (tests match)
    (export run-match-tests)
    (import (rnrs)
	    (match)
	    (srfi :64))

  (define-record-type employee
    (fields name title))

  ;; syntax-rules implementation problem.
  ;; (match) uses R7RS syntax-rules however, it really does not coopeate with
  ;; R6RS syntax-rules. needs to be fixed.
  (define (run-match-tests)
    (test-assert "match-1" (let ((ls (list 1 2 3))) (match ls ((1 2 3) #t))))
    (test-equal "match-bind"
		2
		(match (list 1 2 3)
		  ((a b c) b)))
    (test-equal "match-bind 2"
		2
		(match (list 1 2 1)
		  ((a a b) 1)
		  ((a b a) 2)))

    (test-equal "match-_"
		1
		(match (list 1 2 1) ((_ _ b) 1) ((a b a) 2)))
    (test-equal "'a"
		2
		(match 'a ('b 1) ('a 2)))
    (test-equal "match ellipsis"
		'(#t #t #t)
		(list (match (list 1 2) ((1 2 3 ...) #t))
		      (match (list 1 2 3) ((1 2 3 ...) #t))
		      (match (list 1 2 3 3 3) ((1 2 3 ...) #t))))
    (test-equal "match ellipsis 2"
		'(() (3) (3 4 5))
		(list (match (list 1 2) ((a b c ...) c))
		      (match (list 1 2 3) ((a b c ...) c))
		      (match (list 1 2 3 4 5) ((a b c ...) c))))

    (test-equal "match ellipsis 3"
		'(() (3) (3 4 5))
		(list (match (list 1 2 3 4) ((a b c ... d e) c))
		      (match (list 1 2 3 4 5) ((a b c ... d e) c))
		      (match (list 1 2 3 4 5 6 7) ((a b c ... d e) c))))

    (test-equal "match ..1"
		'(#t (3))
		(list (guard (e ((error? e))
				(else #f))
			(match (list 1 2) ((a b c ..1) c)))
		      (match (list 1 2 3) ((a b c ..1) c))))

    (test-equal "match and"
		'(#t 1 1)
		(list (match 1 ((and) #t))
		      (match 1 ((and x) x))
		      (match 1 ((and x 1) x))))

    (test-equal "match or"
		'(#f 1 1)
		(list (match 1 ((or) #t) (else #f))
		      (match 1 ((or x) x))
		      (match 1 ((or x 1) x))))

    (test-assert "match not" (match 1 ((not 2) #t)))
    (test-equal "match pred"
		1
		(match 1 ((? odd? x) x)))
    (test-equal "match ="
		(list 1 2)
		(list (match '(1 . 2) ((= car x) x))
		      (match 4 ((= sqrt x) x))))
    ;; ditto
    (test-equal "match $"
		(list "Doctor" "Bob")
		(match (make-employee "Bob" "Doctor")
		  (($ employee n t) (list t n))))

    (test-equal "match set!"
		(list '(1 . 3) 2)
		(list (let ((x (cons 1 2))) (match x ((1 . (set! s)) (s 3) x)))
		      (match '(1 . 2) ((1 . (get! g)) (g)))))

    (test-equal "match ***"
		(list '(a a a)
		      '(a c f))
		(list (match '(a (a (a b))) ((x *** 'b) x))
		      (match '(a (b) (c (d e) (f g))) ((x *** 'g) x))))
    )
)
