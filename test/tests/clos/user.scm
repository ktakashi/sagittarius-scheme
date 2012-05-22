;; -*- mode: scheme; coding: utf-8; -*-
(import (rnrs)
	(clos user)
	(srfi :64 testing))


(test-begin "CLOS user APIs test")
(define-class <person> () 
  ((name :init-keyword :name)
   (age  :init-keyword :age)))

(let ((sam (make <person> :name "Sam" :age 30)))
  (test-assert "person" (is-a? sam <person>))
  (test-equal "name" "Sam" (slot-ref sam 'name))
  (test-equal "age" 30 (slot-ref sam 'age)))

(define-class <singer> (<person>) ())
(define (sing singer) "sing a song")
(define-method object-apply ((self <singer>))
  (sing self))
(let ((singer (make <singer> :name "Kei" :age 30)))
  (test-assert "person" (is-a? singer <person>))
  (test-assert "singer" (is-a? singer <singer>))
  (test-equal "name" "Kei" (slot-ref singer 'name))
  (test-equal "age" 30 (slot-ref singer 'age))
  (test-equal "apply" "sing a song" (singer)))

(define-method object-equal? ((p1 <person>) (p2 <person>))
  (and (equal? (slot-ref p1 'name) (slot-ref p2 'name))
       (eqv? (slot-ref p1 'age) (slot-ref p2 'age))))

(test-assert "same person"
	     (equal? (make <person> :name "Marion" :age 26)
		     (make <person> :name "Marion" :age 26)))

(test-end)