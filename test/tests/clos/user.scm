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

;; qualifier tests
(define-method say-hi :before ((o <person>))
  (display "before hi "))
(define-method say-hi :after ((o <person>))
  (display " after hi"))

(import (sagittarius io))

(define-method say-hi ((o <person>)) 
  (display "hi"))
(test-equal "say-hi" "before hi hi after hi"
	    (with-output-to-string
	      (lambda ()
		(say-hi (make <person>)))))

(define-method say-hi ((o <singer>)) 
  (display "lalala"))
(test-equal "say-hi" "before hi lalala after hi"
	    (with-output-to-string
	      (lambda ()
		(say-hi (make <singer>)))))

(define-method say-hi :around ((o <person>))
  (display "around (")
  (call-next-method)
  (display ")"))

(define-method say-hi :around ((o <singer>))
  (display "around singer (")
  (call-next-method)
  (display ")"))
(test-equal "say-hi with around" "around (before hi hi after hi)"
	    (with-output-to-string
	      (lambda ()
		(say-hi (make <person>)))))
(test-equal "say-hi with around"
	    "around singer (around (before hi lalala after hi))"
	    (with-output-to-string
	      (lambda ()
		(say-hi (make <singer>)))))

;; eql specializer
(define-method fact ((n (eql 0))) 1)
(define-method fact ((n <integer>)) (* n (fact (- n 1))))
(test-equal "generic fact (call)"  3628800 (fact 10))
(test-equal "generic fact (apply)" 3628800 (apply fact '(10)))

(define-method multi-eql ((m (eql 0)) (n (eql 1))) 'first)
(define-method multi-eql ((m (eql 0)) (n (eql 2))) 'second)
(define-method multi-eql (m n ) 'else)
(test-equal "(multi-eql 0 1)" 'first  (multi-eql 0 1))
(test-equal "(multi-eql 0 2)" 'second (multi-eql 0 2))
(test-equal "(multi-eql 'a 'a)" 'else (multi-eql 'a 'a))

;; issue 153
(define-method unpack-args :before ((a <symbol>) . args)  args)
(define-method unpack-args ((a <symbol>) . args) args)
(test-equal "unpack-args(1)" '() (unpack-args 'a))
(test-equal "unpack-args(2)" '(b) (unpack-args 'a 'b))
(test-equal "unpack-args(3)" '(b c) (unpack-args 'a 'b 'c))

(test-end)