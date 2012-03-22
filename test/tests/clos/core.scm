;; -*- mode: scheme; coding: utf-8; -*-
(import (rnrs)
	(clos core)
	(srfi :64 testing))

(define <person> (make <class> 
		   :direct-supers (list <class>)
		   :direct-slots '(name age)
		   :definition-name '<person>))

(add-method initialize
	    (make <method>
	      :specializers (list <person>)
	      :generic initialize
	      :lambda-list '(person . initargs)
	      :procedure (lambda (call-next-method person initargs)
			   (initialize-direct-slots person <person> initargs))))

(define <painter> (make <class>
		    :direct-supers (list <person>)
		    :direct-slots '(pen)
		    :definition-name '<painter>))
(add-method initialize
	    (make <method>
	      :specializers (list <painter>)
	      :generic initialize
	      :lambda-list '(painter . initargs)
	      :procedure (lambda (call-next-method painter initargs)
			   (call-next-method)
			   (initialize-direct-slots
			    painter <painter> initargs))))

(define sam (make <person> :name "Sam" :age 38))
(define koch (make <painter> :name "Koch" :age 18 :pen "Pencil"))

(define person-name (make <generic> :definition-name 'person-name))
(define person-age (make <generic> :definition-name 'person-age))
(add-method person-name
	    (make <method>
	      :specializers (list <person>)
	      :generic person-name
	      :lambda-list '(person)
	      :procedure (lambda (call-next-method person)
			   (slot-ref person 'name))))
(add-method person-age
	    (make <method>
	      :specializers (list <person>)
	      :generic person-age
	      :lambda-list '(person)
	      :procedure (lambda (call-next-method person)
			   (slot-ref person 'age))))

(define painter-pen (make <generic> :definition-name 'painter-pen))
(add-method painter-pen
	    (make <method>
	      :specializers (list <painter>)
	      :generic painter-pen
	      :lambda-list '(painter)
	      :procedure (lambda (call-next-method painter)
			   (slot-ref painter 'pen))))

(test-begin "clos core test")
;; basic
(test-equal 38 (slot-ref sam 'age))
(test-equal 39 (begin (slot-set! sam 'age (+ (slot-ref sam 'age) 1))
		      (slot-ref sam 'age)))
(test-equal "Pencil" (slot-ref koch 'pen))

;; generic  
(test-equal "Sam" (person-name sam))
(test-equal "Koch" (person-name koch))
(test-equal "Pencil" (painter-pen koch))
;; sam is not a painter
(test-error (painter-pen sam))

(test-end)
