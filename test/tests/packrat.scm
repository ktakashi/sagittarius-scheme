;; -*- scheme -*-
#!compatible

(import (rnrs)
	(packrat)
	(srfi :64))

;; from packrat.pdf
(define (generator tokens)
  (let ((stream tokens))
    (lambda ()
      (if (null? stream)
	  (values #f #f)
	  (let ((base-token (car stream)))
	    (set! stream (cdr stream))
	    (values #f base-token))))))

(define calc (packrat-parser expr
			     (expr ((a <- mulexp '+ b <- mulexp)
				    (+ a b))
				   ((a <- mulexp) a))
			     (mulexp ((a <- simple '* b <- simple)
				      (* a b))
				     ((a <- simple) a))
			     (simple ((a <- 'num) a)
				     (('oparen a <- expr 'cparen) a))))

(test-begin "packrat")

(let* ((g (generator '((num . 1) (+) (num . 2) (*) (num . 3))))
       (r (calc (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test-equal 7 (parse-result-semantic-value r)))

(let* ((g (generator
	   '((oparen) (num . 1) (+) (num . 2) (cparen) (*) (num . 3))))
       (r (calc (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test-equal 9 (parse-result-semantic-value r)))

(test-end)
