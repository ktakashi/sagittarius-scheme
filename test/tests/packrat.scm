;; -*- scheme -*-
#!compatible

(import (rnrs)
	(packrat)
	(srfi :64 testing))

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


(define greedy (packrat-parser
		top
		(top (('! vs <- (+ item+)) vs)
		     (('% vs <- (* '*)) vs)
		     (('$ vs <- (? '?)) vs))
		(item+ (('+) '+))
		(item* (('*) '*))
		(item? (('?) '?))))

(let ((g-ok (generator '((!) (+) (+) (+))))
      (g-ng (generator '((!) (*)))))
  (let ((ok (greedy (base-generator->results g-ok)))
	(ng (greedy (base-generator->results g-ng))))
    (test-assert "success?" (parse-result-successful? ok))
    (test-equal "result" '(+ + +) (parse-result-semantic-value ok))
    (test-assert "failed" (not (parse-result-successful? ng)))))

(let ((g-ok (generator '((!) (+) (+) (+))))
      (g-ng (generator '((!) (*)))))
  (let ((ok (greedy (base-generator->results g-ok)))
	(ng (greedy (base-generator->results g-ng))))
    (test-assert "success?" (parse-result-successful? ok))
    (test-equal "result" '(+ + +) (parse-result-semantic-value ok))
    (test-assert "failed" (not (parse-result-successful? ng)))))

;; semantic value is cdr part otherwise it would fail
(let ((g-ok (generator '((%) (* . *) (* . *) (* . *))))
      (g-ok-2 (generator '((%)))))
  (let ((ok (greedy (base-generator->results g-ok)))
	(ok2 (greedy (base-generator->results g-ok-2))))
    (test-assert "success?" (parse-result-successful? ok))
    (test-equal "result" '(* * *) (parse-result-semantic-value ok))
    (test-assert "null match" (parse-result-successful? ok2))
    (test-equal "result null" '() (parse-result-semantic-value ok2))))

;; semantic value is cdr part otherwise it would fail
(let ((g-ok (generator '(($) (? . 1) (? . 2) (? . 3))))
      (g-ok-2 (generator '(($)))))
  (let ((ok (greedy (base-generator->results g-ok)))
	(ok2 (greedy (base-generator->results g-ok-2))))
    (test-assert "success?" (parse-result-successful? ok))
    (test-equal "result" '(1) (parse-result-semantic-value ok))
    (test-assert "null match" (parse-result-successful? ok2))
    (test-equal "result null" '() (parse-result-semantic-value ok2))))

(test-end)
