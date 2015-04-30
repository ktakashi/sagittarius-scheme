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

(define quantifier (packrat-parser
		    top
		    (top (('~ vs <- (+ item+)) vs)
			 (('% vs <- (* '*)) vs)
			 (('$ vs <- (? '?)) vs)
			 (('^ vs <- (+ (/ item* item?))) vs)
			 (('|#| vs <- (+ item+ item*)) vs))
		    (item+ (('+) '+))
		    (item* (('*) '*))
		    (item? (('?) '?))))

(let ((g-ok (generator '((~) (+) (+) (+))))
      (g-ng (generator '((~) (*)))))
  (let ((ok (quantifier (base-generator->results g-ok)))
	(ng (quantifier (base-generator->results g-ng))))
    (test-assert "success?" (parse-result-successful? ok))
    (test-equal "result(1)" '(+ + +) (parse-result-semantic-value ok))
    (test-assert "failed" (not (parse-result-successful? ng)))))

;; semantic value is cdr part otherwise it would fail
(let ((g-ok (generator '((%) (* . *) (* . *) (* . *))))
      (g-ok-2 (generator '((%)))))
  (let ((ok (quantifier (base-generator->results g-ok)))
	(ok2 (quantifier (base-generator->results g-ok-2))))
    (test-assert "success?" (parse-result-successful? ok))
    (test-equal "result(2)" '(* * *) (parse-result-semantic-value ok))
    (test-assert "null match" (parse-result-successful? ok2))
    (test-equal "result null" '() (parse-result-semantic-value ok2))))

;; semantic value is cdr part otherwise it would fail
(let ((g-ok (generator '(($) (? . 1) (? . 2) (? . 3))))
      (g-ok-2 (generator '(($)))))
  (let ((ok (quantifier (base-generator->results g-ok)))
	(ok2 (quantifier (base-generator->results g-ok-2))))
    (test-assert "success?" (parse-result-successful? ok))
    (test-equal "result(3)" '(1) (parse-result-semantic-value ok))
    (test-assert "null match" (parse-result-successful? ok2))
    (test-equal "result null" '() (parse-result-semantic-value ok2))))

(let ((g-ok (generator '((^) (*) (?) (+))))
      (g-ng (generator '((^) (+) (?) (*)))))
  (let ((ok (quantifier (base-generator->results g-ok)))
	(ng (quantifier (base-generator->results g-ng))))
    (test-assert "success?" (parse-result-successful? ok))
    (test-equal "result(4)" '(* ?) (parse-result-semantic-value ok))
    (test-assert "failed" (not (parse-result-successful? ng)))))

;; sequence
(let ((g-ok (generator '((|#|) (+) (*) (+) (*))))
      (g-ng (generator '((|#|) (+) (?) (*)))))
  (let ((ok (quantifier (base-generator->results g-ok)))
	(ng (quantifier (base-generator->results g-ng))))
    (test-assert "success?" (parse-result-successful? ok))
    (test-equal "result(5)" '((+ *) (+ *)) (parse-result-semantic-value ok))
    (test-assert "failed" (not (parse-result-successful? ng)))))

;; more extensive tests
(define quantifier2 (packrat-parser
		     top
		     (top (('~ vs <- (+ (+ item+) (* item*))) vs)
			  (('$ vs <- (+ item++ item*)) vs))
		     (item++ ((vs <- (+ item+)) vs))
		     (item+ (('+) '+))
		     (item* (('*) '*))
		     (item? (('?) '?))))

(let* ((g-ok (generator '((~) (+) (*) (+) (*))))
       (ok (quantifier2 (base-generator->results g-ok))))
  (test-assert "success?" (parse-result-successful? ok))
  (test-equal "result(6)" '(((+) (*)) ((+) (*)))
	      (parse-result-semantic-value ok)))

(let* ((g-ok (generator '(($) (+) (*) (+) (*))))
       (ok (quantifier2 (base-generator->results g-ok))))
  (test-assert "success?" (parse-result-successful? ok))
  (test-equal "result(7)" '(((+) *) ((+) *))
	      (parse-result-semantic-value ok)))

;; *, + and ?
(let ()
  (define checker (packrat-parser expr
				  (expr ((star <- (* '*)) star))))

  (let* ((g (generator '((* . *) (* . *) (* . *))))
	 (r (checker (base-generator->results g))))
    (test-assert (parse-result-successful? r))
    (test-equal "* (with value)" '(* * *) (parse-result-semantic-value r)))
  (let* ((g (generator '()))
	 (r (checker (base-generator->results g))))
    (test-assert (parse-result-successful? r))
    (test-equal "* (without value)" '() (parse-result-semantic-value r))))

(let ()
  (define checker (packrat-parser expr
				  (expr ((plus <- (+ '+)) plus))))
  (let* ((g (generator '((+ . +) (+ . +) (+ . +))))
	 (r (checker (base-generator->results g))))
    (test-assert (parse-result-successful? r))
    (test-equal "+ (with value)" '(+ + +) (parse-result-semantic-value r)))

  (let* ((g (generator '()))
	 (r (checker (base-generator->results g))))
    (test-assert (not (parse-result-successful? r))))
)

(let ()
  (define checker (packrat-parser expr
				  (expr ((ques <- (? '?) '&) ques))))
  (let* ((g (generator '((? . ?) (&))))
	 (r (checker (base-generator->results g))))
    (test-assert (parse-result-successful? r))
    (test-equal "? (with value)" '(?) (parse-result-semantic-value r)))
  (let* ((g (generator '((&))))
	 (r (checker (base-generator->results g))))
    (test-assert (parse-result-successful? r))
    (test-equal "? (without value)" '() (parse-result-semantic-value r)))

  (let* ((g (generator '((? . ?) (? . ?) (&))))
	 (r (checker (base-generator->results g))))
    (test-assert "more than one" (not (parse-result-successful? r))))
)

(let ()
  (define checker (packrat-parser expr
				  (expr ((eq <- (= 1 3 '=) '&) eq))))
  (let* ((g (generator '((= . =) (&))))
	 (r (checker (base-generator->results g))))
    (test-assert (parse-result-successful? r))
    (test-equal "= (min count)" '(=) (parse-result-semantic-value r)))

  (let* ((g (generator '((= . =) (= . =) (&))))
	 (r (checker (base-generator->results g))))
    (test-assert (parse-result-successful? r))
    (test-equal "= (middle count)" '(= =) (parse-result-semantic-value r)))

  (let* ((g (generator '((= . =) (= . =) (= . =) (&))))
	 (r (checker (base-generator->results g))))
    (test-assert (parse-result-successful? r))
    (test-equal "= (max count)" '(= = =) (parse-result-semantic-value r)))

  (let* ((g (generator '((&))))
	 (r (checker (base-generator->results g))))
    (test-assert (not (parse-result-successful? r))))

)

(let ()
  (define checker (packrat-parser expr
				  (expr ((alt <- (/ '= '+ '?) '&) alt))))

  (let* ((g (generator '((= . =) (&))))
	 (r (checker (base-generator->results g))))
    (test-assert (parse-result-successful? r))
    (test-equal "alt (=)" '= (parse-result-semantic-value r)))

  (let* ((g (generator '((+ . +) (&))))
	 (r (checker (base-generator->results g))))
    (test-assert (parse-result-successful? r))
    (test-equal "alt (+)" '+ (parse-result-semantic-value r)))

)  

(test-end)
