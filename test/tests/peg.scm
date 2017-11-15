(import (rnrs)
	(peg)
	(sagittarius generators)
	(srfi :127)
	(srfi :64))

(test-begin "PEG")

(let ()
  (define %simple1
    ($do (a ($satisfy (lambda (v) (eq? (car v) 'num)))) ($return (cdr a))))
  (define %simple2
    ($do ($satisfy (lambda (v) (eq? (car v) 'oparen)))
	 (a %expr)
	 ($satisfy (lambda (v) (eq? (car v) 'cparen)))
	 ($return (cdr a))))
  (define %simple ($or %simple1 %simple2))
  
  (define %mulexp1
    ($do (a %simple)
	 (($satisfy (lambda (v) (eq? (car v) '*))) )
	 (b %simple)
	 ($return (* a b))))
  (define %mulexp2 ($do (a %simple) ($return a)))
  (define %mulexp ($or %mulexp1 %mulexp2))
  
  (define %expr1
    ($do (a %mulexp)
	 (($satisfy (lambda (v) (eq? (car v) '+))))
	 (b %mulexp)
	 ($return (+ a b))))
  (define %expr2 ($do (a %mulexp) ($return a)))
  (define %expr ($or %expr1 %expr2))
  
  (let ((l (generator->lseq
	    (list->generator '((num . 1) (+) (num . 2) (*) (num . 3))))))
    (let-values (((s v nl) (%expr l)))
      (test-assert (parse-success? s))
      (test-assert (null? nl))
      (test-equal 7 v))))

(let ()
  (define (test-a parser input expected first-remain)
    (let ((l (generator->lseq (list->generator (string->list input)))))
      (let-values (((s v nl) (parser l)))
	(test-assert (parse-success? s))
	(test-equal first-remain (lseq-car nl))
	(test-equal expected v))))
  (define (test-a-fail parser input expected)
    (define in-list (string->list input))
    (let ((l (generator->lseq (list->generator in-list))))
      (let-values (((s v nl) (parser l)))
	(test-assert (parse-expect? s))
	(test-equal in-list (lseq-realize nl))
	(test-equal expected v))))
  
  (test-a ($many ($satisfy (lambda (v) (eqv? #\a v))))
	  "aaab"
	  '(#\a #\a #\a)
	  #\b)
  (test-a ($many ($satisfy (lambda (v) (eqv? #\a v))) 2 2)
	  "aaab"
	  '(#\a #\a)
	  #\a)
  (test-a ($many ($satisfy (lambda (v) (eqv? #\a v))) 1 3)
	  "aaaab"
	  '(#\a #\a #\a)
	  #\a)

  (test-a-fail ($many ($satisfy (lambda (v) (eqv? #\a v)) "more than 3 As") 3)
	       "aa" "more than 3 As"))

(test-error assertion-violation? ($many ($fail "incorrect happen") 5 2))

(test-end)
