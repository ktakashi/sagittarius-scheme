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
	(test-equal input expected v))))
  (define (test-a-fail parser input expected)
    (define in-list (string->list input))
    (let ((l (generator->lseq (list->generator in-list))))
      (let-values (((s v nl) (parser l)))
	(test-assert (parse-expect? s))
	(test-equal in-list (lseq-realize nl))
	(test-equal `(,input ,v) expected v))))
  
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
	       "aa" '((expected "more than 3 As") (got eof))))

(test-error assertion-violation? ($many ($fail "incorrect happen") 5 2))

(let-values (((s v n)
	      (($do (c* ($many $any 3 3)) ($return (list->string c*)))
	       (generator->lseq (string->generator "abcdef")))))
  (test-equal "abc" v))

(define (test-$cond condition expected)
  (let-values (((s v n) (($cond ((eq? condition 'fail) ($many $any 3 3))
				((eq? condition 'ok) ($return 'ok))
				(else ($return '??)))
			 (generator->lseq (string->generator "abcdef")))))
    (test-equal expected v)))
(test-$cond 'ok 'ok)
(test-$cond 'fail '(#\a #\b #\c))
(test-$cond 'unknown '??)

(let ((seq (generator->lseq (string->generator "abcdef"))))
  (let-values (((s v l) (($peek $any) seq)))
    (test-assert (parse-success? s))
    (test-equal seq l))
  (let-values (((s v l) (($seq ($eqv? #\a) ($peek $any)) seq)))
    (test-assert (parse-success? s))
    (test-equal #\b v)
    (test-equal (lseq-cdr seq) l))

  (let-values (((s v l)
		(($or ($do (($eqv? #\a)) (($peek ($eqv? #\z))) ($return 'ng))
		      ($do (($eqv? #\a)) (($peek ($eqv? #\b))) ($return 'ok)))
		 seq)))
    (test-assert (parse-success? s))
    (test-equal 'ok v)
    (test-equal (lseq-cdr seq) l)))

(let ((seq (generator->lseq (string->generator "abcdef"))))
  (let-values (((s v l) (($peek-match ($eqv? #\a)
				      ($many $any 3 3)
				      ($fail "unexpected"))
			 seq)))
    (test-assert (parse-success? s))
    (test-equal '(#\a #\b #\c) v))
  (let-values (((s v l) (($peek-match ($eqv? #\b)
				      ($fail "unexpected")
				      ($many $any 3 3))
			 seq)))
    (test-assert (parse-success? s))
    (test-equal '(#\a #\b #\c) v)))

;; $fail let entire expression fail
(let ()
  (define failure ($or ($fail "stop!") ($eqv? #\a)))
  (let ((seq (generator->lseq (string->generator "abcdef"))))
    (let-values (((s v nl) (failure seq)))
      (test-assert  (parse-fail? s))
      (test-equal "stop!" v))))

(test-end)
