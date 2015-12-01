(import (rnrs)
	(text sql)
	(text sql scanner)
	(text sql parser)
	(srfi :64))

(test-begin "SQL")

(define (test-parse sql expected)
  (test-equal sql expected (parse-sql (open-string-input-port sql))))

(test-parse "/* comment */ select b.U&\"$42\" uescape '$' from a,b as b1(a,b)"
	    '(select ((~ b (unicode (! "$42") uescape "$")))
		     (from a (as b (b1 a b)))))
(test-parse "select +1,a||b||c from (select 1,2 from bar) as f(a,b)"
	    '(select ((+ 1) (^ a b c)) 
		     (from (as (select (1 2) (from bar)) (f a b)))))
(test-parse "select 1*1+2/2+3" '(select ((+ (* 1 1) (/ 2 2) 3))))
(test-parse "select 1*1+2/2-3" '(select ((+ (* 1 1) (- (/ 2 2) 3)))))

;; as
(test-parse "select a as b" '(select ((as a b))))
(test-parse "select a b" '(select ((as a b))))
(test-parse "select f.a as a" '(select ((as (~ f a) a))))
(test-parse "select f.a a" '(select ((as (~ f a) a))))

(test-parse "select 'a' as a" '(select ((as "a" a))))
(test-parse "select 'a' a" '(select ((as "a" a))))

(test-end)
