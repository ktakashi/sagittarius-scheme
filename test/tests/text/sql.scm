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
;; numeric operation
(test-parse "select 1*1+2/2+3" '(select ((+ (* 1 1) (/ 2 2) 3))))
(test-parse "select 1*1+2/2-3" '(select ((+ (* 1 1) (- (/ 2 2) 3)))))

;; as
(test-parse "select a as b" '(select ((as a b))))
(test-parse "select a b" '(select ((as a b))))
(test-parse "select f.a as a" '(select ((as (~ f a) a))))
(test-parse "select f.a a" '(select ((as (~ f a) a))))

(test-parse "select 'a' as a" '(select ((as "a" a))))
(test-parse "select 'a' a" '(select ((as "a" a))))

;; *
(test-parse "select *" '(select *))
(test-parse "select foo.*" '(select ((~ foo *))))
;; weird ones
(test-parse "select foo.* as (a, b, c)" '(select ((as (~ foo *) (#f a b c)))))
(test-parse "select (select *).* as (a, b, c)" 
	    '(select ((as (~ (select *) *) (#f a b c)))))

;; string
(test-parse "select 'a'" '(select ("a")))
(test-parse "select U&'a'" '(select ((unicode "a"))))
(test-parse "select U&'a' uescape '$'" '(select ((unicode "a" uescape "$"))))

;; multiset
(test-parse "SELECT ntab MULTISET EXCEPT DISTINCT ntab2 mset_except;"
	    '(select ((as (multiset except distinct ntab ntab2) mset_except))))
(test-parse "SELECT ntab MULTISET UNION DISTINCT ntab2 mset_union;"
	    '(select ((as (multiset union distinct ntab ntab2) mset_union))))
(test-parse "SELECT ntab MULTISET INTERSECT DISTINCT ntab2 mset;"
	    '(select ((as (multiset intersect distinct ntab ntab2) mset))))
;; nested
;; NB: the nested order may change in future such as inner most would be
;;     outer most. (in this case intersect would be the first multiset)
;;     so don't document how it would be yet.
(test-parse "SELECT ntab MULTISET UNION ntab2 MULTISET INTERSECT ntab3 mset;"
	    '(select ((as (multiset union ntab (multiset intersect ntab2 ntab3))
			  mset))))

(test-parse "select * from t group by c1, (c2, c3), rollup (c4), cube (c5), grouping sets (c6, rollup(c7), cube(c8));"
	    '(select * (from t) (group-by c1 (c2 c3) (rollup c4) (cube c5) (grouping-sets c6 (rollup c7) (cube c8)))))

;; where
(test-parse "select * from t where 1=1" '(select * (from t) (where (= 1 1))))
(test-parse "select * from t where 1=1 or 2=2" 
	    '(select * (from t) (where (or (= 1 1) (= 2 2)))))
(test-parse "select * from t where 1=1 and 2=2" 
	    '(select * (from t) (where (and (= 1 1) (= 2 2)))))
(test-parse "select * from t where 1=1 and 2=2 or 3=3" 
	    '(select * (from t) (where (or (and (= 1 1) (= 2 2)) (= 3 3)))))
(test-parse "select * from t where 1=1 and (2=2 or 3=3)" 
	    '(select * (from t) (where (and (= 1 1) (or (= 2 2) (= 3 3))))))

(test-end)
