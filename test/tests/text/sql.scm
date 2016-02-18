(import (rnrs)
	(text sql)
	(text sql scanner)
	(text sql parser)
	(text sql simplifier)
	(text sql serializer)
	(srfi :39)
	(srfi :64))

(test-begin "SQL")

(define (test-parse sql expected)
  (let ((p (parse-sql (open-string-input-port sql))))
    (test-equal sql expected p)
    (test-equal (format "~a (validate)" sql)
		expected
		(parse-sql (open-string-input-port (ssql->sql p))))))

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
	    '(select ((as (multiset-except-distinct ntab ntab2) mset_except))))
(test-parse "SELECT ntab MULTISET UNION DISTINCT ntab2 mset_union;"
	    '(select ((as (multiset-union-distinct ntab ntab2) mset_union))))
(test-parse "SELECT ntab MULTISET INTERSECT DISTINCT ntab2 mset;"
	    '(select ((as (multiset-intersect-distinct ntab ntab2) mset))))

;; nested
;; NB: the nested order may change in future such as inner most would be
;;     outer most. (in this case intersect would be the first multiset)
;;     so don't document how it would be yet.
(test-parse "SELECT ntab MULTISET UNION ntab2 MULTISET INTERSECT ntab3 mset;"
	    '(select ((as (multiset-union ntab (multiset-intersect ntab2 ntab3))
			  mset))))

;; set function
(test-parse "select grouping(a,b,c)" '(select ((grouping a b c))))

;; cast
(test-parse "select cast(a as int)" '(select ((cast a int))))
(test-parse "select cast(a as int array)" '(select ((cast a (int array)))))
(test-parse "select cast(a as int array[1])" '(select ((cast a (int array 1)))))
(test-parse "select cast(a as blob(1K octets))" 
	    '(select ((cast a (blob 1024 octets)))))
(test-parse "select cast(a as ref(t.foo))" 
	    '(select ((cast a (ref (~ t foo))))))
;; TODO more data type tests

;; implicitly typed
(test-parse "select cast(null as int)" '(select ((cast null int))))
(test-parse "select cast(array[] as int)" '(select ((cast (array) int))))
(test-parse "select cast(multiset[] as int)" '(select ((cast (multiset) int))))

;; next value
(test-parse "select next value for seq" '(select ((next-value-for seq))))

;; field reference
(test-parse "select (1+1) . foo" '(select ((~ (+ 1 1) foo))))

;; subtype treatment
(test-parse "select treat(a as ref(t.b))" '(select ((treat a (ref (~ t b))))))

;; method invocation
;; direct
(test-parse "select a.m" '(select ((~ a m)))) ;; the same as identifier chain
(test-parse "select a.m()" '(select (((~ a m)))))
(test-parse "select a.m(b,c)" '(select (((~ a m) b c))))
;; generalized
(test-parse "select (a as int).m" '(select (((~ (as a int) m)))))
(test-parse "select (a as int).m()" '(select (((~ (as a int) m)))))
(test-parse "select (a as int).m(b,c)" '(select (((~ (as a int) m) b c))))

;; FIXME very unfortunate case
;; this is because the first (a) is something else...
(test-parse "select (a).m()" '(select ((~ a (m)))))

;; static method invocation
(test-parse "select foo::m" '(select ((:: foo m))))
(test-parse "select foo::m()" '(select (((:: foo m)))))
(test-parse "select foo.bar::m" '(select ((:: (~ foo bar) m))))
(test-parse "select foo.bar::m()" '(select (((:: (~ foo bar) m)))))

;; new specification
(test-parse "select new m()" '(select ((new (m)))))

;; attribute or method reference
(test-parse "select foo->m" '(select ((-> foo m))))
(test-parse "select foo->m()" '(select (((-> foo m)))))
(test-parse "select foo.bar->m()" '(select (((-> (~ foo bar) m)))))

;; array value constructor
(test-parse "select array[1,2,3]" '(select ((array 1 2 3))))
(test-parse "select array(select 1)" '(select ((array (select (1))))))
(test-parse "select multiset[1,2,3]" '(select ((multiset 1 2 3))))
(test-parse "select multiset(select 1)" '(select ((multiset (select (1))))))
(test-parse "select table(select 1)" '(select ((table (select (1))))))

;; routine invocation
(test-parse "select foo(a)" '(select ((foo a))))

;; join
(test-parse "select * from t join a on t.id = a.id"
	    '(select * (from (t (join a (on (= (~ t id) (~ a id))))))))
(test-parse "select * from t left join a on t.id = a.id"
	    '(select * (from (t (left-join a (on (= (~ t id) (~ a id))))))))
(test-parse "select * from t right join a on t.id = a.id"
	    '(select * (from (t (right-join a (on (= (~ t id) (~ a id))))))))
(test-parse "select * from t full join a on t.id = a.id"
	    '(select * (from (t (full-join a (on (= (~ t id) (~ a id))))))))
(test-parse "select * \
             from t inner join a on t.id = a.id \
             left join b on t.id = b.id"
	    '(select * (from (t (inner-join a (on (= (~ t id) (~ a id))))
				(left-join b  (on (= (~ t id) (~ b id))))))))
;; very rare case but it's valid.
;; NB: because of this, join clause looks a bit weird
(test-parse "select * from t join a on t.id = a.id, w"
	    '(select * (from (t (join a (on (= (~ t id) (~ a id))))) w)))

(test-parse "select * from t full join a using (a, b, c)"
	    '(select * (from (t (full-join a (using a b c))))))

;; group by
(test-parse "select * from t group by c1, (c2, c3), rollup (c4), cube (c5), grouping sets (c6, rollup(c7), cube(c8));"
	    '(select * (from t) (group-by c1 (c2 c3) (rollup c4) (cube c5) (grouping-sets c6 (rollup c7) (cube c8)))))
(test-parse "select * from t group by ()"
	    '(select * (from t) (group-by ())))

;; order by
(test-parse "select * from f order by a" '(select * (from f) (order-by a)))
(test-parse "select * from f order by 1" '(select * (from f) (order-by 1)))
(test-parse "select * from f order by a asc"
	    '(select * (from f) (order-by (a asc))))
(test-parse "select * from f order by a desc"
	    '(select * (from f) (order-by (a desc))))
(test-parse "select * from f order by a desc, b"
	    '(select * (from f) (order-by (a desc) b)))
(test-parse "select * from f order by a desc, b nulls first"
	    '(select * (from f) (order-by (a desc) (b nulls-first))))
(test-parse "select * from f order by a desc, b nulls last"
	    '(select * (from f) (order-by (a desc) (b nulls-last))))
(test-parse "select * from f order by a desc, b asc nulls last"
	    '(select * (from f) (order-by (a desc) (b asc nulls-last))))


;; where
(test-parse "select * from t where 1=1" '(select * (from t) (where (= 1 1))))
(test-parse "select * from t where 1=1 or 2=2" 
	    '(select * (from t) (where (or (= 1 1) (= 2 2)))))
(test-parse "select * from t where 1=1 or 2=2 or 3=3" 
	    '(select * (from t) (where (or (= 1 1) (= 2 2) (= 3 3)))))
(test-parse "select * from t where 1=1 and 2=2" 
	    '(select * (from t) (where (and (= 1 1) (= 2 2)))))
(test-parse "select * from t where 1=1 and 2=2 and 3=3" 
	    '(select * (from t) (where (and (= 1 1) (= 2 2) (= 3 3)))))
(test-parse "select * from t where 1=1 and 2=2 or 3=3" 
	    '(select * (from t) (where (or (and (= 1 1) (= 2 2)) (= 3 3)))))
(test-parse "select * from t where 1=1 and (2=2 or 3=3)" 
	    '(select * (from t) (where (and (= 1 1) (or (= 2 2) (= 3 3))))))

(test-parse "select * from f where a = b||c)"
	    '(select * (from f) (where (= a (^ b c)))))

;; between
(test-parse "select * from t where a between 1 and 2" 
	    '(select * (from t) (where (between a 1 2))))
(test-parse "select * from t where a between asymmetric 1 and 2" 
	    '(select * (from t) (where (between-asymmetric a 1 2))))
(test-parse "select * from t where a between symmetric 1 and 2" 
	    '(select * (from t) (where (between-symmetric a 1 2))))
(test-parse "select * from t where a not between 1 and 2" 
	    '(select * (from t) (where (not-between a 1 2))))
(test-parse "select * from t where a not between asymmetric 1 and 2" 
	    '(select * (from t) (where (not-between-asymmetric a 1 2))))
(test-parse "select * from t where a not between symmetric 1 and 2" 
	    '(select * (from t) (where (not-between-symmetric a 1 2))))

;; in
(test-parse "select * from t where a in (1, 2)" 
	    '(select * (from t) (where (in a (1 2)))))
(test-parse "select * from t where a not in (1, 2)" 
	    '(select * (from t) (where (not-in a (1 2)))))
(test-parse "select * from t where a in (select 1)" 
	    '(select * (from t) (where (in a (select (1))))))

;; like
(test-parse "select * from t where a like 'a'" 
	    '(select * (from t) (where (like a "a"))))
(test-parse "select * from t where a like 'a$_' escape '$'" 
	    '(select * (from t) (where (like a (escape "a$_" "$")))))
(test-parse "select * from t where a not like 'a'" 
	    '(select * (from t) (where (not-like a "a"))))
(test-parse "select * from t where a not like 'a$_' escape '$'" 
	    '(select * (from t) (where (not-like a (escape "a$_" "$")))))
(test-parse "select * from t where a ilike 'a'" 
	    '(select * (from t) (where (ilike a "a"))))
(test-parse "select * from t where a ilike 'a$_' escape '$'" 
	    '(select * (from t) (where (ilike a (escape "a$_" "$")))))
(test-parse "select * from t where a not ilike 'a'" 
	    '(select * (from t) (where (not-ilike a "a"))))
(test-parse "select * from t where a not ilike 'a$_' escape '$'" 
	    '(select * (from t) (where (not-ilike a (escape "a$_" "$")))))

;; similar to
(test-parse "select * from t where a similar to 'b';"
	    '(select * (from t) (where (similar-to a "b"))))
(test-parse "select * from t where a not similar to 'b';"
	    '(select * (from t) (where (not-similar-to a "b"))))

;; is null
(test-parse "select * from t where a is null"  
	    '(select * (from t) (where (null? a))))
(test-parse "select * from t where a is not null"  
	    '(select * (from t) (where (not-null? a))))

;; quantifier
(test-parse "select * from t where a = all (select b from f)"
	    '(select * (from t) (where (=-all a (select (b) (from f))))))
(test-parse "select * from t where a = some (select b from f)"
	    '(select * (from t) (where (=-some a (select (b) (from f))))))
(test-parse "select * from t where a = any (select b from f)"
	    '(select * (from t) (where (=-any a (select (b) (from f))))))

;; normalized
(test-parse "select * from t where a is normalized"
	    '(select * (from t) (where (normalized? a))))
(test-parse "select * from t where a is not normalized"
	    '(select * (from t) (where (not-normalized? a))))

;; match
(test-parse "select * from t where a match (select * from t)"
	    '(select * (from t) (where (match a (select * (from t))))))
(test-parse "select * from t where a match simple (select * from t)"
	    '(select * (from t) (where (match-simple a (select * (from t))))))
(test-parse "select * from t where a match partial (select * from t)"
	    '(select * (from t) (where (match-partial a (select * (from t))))))
(test-parse "select * from t where a match full (select * from t)"
	    '(select * (from t) (where (match-full a (select * (from t))))))
(test-parse "select * from t where a match unique (select * from t)"
	    '(select * (from t) (where (match-unique a (select * (from t))))))
(test-parse "select * from t where a match unique simple (select * from t)"
	    '(select * (from t) 
		     (where (match-unique-simple a (select * (from t))))))
(test-parse "select * from t where a match unique partial (select * from t)"
	    '(select * (from t) 
		     (where (match-unique-partial a (select * (from t))))))
(test-parse "select * from t where a match unique full (select * from t)"
	    '(select * (from t) 
		     (where (match-unique-full a (select * (from t))))))

;; overlaps
(test-parse "select * from t where a overlaps b"
	    '(select * (from t) (where (overlaps a b))))

;; distinct
(test-parse "select * from t where a is distinct from b"
	    '(select * (from t) (where (distinct-from? a b))))

;; member
(test-parse "select * from t where a not member of b"
	    '(select * (from t) (where (not-member-of a b))))
(test-parse "select * from t where a not member b"
	    '(select * (from t) (where (not-member a b))))
(test-parse "select * from t where a member of b"
	    '(select * (from t) (where (member-of a b))))
(test-parse "select * from t where a member b"
	    '(select * (from t) (where (member a b))))

;; submultiset
(test-parse "select * from t where a not submultiset of b"
	    '(select * (from t) (where (not-submultiset-of a b))))
(test-parse "select * from t where a not submultiset b"
	    '(select * (from t) (where (not-submultiset a b))))
(test-parse "select * from t where a submultiset of b"
	    '(select * (from t) (where (submultiset-of a b))))
(test-parse "select * from t where a submultiset b"
	    '(select * (from t) (where (submultiset a b))))

;; set
(test-parse "select * from t where a is a set"
	    '(select * (from t) (where (a-set? a))))
(test-parse "select * from t where a is not a set b"
	    '(select * (from t) (where (not-a-set? a))))

;; type
(test-parse "select * from t where a is of (ta, only tb)"
	    '(select * (from t) (where (of? a ta (only tb)))))
(test-parse "select * from t where a is not of (ta, only tb)"
	    '(select * (from t) (where (not-of? a ta (only tb)))))

;; exists
(test-parse "select * from t where exists (select * from f);"
	    '(select * (from t) (where (exists (select * (from f))))))

;; unique
(test-parse "select * from t where unique (select * from f);"
	    '(select * (from t) (where (unique (select * (from f))))))

;; union, except and intersect
(test-parse "select * from t union select * from w" 
	    '(union (select * (from t)) (select * (from w))))
(test-parse "select * from t union all select * from w" 
	    '(union-all (select * (from t)) (select * (from w))))
(test-parse "select * from t union distinct select * from w" 
	    '(union-distinct (select * (from t)) (select * (from w))))
(test-parse "select * from t except select * from w" 
	    '(except (select * (from t)) (select * (from w))))
(test-parse "select * from t except all select * from w" 
	    '(except-all (select * (from t)) (select * (from w))))
(test-parse "select * from t intersect select * from w" 
	    '(intersect (select * (from t)) (select * (from w))))
(test-parse "select * from t intersect all select * from w" 
	    '(intersect-all (select * (from t)) (select * (from w))))

(test-parse "select * from (select * from t union select * from w) as t" 
	    '(select * (from (as (union (select * (from t)) 
					(select * (from w))) t))))

(test-parse "select (select * from t union select * from w)"
	    '(select ((union (select * (from t)) (select * (from w))))))
(test-parse "select (select * from t union all select * from w)"
	    '(select ((union-all (select * (from t)) (select * (from w))))))
(test-parse "select (select * from t intersect select * from w)"
	    '(select ((intersect (select * (from t)) (select * (from w))))))
(test-parse "select (select * from t intersect all select * from w)"
	    '(select ((intersect-all (select * (from t)) (select * (from w))))))

(test-parse "select ((select a from t) union (select b from w))"
	    '(select ((union (select (a) (from t)) (select (b) (from w))))))
(test-parse "select ((select a from t) intersect (select b from w))"
	    '(select ((intersect (select (a) (from t)) (select (b) (from w))))))
(test-parse "select ((select 1) union (select 2) intersect (select 3))"
	    '(select ((union (select (1)) 
			     (intersect (select (2)) (select (3)))))))

;; delete from
(test-parse "delete from t" '(delete-from t))
(test-parse "delete from t where a = 1" '(delete-from t (where (= a 1))))
(test-parse "delete from t where current of a" 
	    '(delete-from t (where (current-of a))))
(test-parse "delete from t where current of global a" 
	    '(delete-from t (where (current-of (global a)))))
(test-parse "delete from t where current of local a" 
	    '(delete-from t (where (current-of (local a)))))

;; insert into
(test-parse "insert into t (a) values (1)" '(insert-into t (a) (values (1))))
(test-parse "insert into t (a) values (1),(2)"
	    '(insert-into t (a) (values (1) (2))))
(test-parse "insert into t (a) overriding user value values (1),(2)"
	    '(insert-into t (a) overriding-user-value (values (1) (2))))
(test-parse "insert into t (a) overriding system value values (1),(2)"
	    '(insert-into t (a) overriding-system-value (values (1) (2))))
(test-parse "insert into t (a) select b from f"
	    '(insert-into t (a) (select (b) (from f))))
(test-parse "insert into t default values"
	    '(insert-into t default-values))

;; update
(test-parse "update f set a=1,b=2 where i=0" 
	    '(update f (set! (= a 1) (= b 2)) (where (= i 0))))
(test-parse "update f set a[0]=1,b=2 where i=0" 
	    '(update f (set! (= (array-ref a 0) 1) (= b 2)) (where (= i 0))))
(test-parse "update f set a.m=1,b=2 where i=0" 
	    '(update f (set! (= (~ a m) 1) (= b 2)) (where (= i 0))))
(test-parse "update f set a=null where i=0" 
	    '(update f (set! (= a null)) (where (= i 0))))

;; create table
(test-parse "create table t (a int)" '(create-table t ((a int))))
(test-parse "create table t (a int primary key)"
	    '(create-table t ((a int (constraint primary-key)))))
(test-parse "create table t (a int primary key, b varchar)"
	    '(create-table t ((a int (constraint primary-key)) (b varchar))))
(test-parse "create table t (a int default nextval('seq') not null, b varchar)"
	    '(create-table t ((a int (default (nextval "seq")) 
				 (constraint not-null))
			      (b varchar))))
(test-parse "create table t (a int default nextval('seq') not null unique, b varchar)"
	    '(create-table t ((a int (default (nextval "seq")) 
				 (constraint not-null)
				 (constraint unique))
			      (b varchar))))
(test-parse "create table t (id int generated always as identity (start with 1 increment by 1 no cycle))"
	    '(create-table t ((id int (generated-always-as-identity 
				       (start-with 1)
				       (increment-by 1)
				       no-cycle)))))
(test-parse "create table t (id int, name varchar, primary key (id), unique(name))"
	    '(create-table t ((id int)
			      (name varchar)
			      (constraint (primary-key id))
			      (constraint (unique name)))))
(test-parse "create table t (id int, name varchar, primary key (id) initially immediate)"
	    '(create-table t ((id int)
			      (name varchar)
			      (constraint (primary-key id) initially-immediate))))
(test-parse "create table t (id int, primary key (id) initially immediate deferrable)"
	    '(create-table t ((id int)
			      (constraint (primary-key id)
					  initially-immediate deferrable))))
(test-parse "create table t (id int, constraint p_key primary key (id))"
	    '(create-table t ((id int)
			      (constraint p_key (primary-key id)))))
(test-parse "create table t (id int constraint p_key primary key)"
	    '(create-table t ((id int (constraint p_key primary-key)))))
(test-parse "create table t (id int constraint f_key references b(id))"
	    '(create-table t ((id int (constraint f_key (references b id))))))
(test-parse "create table t (id int references b(id))"
	    '(create-table t ((id int (constraint (references b id))))))

;; create sequence
(test-parse "create sequence s" '(create-sequence s))
(test-parse "create sequence p.s" '(create-sequence (~ p s)))
(test-parse "create sequence s as integer" 
	    '(create-sequence s (as integer)))
(test-parse "create sequence s as integer start with 1" 
	    '(create-sequence s (as integer) (start-with 1)))

;; alter table
(test-parse "alter table t add b int" '(alter-table t (add-column b int)))
(test-parse "alter table t add column b int"
	    '(alter-table t (add-column b int)))
(test-parse "alter table t add column b int primary key"
	    '(alter-table t (add-column b int (constraint primary-key))))
(test-parse "alter table t drop b" '(alter-table t (drop-column b)))
(test-parse "alter table t drop column b" '(alter-table t (drop-column b)))
(test-parse "alter table t drop column b cascade"
	    '(alter-table t (drop-column b cascade)))
(test-parse "alter table t drop column b restrict"
	    '(alter-table t (drop-column b restrict)))
(test-parse "alter table t add unique (b)"
	    '(alter-table t (add-constraint (unique b))))
(test-parse "alter table t add constraint u_b unique (b)"
	    '(alter-table t (add-constraint u_b (unique b))))
(test-parse "alter table t drop constraint u_b"
	    '(alter-table t (drop-constraint u_b)))
(test-parse "alter table t drop constraint u_b cascade"
	    '(alter-table t (drop-constraint u_b cascade)))
(test-parse "alter table t drop constraint u_b restrict"
	    '(alter-table t (drop-constraint u_b restrict)))
(test-parse "alter table t alter column b set default 1"
	    '(alter-table t (alter-column b (set-default 1))))
(test-parse "alter table t alter column b drop default"
	    '(alter-table t (alter-column b drop-default)))
(test-parse "alter table t alter column b add scope s"
	    '(alter-table t (alter-column b (add-scope s))))
(test-parse "alter table t alter column b drop scope"
	    '(alter-table t (alter-column b drop-scope)))
(test-parse "alter table t alter column b drop scope cascade"
	    '(alter-table t (alter-column b (drop-scope cascade))))
(test-parse "alter table t alter column b drop scope restrict"
	    '(alter-table t (alter-column b (drop-scope restrict))))

;; grant
(test-parse "grant select on t to role" '(grant (select) (on t) (to role)))
(test-parse "grant select,insert on t to role"
	    '(grant (select insert) (on t) (to role)))
(test-parse "grant select,insert on t to role,role2"
	    '(grant (select insert) (on t) (to role role2)))
(test-parse "grant select,insert on t to role with hierarchy option"
	    '(grant (select insert) (on t) (to role)
		    (with-hierarchy-option)))
(test-parse "grant select,insert on t to role with grant option"
	    '(grant (select insert) (on t) (to role)
		    (with-grant-option)))
(test-parse "grant select,insert on t to role granted by current_user"
	    '(grant (select insert) (on t) (to role)
		    (granted-by current_user)))

(test-parse "grant role1 to me" '(grant (role1) (to me)))
(test-parse "grant role1,role2 to me"
	    '(grant (role1 role2) (to me)))
(test-parse "grant role1,role2 to me with admin option"
	    '(grant (role1 role2) (to me) (with-admin-option)))
(test-parse "grant role1,role2 to me granted by current_user"
	    '(grant (role1 role2) (to me) (granted-by current_user)))


;; commit
(test-parse "commit" '(commit))
;; Should we make and-chain appended to commit?
(test-parse "commit and chain" '(commit and-chain))
(test-parse "commit and no chain" '(commit and-no-chain))
(test-parse "commit work" '(commit-work))
(test-parse "commit work and chain" '(commit-work and-chain))
(test-parse "commit work and no chain" '(commit-work and-no-chain))

(test-parse "rollback" '(rollback))
;; Should we make and-chain appended to rollback?
(test-parse "rollback and chain" '(rollback and-chain))
(test-parse "rollback and no chain" '(rollback and-no-chain))
(test-parse "rollback work" '(rollback-work))
(test-parse "rollback work and chain" '(rollback-work and-chain))
(test-parse "rollback work and no chain" '(rollback-work and-no-chain))
(test-parse "rollback to savepoint foo" '(rollback (to-savepoint foo)))

;; savepoint
(test-parse "savepoint foo" '(savepoint foo))
(test-parse "release savepoint foo" '(release-savepoint foo))

;; found a bug on local-or-schema-qualified-name
(test-parse "select * from s.t" '(select * (from (~ s t))))

;; simplifier
(define (test-simplify ssql expected)
  (test-equal ssql expected (simplify-ssql ssql)))

;; one identifier if possible
(test-simplify '(~ a b) 'a.b)
(test-simplify '(~ a b (! "c")) 'a.b.c)
(test-simplify '(~ a b (! "c.d")) '(~ a.b (! "c.d")))

;; unicode escape
(test-simplify '(~ a (unicode (! "\\0042"))) 'a.B)
(test-simplify '(~ a (unicode (! "$0042") uescape "$")) 'a.B)
(test-simplify '(~ a (unicode (! "$0042\\") uescape "$")) '|a.B\\\\|)

;; escape (for like or similar operator)
(test-simplify '(escape "%_" "%") "\\_")
;; error case
(test-simplify '(escape "%_") '(escape "%_"))
(test-simplify '(escape) '(escape))

;; serializer
;; if the serialized SQL is has the same token as expected SQL
;; then it can be parsed by parser. assume this is good enough
;; for tests.
(define (test-serializer ssql expected)
  (define (scan-all sql)
    (define scanner (make-sql-scanner (open-string-input-port sql)))
    (let loop ((r '()))
      (let-values (((token p l) (scanner)))
	(if token
	    (loop (cons token r))
	    r))))
  ;; historical reason...
  (parameterize ((*preserve-case* #f))
    (test-equal ssql (scan-all expected) (scan-all (ssql->sql ssql)))))

;; unicode identifier
(test-serializer '(unicode "s" uescape "$") " U&'s' UESCAPE '$'")
(test-serializer '(unicode "s") " U&'s'")
(test-serializer '(unicode (! "s") uescape "$") " U&\"s\" UESCAPE '$'")
(test-serializer '(unicode (! "s")) " U&\"s\"")
(test-serializer '(unicode (! "s\"")) " U&\"s\"\"\"")

;; from clause
(test-serializer '(from t) "FROM T")
(test-serializer '(from t a) "FROM T,A")
(test-serializer '(from (t (join a (using id)))) "FROM T JOIN A USING (ID)")
(test-serializer '(from (t (join a (using id))) b) "FROM T JOIN A USING (ID),B")

;; where clause
(test-serializer '(where (= a 1)) "WHERE a = 1")
(test-serializer '(where (and (= a 1) (= b 1))) "WHERE A = 1 AND B = 1")

;; select
(test-serializer '(select *) "SELECT *")
(test-serializer '(select * (from t)) "SELECT * FROM T")
(test-serializer '(select (a) (from t)) "SELECT A FROM T")

;; insert
(test-serializer '(insert-into t (values (1))) "INSERT INTO T VALUES (1)")
(test-serializer '(insert-into t (values (1) (2))) 
		 "INSERT INTO T VALUES (1),(2)")
(test-serializer '(insert-into t (a) (values (1))) 
		 "INSERT INTO T (A) VALUES (1)")

;; update
(test-serializer '(update t (set! (= a 1))) "UPDATE T SET A = 1")
(test-serializer '(update t (set! (= a 1) (= b 1))) "UPDATE T SET A = 1, B = 1")
(test-serializer '(update t (set! (= a 1) (= b 1)) (where (= c 1)))
		 "UPDATE T SET A = 1, B = 1 WHERE c = 1")

;; delete
(test-serializer '(delete-from t) "DELETE FROM T")
(test-serializer '(delete-from t (where (= a 1))) "DELETE FROM T WHERE A = 1")

;; TBD more

;; tests for operator priority
;; parse -> serialize -> parse must be the same result.
(define (test-validity sql)
  (let ((p (parse-sql (open-string-input-port sql))))
    (test-equal sql p (parse-sql (open-string-input-port (ssql->sql p))))))

(test-validity "select * from t where (1<>1 or 2=2) and 3=3 or 4<>4")
(test-validity "select 1+(1+2)*3+1")

;; others
(test-error "parse error" sql-parser-error?
	    (sql->ssql (open-string-input-port "select * from")))
(test-equal "non strict" 
	    ;; sql->ssql returns with *TOP*
	    '(*TOP* (select *))
	    (sql->ssql (open-string-input-port "select * from") :strict? #f))

(test-end)
