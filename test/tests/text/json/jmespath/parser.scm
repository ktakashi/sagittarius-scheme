(import (rnrs)
	(text json jmespath parser)
	(sagittarius generators)
	(peg)
	(srfi :127)
	(srfi :64))

(test-begin "JMESPath parser")

(define (test-parser parser expected string)
  (let ((lseq (generator->lseq (string->generator string))))
    (let-values (((s v nl) (parser lseq)))
      (test-assert (parse-success? s))
      (test-assert (null? nl))
      (test-equal (list string v) expected v))))

(test-parser jmespath:identifier "foo" "foo")
(test-parser jmespath:identifier "fo\"o" "\"fo\\\"o\"")

(test-parser jmespath:not-expression '(not "foo") "!foo")
(test-parser jmespath:not-expression '(not "foo") "! foo")

(test-parser jmespath:paren-expression '"foo" "(foo)")
(test-parser jmespath:paren-expression '"foo" "( foo )")
(test-parser jmespath:paren-expression '(not "foo") "(!foo)")
(test-parser jmespath:paren-expression '(not "foo") "( ! foo)")

(test-parser jmespath:multi-select-list '("foo") "[foo]")
(test-parser jmespath:multi-select-list '("foo" "bar") "[foo, bar]")
(test-parser jmespath:multi-select-list '("foo" (not "bar")) "[foo, !bar]")

(test-parser jmespath:multi-select-hash '#(("key" . "value"))
	     "{ key: value }")
(test-parser jmespath:multi-select-hash
	     '#(("key" . "value") ("key2" . "value2"))
	     "{ key: value, key2: value2 }")

(test-parser jmespath:function-expression
	     '(abs "foo") "abs(foo)")
(test-parser jmespath:function-expression
	     '(abs "foo" (& "bar")) "abs(foo, &bar)")
(test-parser jmespath:function-expression
	     '(abs "foo" (& "bar") "buz") "abs(foo, &bar, buz)")

(test-parser jmespath:bracket-specifier '(index 0) "[0]")
(test-parser jmespath:bracket-specifier '(slice 0 2 1) "[0:2]")
(test-parser jmespath:bracket-specifier '(slice 0 #f 1) "[0:]")
(test-parser jmespath:bracket-specifier '(slice #f #f 1) "[::]")
(test-parser jmespath:bracket-specifier '(index *) "[*]")
(test-parser jmespath:bracket-specifier 
	     '(filter (= "state" '"running")) "[?state=='running']")
(test-parser jmespath:bracket-specifier '(flatten) "[]")

(test-parser jmespath:literal '(quote #(("key" . "value")))
	     "`{\"key\": \"value\"}`")

(test-parser jmespath:raw-string '(quote "'\\a") "'\\'\\\\a'")

(test-parser jmespath:expression '* "*")
(test-parser jmespath:expression '* " * ")
(test-parser jmespath:expression '@ "@")
(test-parser jmespath:expression '@ " @ ")
(test-parser jmespath:expression
	     '(abs "foo" (& "bar") "buz") "abs(foo, &bar, buz)")

(test-parser jmespath:expression '(flatten) "[]")
(test-parser jmespath:expression '(pipe "foo" "bar") "foo | bar")
(test-parser jmespath:expression '(pipe "foo" "bar") "foo|bar")
(test-parser jmespath:expression '(pipe "foo" "bar" "boo") "foo | bar | boo")
(test-parser jmespath:expression '(or "foo" "bar") "foo || bar")
(test-parser jmespath:expression '(or "foo" "bar") "foo||bar")
(test-parser jmespath:expression '(or "foo" "bar" "boo") "foo || bar || boo")
(test-parser jmespath:expression '(and "foo" "bar") "foo && bar")
(test-parser jmespath:expression '(and "foo" "bar") "foo&&bar")
(test-parser jmespath:expression '(and "foo" "bar" "boo") "foo && bar && boo")
(test-parser jmespath:expression '(ref "foo" "bar") "foo.bar")
(test-parser jmespath:expression '(ref "foo" "bar") "foo . bar")
(test-parser jmespath:expression '(ref "foo" * "bar") "foo.* . bar")

(test-parser jmespath:expression '(pipe "a" (ref "b" "c")) "a | b.c")
#; (test-parser (jmespath:expression)
	     '(-> (pipe "people" (index 0))) "people | [0] . name")

(test-parser jmespath:expression '(ref "a" (index 0)) "a[0]")
(test-parser jmespath:expression '(ref "a" "b" (index 0)) "a.b[0]")
(test-parser jmespath:expression '(ref "a" "b" (index 0) "c" (index 0)) "a.b[0].c[0]")
(test-parser jmespath:expression '(ref "a" "b" (index 0) "c" (index *)) "a.b[0].c[*]")

(test-parser jmespath:expression '(< "a" "b")  "a < b")
(test-parser jmespath:expression '(<= "a" "b") "a <= b")
(test-parser jmespath:expression '(= "a" "b") "a == b")
(test-parser jmespath:expression '(>= "a" "b") "a >= b")
(test-parser jmespath:expression '(> "a" "b")  "a > b")
(test-parser jmespath:expression '(!= "a" "b") "a != b")

;; considering associsativity?
;; NOTE: I can't read from the specification and tutorial act like this
;;       so should be fine?
(test-parser jmespath:expression '(pipe "foo" (or "bar" "buz"))
	     "foo | bar || buz")

(test-parser jmespath:expression '(or (pipe "foo""bar" ) "buz")
	     "(foo | bar) || buz")

(test-end)
