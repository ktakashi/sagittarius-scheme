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
      (test-equal expected v))))

(test-parser jmespath:identifier "foo" "foo")
(test-parser jmespath:identifier "fo\"o" "\"fo\\\"o\"")

(test-parser jmespath:not-expression '(not "foo") "!foo")
(test-parser jmespath:not-expression '(not "foo") "! foo")

(test-parser jmespath:paren-expression '("foo") "(foo)")
(test-parser jmespath:paren-expression '("foo") "( foo )")
(test-parser jmespath:paren-expression '((not "foo")) "(!foo)")
(test-parser jmespath:paren-expression '((not "foo")) "( ! foo)")

(test-parser jmespath:multi-select-list '(list) "[]")
(test-parser jmespath:multi-select-list '(list "foo") "[foo]")
(test-parser jmespath:multi-select-list '(list "foo" "bar") "[foo, bar]")
(test-parser jmespath:multi-select-list '(list "foo" (not "bar")) "[foo, !bar]")

(test-parser jmespath:multi-select-hash '(hash) "{}")
(test-parser jmespath:multi-select-hash '(hash ("key" . "value"))
	     "{ key: value }")
(test-parser jmespath:multi-select-hash
	     '(hash ("key" . "value") ("key2" . "value2"))
	     "{ key: value, key2: value2 }")

(test-parser jmespath:function-expression
	     '(function "abs" "foo") "abs(foo)")
(test-parser jmespath:function-expression
	     '(function "abs" "foo" (& "bar")) "abs(foo, &bar)")
(test-parser jmespath:function-expression
	     '(function "abs" "foo" (& "bar") "buz") "abs(foo, &bar, buz)")

(test-parser jmespath:top-expression '* "*")
(test-parser jmespath:top-expression '* " * ")
(test-parser jmespath:top-expression '@ "@")
(test-parser jmespath:top-expression '@ " @ ")
(test-parser jmespath:top-expression
	     '(function "abs" "foo" (& "bar") "buz") "abs(foo, &bar, buz)")

(test-parser (jmespath:expression) '(pipe "foo" "bar") "foo | bar")
(test-parser (jmespath:expression) '(pipe "foo" "bar") "foo|bar")
(test-parser (jmespath:expression) '(pipe "foo" "bar" "boo") "foo | bar | boo")
(test-parser (jmespath:expression) '(or "foo" "bar") "foo || bar")
(test-parser (jmespath:expression) '(or "foo" "bar") "foo||bar")
(test-parser (jmespath:expression) '(or "foo" "bar" "boo") "foo || bar || boo")
(test-parser (jmespath:expression) '(and "foo" "bar") "foo && bar")
(test-parser (jmespath:expression) '(and "foo" "bar") "foo&&bar")
(test-parser (jmespath:expression) '(and "foo" "bar" "boo") "foo && bar && boo")
(test-parser (jmespath:expression) '(-> "foo" "bar") "foo.bar")
(test-parser (jmespath:expression) '(-> "foo" "bar") "foo . bar")
(test-parser (jmespath:expression) '(-> "foo" * "bar") "foo.* . bar")

(test-parser (jmespath:expression) '(pipe "a" (-> "b" "c")) "a | b.c")
#; (test-parser (jmespath:expression)
	     '(-> (pipe "people" (index 0))) "people | [0] . name")

;; considering associsativity?
;; NOTE: I can't read from the specification and tutorial act like this
;;       so should be fine?
(test-parser (jmespath:expression) '(pipe "foo" (or "bar" "buz"))
	     "foo | bar || buz")

(test-end)
