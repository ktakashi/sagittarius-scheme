(import (rnrs)
	(text json)
	(text json jmespath)
	(srfi :64))

(test-begin "JMESPath")

;; non standard builtin 
(test-equal '(#(("a" . #(("foo" . 1))) ("b" . #(("foo" . 1))))
	      #(("a" . #(("foo" . 1))) ("b" . #(("foo" . 1)))))
	    ((jmespath "*.foo.parent(@).parent(@)")
	     '#(("a" . #(("foo" . 1)))
		("b" . #(("foo" . 1))))))

(test-equal '()
	    ((jmespath "*.foo.parent(@).parent(@).parent(@)")
	     '#(("a" . #(("foo" . 1)))
		("b" . #(("foo" . 1))))))

(test-equal 'null
	    ((jmespath "a.foo.parent(@).parent(@).parent(@)")
	     '#(("a" . #(("foo" . 1)))
		("b" . #(("foo" . 1))))))

(test-equal '(#(("a" . #(("foo" . 1)))))
	    ((jmespath "unique(@)")
	     '(#(("a" . #(("foo" . 1))))
	       #(("a" . #(("foo" . 1)))))))

(test-equal '(1 2 3) ((jmespath "unique(@)") '(1 2 1 2 3)))

(test-equal '(#(("a" . #(("foo" . 1))) ("b" . #(("foo" . 1)))))
	    ((jmespath "*.foo.parent(@).parent(@) | unique(@)")
	     '#(("a" . #(("foo" . 1)))
		("b" . #(("foo" . 1))))))

(test-equal '(1 3 5) ((jmespath "remove(@, &is_even(@))") '(1 2 3 4 5)))
(test-equal '(2 4) ((jmespath "remove(@, &is_odd(@))") '(1 2 3 4 5)))
(test-equal '#(("key" . 1)) ((jmespath "remove(@, &is_even(@))")
			     '#(("key" . 1) ("key2" . 2))))
(test-error jmespath-runtime-error? ((jmespath "remove(@, &is_odd(@))") "s"))
(test-error jmespath-runtime-error? ((jmespath "remove(@, &is_odd(@))") 1))

(test-equal '#(("key" . 1)) ((jmespath "remove_entry(@, `[\"key2\"]`)")
			     '#(("key" . 1) ("key2" . 2))))
(test-equal '#() ((jmespath "remove_entry(@, `[\"key\", \"key2\"]`)")
		  '#(("key" . 1) ("key2" . 2))))
(test-equal '#(("key" . 1))
	    ((jmespath "remove_entry(@, &contains(`[\"key2\"]`, @))")
	     '#(("key" . 1) ("key2" . 2))))
(test-error jmespath-runtime-error? ((jmespath "remove_entry(@, `[1,2,3]`)")
				     '(1 2 3)))
(test-error jmespath-runtime-error? ((jmespath "remove_entry(@, 'key', 'key2')")
				     '(1 2 3)))
(test-error jmespath-runtime-error? ((jmespath "remove_entry(@, `1`)") '#()))

(test-equal '(1 2 3) ((jmespath "array_of(`1`, `2`, `3`)") '#()))

(define (test-sexp-jmespath expected path input)
  (let ((json (json-read (open-string-input-port input))))
    (test-equal path expected ((jmespath path) json))))

;; Sexp jmespath
(test-sexp-jmespath '(#(("bar" . 10))) '(~ "foo" (? (= "bar" '10)))
		    "{\"foo\": [{\"bar\": 1}, {\"bar\": 10}]}")
(test-sexp-jmespath '(#(("bar" . 10))) '(? (= "bar" '10))
		    "[{\"bar\": 1}, {\"bar\": 10}]")
(test-sexp-jmespath "baz" '(-> "foo" "bar") "{\"foo\": {\"bar\": \"baz\"}}")
(test-sexp-jmespath '("first1" "second1")
		    '(-> (~ "foo" (*) "bar") (index 0))
		    "{\"foo\": [{\"bar\": [\"first1\", \"second1\"]}, {\"bar\": [\"first2\", \"second2\"]}]}")

(test-sexp-jmespath #f '(! "True") "{\"True\": true}")
(test-sexp-jmespath #t '(! "False") "{\"False\": false}")
(test-sexp-jmespath #f '(! "Number") "{\"Number\": 5}")
(test-sexp-jmespath #t '(! "EmptyList") "{\"EmptyList\": []}")

(test-sexp-jmespath #t '(ends-with @ '"baz") "\"foobarbaz\"")
(test-sexp-jmespath #f '(ends-with @ '"foo") "\"foobarbaz\"")
(test-sexp-jmespath #t '(ends-with @ '"z") "\"foobarbaz\"")

(define people-json
  '#(
     ("people"
      #(("age" . 20) ("age_str" . "20") ("bool" . #t) ("name" . "a")
	("extra" . "foo"))
      #(("age" . 40) ("age_str" . "40") ("bool" . #f) ("name" . "a")
	("extra" . "bar"))
      #(("age" . 30) ("age_str" . "30") ("bool" . #t) ("name" . "c"))
      #(("age" . 50) ("age_str" . "50") ("bool" . #f) ("name" . "d"))
      #(("age" . 10) ("age_str" . "10") ("bool" . #t) ("name" . 3))
      )
     ))
(define people-json-string
  (let-values (((out extract) (open-string-output-port)))
    (json-write people-json out)
    (extract)))

(test-sexp-jmespath '#(("age" . 50) ("age_str" . "50")
		       ("bool" . #f) ("name" . "d"))
		    '(max-by "people" (& "age")) people-json-string)
(test-sexp-jmespath '50 '(ref (max-by "people" (& "age")) "age")
		    people-json-string)
(test-sexp-jmespath '#(("age" . 50) ("age_str" . "50")
		       ("bool" . #f) ("name" . "d"))
		    '(max-by "people" (& (->number "age_str")))
		    people-json-string)
;; difference between spec and compliance test
(test-sexp-jmespath '#(("age" . 50) ("age_str" . "50")
		       ("bool" . #f) ("name" . "d"))
		    '(max-by "people" (& "age_str")) people-json-string)

(test-sexp-jmespath '#(("age" . 10) ("age_str" . "10")
		       ("bool" . #t) ("name" . 3))
		    '(min-by "people" (& "age")) people-json-string)
(test-sexp-jmespath '10 '(ref (min-by "people" (& "age")) "age")
		    people-json-string)
(test-sexp-jmespath '#(("age" . 10) ("age_str" . "10")
		       ("bool" . #t) ("name" . 3))
		    '(min-by "people" (& (->number "age_str")))
		    people-json-string)
;; difference between spec and compliance test
(test-sexp-jmespath '#(("age" . 10) ("age_str" . "10")
		       ("bool" . #t) ("name" . 3))
		    '(min-by "people" (& "age_str")) people-json-string)

(test-sexp-jmespath '() '(not-null "not_exist" "a" "b" "c" "d")
		    "{\"a\": null, \"b\": null, \"c\": [], \"d\": \"foo\"}")
(test-sexp-jmespath "foo" '(not-null "a" "b" 'null "d" "c")
		    "{\"a\": null, \"b\": null, \"c\": [], \"d\": \"foo\"}")
(test-sexp-jmespath 'null '(not-null "a" "b")
		    "{\"a\": null, \"b\": null, \"c\": [], \"d\": \"foo\"}")

(test-sexp-jmespath '(10 20 30 40 50)
		    '(~ (sort-by "people" (& "age")) (flatten) "age")
		    people-json-string)
(test-sexp-jmespath '(10 20 30 40 50)
		    '(~ (flatten (sort-by "people" (& "age"))) "age")
		    people-json-string)
(test-sexp-jmespath '#(("age" . 10) ("age_str" . "10")
		       ("bool" . #t) ("name" . 3))
		    '(~ (sort-by "people" (& "age")) (index 0))
		    people-json-string)
(test-sexp-jmespath '#(("age" . 10) ("age_str" . "10")
		       ("bool" . #t) ("name" . 3))
		    '(~ (sort-by "people" (& "age_str")) (index 0))
		    people-json-string)

(test-sexp-jmespath #t '(starts-with @ '"foo") "\"foobarbaz\"")
(test-sexp-jmespath #f '(starts-with @ '"baz") "\"foobarbaz\"")
(test-sexp-jmespath #t '(starts-with @ '"f") "\"foobarbaz\"")

(test-sexp-jmespath '(1 2) '(->array '(1 2)) "{}")
(test-sexp-jmespath '("string") '(->array '"string") "{}")
(test-sexp-jmespath '(0) '(->array '0) "{}")
(test-sexp-jmespath '(#t) '(->array '#t) "{}")
(test-sexp-jmespath '(#(("foo" . "bar"))) '(->array '#(("foo" . "bar"))) "{}")

(test-sexp-jmespath "2" '(->string '2) "{}")
(test-sexp-jmespath "1" '(->string '"1") "{}")
(test-sexp-jmespath "[1,2]" '(->string '(1 2)) "{}")
(test-sexp-jmespath "{\"foo\":1,\"bar\":2}"
		    '(->string '#(("foo" . 1) ("bar" . 2))) "{}")

(test-sexp-jmespath 2 '(->number '"2") "{}")
(test-sexp-jmespath 1 '(->number '1) "{}")
(test-sexp-jmespath 'null '(->number '#t) "{}")
(test-sexp-jmespath 'null '(->number '()) "{}")
(test-sexp-jmespath 'null '(->number '#()) "{}")
(test-sexp-jmespath 'null '(->number '"abc") "{}")

(test-end)
