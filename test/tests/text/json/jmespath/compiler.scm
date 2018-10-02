(import (rnrs)
	(text json)
	(text json jmespath compiler)
	(srfi :64))

(test-begin "JMESPath compiler")

(define (test-compiler expected ast input)
  (let ((e (jmespath:compile ast))
	(json (json-read (open-string-input-port input))))
    (test-assert (procedure? e))
    (test-equal (list ast expected) expected (e json))))

(define (test-compile-error ast input)
  (test-error jmespath-compile-error (jmespath:compile ast)))
(define (test-runtime-error ast input)
  (let ((e (jmespath:compile ast))
	(json (json-read (open-string-input-port input))))
    (test-assert (procedure? e))
    (test-error (list ast input) jmespath-runtime-error? (e json))))

(test-group "Identifiers"
  (test-compiler "value" "foo" "{\"foo\": \"value\"}")
  (test-compiler 'null "bar" "{\"foo\": \"value\"}")
  (test-compiler '(0 1 2) "foo" "{\"foo\": [0, 1, 2]}")
  (test-compiler "value" "with space" "{\"with space\": \"value\"}")
  (test-compiler "value" "special chars: !@#"
		 "{\"special chars: !@#\": \"value\"}")
  (test-compiler "value" "quote\"char" "{\"quote\\\"char\": \"value\"}")
  (test-compiler "value" "\x2713;" "{\"\\u2713\": \"value\"}"))

(test-group "Sub expressions"
  (test-compiler "value" '(ref "foo" "bar") 
		 "{\"foo\": {\"bar\": \"value\"}}")
  (test-compiler 'null '(ref "foo" "bar") 
		 "{\"foo\": {\"baz\": \"value\"}}")
  (test-compiler "value" '(ref "foo" "bar" "baz") 
		 "{\"foo\": {\"bar\": {\"baz\": \"value\"}}}")
  (test-compiler '#() '(ref "a") "{\"a\": {}}")
  (test-compiler 'null '(ref "a" "b") "{\"a\": {}}"))

(test-group "Index expressions"
  (test-compiler 0 '(index 0) "[0,1,2,3,4,5]")
  (test-compiler 5 '(index 5) "[0,1,2,3,4,5]")
  (test-compiler 'null '(index 6) "[0,1,2,3,4,5]")
  (test-compiler 5 '(index -1) "[0,1,2,3,4,5]")
  (test-compiler 0 '(index -6) "[0,1,2,3,4,5]")
  (test-compiler 'null '(index -7) "[0,1,2,3,4,5]")
  (test-compiler '(0 1 2 3 4 5) '(*) "[0,1,2,3,4,5]"))

(test-group "Slice expressions"
  (test-compiler '(0 1 2 3) '(slice 0 4 1) "[0,1,2,3]")
  (test-compiler '(0 1 2) '(slice 0 3 1) "[0,1,2,3]")
  (test-compiler '(0 1) '(slice #f 2 1) "[0,1,2,3]")
  (test-compiler '(0 2) '(slice #f #f 2) "[0,1,2,3]")
  (test-compiler '(3 2 1 0) '(slice #f #f -1) "[0,1,2,3]")
  (test-compiler '(2 3) '(slice -2 #f 1) "[0,1,2,3]"))

(test-group "Flatten operator"
  (test-compiler "first" '(index 0) "[\"first\", \"second\", \"third\"]")
  (test-compiler "third" '(index -1) "[\"first\", \"second\", \"third\"]")
  (test-compiler 'null '(index 100) "[\"first\", \"second\", \"third\"]")
  (test-compiler "first" '(ref "foo" (index 0))
		 "{\"foo\": [\"first\", \"second\", \"third\"]}")
  (test-compiler 'null '(ref "foo" (index 100))
		 "{\"foo\": [\"first\", \"second\", \"third\"]}")
  (test-compiler 0 '(ref "foo" (index 0) (index 0))
		 "{\"foo\": [[0,1],[1,2]]}")
  (test-compiler '(1 2 3 4 5) '(flatten) "[1,2,3,4,5]")
  (test-compiler '(1 2 3 4 5) '(flatten) "[1,2,3,[4,5]]")
  (test-compiler '(1 2 4 5) '(flatten) "[1,2,null,[4,5]]")
  (test-compiler '(1 2) '(flatten) "[1,null,[2,null]]")
  (test-compiler '(1 2 (null)) '(flatten) "[1,null,[2,[null]]]"))

(test-group "Or expressions"
  (test-compiler "foo-value" '(or "foo" "bar") "{\"foo\": \"foo-value\"}")
  (test-compiler "bar-value" '(or "foo" "bar") "{\"bar\": \"bar-value\"}")
  (test-compiler "foo-value" '(or "foo" "bar")
		 "{\"foo\": \"foo-value\", \"bar\": \"bar-value\"}")
  (test-compiler 'null '(or "foo" "bar") "{\"baz\": \"baz-value\"}")
  (test-compiler "baz-value" '(or "foo" "bar" "baz") "{\"baz\": \"baz-value\"}")
  (test-compiler "two" '(or "override" (ref "mylist" (index -1)))
		 "{\"mylist\": [\"one\", \"two\"]}")
  (test-compiler "yes" '(or "override" (ref "mylist" (index -1)))
		 "{\"mylist\": [\"one\", \"two\"], \"override\": \"yes\"}"))

(test-group "And expressions"
  (test-compiler #f '(and "True" "False") "{\"True\": true, \"False\": false}")
  (test-compiler '() '(and "Number" "EmptyList")
		 "{\"Number\": 5, \"EmptyList\": []}")
  (test-compiler '() '(and "EmptyList" "Number")
		 "{\"Number\": 5, \"EmptyList\": []}"))

(test-group "Not expressions"
  (test-compiler #f '(not "True") "{\"True\": true}")
  (test-compiler #t '(not "False") "{\"False\": false}")
  (test-compiler #f '(not "Number") "{\"Number\": 5}")
  (test-compiler #t '(not "EmptyList") "{\"EmptyList\": []}"))

(test-group "Multi select list"
  (test-compiler '("a" "b") '("foo" "bar")
		 "{\"foo\": \"a\", \"bar\": \"b\", \"baz\": \"c\"}")
  (test-compiler '("a" "b") '("foo" (ref "bar" (index 0)))
		 "{\"foo\": \"a\", \"bar\": [\"b\"], \"baz\": \"c\"}")
  (test-compiler '("a" "b")  '("foo" (ref "bar" "baz"))
		 "{\"foo\": \"a\", \"bar\": { \"baz\": \"b\"}}")
  (test-compiler '("a" null) '("foo" "bar")
		 "{\"foo\": \"a\", \"baz\": \"b\"}"))

(test-group "Multi select hash"
  (test-compiler '#(("foo" . "a") ("bar" . "b"))
		 '#(("foo" . "foo") ("bar" . "bar"))
		 "{\"foo\": \"a\", \"bar\": \"b\", \"baz\": \"c\"}")
  (test-compiler '#(("foo" . "a") ("firstbar" . "b"))
		 '#(("foo" . "foo") ("firstbar" . (ref "bar" (index 0))))
		 "{\"foo\": \"a\", \"bar\": [\"b\"], \"baz\": \"c\"}")
  (test-compiler '#(("foo" . "a") ("bar.baz" . "b"))
		 '#(("foo" . "foo") ("bar.baz" . (ref "bar" "baz")))
		 "{\"foo\": \"a\", \"bar\": { \"baz\": \"b\"}}")
  (test-compiler '#(("foo" . "a") ("baz" . null))
		 '#(("foo" . "foo") ("baz" . "baz"))
		 "{\"foo\": \"a\", \"bar\": \"b\"}"))

(test-group "Comparator expressions"
  (test-compiler #t '(< "one" "two") "{\"one\": 1, \"two\": 2}")
  (test-compiler #f '(< "two" "one") "{\"one\": 1, \"two\": 2}")
  (test-compiler #t '(<= "one" "two") "{\"one\": 1, \"two\": 2}")
  (test-compiler #t '(<= "one" "one") "{\"one\": 1, \"two\": 2}")
  (test-compiler #f '(<= "two" "one") "{\"one\": 1, \"two\": 2}")
  (test-compiler #t '(> "two" "one") "{\"one\": 1, \"two\": 2}")
  (test-compiler #f '(> "one" "two") "{\"one\": 1, \"two\": 2}")
  (test-compiler #t '(>= "two" "one") "{\"one\": 1, \"two\": 2}")
  (test-compiler #t '(>= "two" "two") "{\"one\": 1, \"two\": 2}")
  (test-compiler #f '(>= "one" "two") "{\"one\": 1, \"two\": 2}")
  (test-compiler #t '(= "one" "one") "{\"one\": 1, \"two\": 2}")
  (test-compiler #f '(= "one" "two") "{\"one\": 1, \"two\": 2}")
  (test-compiler #t '(!= "one" "two") "{\"one\": 1, \"two\": 2}")
  (test-compiler #f '(!= "one" "one") "{\"one\": 1, \"two\": 2}")
  (test-compiler 'null '(< "one" "bar") "{\"one\": 1, \"bar\": true}")
  (test-compiler 'null '(< "bar" "one") "{\"one\": 1, \"bar\": true}")

  (test-compiler #t '(= "foo" "bar")
    "{\"foo\": {\"a\": 1, \"b\": 2}, \"bar\": {\"b\": 2, \"a\": 1}}")
  (test-compiler #f '(= "foo" "bar")
    "{\"foo\": {\"a\": 1, \"b\": 2}, \"bar\": {\"b\": 2, \"c\": 1}}")
  (test-compiler #t '(= "foo" "bar") "{\"foo\": [1,2], \"bar\": [1,2]}")
  (test-compiler #f '(= "foo" "bar") "{\"foo\": [1,2], \"bar\": [2,1]}"))

(test-group "Wildcard expressions"
  (test-compiler '(1 2 3) '(ref (*) "foo")
		 "[{\"foo\": 1},{\"foo\": 2},{\"foo\": 3}]")
  (test-compiler '(1 2) '(ref (*) "foo")
		 "[{\"foo\": 1},{\"foo\": 2},{\"bar\": 3}]")
  (test-compiler '(1 2) '(ref * "foo")
    "{\"a\": {\"foo\": 1}, \"b\": {\"foo\": 2}, \"c\": {\"bar\": 1}}"))

(test-group "Current expressions"
  (test-compiler '(#(("foo" . 1))) '@ "[{\"foo\": 1}]"))

(test-group "Literal expressions"
  (test-compiler "foo" '(quote "foo") "{\"foo\": true}")
  (test-compiler 1 '(quote 1) "{\"foo\": true}")
  (test-compiler "foo" '(or "bar" '"foo") "{\"foo\": true}"))

(test-group "Filter expressions"
  (test-compiler '(#(("bar" . 10))) '(ref "foo" (filter (= "bar" '10)))
		 "{\"foo\": [{\"bar\": 1}, {\"bar\": 10}]}")
  (test-compiler '(#(("bar" . 10))) '(filter (= "bar" '10))
		 "[{\"bar\": 1}, {\"bar\": 10}]")
  (test-compiler '(#(("a" . 2) ("b" . 2))) '(ref "foo" (filter (= "a" "b")))
		 "{\"foo\": [{\"a\": 1, \"b\": 2}, {\"a\": 2, \"b\": 2}]}"))

#|
"people": [
  {"age": 20, "age_str": "20", "bool": true, "name": "a", "extra": "foo"},
  {"age": 40, "age_str": "40", "bool": false, "name": "b", "extra": "bar"},
  {"age": 30, "age_str": "30", "bool": true, "name": "c"},
  {"age": 50, "age_str": "50", "bool": false, "name": "d"},
  {"age": 10, "age_str": "10", "bool": true, "name": 3}
]
|#
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

(test-group "Functions expressions"
  (test-compiler 1 '(abs "foo") "{\"foo\": 1, \"bar\": 2}")
  (test-compiler 1 '(abs "foo") "{\"foo\": -1, \"bar\": 2}")
  ;; projection
  (test-compiler '(1 2 3) '(ref (flatten) (abs @)) "[1, -2, 3]")
  (test-runtime-error '(abs "foo") "{\"foo\": true, \"bar\": 2}")
  (test-runtime-error '(abs) "{\"foo\": true, \"bar\": 2}")
  (test-runtime-error '(abs "foo" "bar") "{\"foo\": true, \"bar\": 2}")

  (test-compiler 15 '(avg @) "[10, 15, 20]")
  (test-runtime-error '(avg @) "[10, false, 20]")
  (test-runtime-error '(avg @) "[false]")
  (test-runtime-error '(avg @) "false")
  (test-runtime-error '(avg @) "5")
  ;; projection applies the result list one by one so it has to be an error
  (test-runtime-error '(ref (flatten) (abs @) (avg @)) "[1,-2,3]")

  (test-compiler #t '(contains '"foobar" '"foo") "{}")
  (test-compiler #f '(contains '"foobar" '"not") "{}")
  (test-compiler #t '(contains '"foobar" '"bar") "{}")
  (test-runtime-error '(contains '#f "bar") "5")
  (test-compiler #f '(contains '"foobar" '123) "{}")
  (test-compiler #t '(contains @ '"a") "[\"a\", \"b\"]")
  (test-compiler #t '(contains @ '"a") "[\"a\"]")
  (test-compiler #f '(contains @ '"b") "[\"a\"]")
  (test-compiler #t '(contains @ '"foo") "[\"foo\", \"bar\"]")
  (test-compiler #f '(contains @ '"b") "[\"foo\", \"bar\"]")
  (test-runtime-error '(contains '#() "bar") "5")
  (test-runtime-error '(contains '1 "bar") "5")
  (test-runtime-error '(contains 'null "bar") "5")

  (test-compiler 2 '(ceil '1.001) "{}")
  (test-compiler 2 '(ceil '1.9) "{}")
  (test-compiler 1 '(ceil '1) "{}")
  (test-runtime-error '(ceil '"abc") "{}")

  (test-compiler #t '(ends_with @ '"baz") "\"foobarbaz\"")
  (test-compiler #f '(ends_with @ '"foo") "\"foobarbaz\"")
  (test-compiler #t '(ends_with @ '"z") "\"foobarbaz\"")
  (test-runtime-error '(ends_with 'null "bar") "{}")
  (test-runtime-error '(ends_with "bar" 'null) "{}")

  (test-compiler 1 '(floor '1.001) "{}")
  (test-compiler 1 '(floor '1.9) "{}")
  (test-compiler 1 '(floor '1) "{}")
  (test-compiler '(1 2 3) '(ref (flatten) (abs @) (floor @)) "[1,-2,3]")
  (test-runtime-error '(floor '"abc") "{}")

  (test-compiler "a, b" '(join '", " @) "[\"a\", \"b\"]")
  (test-compiler "ab" '(join '"" @) "[\"a\", \"b\"]")
  (test-runtime-error '(join '", " @) "[\"a\", false, \"b\"]")
  (test-runtime-error '(join '", " @) "[false]")

  (test-compiler '("foo" "bar") '(keys @) 
		 "{\"foo\": \"baz\", \"bar\": \"bam\"}")
  (test-compiler '() '(keys @) "{}")
  (test-runtime-error '(keys @) "false")
  (test-runtime-error '(keys @) "[\"b\", \"a\", \"c\"]")

  (test-compiler 3 '(length '"abc") "{}")
  (test-compiler 7 '(length @) "\"current\"")
  (test-runtime-error '(length "not_there") "\"current\"")
  (test-compiler 3 '(length @) "[\"a\", \"b\", \"c\"]")
  (test-compiler 0 '(length @) "[]")
  (test-compiler 0 '(length @) "{}")
  (test-compiler 2 '(length @) "{\"foo\": \"baz\", \"bar\": \"bam\"}")

  (test-compiler '("a" "b" null null "f") '(map (& "foo") "array")
		 "{\"array\": [{\"foo\": \"a\"}, {\"foo\": \"b\"}, {}, [], {\"foo\": \"f\"}]}")
  (test-compiler '((1 2 3 4) (5 6 7 8 9)) '(map (& (flatten)) @)
		 "[[1, 2, 3, [4]], [5, 6, 7, [8, 9]]]")

  (test-compiler 15 '(max @) "[10, 15]")
  (test-compiler "b" '(max @) "[\"a\", \"b\"]")
  (test-runtime-error '(max @) "[\"a\", 2, \"b\"]")
  (test-runtime-error '(max @) "[10, false, 15]")

  (test-compiler '#(("age" . 50) ("age_str" . "50")
		    ("bool" . #f) ("name" . "d"))
		 '(max_by "people" (& "age")) people-json-string)
  (test-compiler '50 '(ref (max_by "people" (& "age")) "age")
		 people-json-string)
  (test-compiler '#(("age" . 50) ("age_str" . "50")
		    ("bool" . #f) ("name" . "d"))
		 '(max_by "people" (& (to_number "age_str")))
		 people-json-string)
  ;; difference between spec and compliance test
  (test-compiler '#(("age" . 50) ("age_str" . "50")
		    ("bool" . #f) ("name" . "d"))
		 '(max_by "people" (& "age_str")) people-json-string)
  (test-runtime-error '(max_by "people" "age") people-json-string)
  
  (test-compiler '#(("a" . "b") ("c" . "d"))
		 '(merge '#(("a" . "b")) '#(("c" . "d"))) "{}")
  (test-compiler '#(("a" . "override"))
		 '(merge '#(("a" . "b")) '#(("a" . "override"))) "{}")
  (test-compiler '#(("a" . "x") ("b" . "override") ("c" . "z"))
		 '(merge '#(("a" . "x") ("b" . "y"))
			 '#(("b" . "override") ("c" . "z"))) "{}")
  (test-compiler '#() '(merge) "{}")
  (test-runtime-error '(merge '1) "{}")

  (test-compiler 10 '(min @) "[10, 15]")
  (test-compiler "a" '(min @) "[\"a\", \"b\"]")
  (test-runtime-error '(min @) "[\"a\", 2, \"b\"]")
  (test-runtime-error '(min @) "[10, false, 15]")

  (test-compiler '#(("age" . 10) ("age_str" . "10")
		    ("bool" . #t) ("name" . 3))
		 '(min_by "people" (& "age")) people-json-string)
  (test-compiler '10 '(ref (min_by "people" (& "age")) "age")
		 people-json-string)
  (test-compiler '#(("age" . 10) ("age_str" . "10")
		    ("bool" . #t) ("name" . 3))
		 '(min_by "people" (& (to_number "age_str")))
		 people-json-string)
  ;; difference between spec and compliance test
  (test-compiler '#(("age" . 10) ("age_str" . "10")
		    ("bool" . #t) ("name" . 3))
		 '(min_by "people" (& "age_str")) people-json-string)
  (test-runtime-error '(min_by "people" "age") people-json-string)

  
  (test-compiler '() '(not_null "not_exist" "a" "b" "c" "d")
		 "{\"a\": null, \"b\": null, \"c\": [], \"d\": \"foo\"}")
  (test-compiler "foo" '(not_null "a" "b" 'null "d" "c")
		 "{\"a\": null, \"b\": null, \"c\": [], \"d\": \"foo\"}")
  (test-compiler 'null '(not_null "a" "b")
		 "{\"a\": null, \"b\": null, \"c\": [], \"d\": \"foo\"}")
  (test-runtime-error '(not_null) "{}")

  (test-compiler '(4 3 2 1 0) '(reverse @) "[0,1,2,3,4]")
  (test-compiler '() '(reverse @) "[]")
  (test-compiler '(3 2 1 "c" "b" "a") '(reverse @) "[\"a\",\"b\",\"c\",1,2,3]")
  (test-compiler "dcba" '(reverse @) "\"abcd\"")
  (test-runtime-error '(reverse @) "true")
  (test-runtime-error '(reverse @) "{}")
  (test-runtime-error '(reverse @) "1")

  (test-compiler '(1 2 3) '(sort @) "[2,3,1]")
  (test-compiler '("a" "b" "c") '(sort @) "[\"c\",\"a\",\"b\"]")
  ;; The specification example shows wrong or specification is wrong.
  (test-runtime-error '(sort @) "[1, false, []]")

  (test-compiler '(10 20 30 40 50)
		 '(ref (sort_by "people" (& "age")) (flatten) "age")
		 people-json-string)
  (test-compiler '(10 20 30 40 50)
		 '(ref (flatten (sort_by "people" (& "age"))) "age")
		 people-json-string)
  (test-compiler '#(("age" . 10) ("age_str" . "10")
		    ("bool" . #t) ("name" . 3))
		 '(ref (sort_by "people" (& "age")) (index 0))
		 people-json-string)
  (test-compiler '#(("age" . 10) ("age_str" . "10")
		    ("bool" . #t) ("name" . 3))
		 '(ref (sort_by "people" (& "age_str")) (index 0))
		 people-json-string)
  (test-runtime-error '(sort_by "people" "age") people-json-string)
  
  (test-compiler #t '(starts_with @ '"foo") "\"foobarbaz\"")
  (test-compiler #f '(starts_with @ '"baz") "\"foobarbaz\"")
  (test-compiler #t '(starts_with @ '"f") "\"foobarbaz\"")
  (test-runtime-error '(starts_with 'null "bar") "{}")
  (test-runtime-error '(starts_with "bar" 'null) "{}")

  (test-compiler 25 '(sum @) "[10, 15]")
  (test-compiler 0 '(sum @) "[]")
  (test-compiler 30 '(sum (ref (flatten) (to_number @))) "[10, false, 20]")
  (test-runtime-error '(sum @) "[10, false, 20]")

  (test-compiler '(1 2) '(to_array '(1 2)) "{}")
  (test-compiler '("string") '(to_array '"string") "{}")
  (test-compiler '(0) '(to_array '0) "{}")
  (test-compiler '(#t) '(to_array '#t) "{}")
  (test-compiler '(#(("foo" . "bar"))) '(to_array '#(("foo" . "bar"))) "{}")

  (test-compiler "2" '(to_string '2) "{}")
  (test-compiler "1" '(to_string '"1") "{}")
  (test-compiler "[1,2]" '(to_string '(1 2)) "{}")
  (test-compiler "{\"foo\":1,\"bar\":2}"
		 '(to_string '#(("foo" . 1) ("bar" . 2))) "{}")

  (test-compiler "string" '(type @) "\"foo\"")
  (test-compiler "boolean" '(type @) "true")
  (test-compiler "boolean" '(type @) "false")
  (test-compiler "null" '(type @) "null")
  (test-compiler "number" '(type @) "123")
  (test-compiler "number" '(type @) "123.05")
  (test-compiler "array" '(type @) "[\"abc\"]")
  (test-compiler "object" '(type @) "{\"abc\": \"123\"}")
  
  (test-compiler 2 '(to_number '"2") "{}")
  (test-compiler 1 '(to_number '1) "{}")
  (test-compiler 'null '(to_number '#t) "{}")
  (test-compiler 'null '(to_number '()) "{}")
  (test-compiler 'null '(to_number '#()) "{}")
  (test-compiler 'null '(to_number '"abc") "{}")

  (test-compiler '("baz" "bam") '(values @)
		 "{\"foo\": \"baz\", \"bar\": \"bam\"}")
  (test-runtime-error '(values @) "[\"a\", \"b\"]")
  (test-runtime-error '(values @) "false")
  
  (test-compiler 'null '(parent) "{\"foo\": true}")
  (test-compiler '#(("foo" . #(("bar" . #t))))
		 '(ref "foo" (parent))
		 "{\"foo\": { \"bar\": true} }")
  (test-compiler'#(("baz" . "value"))
		'(ref "foo" "bar" "baz" (parent))
		"{\"foo\": {\"bar\": {\"baz\": \"value\"}}}"))

(test-group "Pipe expression"
  (test-compiler "baz" '(pipe "foo" "bar") 
		 "{\"foo\": {\"bar\": \"baz\"}}")
  (test-compiler '("first1" "second1")
		 '(pipe (ref "foo" (*) "bar") (index 0))
		 "{\"foo\": [{\"bar\": [\"first1\", \"second1\"]}, {\"bar\": [\"first2\", \"second2\"]}]}")
  (test-compiler '0 '(pipe "foo" (index 0)) "{\"foo\": [0,1,2]}"))

(test-end)
