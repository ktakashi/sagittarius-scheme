(import (rnrs)
	(text json)
	(text json jmespath compiler)
	(srfi :64))

(test-begin "JMESPath compiler")

(define (test-compiler expected ast input)
  (let ((e (jmespath:compile ast))
	(json (json-read (open-string-input-port input))))
    (test-assert (procedure? e))
    (test-equal expected (e json))))

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
  (test-compiler "value" '(-> "foo" "bar") 
		 "{\"foo\": {\"bar\": \"value\"}}")
  (test-compiler 'null '(-> "foo" "bar") 
		 "{\"foo\": {\"baz\": \"value\"}}")
  (test-compiler "value" '(-> "foo" "bar" "baz") 
		 "{\"foo\": {\"bar\": {\"baz\": \"value\"}}}")
  (test-compiler '#() '(-> "a") "{\"a\": {}}")
  (test-compiler 'null '(-> "a" "b") "{\"a\": {}}"))

(test-group "Index expressions"
  (test-compiler 0 '(index 0) "[0,1,2,3,4,5]")
  (test-compiler 5 '(index 5) "[0,1,2,3,4,5]")
  (test-compiler 'null '(index 6) "[0,1,2,3,4,5]")
  (test-compiler 5 '(index -1) "[0,1,2,3,4,5]")
  (test-compiler 0 '(index -6) "[0,1,2,3,4,5]")
  (test-compiler 'null '(index -7) "[0,1,2,3,4,5]")
  (test-compiler '(0 1 2 3 4 5) '(index *) "[0,1,2,3,4,5]"))

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
  (test-compiler "first" '(-> "foo" (index 0))
		 "{\"foo\": [\"first\", \"second\", \"third\"]}")
  (test-compiler 'null '(-> "foo" (index 100))
		 "{\"foo\": [\"first\", \"second\", \"third\"]}")
  (test-compiler 0 '(-> "foo" (index 0) (index 0))
		 "{\"foo\": [[0,1],[1,2]]}"))

(test-group "Or expressions"
  (test-compiler "foo-value" '(or "foo" "bar") "{\"foo\": \"foo-value\"}")
  (test-compiler "bar-value" '(or "foo" "bar") "{\"bar\": \"bar-value\"}")
  (test-compiler "foo-value" '(or "foo" "bar")
		 "{\"foo\": \"foo-value\", \"bar\": \"bar-value\"}")
  (test-compiler 'null '(or "foo" "bar") "{\"baz\": \"baz-value\"}")
  (test-compiler "baz-value" '(or "foo" "bar" "baz") "{\"baz\": \"baz-value\"}")
  (test-compiler "two" '(or "override" (-> "mylist" (index -1)))
		 "{\"mylist\": [\"one\", \"two\"]}")
  (test-compiler "yes" '(or "override" (-> "mylist" (index -1)))
		 "{\"mylist\": [\"one\", \"two\"], \"override\": \"yes\"}"))

(test-group "And expressions"
  (test-compiler #f '(and "True" "False") "{\"True\": true, \"False\": false}")
  (test-compiler '() '(and "Number" "EmptyList") "{\"Number\": 5, \"EmptyList\": []}")
  (test-compiler '() '(and "EmptyList" "Number") "{\"Number\": 5, \"EmptyList\": []}"))

(test-group "Not expressions"
  (test-compiler #f '(not "True") "{\"True\": true}")
  (test-compiler #t '(not "False") "{\"False\": false}")
  (test-compiler #f '(not "Number") "{\"Number\": 5}")
  (test-compiler #t '(not "EmptyList") "{\"EmptyList\": []}"))

(test-group "Functions expressions"
  (test-compiler 'null '(function "parent") "{\"foo\": true}")
  (test-compiler '#(("foo" . #(("bar" . #t))))
		 '(-> "foo" (function "parent"))
		 "{\"foo\": { \"bar\": true} }")
  (test-compiler'#(("baz" . "value"))
		'(-> "foo" "bar" "baz" (function "parent"))
		 "{\"foo\": {\"bar\": {\"baz\": \"value\"}}}"))


(test-end)
