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
		 "{\"foo\": {\"bar\": {\"baz\": \"value\"}}}"))

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
