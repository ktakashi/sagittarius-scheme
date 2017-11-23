(import (rnrs)
	(rfc json-pointer)
	(text json)
	(srfi :64))


(test-begin "JSON Pointer")

(let ()
  (define json (json-read
		(open-string-input-port "{\"foo\":[ {\"bar\":\"buz\"}]}")))
  (define p (json-pointer "/foo"))
  
  
  (test-equal 0 ((json-pointer "/foo/bar")
		 (json-read (open-string-input-port "{\"foo\":{\"bar\": 0}}"))))
  
  (test-equal "buz" ((json-pointer "/foo/0/bar") json))
  (test-equal '#(("foo" #(("bar" . "buz")))) ((json-pointer "") json))
  (test-equal "buz" ((json-pointer "/0/bar" p) json))
  
  (test-equal '("foo" "") (parse-json-pointer "/foo/")))

(test-end)
