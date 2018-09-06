(import (rnrs)
	(rfc uri-template)
	(srfi :64))

(test-begin "RFC URI Template")

(define (uri-parse template)
  (parse-uri-template (open-string-input-port template)))
(define (test-parse expected template)
  (test-equal expected (uri-parse template)))

(test-parse '("http://www.example.com/foo" (#\? "query" "number"))
	    "http://www.example.com/foo{?query,number}")
(test-parse '("http://www.example.com/foo" (("query" 6) ("number" *)))
	    "http://www.example.com/foo{query:6,number*}")
(test-parse '("http://www.example.com/foo" (#\+ "query"))
	    "http://www.example.com/foo{+query}")
;; reserved but can be parsed
(test-parse '("http://www.example.com/foo" (#\@ "query"))
	    "http://www.example.com/foo{@query}")

(test-error uri-template-parse-error? (uri-parse "http://www.<>example.com"))
(guard (e ((uri-template-parse-error? e)
	   (test-equal "http://www.<>example.com"
		       (uri-template-parsing-template e)))
	  (else (test-assert "Not an expected condition" #f)))
  (uri-parse "http://www.<>example.com")
  (test-assert "Condition must be raised" #f))

(test-end)
