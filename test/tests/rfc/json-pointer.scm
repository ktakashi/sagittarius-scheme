(import (rnrs)
	(rfc json-pointer)
	(text json)
	(srfi :39)
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

(define (test-json)
  (define json-string "
{
  \"a\": 1,
  \"b\": {
    \"c\": 2
  },
  \"d\": {
    \"e\": [{ \"a\": 3 }, { \"b\": 4 }, { \"c\": 5 }]
  }
}
")
  (define json (json-read (open-string-input-port json-string)))
  (test-equal 1 ((json-pointer "/a") json))
  (test-equal 2 ((json-pointer "/b/c") json))
  (test-equal 3 ((json-pointer "/d/e/0/a") json))
  (test-equal 4 ((json-pointer "/d/e/1/b") json))
  (test-equal 5 ((json-pointer "/d/e/2/c") json)))

(test-json)
(parameterize ((*json-map-type* 'alist)) (test-json))

(define (test-from-draft)
  (define example "\
{\
  \"foo\": [\"bar\", \"baz\"],\
  \"\": 0,\
  \"a/b\": 1,\
  \"c%d\": 2,\
  \"e^f\": 3,\
  \"g|h\": 4,\
  \"i\\\\\\\\j\": 5,\
  \"k'l\": 6,\
  \" \": 7,\
  \"m~n\": 8\
}")
  (define json (json-read (open-string-input-port example)))
  (let ((foo ((json-pointer "/foo") json)))
    (case (*json-map-type*)
      ((vector)
       (test-equal 2 (length foo))
       (test-equal "bar" (car foo))
       (test-equal "baz" (cadr foo)))
      ((alist)
       (test-equal 2 (vector-length foo))
       (test-equal "bar" (vector-ref foo 0))
       (test-equal "baz" (vector-ref foo 1)))))
  (test-equal "bar" ((json-pointer "/foo/0") json))
  (test-equal 0 ((json-pointer "/") json))
  (test-equal 1 ((json-pointer "/a~1b") json))
  (test-equal 2 ((json-pointer "/c%d") json))
  (test-equal 3 ((json-pointer "/e^f") json))
  (test-equal 4 ((json-pointer "/g|h") json))
  (test-equal 5 ((json-pointer "/i\\\\j") json))
  (test-equal 6 ((json-pointer "/k'l") json))
  (test-equal 7 ((json-pointer "/ ") json))
  (test-equal 8 ((json-pointer "/m~0n") json))
  (test-assert (json-pointer-not-found? ((json-pointer "/foo/-") json))))

(test-from-draft)
(parameterize ((*json-map-type* 'alist)) (test-from-draft))

(test-end)
