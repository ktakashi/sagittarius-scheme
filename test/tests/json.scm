;; -*- scheme -*-
#!compatible

(import (rnrs)
	(json)
	(pp)
	(srfi :26 cut)
	(srfi :64 testing))

(test-begin "JSON tests")

(define synopsis-data (string-append (current-directory)
				     "/test/data/json-synopsis.json"))

;; basic test
(test-equal "synopsis (read)"
	    '#(("some" . #(("partial" 42)))
	       ("other" . #(("partial" . "a string")))
	       ("more" . #(("more" . "stuff"))))
	    (call-with-input-file synopsis-data
	      (cut json-read <>)))

(test-equal "synopsis (write)"
	    "{\"some\": {\"partial\": [42]}, \
              \"other\": {\"partial\": \"a string\"}, \
              \"more\": {\"more\": \"stuff\"}}"
	    (call-with-string-output-port
	     (cut json-write
		  '#(("some" . #(("partial" 42)))
		     ("other" . #(("partial" . "a string")))
		     ("more" . #(("more" . "stuff"))))
		  <>)))

(define (parse-json-string str)
  (call-with-port
   (open-string-input-port str)
   (cut json-read <>)))

;; test cases are from Gauche
(let ()
  (define (t str val)
    (test-equal (string-append "primitive " str)
		`#(("x" . ,val)) (parse-json-string str)))
  (t "{\"x\": 100 }" 100)
  (t "{\"x\" : -100}" -100)
  (t "{\"x\":  +100 }" 100)
  (t "{\"x\": 12.5} " 12.5)
  (t "{\"x\":-12.5}" -12.5)
  (t "{\"x\":+12.5}"  12.5)
  (t "{\"x\": 1.25e1 }" 12.5)
  (t "{\"x\":125e-1}" 12.5)
  (t "{\"x\":1250.0e-2}" 12.5)
  (t "{\"x\":  false  }" #f)
  (t "{\"x\":true}" #t)
  (t "{\"x\":null}" 'null)

  (t "{\"x\": \"abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0040abc\"}"
     "abc\"\\/\x0008;\x000c;\x000a;\x000d;\x0009;@abc")
  )

(let ()
  (define (t str)
    (test-error (string-append "parse error " str)
		(parse-json-string str)))
  (t "{\"x\": 100")
  (t "{x : 100}}"))

(define two-objects-data (string-append (current-directory)
					"/test/data/json-two-objects.json"))

(test-equal "parsing an array containing two objects"
	    '(#(("precision" . "zip")
		("Latitude" . 37.7668)
		("Longitude" . -122.3959)
		("Address" . "")
		("City" . "SAN FRANCISCO")
		("State" . "CA")
		("Zip" . "94107")
		("Country" . "US"))
	      #(("precision" . "zip")
		("Latitude" . 37.371991)
		("Longitude" . -122.026020)
		("Address" . "")
		("City" . "SUNNYVALE")
		("State" . "CA")
		("Zip" . "94085")
		("Country" . "US")))
	    (call-with-input-file two-objects-data
	      (cut json-read <>)))

(test-end)