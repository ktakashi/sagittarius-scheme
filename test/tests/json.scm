;; -*- mode:scheme; coding:utf-8; -*-
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

(test-equal "json null" "null" 
	    (call-with-string-output-port
	     (lambda (out) 
	       (json-write (json-read (open-string-input-port "null"))
			   out))))

;; surrogate pair
;; The character may not be shown on Emacs 
(define-syntax test-surrogate
  (syntax-rules ()
    ((_ name e s)
     (test-equal name e (json-read (open-string-input-port s))))))
(test-surrogate "surrogate pairs(1)" "\x29e3d;" "\"\\ud867\\ude3d\"")
(test-surrogate "surrogate pairs(2)" "\x10000;" "\"\\ud800\\udc00\"")
(test-surrogate "surrogate pairs(3)" "\x1d11e;" "\"\\ud834\\udd1e\"")
(test-surrogate "surrogate pairs(4)" "\x10fffd;" "\"\\udbff\\udffd\"")

;; TODO error test cases

;; mode change
(import (prefix (text json) text:)
	(srfi :39 parameters))

(parameterize ((text:*json-map-type* 'alist))
  (define json-string "{\"some\": {\"partial\": [42]}, \
                        \"other\": {\"partial\": \"a string\"}, \
                        \"more\": {\"more\": \"stuff\"}}")
  (test-equal "json Gauche compat (read)"
	      '(("some"  . (("partial" . #(42))))
		("other" . (("partial" . "a string")))
		("more"  . (("more" . "stuff"))))
	      (text:json-read (open-string-input-port json-string)))
  (test-equal "json Gauche compat (write)"
	      json-string
	      (call-with-string-output-port
	       (cut text:json-write '(("some"  . (("partial" . #(42))))
				      ("other" . (("partial" . "a string")))
				      ("more"  . (("more" . "stuff"))))
		    <>)))

  ;; (json) doesn't get affected
  (test-equal "don't get affected (read)"
	      '#(("some" . #(("partial" 42)))
		 ("other" . #(("partial" . "a string")))
		 ("more" . #(("more" . "stuff"))))
	      (json-read (open-string-input-port json-string)))
  (test-equal "don't get affected (write)"
	    "{\"some\": {\"partial\": [42]}, \
              \"other\": {\"partial\": \"a string\"}, \
              \"more\": {\"more\": \"stuff\"}}"
	    (call-with-string-output-port
	     (cut json-write
		  '#(("some" . #(("partial" 42)))
		     ("other" . #(("partial" . "a string")))
		     ("more" . #(("more" . "stuff"))))
		  <>)))
  )


(test-end)