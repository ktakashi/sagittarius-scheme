(import (rnrs)
	(text json schema validators)
	(srfi :133)
	(srfi :64))

(test-begin "JSON Schema validators")

(define (test-validator validator . expected*)
  (define (wrap validator)
    (lambda (e)
      (with-exception-handler
       (lambda (e) #f)
       (lambda () (validator e)))))
  (let ((wrapped-validator (wrap validator)))
    (for-each (lambda (e)
		(test-equal e (car e) (wrapped-validator (cadr e))))
	      expected*)))

;; core
(test-group "JSON Schema"
(test-group "8.3. Schema References With '$ref'"
 ;; From the specification
 ;; {
 ;;     "$id": "http://example.com/root.json",
 ;;     "definitions": {
 ;;         "A": { "$id": "#foo" },
 ;;         "B": {
 ;;             "$id": "other.json",
 ;;             "definitions": {
 ;;                 "X": { "$id": "#bar" },
 ;;                 "Y": { "$id": "t/inner.json" }
 ;;             }
 ;;         },
 ;;         "C": {
 ;;             "$id": "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f"
 ;;         }
 ;;     }
 ;; }
 (let ((base-schema '#(("$id" . "http://example.com/root.json")
		       ("definitions" . 
			#(("A" . #(("$id" . "#foo")
				   ("maximum" . 5)))
			  ("B" .
			   #(("$id" . "other.json")
			     ("definitions" .
			      #(("X" . #(("$id" . "#bar")
					 ("minimum" . 0)))
				("Y" . #(("$id" . "t/inner.json")
					 ("type" . "number")))))))
			  ("C" .
			   #(("$id" . "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f")
			     ("type" . "string"))))))))
   (define (make-$ref . $refs)
     (define (->vec $refs)
       (list->vector (map (lambda ($ref) (cons "$ref" $ref)) $refs)))
     (vector-append (->vec $refs) base-schema))
   (test-equal '#(("$id" . "#foo") ("maximum" . 5))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "#/definitions/A")))
   (test-equal '#(("$id" . "#foo") ("maximum" . 5))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "http://example.com/root.json#/definitions/A")))
   (test-equal '#(("$id" . "#foo") ("maximum" . 5))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "http://example.com/root.json#foo")))
   (test-equal '#(("$id" . "#bar") ("minimum" . 0))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "other.json#/definitions/X")))
   (test-equal '#(("$id" . "#bar") ("minimum" . 0))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "http://example.com/other.json#bar")))
   (test-equal '#(("$id" . "#bar") ("minimum" . 0))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "http://example.com/other.json#/definitions/X")))
   (test-equal '#(("$id" . "t/inner.json") ("type" . "number"))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "t/inner.json#")))
   (test-equal '#(("$id" . "t/inner.json") ("type" . "number"))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "t/inner.json")))

   (test-equal '#(("$id" . "t/inner.json") ("type" . "number"))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "http://example.com/other.json#/definitions/Y")))
   (test-equal '#(("$id" . "t/inner.json") ("type" . "number"))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "http://example.com/root.json#/definitions/B/definitions/Y")))
   
   (test-equal '#(("$id" . "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f")
		  ("type" . "string"))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "#/definitions/C")))
   (test-equal '#(("$id" . "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f")
		  ("type" . "string"))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f")))
   (test-equal '#(("$id" . "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f")
		  ("type" . "string"))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f#")))
   (test-equal '#(("$id" . "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f")
		  ("type" . "string"))
	       (resolve-$ref "http://example.com/root.json"
			     (make-$ref "http://example.com/root.json#/definitions/C")))
   )

 )

)

;; validation tests
(test-group "JSON Schema Validation"
(test-group "6.1.  Validation Keywords for Any Instance Type"
 (test-group "6.1.1.  type"
   (test-error assertion-violation? (json-schema:type '#()))
   (test-error assertion-violation? (json-schema:type 'null))
   (test-error assertion-violation? (json-schema:type '()))
   (test-error assertion-violation? (json-schema:type '(1 2)))
   (test-error assertion-violation? (json-schema:type "foo"))

   (test-validator (json-schema:type "string")
		   '(#t "string")
		   '(#f 1))
   (test-validator (json-schema:type "integer")
		   '(#t 1)
		   '(#t 1.0)
		   '(#f 1.9)
		   '(#f "string"))
   (test-validator (json-schema:type "number")
		   '(#t 1)
		   '(#t 1.0)
		   '(#t 1.9)
		   '(#f "string"))
   (test-validator (json-schema:type "object")
		   '(#t #())
		   '(#t #(("key" . "value")))
		   '(#f "string"))
   (test-validator (json-schema:type "array")
		   '(#t ())
		   '(#t (1 2 3))
		   '(#f #(("key" . "value")))
		   '(#f "string"))
   (test-validator (json-schema:type "boolean")
		   '(#t #t)
		   '(#t #f)
		   '(#f "string"))
   (test-validator (json-schema:type "null")
		   '(#t null)
		   '(#f #t)
		   '(#f #f)
		   '(#f "string"))
   (test-validator (json-schema:type '("string" "number"))
		   '(#t "string")
		   '(#t 1)
		   '(#f null))
   )
 (test-group "6.1.2.  enum"
  (test-error assertion-violation? (json-schema:enum "string"))
  (test-error assertion-violation? (json-schema:enum '()))
  (test-error assertion-violation? (json-schema:enum '(1 1)))
  
  (test-validator (json-schema:enum '(1 null ()))
		  '(#t 1) '(#t null) '(#f 1.0) '(#t ()) '(#f "string"))
  (test-validator (json-schema:enum '((1) #(("key" . 1))))
		  '(#t (1)) '(#f (1.0))
		  '(#t #(("key" . 1))) '(#f #(("key" . 1.0)))))
 (test-group "6.1.3.  const"
  (test-validator (json-schema:const "string") '(#t "string") '(#f "str"))
  (test-validator (json-schema:const '(1)) '(#t (1)) '(#f (1.0)))
  (test-validator (json-schema:const '#(("key" . 1)))
		  '(#t #(("key" . 1))) '(#f #(("key" . 1.0))))))

(test-group "6.2. Validation Keywords for Numeric Instances"
 (test-group "6.2.1. multipleOf"
  (test-error assertion-violation? (json-schema:multiple-of 'null))
  (test-validator (json-schema:multiple-of 5) '(#t 25) '(#f 24)))

 (test-group "6.2.2. maximum"
  (test-error assertion-violation? (json-schema:maximum 'null))
  (test-error assertion-violation? (json-schema:maximum 1+1i))
  (test-validator (json-schema:maximum 5) '(#t 5) '(#f 6)))
 (test-group "6.2.3. exclusiveMaximum"
  (test-error assertion-violation? (json-schema:exclusive-maximum 'null))
  (test-error assertion-violation? (json-schema:exclusive-maximum 1+1i))
  (test-validator (json-schema:exclusive-maximum 5) '(#t 4) '(#f 5)))
 (test-group "6.2.4. minimum"
  (test-error assertion-violation? (json-schema:minimum 'null))
  (test-error assertion-violation? (json-schema:minimum 1+1i))
  (test-validator (json-schema:minimum 5) '(#t 5) '(#f 4)))
 (test-group "6.2.5. exclusiveMinimum"
  (test-error assertion-violation? (json-schema:exclusive-minimum 'null))
  (test-error assertion-violation? (json-schema:exclusive-minimum 1+1i))
  (test-validator (json-schema:exclusive-minimum 5) '(#t 6) '(#f 5)))
 )

(test-group "6.3. Validation Keywords for Strings"
 (test-group "6.3.1. maxLength"
  (test-error assertion-violation? (json-schema:max-length "s"))
  (test-error assertion-violation? (json-schema:max-length -1))
  (test-error assertion-violation? (json-schema:max-length 1.1))
  (test-validator (json-schema:max-length 5)
		  '(#t "12345") '(#f "123456") '(#f 1)))
 (test-group "6.3.2. minLength"
  (test-error assertion-violation? (json-schema:min-length "s"))
  (test-error assertion-violation? (json-schema:min-length -1))
  (test-error assertion-violation? (json-schema:min-length 1.1))
  (test-validator (json-schema:min-length 3)
		  '(#t "1234") '(#f "12") '(#f 1)))
 (test-group "6.3.3. pattern"
  (test-error assertion-violation? (json-schema:pattern 1))
  (test-error assertion-violation? (json-schema:pattern "[]"))
  (test-validator (json-schema:pattern "^\\d\\w$")
		  '(#t "1a") '(#f "aa") '(#f "1aa"))))

(test-group "6.4. Validation Keywords for Arrays"
 (test-group "6.4.1. items"
  (test-error assertion-violation? (json-schema:items '#() "s"))
  (test-error assertion-violation? (json-schema:items '#() '("a")))
  (test-validator (json-schema:items '#() '#(("type" . "integer")))
		  '(#t (1 2 3 4))
		  '(#f (1 2 "3" 4)))
  (test-validator (json-schema:items '#(("additionalItems" .
					 #(("type" . "string"))))
				     '(#(("type" . "integer"))))
		  '(#t (1 "2" "3"))
		  '(#f (1 "2" 3 4)))
  (test-validator (json-schema:items '#(("additionalItems" .
					 #(("type" . "object"))))
				     '(#(("type" . "integer"))
				       #(("type" . "string"))))
		  '(#t (1))
		  '(#t (1 "2"))
		  '(#t (1 "2" #()))
		  '(#f (1 "2" 3))
		  '(#f (1 "2" #() 3)))
  (test-validator (json-schema:items '#() #f)
		  '(#t ())
		  '(#f (1))
		  '(#f (1 "2"))
		  '(#f (1 "2" #()))
		  '(#f (1 "2" 3))
		  '(#f (1 "2" #() 3))))
 (test-group "6.4.3. maxItems"
  (test-error assertion-violation? (json-schema:max-items "s"))
  (test-error assertion-violation? (json-schema:max-items -1))
  (test-error assertion-violation? (json-schema:max-items 1.1))
  (test-validator (json-schema:max-items 5)
		  '(#t (1 2 3 4 5)) '(#f (1 2 3 4 5 6)) '(#f 1)))
 (test-group "6.4.4. minItems"
  (test-error assertion-violation? (json-schema:min-items "s"))
  (test-error assertion-violation? (json-schema:min-items -1))
  (test-error assertion-violation? (json-schema:min-items 1.1))
  (test-validator (json-schema:min-items 3)
		  '(#t (1 2 3 4)) '(#f (1 2)) '(#f 1)))
 (test-group "6.4.5. uniqueItems"
  (test-error assertion-violation? (json-schema:unique-items "s"))
  (test-validator (json-schema:unique-items #t) '(#t (1 2)) '(#f (1 1)))
  (test-validator (json-schema:unique-items #f) '(#t (1 2)) '(#t (1 1))))
 (test-group "6.4.6. contains"
  (test-error assertion-violation? (json-schema:contains 1))
  (test-validator (json-schema:contains #t) '(#f ()) '(#t (1 2 3 4)))
  (test-validator (json-schema:contains #f) '(#f ()) '(#f (1 2 3 4)))
  (test-validator (json-schema:contains '#(("type" . "string")))
		  '(#t (1 "2" 3 4))
		  '(#f (1 2 3 4))))
)

(test-group "6.5. Validation Keywords for Objects"
 (test-group "6.5.1. maxProperties"
  (test-error assertion-violation? (json-schema:max-properties "s"))
  (test-error assertion-violation? (json-schema:max-properties -1))
  (test-error assertion-violation? (json-schema:max-properties 1.1))
  (test-validator (json-schema:max-properties 2)
		  '(#t #(("k" . "v") ("k2" . "v")))
		  '(#f #(("k" . "v") ("k2" . "v") ("k3" . "v")))))
 (test-group "6.5.2. minProperties"
  (test-error assertion-violation? (json-schema:min-properties "s"))
  (test-error assertion-violation? (json-schema:min-properties -1))
  (test-error assertion-violation? (json-schema:min-properties 1.1))
  (test-validator (json-schema:min-properties 3)
		  '(#t #(("k" . "v") ("k2" . "v") ("k3" . "v")))
		  '(#f #(("k" . "v") ("k2" . "v")))))
 (test-group "6.5.3. required"
  (test-error assertion-violation? (json-schema:required "s"))
  (test-error assertion-violation? (json-schema:required '(1)))
  (test-error assertion-violation? (json-schema:required '("s" "s")))
  (test-validator (json-schema:required '("k" "k2"))
		  '(#t #(("k" . "v") ("k2" . "v") ("k3" . "v")))
		  '(#f #(("k3" . "v")))))

 (test-group "6.5.4. properties"
  (test-validator (json-schema:properties 
		   '#(("properties" . #(("name" . #(("type" . "string")))))
		      ("patternProperties" .
		       #(("f.*" . #(("type" . "integer")))))
		      ("additionalProperties" . #f))
		   'dummy)
		  '(#t #(("name" . "v") ("foo" . 1)))
		  '(#t #(("name" . "v") ("name" . "v2") ("foo" . 1)))
		  '(#t #(("name" . "v") ("foo" . 1) ("fff" . 1)))
		  '(#f #(("name" . "v") ("foo" . "v")))
		  '(#f #(("name" . 1) ("foo" . 1)))
		  '(#f #(("name" . "v") ("foo" . 1) ("dummy" . 1))))
  (test-validator (json-schema:properties 
		   '#(("properties" . #(("name" . #(("type" . "string")))))
		      ("patternProperties" .
		       #(("f.*" . #(("type" . "integer")))))
		      ("additionalProperties" . #t))
		   'dummy)
		  '(#t #(("name" . "v") ("foo" . 1) ("dummy" . 1))))
  (test-validator (json-schema:properties 
		   '#(("properties" . #(("name" . #(("type" . "string")))))
		      ("patternProperties" .
		       #(("f.*" . #(("type" . "integer")))))
		      ("additionalProperties" . #(("type" . "integer"))))
		   'dummy)
		  '(#t #(("name" . "v") ("foo" . 1) ("dummy" . 1)))
		  '(#f #(("name" . "v") ("foo" . 1) ("dummy" . #t))))
  (test-validator (json-schema:properties 
		   '#(("patternProperties" .
		       #(("^f" . #(("type" . "integer"))))))
		   'dummy)
		  '(#t #(("foo" . 1)))
		  '(#f #(("foo" . "s")))))

  (test-group "6.5.5. dependencies"
   (test-error assertion-violation? (json-schema:dependencies "s"))
   (test-error assertion-violation? (json-schema:dependencies
				     '#(("k" "s" "s"))))
   (test-validator (json-schema:dependencies '#(("k" "s" "v")))
		   '(#t #(("k" . "v") ("s" . "v") ("v" . "v")))
		   '(#t #(("k2" . "v")))
		   '(#f #(("k" . "v") ("s" . "v"))))
   (test-validator (json-schema:dependencies
		    '#(("k" . #(("properties" .
				 #(("s" . #(("type" . "number")))))
				("required" "s")))))
		   '(#t #(("k" . "v") ("s" . 1)))
		   '(#t #(("k2" . "v")))
		   '(#f #(("k" . "v") ("s" . "v")))))

  (test-group "6.5.8. propertyNames"
   (test-error assertion-violation? (json-schema:property-names "s"))
   (test-error assertion-violation? (json-schema:property-names '(1)))
   (test-validator (json-schema:property-names #t) '(#t #(("v" . "v"))))
   (test-validator (json-schema:property-names #f) '(#f #(("v" . "v"))))
   (test-validator (json-schema:property-names '#(("pattern" . "^f")))
		   '(#t #(("f" . "v")))
		   '(#f #(("v" . "v")))))
(test-group "6.6. Keywords for Applying Subschemas Conditionally"
 (test-error assertion-violation? (json-schema:if '#() "s"))
 (test-error assertion-violation? (json-schema:if '#(("then" . 1)) #t))
 (test-error assertion-violation? (json-schema:if '#(("else" . 1)) #t))
 (test-validator (json-schema:if '#(("then" . #(("enum" "100")))
				    ("else" . #(("type" . "integer"))))
				 '#(("type" . "string")))
		 '(#t "100") '(#t 100) '(#f "s") '(#f ())))

(test-group "6.7. Keywords for Applying Subschemas With Boolean Logic"
 (test-group "6.7.1. allOf"
  (test-error assertion-violation? (json-schema:all-of "s"))
  (test-error assertion-violation? (json-schema:all-of '()))
  (test-error assertion-violation? (json-schema:all-of '("s")))
  (test-validator (json-schema:all-of '(#(("type" . "string"))
					#(("maxLength" . 5))))
		  '(#t "12345") '(#f "123456") '(#f 12345))
  ;; impossible pattern
  (test-validator (json-schema:all-of '(#(("type" . "string"))
					#(("type" . "number"))))
		  '(#f "12345") '(#f 12345)))
 (test-group "6.7.2. anyOf"
  (test-error assertion-violation? (json-schema:any-of "s"))
  (test-error assertion-violation? (json-schema:any-of '()))
  (test-error assertion-violation? (json-schema:any-of '("s")))
  (test-validator (json-schema:any-of '(#(("type" . "string")
					  ("maxLength" . 5))
					#(("type" . "number")
					  ("minimum" . 0))))
		  '(#t "12345") '(#f "123456")
		  '(#t 1) '(#f -5)
		  '(#f #())))
 (test-group "6.7.3. oneOf"
  (test-error assertion-violation? (json-schema:one-of "s"))
  (test-error assertion-violation? (json-schema:one-of '()))
  (test-error assertion-violation? (json-schema:one-of '("s")))
  (test-validator (json-schema:one-of '(#(("multipleOf" . 3))
					#(("multipleOf" . 5))))
		  '(#t 3) '(#t 5) '(#f 2) '(#f 15)))
 (test-group "6.7.4. not"
  (test-error assertion-violation? (json-schema:not "s"))
  (test-error assertion-violation? (json-schema:not '()))
  (test-error assertion-violation? (json-schema:not '("s")))
  (test-validator (json-schema:not '#(("type" . "string")))
		  '(#t 3) '(#t #()) '(#t ()) '(#f "s"))))
(test-group "7.3. Defined Formats"
 (test-group "7.3.1. Dates and Times"
  (test-validator (json-schema:format "date")
		  '(#t "2018-08-24") '(#t "2018-13-24")
		  '(#f "10:11:56Z") '(#f "2018-08-24T10:11:56Z"))
  (test-validator (json-schema:format "time")
		  '(#t "10:11:56Z") '(#t "56:11:56Z")
		  '(#f "2018-08-24") '(#f "2018-13-24")
		  '(#f "2018-08-24T10:11:56Z"))
  (test-validator (json-schema:format "date-time")
		  '(#t "2018-08-24T10:11:56Z")
		  '(#t "2018-08-24T10:11:56.001Z")
		  '(#t "2018-08-24T10:11:56+10:00")
		  '(#t "2018-08-24T10:11:56.001+09:00")
		  '(#f "2018-08-24T10:11:56")
		  '(#f "10:11:56Z") '(#f "2018-08-24")))
 (test-group "7.3.2. Email Addresses"
  (test-validator (json-schema:format "email")
		  '(#t "ktakashi@ymail.com") '(#f "ktakashi@"))
  (test-validator (json-schema:format "idn-email")
		  '(#t "ktákáshí@ymail.com")
		  '(#t "ktákáshí@ymail@.com")))
 (test-group "7.3.3. Hostnames"
  (test-validator (json-schema:format "hostname")
		  '(#t "example.com") '(#t "my-example.com")
		  '(#f ".example.com") '(#f "-example.com")
		  '(#f "-example .com")
		  '(#f "-example.com.") '(#f "-example.com-"))
  (test-validator (json-schema:format "idn-hostname")
		  '(#t "éxámplé.cóm") '(#t "mý-éxample.com")
		  '(#f ".éxample.com") '(#f "-example.com")
		  '(#f "-éxample.com.") '(#f "-example.com-")))
 (test-group "7.3.4. IP Addresses"
  (test-validator (json-schema:format "ipv4")
		  '(#t "192.168.1.56")
		  '(#t "10.12.1.255")
		  '(#t "1.12.1.255")
		  '(#f "192.12.1.255.245"))
  (test-validator (json-schema:format "ipv6")
		  '(#t "1200:0000:AB00:1234:0000:2552:7777:1313")
		  '(#t "21DA:D3:0:2F3B:2AA:FF:FE28:9C5A")
		  ;; regex can't check this...
		  ;; '(#f "1200::AB00:1234::2552:7777:1313")
		  '(#f "1200:0000:AB00:1234:O000:2552:7777:1313")))
 (test-group "7.3.5. Resource Identifiers"
  (test-validator (json-schema:format "uri")
		  '(#t "uri:bla") '(#t "http://foo")
		  '(#t "http:foo") ;; this is valid though...
		  ;; no scheme so invalid...
		  '(#f "foo-bar"))
  (test-validator (json-schema:format "iri")
		  '(#t "uri:bla") '(#t "http://foo")
		  '(#t "http:foo")
		  '(#f "foo-bar"))
  ;; I'm not sure what's invalid as uri-reference...
  (test-validator (json-schema:format "uri-reference")
		  '(#t "uri:bla") '(#t "http://foo")
		  '(#t "http:foo")
		  '(#t "foo-bar"))
  (test-validator (json-schema:format "iri-reference")
		  '(#t "uri:bla") '(#t "http://foo")
		  '(#t "http:foo")
		  '(#t "foo-bar")))
 (test-group "7.3.6. uri-template"
  ;; I'm not sure what's invalid as uri-template...
  (test-validator (json-schema:format "uri-template")
		  '(#t "uri:bla") '(#t "http://foo/{bla}")
		  '(#t "http:foo")
		  '(#t "foo-bar")))
 (test-group "7.3.7. JSON Pointers"
  (test-validator (json-schema:format "json-pointer")
		  '(#t "/foo/bar") '(#t "/foo"))
  (test-validator (json-schema:format "relative-json-pointer")
		  '(#t "0/foo") '(#f "/foo/bar")))
 (test-group "7.3.8. regex"
  (test-validator (json-schema:format "regex")
		  '(#t "foo") '(#t ".*") '(#f "[a-")))
 )
))

(test-end)
