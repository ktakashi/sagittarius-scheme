(import (rnrs)
	(text json schema validators)
	(srfi :64))

(test-begin "JSON Schema validators")

(define (test-validator validator . expected*)
  (for-each (lambda (e)
	      (test-equal e (car e) (validator (cadr e))))
	    expected*))

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
  (test-validator (json-schema:unique-items #f) '(#t (1 2)) '(#t (1 1)))))

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
   (test-error assertion-violation? (json-schema:dependencies '#(("k" . #t))))
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
)

(test-end)
