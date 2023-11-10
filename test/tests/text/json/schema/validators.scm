(import (rnrs)
	(text json validator)
	(text json schema validators)
	(srfi :133)
	(srfi :64))

(test-begin "JSON Schema validators")

(test-error (json-schema->json-validator ""))
(test-error (json-schema->json-validator #(("if" . ""))))

;; schemata from https://json-schema.org/blog/posts/dynamicref-and-generics
(define list-of-t
  #(
    ("$schema" . "https://json-schema.org/draft/2020-12/schema")
    ("$id" . "https://json-schema.example/list-of-t")
    ("type" . "array")
    ("items" .
     #(
       ("$dynamicRef" . "#T")
       ))
    ("$defs" .
     #(
       ("content" .
	#(
	  ("$dynamicAnchor" . "T")
	  ("not" . #t)
	  ))
       )
     )
    ))

(define list-of-string
  #(
    ("$schema" . "https://json-schema.org/draft/2020-12/schema")
    ("$id" . "https://json-schema.example/list-of-string")
    ("$ref" . "list-of-t")
    ("$defs" .
     #(
       ("string-items" .
	#(
	  ("$dynamicAnchor" . "T")
	  ("type" . "string")
	  )
	)
       )
     )
    ))

(define list-of-int
  #(
    ("$schema" . "https://json-schema.org/draft/2020-12/schema")
    ("$id" . "https://json-schema.example/list-of-int")
    ("$ref" . "list-of-t")
    ("$defs" .
     #(
       ("int-items" .
	#(
	  ("$dynamicAnchor" . "T")
	  ("type" . "integer")
	  )
	)
       )
     )
    ))

(define (test-validator validator . inputs)
  (for-each (lambda (expects&input)
	      (let ((expects (car expects&input))
		    (input (cadr expects&input)))
		(test-equal input expects (validate-json validator input))))
	    inputs))

(test-validator (json-schema->json-validator list-of-string list-of-t)
		'(#t ("a" "b"))
		'(#f (null))
		'(#f (1 2))
		'(#f (#() ())))

(test-validator (json-schema->json-validator list-of-int list-of-t)
		'(#f ("a" "b"))
		'(#f (null))
		'(#t (1 2))
		'(#f (#() ())))

(test-validator (json-schema->json-validator
		 list-of-string (json-schema->json-validator list-of-t))
		'(#t ("a" "b"))
		'(#f (null))
		'(#f (1 2))
		'(#f (#() ())))

(test-end)
