(import (rnrs) (rfc jrd) (srfi :64) (text json))

(test-begin "RFC 7033 - JRD")

(define json-string "{
  \"subject\" : \"http://blog.example.com/article/id/314\",
  \"aliases\" :
  [
    \"http://blog.example.com/cool_new_thing\",
    \"http://blog.example.com/steve/article/7\"
  ],
  \"properties\" :
  {
    \"http://blgx.example.net/ns/version\" : \"1.3\",
    \"http://blgx.example.net/ns/ext\" : null
  },
  \"links\" :
  [
    {
      \"rel\" : \"copyright\",
      \"href\" : \"http://www.example.com/copyright\"
    },
    {
      \"rel\" : \"author\",
      \"href\" : \"http://blog.example.com/author/steve\",
      \"titles\" :
      {
        \"en-us\" : \"The Magical World of Steve\",
        \"fr\" : \"Le Monde Magique de Steve\"
      },
      \"properties\" :
      {
        \"http://example.com/role\" : \"editor\"
      }
    }
  ]
}")
(let ((jrd (json-string->jrd json-string)))
  (test-equal "http://blog.example.com/article/id/314" (jrd-subject jrd))
  (test-equal '("http://blog.example.com/cool_new_thing"
		"http://blog.example.com/steve/article/7")
	      (jrd-aliases jrd))
  (let ((jrd-props (jrd-properties jrd)))
    (test-assert (list? jrd-props))
    (test-equal 2 (length jrd-props))
    (test-assert (for-all jrd:property? jrd-props))
    (for-each (lambda (p kv)
		(test-equal (car kv) (jrd:property-name p))
		(test-equal (cdr kv) (jrd:property-value p)))
	      jrd-props
	      '(("http://blgx.example.net/ns/version" . "1.3")
		("http://blgx.example.net/ns/ext" . null))))
  (let ((links (jrd-links jrd)))
    (test-assert (list? links))
    (test-equal 2 (length links))
    (test-assert (for-all jrd:link? links))

    (for-each (lambda (l rh)
		(test-equal (car rh) (jrd:link-rel l))
		(test-equal (cdr rh) (jrd:link-href l)))
	      links
	      '(("copyright" . "http://www.example.com/copyright")
		("author" . "http://blog.example.com/author/steve")))

    (let ((titles (jrd:link-titles (cadr links)))
	  (props  (jrd:link-properties (cadr links))))
      (test-assert (list? titles))
      (test-equal 2 (length titles))
      (test-assert (for-all jrd:title? titles))
      (for-all (lambda (t lt)
		 (test-equal (car lt) (jrd:title-language t))
		 (test-equal (cdr lt) (jrd:title-title t)))
	       titles
	       '(("en-us" . "The Magical World of Steve")
		 ("fr" . "Le Monde Magique de Steve")))
      (test-assert (list? props))
      (test-equal 1 (length props))
      (test-assert (for-all jrd:property? props))
      (for-each (lambda (p kv)
		  (test-equal (car kv) (jrd:property-name p))
		  (test-equal (cdr kv) (jrd:property-value p)))
		props
		'(("http://example.com/role" . "editor")))))

  (let ((s (jrd->json-string jrd)))
    (test-equal (json-read (open-string-input-port json-string))
		(json-read (open-string-input-port s)))))

(test-end)
