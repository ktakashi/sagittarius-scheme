[§2] (text json schema) - JSON Schema {#text.json.schema}
-------------

###### [!Library] `(text json schema)` 

This library provides JSON Schema procedures.

Currently, it only supports validators not hyperlinks of draft-7.


[§2] JSON Schema validator {#text.json.schema-validator}
-------------

The following example shows how to use the JSON Schema validator.

The following JSON Schema defines the structure of product catalogue. It's
saved in the file `product.schema.json`
``````````scheme
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "http://example.com/product.schema.json",
  "title": "Product",
  "description": "A product from Acme's catalog",
  "type": "object",
  "properties": {
    "productId": {
      "description": "The unique identifier for a product",
      "type": "integer"
    },
    "productName": {
      "description": "Name of the product",
      "type": "string"
    },
    "price": {
      "description": "The price of the product",
      "type": "number",
      "exclusiveMinimum": 0
    }
  },
  "required": [ "productId", "productName", "price" ]
}
``````````

We want to validate the following 2 JSON files whose content are the below:


- `valid-product.json`
- `invalid-product.json`

``````````scheme
{
  "productId": 1,
  "productName": "A green door",
  "price": 12.50,
  "tags": [ "home", "green" ]
}
``````````

``````````scheme
{
  "productId": "This must be an integer",
  "productName": 1234,
  "price": -1
}
``````````

For the simple validation, you can write the following code:

``````````scheme
(import (rnrs)
        (text json)
        (text json schema)
        (text json validator))

(define product-catalogue-schema
  (json-schema->json-validator
   (call-with-input-file "product.schema.json" json-read)))

(define valid-catalogue
  (call-with-input-file "valid-product.json" json-read))

(define invalid-catalogue
  (call-with-input-file "invalid-product.json" json-read))

(values (validate-json product-catalogue-schema valid-catalogue)
        (validate-json product-catalogue-schema invalid-catalogue))
``````````
=> ``(values #t #f)``

If you want to see the first invalid property, then you can write like this:

``````````scheme
(import (rnrs)
        (text json)
        (text json schema)
        (text json validator)
        (srfi :39 parameters))

(define product-catalogue-schema
  (json-schema->json-validator
   (call-with-input-file "product.schema.json" json-read)))

(define valid-catalogue
  (call-with-input-file "valid-product.json" json-read))

(define invalid-catalogue
  (call-with-input-file "invalid-product.json" json-read))

(parameterize ((*json-schema:validator-error-reporter*
                simple-json-schema-error-reporter))
  (values (validate-json product-catalogue-schema valid-catalogue)
          (validate-json product-catalogue-schema invalid-catalogue)))
;; Prints the following
#|
/productId
        object: "This must be an integer"
        type: integer
|#
``````````
=> ``(values #t #f)``

If you want to run as a lint mode, which goes through entire JSON,
you can write like this:

``````````scheme
(import (rnrs)
        (text json)
        (text json schema)
        (text json validator)
        (srfi :39 parameters))

(define product-catalogue-schema
  (json-schema->json-validator
   (call-with-input-file "product.schema.json" json-read)))

(define valid-catalogue
  (call-with-input-file "valid-product.json" json-read))

(define invalid-catalogue
  (call-with-input-file "invalid-product.json" json-read))

(parameterize ((*json-schema:validator-error-reporter*
                simple-json-schema-error-reporter)
               (*json-schema:lint-mode?* #t))
  (values (validate-json product-catalogue-schema valid-catalogue)
          (validate-json product-catalogue-schema invalid-catalogue)))
;; Prints the following
#|
/productId
        object: "This must be an integer"
        type: integer
/productName
        object: 1234
        type: string
/price
        object: -1
        exclusive-minimum: 0
|#
``````````
=> ``(values #t #t)``

NOTE: On the lint mode, the validation result will always be `#t`.

###### [!Function] `json-schema->json-validator`  _schema_ _referencing-validators_ _..._

Creates JSON validator object of the given JSON Schema _schema_.

The JSON Schema must be a vector represented S-expression JSON
(see [JSON parser](#text.json)).

JSON validator is described in [JSON validator](#text.json.validator).

The optional arguments _referencing-validators_ must be
JSON Schema validators, if it's given, and will be used to reference
external schemas.


###### [!Parameter] `*json-schema:resolve-external-schema?*` 

Specifying if the validator creation procedure to resolve external
reference of schema. e.g. `"$ref": "http://json-schema.org/schema#"`.

The default value is `#f`.


###### [!Parameter] `*json-schema:validate-format?*` 

Specifying if the validator validates `"format"` keywords.

The default value is `#t`.


###### [!Parameter] `*json-schema:validator-error-reporter*` 

Specifying error reporter.

The supporting error reporter is the  `simple-json-schema-error-reporter`.


###### [!Parameter] `*json-schema:report-port*` 

Specifying the port to be used to report error.

The default value is `(current-error-port)`

###### [!Parameter] `*json-schema:lint-mode?*` 

Specifying if the validator should act as if it's a linter.

To make this parameter effected, then
`*json-schema:validator-error-reporter*` must also be specified.

The default value is `#f`.


###### [!Function] `simple-json-schema-error-reporter`  _report_

The pre-defined error repoter for JSON schema validator.

