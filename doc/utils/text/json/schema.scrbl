@; -*- coding: utf-8 -*-
@subsection[:tag "text.json.schema"]{(text json schema) - JSON Schema}

@define[Library]{@name{(text json schema)}}
@desc{This library provides JSON Schema procedures.

Currently, it only supports validators not hyperlinks of draft-7.
}

@subsection[:tag "text.json.schema-validator"]{JSON Schema validator}

The following example shows how to use the JSON Schema validator.

The following JSON Schema defines the structure of product catalogue. It's
saved in the file @code{product.schema.json}
@codeblock{
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
}

We want to validate the following 2 JSON files whose content are the below:

@itemlist[
  @item{@code{valid-product.json}}
  @item{@code{invalid-product.json}}
]

@codeblock{
{
  "productId": 1,
  "productName": "A green door",
  "price": 12.50,
  "tags": [ "home", "green" ]
}
}

@codeblock{
{
  "productId": "This must be an integer",
  "productName": 1234,
  "price": -1
}
}

For the simple validation, you can write the following code:
@codeblock[=> (values #t #f)]{
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
}

If you want to see the first invalid property, then you can write like this:

@codeblock[=> (values #t #f)]{
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
}

If you want to run as a lint mode, which goes through entire JSON,
you can write like this:

@codeblock[=> (values #t #t)]{
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
}

NOTE: On the lint mode, the validation result will always be @code{#t}.

@define[Function]{@name{json-schema->json-validator}
 @args{schema referencing-validators @dots{}}}
@desc{Creates JSON validator object of the given JSON Schema @var{schema}.

The JSON Schema must be a vector represented S-expression JSON
(see @secref["text.json"]{JSON parser}).

JSON validator is described in @secref["text.json.validator"]{JSON validator}.

The optional arguments @var{referencing-validators} must be
JSON Schema validators, if it's given, and will be used to reference
external schemas.
}

@define[Parameter]{@name{*json-schema:resolve-external-schema?*}}
@desc{Specifying if the validator creation procedure to resolve external
reference of schema. e.g. @code{"$ref": "http://json-schema.org/schema#"}.

The default value is @code{#f}.
}

@define[Parameter]{@name{*json-schema:validate-format?*}}
@desc{Specifying if the validator validates @code{"format"} keywords.

The default value is @code{#t}.
}

@define[Parameter]{@name{*json-schema:validator-error-reporter*}}
@desc{Specifying error reporter.

The supporting error reporter is the  @code{simple-json-schema-error-reporter}.
}

@define[Parameter]{@name{*json-schema:report-port*}}
@desc{Specifying the port to be used to report error.

The default value is @code{(current-error-port)}
}

@define[Parameter]{@name{*json-schema:lint-mode?*}}
@desc{Specifying if the validator should act as if it's a linter.

To make this parameter effected, then
@code{*json-schema:validator-error-reporter*} must also be specified.

The default value is @code{#f}.
}

@define[Function]{@name{simple-json-schema-error-reporter} @args{report}}
@desc{The pre-defined error repoter for JSON schema validator.}
