[§2] (rfc uri-template) - URI template {#rfc.uri-template}
-------------

###### [!Library] `(rfc uri-template)` 

This library provides RFC6570 'URI Template' procedures.

###### [!Function] `parse-uri-template`  _in_

Parses the given URI template into S-expression.

The given _in_ must be either a textual input port or a string.

The parsed form consists with the following rules:


- uri-template ::= `(template-unit)`
- template-unit ::= `string`
- template-unit ::= `(template)`
- template ::= `(operator varaible)`
- operator ::= `character`
- variable ::= `string`
- variable ::= `(string modifier)`
- modifier ::= `*`
- modifier ::= `positive exact integer`

_operator_ must be a valid URI template operator

``(parse-uri-template "http://{domain*}/{/path}")
`` => ``(http:// ((domain \*)) / (/ path))``



###### [!Function] `expand-uri-template`  _uri-template_ _parameter_

Expands the given _uri-template_ and returns a string.

The _parameter_ must be a vector of pair of string and _value_.

The _value_ must be one of the followings:

- string
- list of string
- vector of pair of string

``(expand-uri-template '((#\? ("keys" *))) '#(("keys" . #(("semi" . ";") ("dot" . ".") ("comma" . ",")))))
`` => ``?semi=%3B&dot=.&comma=%2C``



