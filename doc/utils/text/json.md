[§2] (text json) -- JSON parser {#text.json}
-------------

###### [!Library] `(text json)` 

This library provides JSON parser and writer. 


###### [!Parameter] `*json-map-type*` 

Controls mapping of JSON map and array. The value must be either
`'vector` or `'alist`. The first one is compatible with Chicken
Scheme's json module, the latter one is compatible with Gauche's 
`rfc.json` module. By default, it's set to `vector` for backward
compatibility.

Conversion rules for `vector`:

```scheme
JSON array   <-> list
JSON map     <-> vector
JSON boolean <-> boolean
JSON null    <-> symbol `null`
```

Conversion rules for `alist`:

```scheme
JSON array   <-> vector
JSON map     <-> alist
JSON boolean <-> boolean
JSON null    <-> symbol `null`
```

This parameter affects the read and write procedures.


###### [!Function] `json-read`  :optional (_port_ `(current-input-port)`)

Reads JSON from given _port_ and returns representing S-expression.

###### [!Function] `json-write`  _json_ :optional (_port_ `(current-output-port)`)

Writes the given S-expression JSON representing object to given
_port_.

###### [!Function] `json-write/normalized`  _json_ :optional (_port_ `(current-output-port)`)

Writes the given S-expression JSON representing object to given
_port_.

The written JSON doesn't have excess space.

###### [!Function] `json-write/pretty`  _json_ :optional (_port_ `(current-output-port)`) **[@since] `0.9.12`**

Writes the given S-expression JSON representing object to given
_port_.

The written JSON is formatted to human readable.

###### [!Function] `json-read-error?` _obj_
###### [!Function] `json-write-error?` _obj_

Returns `#t` if the given _obj_ is `&json-read` or `&json-write` respectively,
otherwise `#f`.

### Strict JSON parsing

By default, the `json-read` is tolerant for incorrect JSON structure.
For example, string escape, UTF-16 handling and comment enhancement.

The example below shows how to make strict parser.

```scheme
(import (rnrs)
        (text json))

(define strict-config (json-parser-config
                       (strict? #t)
                       (allow-comment? #f))) ;; no comment enchancment
(define strict-json-read (make-json-reader strict-config))

(strict-json-read (open-string-input-port "\"\\q\"")) ;; -> &json-read
```

###### [!Macro] `json-parser-config`

Record builder macro to build `json-parser-config` record.
The `json-parser-config` record type has the fields below

`strict?`
: The parser should parse strictly or not, default `#f`.

`allow-comment?`
: The parser should accept defact comment or not, default `#t`.

`max-nesting-depth`
: The max nested depth of JSON object and array. `#f` means indefinite.
  default `#f`.

###### [!Function] `json-parser-config?` _obj_

Returns `#t` if the given _obj_ is `json-parser-config` record, otherwise `#f`.

###### [!Function] `make-json-reader` (_config_ `json-parser-config?`)

Creates a JSON reader whose signature is the same as `json-read`.

The returning parser procedure parses JSON according to the
given _config_.
