[§2] (net http) - Async HTTP client {#net.http}
-------------

###### [!Library] `(net http)`

This library provides a wrapper of `(net http-client)` library,
for convenience especially scripting purpose.

To simplify the document, here we use __methods__, __nobody-methods__
and __bodied-methods__ to refer sets of HTTP methods. The mapping
of the methods are blow:

__methods__
: All HTTP methods, at this moment we support the below methods:
  - GET
  - HEAD
  - OPTIONS
  - POST
  - PUT
  - PATCH
  - DELETE
  
__nobody-methods__
: HTTP methods which don't have body, these are the ones:
  - GET
  - HEAD
  - OPTIONS
  
__bodied-methods__
: HTTP methods which may have body, these are the ones:
  - POST
  - PUT
  - PATCH
  - DELETE

If these names are used in the procedure names, then lower cased
names are used. e.g. `http-{nobody-methods}` will be `http-get` and others.

### [§3] HTTP request procedures

###### [!Function] `http-{nobody-methods}` _request-context_
###### [!Function] `http-{nobody-methods}` _uri_ _callback_
###### [!Function] `http-{bodied-methods}` _request-context_
###### [!Function] `http-{bodied-methods}` _uri_ _body_
###### [!Function] `http-{bodied-methods}` _uri_ _body_ _callback_

_request-context_ must be a HTTP request context.  
_uri_ must be a string representation of HTTP URI.--
_callback_ must be a procedure which accepts one argument of HTTP response.  
_body_ must be a bytevector, HTTP request payload or `#f`.

Synchronous HTTP request procedure. _callback_ is called when the response
is received.

These procedure blocks the process.

If the _body_ is a bytevector, then it is sent with content type of
`application/octet-stream`.

###### [!Function] `async-http-{nobody-methods}` _request-context_
###### [!Function] `async-http-{nobody-methods}` _uri_ _callback_
###### [!Function] `async-http-{bodied-methods}` _request-context_
###### [!Function] `async-http-{bodied-methods}` _uri_ _body_
###### [!Function] `async-http-{bodied-methods}` _uri_ _body_ _callback_

Asynchronous version of HTTP request procedures. The return value is
future object.

###### [!Function] `async-http-{nobody-methods}/client` _http-client_ _request-context_
###### [!Function] `async-http-{nobody-methods}/client` _http-client_ _uri_ _callback_
###### [!Function] `async-http-{bodied-methods}/client` _http-client_ _request-context_
###### [!Function] `async-http-{bodied-methods}/client` _http-client_ _uri_ _body_
###### [!Function] `async-http-{bodied-methods}/client` _http-client_ _uri_ _body_ _callback_

The same as asynchronous version of HTTP request procedures, this procedures
accept HTTP client instead of using default one provided by this library.

###### [!Function] `async-http-request` _request-context_
###### [!Function] `async-http-request/client` _http-client_ _request-context_

Underlying HTTP requesting procedure of all the above per method procedure.
These procedures can be used not to write manual dispatch by the HTTP method.

The `async-http-request` uses default HTTP client as underlying HTTP client,
described below section.

#### [§4] Default HTTP client

The default HTTP client is configured as below specification:

- Having pooling connection manager of max connection per route of `100`
- DNS timeout of `30` seconds
- Connection timeout of `60` seconds
- Read timeout of `120` seconds
- Always follows redirection


#### [§3] HTTP request context

A HTTP request context is a sub record of `<http:request>` defined in 
`(net http-client)`. It has extra fields `payload` and `callback`.

`payload` fields provides a convenient access to the `content-type` and
`body` fields of the `<http:request>`.

`callback` fields provides a callback for the response. 


###### [!Function] `http-request-context?` _obj_

Returns `#t` if the given _obj_ is a HTTP request context, otherwise `#f`.

###### [!Macro] `http-request-context-builder` _field*_ _..._

Builds a HTTP request context. For the detail of _field_, 
see [`(net http-client)`](#net.http-client).


#### [§4] HTTP request payload

The library provides a convenient framework to compose HTTP request body
and associated content type. HTTP request payload is a record which
contains 3 fields; `content-type`, `content` and `converter`.
The `content-type` is the content type value to be sent. The `content`
is the raw content of the reuqest payload, for example, a list of
key and value pairs. Then the `converter` convers the `content` to
a bytevector.

The library provides 3 most used content types:

- `application/octet-stream`
- `application/json`
- `application/x-www-form-urlencoded`

Users can make own request payload as well.

###### [!Record type] `<http-request-payload>`

The base record of HTTP request payload. The record has below 3 fields;

- `content-type`
  :Accessor: `http-request-payload-content-type`
- `content`
  :Accessor: `http-request-payload-content`
- `converter`
  :Accessor: `http-request-payload-converter`

###### [!Function] `http-request-payload?` _obj_

Returns `#t` if the given _obj_ is HTTP request payload, otherwise `#f`.

One of the supporting payload, `<json-request-payload>` is defined like this:

```scheme
(define-record-type <json-request-payload>
  (parent <http-request-payload>)
  (protocol (lambda (p)
              (define (json->string json)
                (let-values (((out e) (open-string-output-port)))
                  (json-write/normalized json out)
                  (e)))
              (define (json->bytevector json) (string->utf8 (json->string json)))
              (lambda (json)
                ((p "application/json" json json->bytevector))))))
```

###### [!Record type] `<octet-stream-request-payload>`
###### [!Function] `octet-stream-request-payload?` _obj_
###### [!Function] `octet-stream-payload` (_bv_ `bytevector?`)

HTTP request payload for octet stream.

###### [!Record type] `<json-request-payload>`
###### [!Function] `json-request-payload?` _obj_
###### [!Function] `json-payload` _json-sexp_

HTTP request payload for JSON object. The _json-sexp_ must be a valid
JSON S-expression.

NOTE: On Sagittarius, JSON S-expression is Chicken's egg style, it's not
the same as SRFI JSON. For more detail, see [`(text json)`](#text.json)

###### [!Record type] `<x-www-form-urlencoded-request-payload>`
###### [!Function] `x-www-form-urlencoded-request-payload?` _obj_
###### [!Function] `x-www-form-urlencoded-payload` _kv_

HTTP request payload for `application/x-www-form-urlencoded`.

_kv_ must be a list of key/value pair.

Example:
```scheme
(x-www-form-urlencoded-payload '(("key0" . "value0") ("key1" . "value1")))
;; -> represents `key0=value0&key1=value1`
```

Both key and value are encoded when it's converted to a bytevector.

#### [§3] HTTP response

HTTP response related procedures are re-export from `(net http-client)`,
for more details, see [HTTP request and response](#net.http-client.request)
