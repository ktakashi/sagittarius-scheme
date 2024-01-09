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

#### [§4] Default HTTP client

The default HTTP client is configured as below specification:

- Having pooling connection manager of max connection per route of `100`
- DNS timeout of `30` seconds
- Connection timeout of `60` seconds
- Read timeout of `120` seconds
- Always follows redirection

