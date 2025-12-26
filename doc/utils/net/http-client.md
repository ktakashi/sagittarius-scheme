[§2] (net http-client) - Modern HTTP client {#net.http-client}
-------------

###### [!Library] `(net http-client)` 

Providing, probably, modern HTTP client.

The `(net http-client)` provides a modern HTTP client, which handles
connection pooling, client certificate, redirect and cookie. The client
also provides asynchronous calling atop future object of
`(util concurrent)`.

The following example shows how to use in a nutshell.

* @[-[Http Client](../../../example/net/http-client0.scm)]

### [§3] HTTP client

###### [!Function] `http:client?`  _o_

Returns `#t` if the given _o_ is a HTTP client, otherwise `#f`.

###### [!Function] `http:client-send` (_client_ `http:client?`) (_request_ `http:request?`)

Sends the given _request_ via the _client_ and returns `http:response`.

This procedure is synchronous procedure, so it blocks.

###### [!Function] `http:client-send-async` (_client_ `http:client?`) (_request_ `http:request?`)

Sends the given _request_ via the _client_ and returns a future which
returns `http:response`.

###### [!Function] `http:client-shutdown!` (_client_ `http:client?`) :optional (_shutdown-executor?_ `#t`)

Shutdowns the given _client_. If the optional argument _shutdown-executor?_
is specified to `#f`, then the underlying executor won't be shutdown.

If the underlying executor is the default executor, then it won't be shutdown,
even if the optional argument is true value.

###### [!Macro] `http:client-builder`  _field_ _..._

A constructor macro to build a HTTP client.

The _field_s must be one or more of the following:

`follow-redirects`
: Specifies redirection strategy, the value must be a valid value of `http:redirect`.

`cookie-handler`
: Specifies a cookie handler. Currently, `http:make-default-cookie-handler` is the only option.

`connection-manager`
: Specifies a connection manager. See Connection Manager section for more detail.

`version`
: Specifies a HTTP version. The value must be a valid value of `http:version`. The client handles both HTTP/1.1 and HTTP/2 by default.

`executor`
: Specifies an executor. The the default executor has 5 threads, but it'd be shared by all the client created without this field, so if you want to use the client in a intense situation, it's a good idea to specify own executor.



###### [!Macro] `http:version`  _value_

A macro checks if the given _value_ is a valid supporting HTTP
version.

The value must be either `http/1.1` or `http/2`. If the later one
is specified, it first tries to connect with HTTP/2, and falls back to
HTTP/1.1 if HTTP/2 is not avaiable.

HTTP/2 works only on TLS environment, as the client uses ALPN to detect the
version.


###### [!Macro] `http:redirect`  _value_

A macro checks if the given _value_ is a valid supporting redirect
strategy.

The value must be one of `never`, `always` or `normal`. The
default value is `never`.

`never` 
: Won't redirect, it is suitable if you want to handle 3xx result
  manually.

`always`
: Redirects as long as the 3xx status is returned. This means
  insecure redirection may happen (i.e. HTTPS to HTTP)

`normal`
: Redirects 3xx status if the scheme is the same or more secure.


### [§3] HTTP request and response {#net.http-client.request}

###### [!Function] `http:request?`  _o_

Returns #t if the given _o_ is an HTTP request object, otherwise #f.

###### [!Macro] `http:request-builder`  _field*_ _..._

Builds a HTTP request object.

The `field` must be one or more the followings.

`uri`
: The URI of the request, it has to be an URI object of `(net uri)` or valid URI string. This field is mandatory.

`method`
: A valid HTTP method. Default value is `GET`.

`content-type`
: A content type header, this is used when the method is allowed to have body. Default value `"application/octet-stream"`.

`body`
: Content of the request. The value must be either a bytevector ot binary input port.

`headers`
: A list of headers or a HTTP header object. If it's a list of header, then the element must contain a pair of name and values.

`cookies`
: A list of cookies. The elements must be cookie object of `(rfc cookie)`

`auth`
: An authentication provider. Must be a thunk providing the `Authorization` 
  header value.


###### [!Function] `http:headers?`  _o_

Returns #t if the given _o_ is a HTTP headers object, otherwise #f.

###### [!Function] `http:make-headers` 

Make a HTTP headers object.

A HTTP headers object can contain multiple values on one headers.


###### [!Function] `http:headers-ref*`  _headers_ _name_

Retrieve a list of value of the header _name_ from _headers_.

If the _name_ doesn't exists, then it returns `()`.

###### [!Function] `http:headers-ref`  _headers_ _name_

Retrieve the first value of the header _name_ from _headers_.

If the _name_ doesn't exists, then it returns `#f`.

###### [!Function] `http:headers-set!`  _headers_ _name_ _value_

Sets the single value of _value_ into the _headers_ with
header name of _name_.

###### [!Function] `http:headers-add!`  _headers_ _name_ _value_

Adds the _value_ in the _headers_ to the header value
list of _name_.

If the _name_ doesn't exist, then it works the same as
`http:headers-set!`.


###### [!Function] `http:headers-names`  _headers_

Returns a list of header names of the given _headers_

###### [!Function] `http:response?`  _o_

Returns #t if the given _o_ is an HTTP response object, otherwise #f.

###### [!Function] `http:response-status`  _response_

Returns the response HTTP status code.

The code is a string.


###### [!Function] `http:response-headers`  _response_

Returns the response HTTP headers object.

###### [!Function] `http:response-cookies`  _response_

Returns a list of the response cookie, if exists.

The element of the list is a cookie object of `(rfc cookie)`.


###### [!Function] `http:response-body`  _response_

Returns the body of the response.
The value is bytevector.


#### [§4] Authorization header value providers

Authorization header value providers can be used for `auth` field of the
HTTP request.

###### [!Function] `http:request-basic-auth` _username_ _password_

Provides basic auth value provider of the given _username_ and _password_.

###### [!Function] `http:request-basic-bearer` _token_

Provides bearer auth value provider of the given _token_.


### [§3] Connection Manager

A connection manager manages the connections of a HTTP client. By default,
we support these three types of connection managers; ephemeral, pooling, and
logging.

The ephemeral connection manager creates a connection per request and
discards it. This connection manager should only be used in disposable
scripts.

The pooling connection manager pools the created connection and try to
reuse if possible.

Below example shows how to create a HTTP client with pooling connection
manager.

```scheme
(define pool-config
  (http-pooling-connection-config-builder
    (connection-timeout 1000)     ;; 1 sec, connection timeout
    (max-connection-per-route 20) ;; pooling connection number per route
    (read-timeout 3000)           ;; 3 sec, read timeout
    (dns-timeout 1000)            ;; 1 sec, DNS lookup timeout
    (time-to-live 120)            ;; 120 sec, expiration time of a connection
	))
(define http-client
  (http:client-builder
    (connection-manager (make-http-pooling-connection-manager pool-config))))
```


###### [!Function] `make-http-default-connection-manager` 

Creates a default connection manager. This is at this moment an
ephemeral connection manager without any timeout or key configuration.


#### [§4] Ephemeral Connection Manager

###### [!Function] `http-connection-config?`  _obj_

Returns #t if the _obj_ is an instance of
`http-connection-config`, otherwise #f.

###### [!Macro] `http-connection-config-builder`  _field_ _..._

A constructor macro to build a `http-connection-config` record.

The _field_s must be one or more of the following:

`connection-timeout`
: Specifies connection timeout in milli second.

`read-timeout`
: Specifies read timeout in milli second.

`key-manager`
: Specifies a key manager. See Key Manager section for more detail.



###### [!Function] `http-ephemeral-connection-manager?`  _obj_

Returns #t if the _obj_ is an ephemeral connection manager,
otherwise #f.

###### [!Function] `make-http-ephemeral-connection-manager`  _config_

_Config_ must be a `http-connection-config` record instance.

Creates an ephemeral connection manager.

#### [§4] Pooling Connection Manager

###### [!Function] `http-pooling-connection-config?`  _obj_

Returns #t if the _obj_ is an instance of
`http-pooling-connection-config`, otherwise #f.

The `http-pooling-connection-config` record is a child record of
`http-connection-config`.

###### [!Macro] `http-pooling-connection-config-builder`  _field_ _..._

A constructor macro to build a `http-pooling-connection-config`record.

The _field_s must be one or more of the following and the ones from
`http-connection-config-builder`:

`connection-request-timeout`
: Specifies connection request timeout in milli second. The timeout is only
  for the waiting time of the connection retrieval from the pool.

`max-connection-per-route`
: Specifies pool size per route. A route is defined with the combination of
  hostname and port. Default value is 5.

`time-to-live`
: Specifies the life time of the pooled connection in seconds

`delegate-provider`
: Specifies connection manager delegate provider. Default value is
  `default-delegate-connection-manager-provider`

A delegate connection manager is an underlying connection manager of
the pooling connection manager. The delegate creates and closes the
actual connection.


###### [!Function] `http-pooling-connection-manager?`  _obj_

Returns #t if the _obj_ is a pooling connection manager,
otherwise #f.

###### [!Function] `make-http-pooling-connection-manager`  _config_

_Config_ must be a `http-pooling-connection-config`record instance.

Creates a pooling connection manager.

#### [§4] Delegate Connection Manager providers

A delegate connection provider is a procedure which accepts one
argument _config_ and returns a connection manager.

###### [!Function] `default-delegate-connection-manager-provider`  _config_

Synonym of `make-http-ephemeral-connection-manager`.

### [§3] Key Manager

Key manager manages client certificate of HTTP connections. A key manager
may contain multiple of key providers. A key provider should provide a
list whose the first element is a private key and the rest are certificate
chain.

The following example shows how to make a key manager with multiple
key providers.

```scheme
(import (rnrs)
        (net http-client)
        (rfc base64)
        (security keystore))
(define (host-a p)
  (define node (socket-parameter-socket-node p))
  (cond ((string=? node "host-a.com") "host-a-key-alias")
        (else #f)))
(define (host-b p)
  (define node (socket-parameter-socket-node p))
  (and (string-suffix? ".host-b.com" node) "host-b-key-alias"))
(define keystores
  ;; keystore file (PKCS12 base64 encoded), store pass, key pass, alias selector
  `(("host-a.b64" "password0" "password0a" ,host-a)
    ("host-b.b64" "password1" "password1a" ,host-b)))

(define (->keystore-key-provider keystore-info)
  (let ((file (car keystore-info))
        (storepass (cadr keystore-info))
        (keypass (caddr keystore-info))
        (strategy (cadddr keystore-info)))
    (make-keystore-key-provider
     (call-with-input-file file
       (lambda (in)
         (let ((bin (open-base64-decode-input-port in)))
           (load-keystore 'pkcs12 bin storepass)))
       :transcoder #f)
     keypass
     strategy)))

(make-key-manager (map ->keystore-key-provider keystores))
```

###### [!Function] `key-manager?`  _obj_

Returns #t if the _obj_ is a key manager,
otherwise #f.

###### [!Function] `make-key-manager`  _key-providers_
###### [!Function] `list->key-manager`  _key-providers_

_Key-providers_ must be a list of key provider.

Creates a key manager.

###### [!Function] `key-manager`  _key-provider_ ...

_Key-provider_ must be a provider.

Creates a key manager.


#### [§4] Key Provider

Key provider provides keys. The key can be retrieved from anywhere but
must be a format of a list of private key and certificate chain.

We provide keystore key provider by default. If you need other key
provider, you need to create a custom one.

###### [!Record Type] `<key-provider>` 

An abstruct record type of key provider. Users must inherit this
record type to create a custom key provider.

This record type has a filed named `key-retrievers`.


###### [!Function] `key-provider?`  _obj_

Returns #t if the _obj_ is a key provider
otherwise #f.

###### [!Function] `make-key-provider` _key-obtainer_

_key-obtainer_ must be a procedure which receives one argument,
`socket-parameter`.

Creates a key provider.

###### [!Function] `key-provider-key-retrievers`  _key-provider_

Returns the value of field `key-retrievers` of the key provider
_key-provider_.

This procedure is not meant to be used by a user of HTTP client.


###### [!Function] `keystore-key-provider?`  _obj_

Returns #t if the _obj_ is a keystore key provider
otherwise #f.

###### [!Function] `make-keystore-key-provider`  _keystore_ _password_ _alias-provider_

_Keystore_ must be a keystore object of `(security keystore)`.
_password_ must be a string of valid key password.
_alias-provider_ must be a procedure which accepts one argument,
socket parameter, and returns either string of key alias or #f.

Creates a key provider with one key retriever.

###### [!Function] `keystore-key-provider-add-key-retriever!`  _keystore-key-provider_ _password_ _alias-provider_

Add a key retriever to the _keystore-key-provider_ and returns
_keystore-key-provider_.

This procedure is convenient if you have multiple keys in one keystore.

#### [§4] Socket parameter

Socket parameter is used to determine which private key to be used.
The object contains basic socket information.

###### [!Function] `socket-parameter?` _obj_

Returns `#t` if the given _obj_ is a socket parameter, otherwise `#f`.

###### [!Function] `socket-parameter-socket-hostname` (_sp_ `socket-parameter?`)
###### [!Function] `socket-parameter-socket-ip-address` (_sp_ `socket-parameter?`)
###### [!Function] `socket-parameter-socket-node` (_sp_ `socket-parameter?`)
###### [!Function] `socket-parameter-socket-service` (_sp_ `socket-parameter?`)

Returns `hostname`, `ip-address`, `node` and `service` respectively.

The first 2 values are peer information, the latter 2 are local information.
