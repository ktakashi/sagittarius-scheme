[§2] (rfc http2) - HTTP/2 client {#rfc2.http}
-------------

###### [!Library] `(rfc http2)` 

This library provides simple client HTTP/2 defined in
[RFC 7540](https://tools.ietf.org/html/rfc7540).

The library does not support `Upgrade` header defined in HTTP/1.1. Thus
users must know if the server supports HTTP/2 or not, ahead.


Following is the simple example to send GET method to a HTTP/2 server.

``````````scheme
(import (rfc http2))

(define conn (make-http2-client-connection "twitter.com" "443" :secure? #t))
(http2-get conn "/")
;; -> returns 2 values
;; header
;; body
(close-http2-client-connection! conn)
``````````

###### [!Function] `http2-client-connection?`  _obj_

Returns #t if the given _obj_ is an HTTP/2 client connection,
otherwise #f.

###### [!Function] `make-http2-client-connection`  _server_ _port_ _:key_ _(secure?_ _#f)_ _user-agent_

Creates an HTTP/2 client connection.

The _server_ must be a string indicating an existing server name.

The _post_ must be a string of either port number or service.

The keyword argument _secure?_ specifies to use TLS or not.

The keyword argument _user-agent_ specifies the value of `user-agent`header. The default value is `Sagittarius-` followed by version number.

NOTE: The connection does not guess if it should use secure connection or
not by looking at port number or service name. Which means, even if you
specify `"https"` or `"443"` however _secure?_ keyword
argument must be passed.

NOTE2: Created connections hold opened socket.


###### [!Function] `close-http2-client-connection!`  _conn_

Closes given HTTP/2 connection.

### [§3] High level APIs

###### [!Function] `http2-get`  _conn_ _uri_ _:key_ _receiver_ _redirect-handler_ _:allow-other-keys_ _headers_

Sends HTTP/2 GET request to given _uri_ of HTTP/2 client
connection _conn_ and returns 2 values of the response, header
and content.

The rest arguments _headers_ must be a list of keyword and string.
The procedure sends these pairs as extra headers.

If the keyword argument _receiver_ is specified, then the procedure
uses given _receiver_ to receive data. The default value is
`(make-gzip-receiver)`.

If the keyword argument _redirect-handler_ is specified, then the procedure
uses given _redirect-handler_ to handle redirection.


###### [!Function] `http2-post`  _conn_ _uri_ _data_ _:key_ _receiver_ _redirect-handler_ _:allow-other-keys_ _headers_

Sends HTTP/2 POST request to given _uri_ of HTTP/2 client
connection _conn_ with given _data_  and returns 2 values of
the response, header and content. _data_ must be a bytevector.

The rest arguments _headers_ must be a list of keyword and string.
The procedure sends these pairs as extra headers.

The keyword arguments _receiver_ and _redirect-handler_ are the same
as `http2-get`.


###### [!Function] `http2-head`  _conn_ _uri_ _:key_ _receiver_ _redirect-handler_ _:allow-other-keys_ _headers_

Sends HTTP/2 HEAD request to given _uri_ of HTTP/2 client
connection _conn_ and returns 2 values of the response, header
and content.

The rest arguments _headers_ must be a list of keyword and string.
The procedure sends these pairs as extra headers.

The keyword arguments _receiver_ and _redirect-handler_ are the same
as `http2-get`.


###### [!Macro] `http2-request`  _conn_ _method_ _uri_ _sender_ _receiver_ _:key_ _redirect-handler_

Sends an HTTP/2 request to given HTTP/2 client connection _conn_.

_method_ must be a symbol.

_sender_ must be a sender described below section.

_receiver_ must be a receiver described below section.

_redirect-handler_ is the same as `http2-get`.

The procedure returns a list of response header and content.


#### [§4] HTTP/2 sender

A sender is a procedure which accepts 2 arguments, internal HTTP/2 stream and
flag. At this moment, the internal HTTP/2 stream is not exposed so users cannot
create own sender.

###### [!Function] `http2-headers-sender`  _header_ _..._

Returns a sender which sends given _header_ as HTTP/2 header.

###### [!Function] `http2-data-sender`  _data_

Returns a sender which sends given _data_ as HTTP/2 data.

###### [!Function] `http2-composite-sender`  _sender_ _..._

Returns a composite sender.

#### [§4] HTTP/2 receiver

A receiver is a procedure which accepts 4 arguments, internal HTTP/2 stream,
header, frame and flag. The same restriction as sender is applied, thus
users cannot create own receiver, at this moment.

###### [!Function] `http2-data-receiver`  _sink_ _flusher_

Returns a receiver which receives HTTP/2 data frame and put the
value into _sink_. When the stream reaches to the end, then _flusher_is called with _sink_.


###### [!Function] `http2-binary-receiver` 

Returns a receives which receives HTTP/2 data frame as a bytevector.

###### [!Function] `http2-null-receiver` 

Returns a receives which receives HTTP/2 data frame and returns empty
bytevector.

###### [!Function] `http2-gzip-receiver`  _receiver_

Returns a receives which receives HTTP/2 data frame. If the data frame
is compressed to gzip, then the returning receiver expand the data and forward
to the given _receiver_. If the data frame is not compressed, then it
simply forward the data frame to the given _receiver_.


###### [!Function] `make-gzip-receiver` 

Convenient procedure whose definition is the following:

``(http2-gzip-receiver (http2-binary-receiver))``



