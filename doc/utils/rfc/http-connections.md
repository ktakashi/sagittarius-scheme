[§2] (rfc http-connections) - HTTP client utilities {#rfc.http-connections}
-------------

###### [!Library] `(rfc http)` 

This library provides common interfaces of 
[`(rfc http)`](#rfc.http) and
[`(rfc http2)`](#rfc.http2) library.


###### [!Function] `http-connection?`  _obj_

Returns #t if the given object is an HTTP connection.

###### [!Function] `http1-connection?`  _obj_

Returns #t if the given object is an HTTP/1.1 connection.

###### [!Function] `make-http1-connection`  _server_ _secure?_

Creates an HTTP/1.1 connection with destination server _server_.

If the _secure?_ is true value, then it uses TLS socket.

The HTTP/1.1 connection uses `http-request` defined in `(rfc http)`library to send an request.


###### [!Function] `http2-connection?`  _obj_

Returns #t if the given object is an HTTP/2 connection.

###### [!Function] `make-http2-connection`  _server_ _secure?_

Creates an HTTP/2 connection with destination server _server_.

If the _secure?_ is true value, then it uses TLS socket.

The HTTP/1.1 connection uses `http2-request` defined in `(rfc http2)`library to send an request.


###### [!Function] `http-connection-secure?`  _http-connection_

Returns #t if the given _http-connection_ uses TLS socket to
connect target server.

###### [!Function] `http-connection-server`  _http-connection_

Returns destination server of the _http-connection_ as string.

###### [!Function] `http-connection-server&port`  _http-connection_

Returns destination server and port of the _http-connection_.

###### [!Function] `open-http-connection!`  _http-connection_

Opens given _http-connection_ and returns it.

###### [!Function] `close-http-connection!`  _http-connection_

Closes given _http-connection_ and returns it.

###### [!Function] `http-request`  _http-connection_ _method_ _path_ _._ _opt_

Sends HTTP request to the path _path_ of destination server
of _http-connection_ with HTTP method _methos_.

The rest value of _opt_ is passed to underling request sending procedure.

The procedure returns 3 values, HTTP status, HTTP header and content as
bytevector.


###### [!Generic] `http-null-receiver` 
###### [!Method] `http-null-receiver`  _(conn_ _http1-connection)_
###### [!Method] `http-null-receiver`  _(conn_ _http2-connection)_

Generic method of null http receiver.

The `http-null-receiver` forwards `http-null-receiver` defined
in `(rfc http)` for `http1-connection` and
`http2-null-receiver` defined
in `(rfc http2)` for `http2-connection`

###### [!Generic] `http-oport-receiver` 
###### [!Method] `http-oport-receiver`  _(conn_ _http1-connection)_ _sink_ _flusher_
###### [!Method] `http-oport-receiver`  _(conn_ _http2-connection)_ _sink_ _flusher_

Generic method of output port http receiver.

The `http-oport-receiver` forwards `http-oport-receiver` defined
in `(rfc http)` for `http1-connection` and
`http2-data-receiver` defined in `(rfc http2)` for
`http2-connection`

###### [!Generic] `http-blob-sender` 
###### [!Method] `http-blob-sender`  _(conn_ _http1-connection)_ _blob_
###### [!Method] `http-blob-sender`  _(conn_ _http2-connection)_ _blob_

Generic method of http blob sender.

The `http-blob-sender` forwards `http-blob-sender` defined
in `(rfc http)` for `http1-connection` and
`http2-data-sender` defined in `(rfc http2)` for
`http2-connection`

###### [!Generic] `http-string-sender` 
###### [!Method] `http-string-sender`  _(conn_ _http1-connection)_ _string_
###### [!Method] `http-string-sender`  _(conn_ _http2-connection)_ _string_

Generic method of http string sender.

The `http-string-sender` forwards `http-string-sender` defined
in `(rfc http)` for `http1-connection` and
`http2-data-sender` defined in `(rfc http2)` with given
_string_ converted by `string->utf8` procedure defined
for `http2-connection`

###### [!Generic] `http-null-sender` 
###### [!Method] `http-null-sender`  _(conn_ _http1-connection)_
###### [!Method] `http-null-sender`  _(conn_ _http2-connection)_

Generic method of http null sender.

The `http-null-sender` forwards `http-null-sender` defined
in `(rfc http)` for `http1-connection` and does nothing
for `http2-connection`.


