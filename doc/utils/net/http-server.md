[§2] (net http-server) - HTTP server {#net.http-server}
-------------

###### [!Library] `(net http-server)`

Providing HTTP server.

The implementation of the server is based on the `(net server)`.

Example how to set up:

* @[-[Http Server](../../../example/net/http-server0.scm)]

### [§3] Exchange context

Each HTTP interaction has an exchange context shared by the request and the
effective response.

###### [!Function] `(http-server:request-exchange request)`

Returns the exchange associated with `request`.

###### [!Function] `(http-server:response-exchange response)`

Returns the exchange associated with `response` if present, otherwise `#f`.

###### [!Function] `(http-server:exchange? obj)`

Returns `#t` if `obj` is an HTTP exchange object.

###### [!Function] `(make-http-server:exchange connection-info)`

Creates an exchange object with initial lifecycle state `created`.

###### [!Function] `(http-server:exchange-attribute-ref exchange key :optional default)`

Looks up an exchange attribute.

###### [!Function] `(http-server:exchange-attribute-set! exchange key value)`

Sets an exchange attribute.

###### [!Function] `(http-server:exchange-connection-info exchange)`

Returns the connection information object for the exchange.

###### [!Function] `(http-server:exchange-state exchange)`

Returns the exchange lifecycle state.

### [§3] Connection information

Connection information is shared by exchanges created on the same connection.

###### [!Function] `(http-server:connection-info? obj)`

Returns `#t` if `obj` is a connection information object.

###### [!Function] `(make-http-server:connection-info remote :optional peer-certificate selected-alpn protocol)`

Creates a connection information object.

###### [!Function] `(http-server:connection-info-remote connection-info)`

Returns the remote endpoint information.

###### [!Function] `(http-server:connection-info-peer-certificate connection-info)`

Returns the peer certificate, or `#f` when unavailable.

###### [!Function] `(http-server:connection-info-selected-alpn connection-info)`

Returns the negotiated ALPN value, or `#f`.

###### [!Function] `(http-server:connection-info-protocol connection-info)`

Returns the protocol tag associated with the connection information.
