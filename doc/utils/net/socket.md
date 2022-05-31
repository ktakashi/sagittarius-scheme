[§2] (net socket) - Modern Socket library {#net.socket}
-------------

###### [!Library] `(net socket)` 

Providing, probably, modern socket library.

Most of the bindings defined in this library is identical as
`(sagittarius socket)`, so this document will describe only the
differences.

###### [!Function] `socket-options?`  _o_
###### [!Function] `tls-socket-options?`  _o_
###### [!Function] `server-tls-socket-options?`  _o_

Returns #t if the given _o_ is a `socket-options`,
`tls-socket-options`, or `server-tls-socket-options`, respectively.

The `socket-options?` returns #t for all 3 types.


###### [!Macro] `socket-options-builder`  _(fields_ _..._ _)_

Builds a socket-options object.
The `field` must be one or multiple of the followings:

_ai-family_
: See `ai-family` of `(sagittarius socket)`

_ai-socktype_
: See `ai-socktype` of `(sagittarius socket)`

_ai-flags_
: See `ai-flags` of `(sagittarius socket)`

_ai-protocol_
: See `ai-protocl` of `(sagittarius socket)`

_non-blocking?_
: Specifies if the socket is non blocking or not, default #t

_connection-timeout_
: Specifies connection timeout in micro seconds, default #f (infinite)

_read-timeout_
: Specifies read timeout, default #f (infinite) see `socket-set-read-timeout!` for more details.



###### [!Macro] `tls-socket-options-builder`  _(fields_ _..._ _)_

Builds a tls-socket-options object.
The `fields` must be the ones from `socket-options-builder` or
one or multiple of the followings:

_handshake_
: Specifes if handshake must be done or not. Default #t

_private-key_
: A client private key

_certificates_
: A list of client certificates, must be specified when `private-key` is specified

_certificate-verifier_
: A certificate verifier procedure

_sni\*_
: A list of server name indicators, a list of string

_alpn\*_
: A list of application layer protocol negotiation names



###### [!Macro] `server-tls-socket-options-builder`  _(fields_ _..._ _)_

Builds a server-tls-socket-options object.
The `fields` must be the ones from `tls-socket-options-builder` or
one or multiple of the followings:

_client-certificate-required?_
: Specifes if client certificate is required or not. Default #f

_trusted-certificates_
: A list of trusted certificates



###### [!Function] `make-client-socket`  _node_ _service_ _:optional_ _options_

Creates a client socket.
This is almost the same as the one from `(sagittarius socket)`, the only
difference is it takes _options_ to specify extra options, such as
read timeout or connection timeout.


###### [!Function] `make-client-tls-socket`  _node_ _service_ _:optional_ _options_

Creates a client socket.
This is almost the same as the one from `(rfc tls)`, the only
difference is it takes _options_ to specify extra options, such as
client certificate, alpn, etc.


###### [!Function] `make-server-socket`  _node_ _service_ _:optional_ _options_

Creates a client socket.
This is almost the same as the one from `(sagittarius socket)`, the only
difference is it takes _options_ to specify extra options, such as
read timeout or connection timeout.


###### [!Function] `make-server-tls-socket`  _node_ _service_ _:optional_ _options_

Creates a client socket.
This is almost the same as the one from `(rfc tls)`, the only
difference is it takes _options_ to specify extra options, such as
private key, etc.


###### [!Function] `socket-options->client-socket`  _options_ _node_ _service_

Creates a client socket from the given _options_, _node_ and
_service_.

This is a convenient procedure to make a socket from the options instead
of let user dispatch manually.


