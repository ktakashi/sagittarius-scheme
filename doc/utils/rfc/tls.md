[§2] (rfc tls) - TLS protocol library {#rfc.tls}
-------------

###### [!Library] `(rfc tls)` 

This library provides TLS protocol socket APIs.

CAUTION: This library is not well tested. So it may have security hole or
incompatible behaviour. If you find such bugs, please let me know.


###### [!Function] `make-client-tls-socket`  _node_ _service_ _:key_ _(prng_ _(secure-randome_ _RC4))_ _
_ _(version_ _*tls-version-1.2*)_ _(session_ _#f)_ _(handshake_ _#t)_ _
_ _(certificates_ _'())_ _(private-key_ _#f)_ _
_ _:allow-other-keys_ _opt_

_node_ and _service_ must be string.

Creates a client TLS socket. _node_, _service_ and _opt_ will be
passed to `make-client-socket` described in
[(sagittarius socket)](#lib.sagittarius.socket).

The keyword argument _prng_ specifies which pseudo random algorithm will
be used for generating security parameters.

The keyword argument _version_ specifies which TLS protocol version will be
used for negotiation. However the real version will be decided by target server.

The keyword argument _session_ is for future extension, so do not specify.

If the keyword argument _handshake_ #f then the procedure won't do
TLS handshake after the socket creation so users must do it manually with
`tls-client-handshake` procedure described below.


The keyword argument _certificates_ is for certificate request message.
The value must be a list of x509 certificates. If the certificates argument
is null, then the procedures send empty certificate list to the server as
a response of certificate request message.

The keyword argument _private-key_ specifies which private key is used.
The value must be private key object described in ["(crypto)"](#crypto).
This is needed if the target server only supports RSA key exchange protocol.


###### [!Function] `tls-client-handshake`  _tls-socket_

Do client side handshake and return a TLS socket. The procedure must
\*NOT\* be called if the socket is created with _handshake_ keyword argument
#t.

CAUTION: This procedure needs to be called only once and calling more than once
might cause infinite loop or raise an error.


###### [!Function] `make-server-tls-socket`  _service_ _certificates_ _:key_ _(prng_ _(secure-randome_ _RC4))_ _
_ _(version_ _*tls-version-1.2*)_ _
_ _(private-key_ _#f)_ _(authorities_ _'())_ _
_ _:allow-other-keys_ _opt_

_service_ must be string. _certificates_ must be a
list of x509 certificate.

Creates a server TLS socket. _service_ and _opt_ will be passed to
`make-server-socket` described in 
[(sagittarius socket)](#lib.sagittarius.socket).

The keyword arguments _prng_ and _version_ are the same meaning as
`make-client-tls-socket`.

The keyword argument _private-key_ is used the same as client socket.
The difference is that it is used also for Deffie-Hellman key exchange.
If this is not specified, then key exhange is done anonymously.
It is strongly recomended to specify this keyword argument.

The keyword argument _authorities_ must be a list of x509 certificate and
if this is not empty list then the server socket will send certificate request
message to the client. 

CAUTION: the _authorities_ keyword argument currently doesn't check
the certificate signature but only issuer DN.


###### [!Function] `tls-socket-send`  _tls-socket_ _bytevector_ _flags_

_tls-socket_ must be the socket created by the procedure
`make-client-tls-socket`.

_flags_ must be non negative exact integer.

Sends given bytevector to _tls-socket_. _flags_ are described in
the section [(sagittarius socket)](#lib.sagittarius.socket). The packet
will be encrypted by _tls-socket_.


###### [!Function] `tls-socket-recv`  _tls-socket_ _size_ _flags_

_tls-socket_ must be the socket created by the procedure
`make-client-tls-socket`.

_size_ and _flags_ must be non negative exact integer.

Receives decrypted packet from _tls-socket_. _size_ indicates how many
octets the procedure should receive, however it might return less octet.
_flags_ will be passed to `socket-recv`.

NOTE: _tls-socket_ have its own buffer to return the value, so that the
procedure can take _size_ argument.


###### [!Function] `tls-socket-close`  _tls-socket_

_tls-socket_ must be the socket created by the procedure
`make-client-tls-socket`.

Sends close notify alert to the socket and close it.


###### [!Function] `tls-socket-closed?`  _tls-socket_

_tls-socket_ must be the socket created by the procedure
`make-client-tls-socket`.

Returns #t if the given socket is closed, otherwise #f.

NOTE: this procedure checks if session is closed. So the real socket might not
be closed yet.


###### [!Function] `tls-socket-accept`  _tls-socket_ _:key_ _(handshake_ _#t)_ _(raise-error_ _#t)_

_tls-socket_ must be a server TLS socket created by
`make-server-tls-socket`.

Wait for an incoming connection request and returns a fresh connected client
socket.

If the keyword argument _handshake_ is #f then the handshake must be done
by manually with `tls-server-handshake` described blow.

The keyword argument _raise-error_ will be passed to
`tls-server-handshake`.


###### [!Function] `tls-server-handshake`  _tls-socket_ _:key_ _(raise-error_ _#t)_

Do server side TLS handshake and returns a TLS socket. The procedure must
\*NOT\* be called if the socket is created with _handshake_ keyword argument
#t.

If the keyword argument _raise-error_ is #f then it won't raise an error
when something happens.


###### [!Function] `tls-socket-peer`  _tls-socket_

Return peer of given _tls-socket_ or #f.

For more details, see [(sagittarius socket)](#lib.sagittarius.socket).


###### [!Function] `tls-socket-peer`  _tls-socket_

Return peer of given _tls-socket_ or #f. This procedure is TLS
version of `socket-peer`.

For more details, see [(sagittarius socket)](#lib.sagittarius.socket).


###### [!Function] `tls-socket-name`  _tls-socket_

Return peer of given _tls-socket_ or #f. This procedure is TLS
version of `socket-name`.

For more details, see [(sagittarius socket)](#lib.sagittarius.socket).


###### [!Function] `tls-socket-info-values`  _tls-socket_

Return peer of given _tls-socket_ or #f. This procedure is TLS
version of `socket-info-values`.

For more details, see [(sagittarius socket)](#lib.sagittarius.socket).


###### [!Constant] `*tls-version-1.2*` 

Constant value of `#x0303` for TLS 1.2

###### [!Constant] `*tls-version-1.1*` 

Constant value of `#x0302` for TLS 1.1

###### [!Constant] `*tls-version-1.0*` 

Constant value of `#x0301` for TLS 1.0

###### [!Function] `tls-socket-port`  _tls-socket_ _:optional_ _(close?_ _#t)_

_tls-socket_ must be the socket created by the procedure
`make-client-tls-socket`.

Returns input/output-port of given _tls-socket_.

If optional argument _close?_ is #f then it won't close the socket when the
port is closed or GCed.


###### [!Function] `tls-socket-input-port`  _tls-socket_
###### [!Function] `tls-socket-output-port`  _tls-socket_

Convert the given TLS socket to input and output port, respectively.

The given socket won't be closed when the port is closed or GCed. So it is the
users responsibility to close.


### [§3] Integration methods

The methods listed below are convenience methods to use TLS socket and
usual socket without changing code.

###### [!Method] `socket-close`  _(socket_ _<tls-socket>)_
###### [!Method] `socket-send`  _(socket_ _<tls-socket>)_ _data_ _:optional_ _(flags_ _0)_
###### [!Method] `socket-recv`  _(socket_ _<tls-socket>)_ _size_ _:optional_ _(flags_ _0)_
###### [!Method] `socket-accept`  _(socket_ _<tls-socket>)_ _._ _opt_
###### [!Method] `socket-accept`  _(socket_ _<tls-socket>)_ _(key_ _<keyword>)_ _._ _dummy_
###### [!Method] `call-with-socket`  _(socket_ _<tls-socket>)_ _proc_
###### [!Method] `socket-peer`  _(socket_ _<tls-socket>)_
###### [!Method] `socket-name`  _(socket_ _<tls-socket>)_
###### [!Method] `socket-info-values`  _(socket_ _<tls-socket>)_
###### [!Method] `socket-port`  _(socket_ _<tls-socket>)_ _:optional_ _(close?_ _#t)_
###### [!Method] `socket-input-port`  _(socket_ _<tls-socket>)_
###### [!Method] `socket-output-port`  _(socket_ _<tls-socket>)_
