[§2] (rfc tls) - TLS protocol library {#rfc.tls}
-------------

###### [!Library] `(rfc tls)` 

This library provides TLS protocol socket APIs.

### [§3] Implementation notes:

The library is implemented different libraries per platform.
Some platform may not support a particular TLS version(s).

On Windows
: The library is implemented atop SChannel (SSP). The supporting TLS
  versions varies depending on the version of Windows. For the
  detailed information please refer Microsoft's support page.
  e.g. [Protocols in TLS/SSL (Schannel SSP)](https://learn.microsoft.com/en-us/windows/win32/secauthn/protocols-in-tls-ssl--schannel-ssp-)
  
On POSIX / Linux
: The library is implemented atop OpenSSL. This library doesn't
  specify the version of OpenSSL, it detects during the build process.
  The library specifies to use the latest TLS version, however,
  it doesn't guarantee using **the** latest due to the variety of
  reasons, e.g. older version of OpenSSL.

If you require to use a specific version of TLS, however this library
only can do its best effort.

###### [!Function] `make-client-tls-socket`  _node_ _service_ :key _handshake_ _certificates_ _private-key_ _certificate-verifier_ :allow-other-keys

_node_ and _service_ must be string.

Creates a client TLS socket. _node_, _service_ and _opt_ will be
passed to `make-client-socket` described in
[(sagittarius socket)](#lib.sagittarius.socket).

The keyword argument _prng_ specifies which pseudo random algorithm will
be used for generating security parameters.

If the keyword argument _handshake_ #f then the procedure won't do
TLS handshake after the socket creation so users must do it manually with
`tls-client-handshake` procedure described below.

The keyword argument _certificates_ is for certificate request message.
The value must be a list of x509 certificates. If the certificates argument
is null, then the procedures send empty certificate list to the server as
a response of certificate request message.

The keyword argument _private-key_ specifies which private key is used.
The value must be private key object described in 
["(sagittarius crypto keys)"](#sagittarius.crypto.keys).
This is needed if the target server only supports RSA key exchange protocol.

The keyword argument _certificate-verifier_ must be either boolean or a procedure
which accepts 3 arguments, certificate depth, previous error and certificate 
as a bytevector. The procedure should check the passed certificate is valid
or not.

###### [!Function] `tls-client-handshake`  _tls-socket_

Do client side handshake and return a TLS socket. The procedure must
**NOT** be called if the socket is created with _handshake_ keyword argument
#t.

CAUTION: This procedure needs to be called only once and calling more than once
might cause infinite loop or raise an error.


###### [!Function] `make-server-tls-socket`  _service_ _certificates_ :key _private-key_ _authorities_ _client-certificate-required?_ _certificate-verifier_ :allow-other-keys

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
The _private-key_ should be specified. This is optional due to the
backward compatibility.

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


###### [!Function] `tls-socket-recv`  _tls-socket_ _size_ :optional (_flags_ `0`)

_tls-socket_ must be the socket created by the procedure
`make-client-tls-socket`.

_size_ and _flags_ must be non negative exact integer.

Receives decrypted packet from _tls-socket_. _size_ indicates how many
octets the procedure should receive, however it might return less octet.
_flags_ will be passed to `socket-recv`.

NOTE: _tls-socket_ have its own buffer to return the value, so that the
procedure can take _size_ argument.

###### [!Function] `tls-socket-recv!`  _tls-socket_ _bv_ _start_ _len_ :optional (_flags_ `0`)

Receives decrypted packet from _tls-socket_ into _bv_.

###### [!Function] `tls-socket-pending?`  _tls-socket_ 

Returns `#t` if the _tls-socket_ has pending bytes to receive, otherwise `#f`.


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


###### [!Function] `tls-socket-accept`  _tls-socket_ :key (handshake `#t`) (_raise-error_ `#t`)

_tls-socket_ must be a server TLS socket created by
`make-server-tls-socket`.

Wait for an incoming connection request and returns a fresh connected client
socket.

If the keyword argument _handshake_ is #f then the handshake must be done
by manually with `tls-server-handshake` described blow.

The keyword argument _raise-error_ will be passed to
`tls-server-handshake`.


###### [!Function] `tls-server-handshake`  _tls-socket_ :key (_raise-error_ `#t`)

Do server side TLS handshake and returns a TLS socket. The procedure must
**NOT** be called if the socket is created with _handshake_ keyword argument
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

###### [!Function] `tls-socket-port`  _tls-socket_ :optional (_close?_ `#t`)

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

###### [!Method] `socket-close`  (_socket_ `<tls-socket>`)
###### [!Method] `socket-send`  (_socket_ `<tls-socket>`) _data_ :optional (_flags_ `0`)
###### [!Method] `socket-recv`  (_socket_ `<tls-socket>`) _size_ :optional (_flags_ `0`)
###### [!Method] `socket-accept`  (_socket_ `<tls-socket>`) . _opt_
###### [!Method] `socket-accept`  (_socket_ `<tls-socket>`) (_key_ `<keyword>`) . _dummy_
###### [!Method] `call-with-socket`  (_socket_ `<tls-socket>`) _proc_
###### [!Method] `socket-peer`  (_socket_ `<tls-socket>`)
###### [!Method] `socket-name`  (_socket_ `<tls-socket>`)
###### [!Method] `socket-info-values`  (_socket_ `<tls-socket>`)
###### [!Method] `socket-port`  (_socket_ `<tls-socket>`) :optional (_close?_ `#t`)
###### [!Method] `socket-input-port`  (_socket_ `<tls-socket>`)
###### [!Method] `socket-output-port`  (_socket_ `<tls-socket>`)
