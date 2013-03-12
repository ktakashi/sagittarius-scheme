@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.tls"]{(rfc tls) - TLS protocol library}

@define[Library]{@name{(rfc tls)}}
@desc{This library provides TLS protocol socket APIs.

CAUTION: This library is not well tested. So it may have security hole or
incompatible behaviour. If you find such bugs, please let me know.
}

@define[Function]{@name{make-client-tls-socket}
 @args{node service :key (prng (secure-randome RC4))
 (version *tls-version-1.2*) (session #f) (handshake #t)
 @;(cipher-suites *cipher-suites*)
 (certificates '()) (private-key #f)
 :allow-other-keys opt}}
@desc{@var{node} and @var{service} must be string.

Creates a client TLS socket. @var{node}, @var{service} and @var{opt} will be
passed to @code{make-client-socket} described in
@secref["lib.sagittarius.socket"]{(sagittarius socket)}.

The keyword argument @var{prng} specifies which pseudo random algorithm will
be used for generating security parameters.

The keyword argument @var{version} specifies which TLS protocol version will be
used for negotiation. However the real version will be decided by target server.

The keyword argument @var{session} is for future extension, so do not specify.

If the keyword argument @var{handshake} #f then the procedure won't do
TLS handshake after the socket creation so users must do it manually with
@code{tls-client-handshake} procedure described below.

@;The keyword argument @var{cipher-suites} specifies which ciphers are used.

The keyword argument @var{certificates} is for certificate request message.
The value must be a list of x509 certificates. If the certificates argument
is null, then the procedures send empty certificate list to the server as
a response of certificate request message.

The keyword argument @var{private-key} specifies which private key is used.
The value must be private key object described in @secref["crypto"]{"(crypto)"}.
This is needed if the target server only supports RSA key exchange protocol.
}

@define[Function]{@name{tls-client-handshake @args{tls-socket}}}
@desc{Do client side handshake and return a TLS socket. The procedure must
*NOT* be called if the socket is created with @var{handshake} keyword argument
#t.

CAUTION: This procedure needs to be called only once and calling more than once
might cause infinite loop or raise an error.
}

@define[Function]{@name{make-server-tls-socket}
 @args{service certificates :key (prng (secure-randome RC4))
 (version *tls-version-1.2*)
 (private-key #f) (authorities '())
 :allow-other-keys opt}}
@desc{@var{service} must be string. @var{certificates} must be a
list of x509 certificate.

Creates a server TLS socket. @var{service} and @var{opt} will be passed to
@code{make-server-socket} described in 
@secref["lib.sagittarius.socket"]{(sagittarius socket)}.

The keyword arguments @var{prng} and @var{version} are the same meaning as
@code{make-client-tls-socket}.

The keyword argument @var{private-key} is used the same as client socket
the difference is that it is used if the client side only supports RSA key
exchange.

The keyword argument @var{authorities} must be a list of x509 certificate and
if this is not empty list then the server socket will send certificate request
message to the client. 

CAUTION: the @var{authorities} keyword argument currently doesn't check
the certificate signature but only issuer DN.
}

@define[Function]{@name{tls-socket-send @args{tls-socket bytevector flags}}}
@desc{@var{tls-socket} must be the socket created by the procedure
@code{make-client-tls-socket}.

@var{flags} must be non negative exact integer.

Sends given bytevector to @var{tls-socket}. @var{flags} are described in
the section @secref["lib.sagittarius.socket"]{(sagittarius socket)}. The packet
will be encrypted by @var{tls-socket}.
}

@define[Function]{@name{tls-socket-recv @args{tls-socket size flags}}}
@desc{@var{tls-socket} must be the socket created by the procedure
@code{make-client-tls-socket}.

@var{size} and @var{flags} must be non negative exact integer.

Receives decrypted packet from @var{tls-socket}. @var{size} indicates how many
octets the procedure should receive, however it might return less octet.
@var{flags} will be passed to @code{socket-recv}.

NOTE: @var{tls-socket} have its own buffer to return the value, so that the
procedure can take @var{size} argument.
}

@define[Function]{@name{tls-socket-close} @args{tls-socket}}
@desc{@var{tls-socket} must be the socket created by the procedure
@code{make-client-tls-socket}.

Sends close notify alert to the socket and close it.
}

@define[Function]{@name{tls-socket-closed?} @args{tls-socket}}
@desc{@var{tls-socket} must be the socket created by the procedure
@code{make-client-tls-socket}.

Returns #t if the given socket is closed, otherwise #f.

NOTE: this procedure checks if session is closed. So the real socket might not
be closed yet.
}

@define[Function]{@name{tls-socket-accept}
 @args{tls-socket :key (handshake #t) (raise-error #t)}}
@desc{@var{tls-socket} must be a server TLS socket created by
@code{make-server-tls-socket}.

Wait for an incoming connection request and returns a fresh connected client
socket.

If the keyword argument @var{handshake} is #f then the handshake must be done
by manually with @code{tls-server-handshake} described blow.

The keyword argument @var{raise-error} will be passed to
@code{tls-server-handshake}.
}

@define[Function]{@name{tls-server-handshake}
 @args{tls-socket :key (raise-error #t)}}
@desc{Do server side TLS handshake and returns a TLS socket. The procedure must
*NOT* be called if the socket is created with @var{handshake} keyword argument
#t.

If the keyword argument @var{raise-error} is #f then it won't raise an error
when something happens.
}

@define[Function]{@name{tls-socket-peer} @args{tls-socket}}
@desc{Return peer of given @var{tls-socket} or #f.

For more details, see @secref["lib.sagittarius.socket"]{(sagittarius socket)}.
}

@define[Function]{@name{tls-socket-peer} @args{tls-socket}}
@desc{Return peer of given @var{tls-socket} or #f. This procedure is TLS
version of @code{socket-peer}.

For more details, see @secref["lib.sagittarius.socket"]{(sagittarius socket)}.
}

@define[Function]{@name{tls-socket-name} @args{tls-socket}}
@desc{Return peer of given @var{tls-socket} or #f. This procedure is TLS
version of @code{socket-name}.

For more details, see @secref["lib.sagittarius.socket"]{(sagittarius socket)}.
}

@define[Function]{@name{tls-socket-info-values} @args{tls-socket}}
@desc{Return peer of given @var{tls-socket} or #f. This procedure is TLS
version of @code{socket-info-values}.

For more details, see @secref["lib.sagittarius.socket"]{(sagittarius socket)}.
}

@define[Constant]{@name{*tls-version-1.2*}}
@desc{Constant value of @code{#x0303} for TLS 1.2}

@define[Constant]{@name{*tls-version-1.1*}}
@desc{Constant value of @code{#x0302} for TLS 1.1}

@define[Constant]{@name{*tls-version-1.0*}}
@desc{Constant value of @code{#x0301} for TLS 1.0}

@define[Function]{@name{tls-socket-port} @args{tls-socket}}
@desc{@var{tls-socket} must be the socket created by the procedure
@code{make-client-tls-socket}.

Returns input/output-port of given @var{tls-socket}.
}

@subsubsection{Integration methods}

The methods listed below are convenience methods to use TLS socket and
usual socket without changing code.

@define[Method]{@name{socket-close} @args{(socket <tls-socket>)}}
@define[Method]{@name{socket-send}
 @args{(socket <tls-socket>) data :optional (flags 0)}}
@define[Method]{@name{socket-recv}
 @args{(socket <tls-socket>) size :optional (flags 0)}}
@define[Method]{@name{socket-accept} @args{(socket <tls-socket>) . opt}}
@define[Method]{@name{socket-accept}
 @args{(socket <tls-socket>) (key <keyword>) . dummy}}
@define[Method]{@name{call-with-socket} @args{(socket <tls-socket>) proc}}
@define[Method]{@name{socket-peer} @args{(socket <tls-socket>)}}
@define[Method]{@name{socket-name} @args{(socket <tls-socket>)}}
@define[Method]{@name{socket-info-values} @args{(socket <tls-socket>)}}
