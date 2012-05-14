@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.tls"]{(rfc tls) - TLS protocol library}

@define[Library]{@name{(rfc tls)}}
@desc{This library provides TLS protocol socket APIs.

CAUTION: This library is not well tested. So it may have security hole or
incompatible behaviour. If you find such bugs, please let me know.
}

@define[Function]{@name{make-client-tls-socket}
 @args{node service :key (prng (secure-randome RC4))
 (version *tls-version-1.2*) (session #f) :allow-other-keys opt}}
@desc{@var{node} and @var{service} are must be string.

Creates a client TLS socket. @var{node}, @var{service} and @var{opt} will be
passed to @code{make-client-socket} described in
@secref["lib.sagittarius.socket"]{(sagittarius socket)}.

The keyword argument @var{prng} specifies which pseudo random algorithm will
be used for generating security parameters.

The keyword argument @var{version} specifies which TLS protocol version will be
used for negotiation. However the real version will be decided by target server.

The keyword argument @var{session} is for future extension, so do not specify.
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
