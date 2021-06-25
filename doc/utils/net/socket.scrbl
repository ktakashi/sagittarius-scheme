@; -*- mode:scribble; coding:utf-8; -*-

@subsection[:tag "net.socket"]{(net socket) - Modern Socket library}

@define[Library]{@name{(net socket)}}
@desc{Providing, probably, modern socket library.}

Most of the bindings defined in this library is identical as
@code{(sagittarius socket)}, so this document will describe only the
differences.

@define[Function]{@name{socket-options?} @args{o}}
@define[Function]{@name{tls-socket-options?} @args{o}}
@define[Function]{@name{server-tls-socket-options?} @args{o}}
@desc{Returns #t if the given @var{o} is a @code{socket-options},
@code{tls-socket-options}, or @code{server-tls-socket-options}, respectively.

The @code{socket-options?} returns #t for all 3 types.
}

@define[Macro]{@name{socket-options-builder} @args{(fields @dots{})}}
@desc{Builds a socket-options object.
The @code{field} must be one or multiple of the followings:
@dl-list[
  @dl-item[@var{ai-family}]{See @code{ai-family} of @code{(sagittarius socket)}}
  @dl-item[@var{ai-socktype}]{See @code{ai-socktype} of @code{(sagittarius socket)}}
  @dl-item[@var{ai-flags}]{See @code{ai-flags} of @code{(sagittarius socket)}}
  @dl-item[@var{ai-protocol}]{See @code{ai-protocl} of @code{(sagittarius socket)}}
  @dl-item[@var{non-blocking?}]{Specifies if the socket is non blocking or not, default #t}
  @dl-item[@var{connection-timeout}]{Specifies connection timeout in micro seconds, default #f (infinite)}
  @dl-item[@var{read-timeout}]{Specifies read timeout, default #f (infinite) see @code{socket-set-read-timeout!} for more details.}
]
}

@define[Macro]{@name{tls-socket-options-builder} @args{(fields @dots{})}}
@desc{Builds a tls-socket-options object.
The @code{fields} must be the ones from @code{socket-options-builder} or
one or multiple of the followings:
@dl-list[
  @dl-item[@var{handshake}]{Specifes if handshake must be done or not. Default #t}
  @dl-item[@var{private-key}]{A client private key}
  @dl-item[@var{certificates}]{A list of client certificates, must be specified when @code{private-key} is specified}
  @dl-item[@var{certificate-verifier}]{A certificate verifier procedure}
  @dl-item[@var{sni*}]{A list of server name indicators, a list of string}
  @dl-item[@var{alpn*}]{A list of application layer protocol negotiation names}
]
}
@define[Macro]{@name{server-tls-socket-options-builder} @args{(fields @dots{})}}
@desc{Builds a server-tls-socket-options object.
The @code{fields} must be the ones from @code{tls-socket-options-builder} or
one or multiple of the followings:
@dl-list[
  @dl-item[@var{client-certificate-required?}]{Specifes if client certificate is required or not. Default #f}
  @dl-item[@var{trusted-certificates}]{A list of trusted certificates}
]
}

@define[Function]{@name{make-client-socket}
 @args{node service :optional options}}
@desc{Creates a client socket.
This is almost the same as the one from @code{(sagittarius socket)}, the only
difference is it takes @var{options} to specify extra options, such as
read timeout or connection timeout.
}

@define[Function]{@name{make-client-tls-socket}
 @args{node service :optional options}}
@desc{Creates a client socket.
This is almost the same as the one from @code{(rfc tls)}, the only
difference is it takes @var{options} to specify extra options, such as
client certificate, alpn, etc.
}

@define[Function]{@name{make-server-socket}
 @args{node service :optional options}}
@desc{Creates a client socket.
This is almost the same as the one from @code{(sagittarius socket)}, the only
difference is it takes @var{options} to specify extra options, such as
read timeout or connection timeout.
}

@define[Function]{@name{make-server-tls-socket}
 @args{node service :optional options}}
@desc{Creates a client socket.
This is almost the same as the one from @code{(rfc tls)}, the only
difference is it takes @var{options} to specify extra options, such as
private key, etc.
}

@define[Function]{@name{socket-options->client-socket}
 @args{options node service}}
@desc{Creates a client socket from the given @var{options}, @var{node} and
@var{service}.

This is a convenient procedure to make a socket from the options instead
of let user dispatch manually.
}
