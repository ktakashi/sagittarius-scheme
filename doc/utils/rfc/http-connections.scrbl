@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.http-connections"]{
(rfc http-connections) - HTTP client utilities
}

@define[Library]{@name{(rfc http)}}
@desc{This library provides common interfaces of 
@secref["rfc.http"]{@code{(rfc http)}} and
@secref["rfc.http2"]{@code{(rfc http2)}} library.
}

@define[Function]{@name{http-connection?} @args{obj}}
@desc{Returns #t if the given object is an HTTP connection.}

@define[Function]{@name{http1-connection?} @args{obj}}
@desc{Returns #t if the given object is an HTTP/1.1 connection.}

@define[Function]{@name{make-http1-connection} @args{server secure?}}
@desc{Creates an HTTP/1.1 connection with destination server @var{server}.

If the @var{secure?} is true value, then it uses TLS socket.

The HTTP/1.1 connection uses @code{http-request} defined in @code{(rfc http)}
library to send an request.
}

@define[Function]{@name{http2-connection?} @args{obj}}
@desc{Returns #t if the given object is an HTTP/2 connection.}

@define[Function]{@name{make-http2-connection} @args{server secure?}}
@desc{Creates an HTTP/2 connection with destination server @var{server}.

If the @var{secure?} is true value, then it uses TLS socket.

The HTTP/1.1 connection uses @code{http2-request} defined in @code{(rfc http2)}
library to send an request.
}

@define[Function]{@name{http-connection-secure?} @args{http-connection}}
@desc{Returns #t if the given @var{http-connection} uses TLS socket to
connect target server.}

@define[Function]{@name{http-connection-server} @args{http-connection}}
@desc{Returns destination server of the @var{http-connection} as string.}

@define[Function]{@name{http-connection-server&port} @args{http-connection}}
@desc{Returns destination server and port of the @var{http-connection}.}

@define[Function]{@name{open-http-connection!} @args{http-connection}}
@desc{Opens given @var{http-connection} and returns it.}

@define[Function]{@name{close-http-connection!} @args{http-connection}}
@desc{Closes given @var{http-connection} and returns it.}

@define[Function]{@name{http-request} @args{http-connection method path . opt}}
@desc{Sends HTTP request to the path @var{path} of destination server
of @var{http-connection} with HTTP method @var{methos}.

The rest value of @var{opt} is passed to underling request sending procedure.

The procedure returns 3 values, HTTP status, HTTP header and content as
bytevector.
}

@define[Generic]{@name{http-null-receiver}}
@define[Method]{@name{http-null-receiver} @args{(conn http1-connection)}}
@define[Method]{@name{http-null-receiver} @args{(conn http2-connection)}}
@desc{Generic method of null http receiver.

The @code{http-null-receiver} forwards @code{http-null-receiver} defined
in @code{(rfc http)} for @code{http1-connection} and
@code{http2-null-receiver} defined
in @code{(rfc http2)} for @code{http2-connection}
}

@define[Generic]{@name{http-oport-receiver}}
@define[Method]{@name{http-oport-receiver}
 @args{(conn http1-connection) sink flusher}}
@define[Method]{@name{http-oport-receiver}
 @args{(conn http2-connection) sink flusher}}
@desc{Generic method of output port http receiver.

The @code{http-oport-receiver} forwards @code{http-oport-receiver} defined
in @code{(rfc http)} for @code{http1-connection} and
@code{http2-data-receiver} defined in @code{(rfc http2)} for
@code{http2-connection}
}

@define[Generic]{@name{http-blob-sender}}
@define[Method]{@name{http-blob-sender} @args{(conn http1-connection) blob}}
@define[Method]{@name{http-blob-sender} @args{(conn http2-connection) blob}}
@desc{Generic method of http blob sender.

The @code{http-blob-sender} forwards @code{http-blob-sender} defined
in @code{(rfc http)} for @code{http1-connection} and
@code{http2-data-sender} defined in @code{(rfc http2)} for
@code{http2-connection}
}

@define[Generic]{@name{http-string-sender}}
@define[Method]{@name{http-string-sender} @args{(conn http1-connection) string}}
@define[Method]{@name{http-string-sender} @args{(conn http2-connection) string}}
@desc{Generic method of http string sender.

The @code{http-string-sender} forwards @code{http-string-sender} defined
in @code{(rfc http)} for @code{http1-connection} and
@code{http2-data-sender} defined in @code{(rfc http2)} with given
@var{string} converted by @code{string->utf8} procedure defined
for @code{http2-connection}
}

@define[Generic]{@name{http-null-sender}}
@define[Method]{@name{http-null-sender} @args{(conn http1-connection)}}
@define[Method]{@name{http-null-sender} @args{(conn http2-connection)}}
@desc{Generic method of http null sender.

The @code{http-null-sender} forwards @code{http-null-sender} defined
in @code{(rfc http)} for @code{http1-connection} and does nothing
for @code{http2-connection}.
}