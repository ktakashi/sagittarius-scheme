@; -*- coding: utf-8 -*-
@subsection[:tag "rfc2.http"]{(rfc http2) - HTTP/2 client}

@define[Library]{@name{(rfc http2)}}
@desc{This library provides simple client HTTP/2 defined in
@hyperlink[:href "https://tools.ietf.org/html/rfc7540"]{RFC 7540}.

The library does not support @code{Upgrade} header defined in HTTP/1.1. Thus
users must know if the server supports HTTP/2 or not, ahead.
}

Following is the simple example to send GET method to a HTTP/2 server.
@codeblock{
(import (rfc http2))

(define conn (make-http2-client-connection "twitter.com" "443" :secure? #t))
(http2-get conn "/")
;; -> returns 2 values
;; header
;; body
(close-http2-client-connection! conn)
}

@define[Function]{@name{http2-client-connection?} @args{obj}}
@desc{Returns #t if the given @var{obj} is an HTTP/2 client connection,
otherwise #f.}

@define[Function]{@name{make-http2-client-connection}
 @args{server port :key (secure? #f) user-agent}}
@desc{Creates an HTTP/2 client connection.

The @var{server} must be a string indicating an existing server name.

The @var{post} must be a string of either port number or service.

The keyword argument @var{secure?} specifies to use TLS or not.

The keyword argument @var{user-agent} specifies the value of @code{user-agent}
header. The default value is @code{Sagittarius-} followed by version number.

NOTE: The connection does not guess if it should use secure connection or
not by looking at port number or service name. Which means, even if you
specify @code{"https"} or @code{"443"} however @var{secure?} keyword
argument must be passed.

NOTE2: Created connections hold opened socket.
}

@define[Function]{@name{close-http2-client-connection!} @args{conn}}
@desc{Closes given HTTP/2 connection.}

@subsubsection{High level APIs}

@define[Function]{@name{http2-get}
 @args{conn uri :key receiver redirect-handler :allow-other-keys headers}}
@desc{Sends HTTP/2 GET request to given @var{uri} of HTTP/2 client
connection @var{conn} and returns 2 values of the response, header
and content.

The rest arguments @var{headers} must be a list of keyword and string.
The procedure sends these pairs as extra headers.

If the keyword argument @var{receiver} is specified, then the procedure
uses given @var{receiver} to receive data. The default value is
@code{(make-gzip-receiver)}.

If the keyword argument @var{redirect-handler} is specified, then the procedure
uses given @var{redirect-handler} to handle redirection.
}

@define[Function]{@name{http2-post}
 @args{conn uri data :key receiver redirect-handler :allow-other-keys headers}}
@desc{Sends HTTP/2 POST request to given @var{uri} of HTTP/2 client
connection @var{conn} with given @var{data}  and returns 2 values of
the response, header and content. @var{data} must be a bytevector.

The rest arguments @var{headers} must be a list of keyword and string.
The procedure sends these pairs as extra headers.

The keyword arguments @var{receiver} and @var{redirect-handler} are the same
as @code{http2-get}.
}

@define[Function]{@name{http2-head}
 @args{conn uri :key receiver redirect-handler :allow-other-keys headers}}
@desc{Sends HTTP/2 HEAD request to given @var{uri} of HTTP/2 client
connection @var{conn} and returns 2 values of the response, header
and content.

The rest arguments @var{headers} must be a list of keyword and string.
The procedure sends these pairs as extra headers.

The keyword arguments @var{receiver} and @var{redirect-handler} are the same
as @code{http2-get}.
}

@define[Macro]{@name{http2-request}
 @args{conn method uri sender receiver :key redirect-handler}}
@desc{Sends an HTTP/2 request to given HTTP/2 client connection @var{conn}.

@var{method} must be a symbol.

@var{sender} must be a sender described below section.

@var{receiver} must be a receiver described below section.

@var{redirect-handler} is the same as @code{http2-get}.

The procedure returns a list of response header and content.
}

@sub*section{HTTP/2 sender}

A sender is a procedure which accepts 2 arguments, internal HTTP/2 stream and
flag. At this moment, the internal HTTP/2 stream is not exposed so users cannot
create own sender.

@define[Function]{@name{http2-headers-sender} @args{header @dots{}}}
@desc{Returns a sender which sends given @var{header} as HTTP/2 header.}

@define[Function]{@name{http2-data-sender} @args{data}}
@desc{Returns a sender which sends given @var{data} as HTTP/2 data.}

@define[Function]{@name{http2-composite-sender} @args{sender @dots{}}}
@desc{Returns a composite sender.}

@sub*section{HTTP/2 receiver}

A receiver is a procedure which accepts 4 arguments, internal HTTP/2 stream,
header, frame and flag. The same restriction as sender is applied, thus
users cannot create own receiver, at this moment.

@define[Function]{@name{http2-data-receiver} @args{sink flusher}}
@desc{Returns a receiver which receives HTTP/2 data frame and put the
value into @var{sink}. When the stream reaches to the end, then @var{flusher}
is called with @var{sink}.
}

@define[Function]{@name{http2-binary-receiver} @args{}}
@desc{Returns a receives which receives HTTP/2 data frame as a bytevector.}

@define[Function]{@name{http2-null-receiver} @args{}}
@desc{Returns a receives which receives HTTP/2 data frame and returns empty
bytevector.}

@define[Function]{@name{http2-gzip-receiver} @args{receiver}}
@desc{Returns a receives which receives HTTP/2 data frame. If the data frame
is compressed to gzip, then the returning receiver expand the data and forward
to the given @var{receiver}. If the data frame is not compressed, then it
simply forward the data frame to the given @var{receiver}.
}

@define[Function]{@name{make-gzip-receiver} @args{}}
@desc{Convenient procedure whose definition is the following:

@snipet{(http2-gzip-receiver (http2-binary-receiver))}
}

@; http2-add-request! and others
@; TBD
@; @subsection{Middle leval APIs}

