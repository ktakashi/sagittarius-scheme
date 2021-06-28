@; -*- mode:scribble; coding:utf-8; -*-

@subsection[:tag "net.http-client"]{(net http-client) - Modern HTTP client}

@define[Library]{@name{(net http-client)}}
@desc{Providing, probably, modern HTTP client.}

The @code{(net http-client)} provides a modern HTTP client, which handles
connection pooling, client certificate, redirect and cookie. The client
also provides asynchronous calling atop future object of
@code{(util concurrent)}.

The following example shows how to use in a nutshell.

@codeblock{
(import (rnrs)
        (net http-client)
        (util concurrent))

(define client (http:client-builder
                (follow-redirects (http:redirect normal))))

(let ((future (http:client-send-async client
                                      (http:request-builder
                                       (uri "http://google.com")))))
  ;; do whatever you need to do during the above HTTP call
  (let ((response (future-get future)))
    (http:response-status response)  ;; -> 200
    (http:response-headers response) ;; -> header object
    (http:response-body response)    ;; -> body as bytevector
    ))
}

@subsubsection{HTTP client}

@define[Function]{@name{http:client?} @args{o}}
@desc{Returns #t if the given @var{o} is a HTTP client, otherwise #f.}

@define[Macro]{@name{http:client-builder} @args{field @dots{}}}
@desc{A constructor macro to build a HTTP client.

The @var{field}s must be one or more of the followings:
@dl-list[
  @dl-item[@code{connection-timeout}]{Specifies connection timeout in milli second.}
  @dl-item[@code{read-timeout}]{Specifies read timeout in milli second.}
  @dl-item[@code{follow-redirects}]{Specifies redirection strategy, the value must be a valid value of @code{http:redirect}.}
  @dl-item[@code{cookie-handler}]{Specifies a cookie handler. Currently, @code{http:make-default-cookie-handler} is the only option.}
  @dl-item[@code{connection-manager}]{Specifies a connection manager. See Connection Manager section for more detail.}
  @dl-item[@code{key-manager}]{Specifies a key manager. See Key Manager section for more detail.}
  @dl-item[@code{version}]{Specifies a HTTP version. The value must be a valid value of @code{http:version}. The client handles both HTTP/1.1 and HTTP/2 by default.}
  @dl-item[@code{executor}]{Specifies an executor. The the default executor has 5 threads, but it'd be shared by all the client created without this field, so if you want to use the client in a intense situation, it's a good idea to specify own executor.}
]
}

@define[Macro]{@name{http:version} @args{value}}
@desc{A macro checks if the given @var{value} is a valid supporting HTTP
version.

The value must be either @code{http/1.1} or @code{http/2}. If the later one
is specified, it first tries to connect with HTTP/2, and falls back to
HTTP/1.1 if HTTP/2 is not avaiable.

HTTP/2 works only on TLS environment, as the client uses ALPN to detect the
version.
}

@define[Macro]{@name{http:redirect} @args{value}}
@desc{A macro checks if the given @var{value} is a valid supporting redirect
strategy.

The value must be one of @code{never}, @code{always} or @code{normal}. The
default value is @code{never}.

@code{never} won't redirect, it is suitable if you want to handle 3xx result
manually.

@code{always} redirects as long as the 3xx status is returned. This means
insecure redirection may happen (i.e. HTTPS to HTTP)

@code{normal} redirects 3xx status if the scheme is the same or more secure.
}

@; TBD the rest...
