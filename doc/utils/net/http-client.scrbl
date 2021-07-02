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


@subsubsection{HTTP request and response}

@define[Function]{@name{http:request?} @args{o}}
@desc{Returns #t if the given @var{o} is an HTTP request object, otherwise #f.}

@define[Macro]{@name{http:request-builder} @args{field* @dots{}}}
@desc{Builds a HTTP request object.

The @code{field} must be one or more the followings.
@dl-list[
  @dl-item[@code{uri}]{The URI of the request, it has to be an URI object of @code{(net uri)} or valid URI string. This field is mandatory.}
  @dl-item[@code{method}]{A valid HTTP method. Default value is @code{GET}.}
  @dl-item[@code{content-type}]{A content type header, this is used when the method is allowed to have body. Default value @code{"application/octet-stream"}.}
  @dl-item[@code{body}]{Content of the request. The value must be either a bytevector ot binary input port.}
  @dl-item[@code{headers}]{A list of headers or a HTTP header object. If it's a list of header, then the element must contain a pair of name and values.}
]

}

@define[Function]{@name{http:headers?} @args{o}}
@desc{Returns #t if the given @var{o} is a HTTP headers object, otherwise #f.}

@define[Function]{@name{http:make-headers}}
@desc{Make a HTTP headers object.

A HTTP headers object can contain multiple values on one headers.
}

@define[Function]{@name{http:headers-ref*} @args{headers name}}
@desc{Retrieve a list of value of the header @var{name} from @var{headers}.

If the @var{name} doesn't exists, then it returns @code{()}.}

@define[Function]{@name{http:headers-ref} @args{headers name}}
@desc{Retrieve the first value of the header @var{name} from @var{headers}.

If the @var{name} doesn't exists, then it returns @code{#f}.}

@define[Function]{@name{http:headers-set!} @args{headers name value}}
@desc{Sets the single value of @var{value} into the @var{headers} with
header name of @var{name}.}

@define[Function]{@name{http:headers-add!} @args{headers name value}}
@desc{Adds the @var{value} in the @var{headers} to the header value
list of @var{name}.

If the @var{name} doesn't exist, then it works the same as
@code{http:headers-set!}.
}

@define[Function]{@name{http:headers-names} @args{headers}}
@desc{Returns a list of header names of the given @var{headers}}


@define[Function]{@name{http:response?} @args{o}}
@desc{Returns #t if the given @var{o} is an HTTP response object, otherwise #f.}

@define[Function]{@name{http:response-status} @args{response}}
@desc{Returns the response HTTP status code.

The code is a string.
}

@define[Function]{@name{http:response-headers} @args{response}}
@desc{Returns the response HTTP headers object.}

@define[Function]{@name{http:response-cookies} @args{response}}
@desc{Returns a list of the response cookie, if exists.

The element of the list is a cookie object of @code{(rfc cookie)}.
}

@define[Function]{@name{http:response-body} @args{response}}
@desc{Returns the body of the response.
The value is bytevector.
}

@; TBD the rest...
