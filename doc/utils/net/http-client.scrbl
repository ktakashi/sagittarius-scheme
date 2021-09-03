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

The @var{field}s must be one or more of the following:
@dl-list[
  @dl-item[@code{follow-redirects}]{Specifies redirection strategy, the value must be a valid value of @code{http:redirect}.}
  @dl-item[@code{cookie-handler}]{Specifies a cookie handler. Currently, @code{http:make-default-cookie-handler} is the only option.}
  @dl-item[@code{connection-manager}]{Specifies a connection manager. See Connection Manager section for more detail.}
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

@subsubsection{Connection Manager}

A connection manager manages the connections of a HTTP client. By default,
we support these three types of connection managers; ephemeral, pooling, and
logging.

The ephemeral connection manager creates a connection per request and
discards it. This connection manager should only be used in disposable
scripts.

The pooling connection manager pools the created connection and try to
reuse if possible.

@; The logging connection manager is logs the request and response header, and
@; more. The rest of the functionalities are the same as ephemeral connection
@; manager.

@define[Function]{@name{make-http-default-connection-manager}}
@desc{Creates a default connection manager. This is at this moment an
ephemeral connection manager without any timeout or key configuration.
}

@sub*section{Ephemeral Connection Manager}

@define[Function]{@name{http-connection-config?} @args{obj}}
@desc{Returns #t if the @var{obj} is an instance of
@code{http-connection-config}, otherwise #f.}

@define[Macro]{@name{http-connection-config-builder} @args{field @dots{}}}
@desc{A constructor macro to build a @code{http-connection-config} record.

The @var{field}s must be one or more of the following:
@dl-list[
  @dl-item[@code{connection-timeout}]{Specifies connection timeout in milli second.}
  @dl-item[@code{read-timeout}]{Specifies read timeout in milli second.}
  @dl-item[@code{key-manager}]{Specifies a key manager. See Key Manager section for more detail.}
]
}

@define[Function]{@name{http-ephemeral-connection-manager?} @args{obj}}
@desc{Returns #t if the @var{obj} is an ephemeral connection manager,
otherwise #f.}

@define[Function]{@name{make-http-ephemeral-connection-manager} @args{config}}
@desc{@var{Config} must be a @code{http-connection-config} record instance.

Creates an ephemeral connection manager.}

@sub*section{Pooling Connection Manager}

@define[Function]{@name{http-pooling-connection-config?} @args{obj}}
@desc{Returns #t if the @var{obj} is an instance of
@code{http-pooling-connection-config}, otherwise #f.}

@define[Macro]{@name{http-pooling-connection-config-builder}
 @args{field @dots{}}}
@desc{A constructor macro to build a @code{http-pooling-connection-config}
record.

The @var{field}s must be one or more of the following and the ones from
@code{http-connection-config-builder}:
@dl-list[
  @dl-item[@code{connection-request-timeout}]{
  Specifies connection request timeout in milli second. The timeout is only
  for the waiting time of the connection retrieval from the pool.}
  @dl-item[@code{max-connection-per-route}]{
  Specifies pool size per route. A route is defined with the combination of
  hostname and port. Default value is 5.}
  @dl-item[@code{time-to-live}]{
  Specifies the life time of the pooled connection in seconds}
  @dl-item[@code{delegate-provider}]{
  Specifies connection manager delegate provider. Default value is
  @code{default-delegate-connection-manager-provider}}
]

A delegate connection manager is an underlying connection manager of
the pooling connection manager. The delegate creates and closes the
actual connection.
}

@define[Function]{@name{http-pooling-connection-manager?} @args{obj}}
@desc{Returns #t if the @var{obj} is a pooling connection manager,
otherwise #f.}

@define[Function]{@name{make-http-pooling-connection-manager} @args{config}}
@desc{@var{Config} must be a @code{http-pooling-connection-config}
record instance.

Creates a pooling connection manager.}

@; TBD logging connection manager

@sub*section{Delegate Connection Manager providers}

A delegate connection provider is a procedure which accepts one
argument @var{config} and returns a connection manager.

@define[Function]{@name{default-delegate-connection-manager-provider}
 @args{config}}
@desc{Synonym of @code{make-http-ephemeral-connection-manager}.}

@; @define[Function]{@name{make-logging-delegate-connection-provider}
@;  @args{logger}}
@; @desc{Creates a logging delegate connection provider. @var{logger} must
@; be a HTTP client logger}


@subsubsection{Key Manager}

Key manager manages client certificate of HTTP connections. A key manager
may contain multiple of key providers. A key provider should provide a
list whose the first element is a private key and the rest are certificate
chain.

The following example shows how to make a key manager with multiple
key providers.

@codeblock{
(import (rnrs)
        (net http-client)
	(rfc base64)
	(security keystore))
(define (host-a p)
  (define node (socket-parameter-socket-node p))
  (cond ((string=? node "host-a.com") "host-a-key-alias")
	(else #f)))
(define (host-b p)
  (define node (socket-parameter-socket-node p))
  (and (string-suffix? ".host-b.com" node) "host-b-key-alias"))
(define keystores
  ;; keystore file (PKCS12 base64 encoded), store pass, key pass, alias selector
  `(("host-a.b64" "password0" "password0a" ,host-a)
    ("host-b.b64" "password1" "password1a" ,host-b)))

(define (->keystore-key-provider keystore-info)
  (let ((file (car keystore-info))
        (storepass (cadr keystore-info))
        (keypass (caddr keystore-info))
        (strategy (cadddr keystore-info)))
    (make-keystore-key-provider
     (call-with-input-file file
       (lambda (in)
         (let ((bin (open-base64-decode-input-port in)))
           (load-keystore 'pkcs12 bin storepass)))
       :transcoder #f)
     keypass
     strategy)))

(make-key-manager (map ->keystore-key-provider keystores))
}

@define[Function]{@name{key-manager?} @args{obj}}
@desc{Returns #t if the @var{obj} is a key manager,
otherwise #f.}

@define[Function]{@name{make-key-manager} @args{key-providers}}
@desc{@var{Key-providers} must be a list of key provider.

Creates a key manager.}

@sub*section{Key Provider}

Key provider provides keys. The key can be retrieved from anywhere but
must be a format of a list of private key and certificate chain.

We provide keystore key provider by default. If you need other key
provider, you need to create a custom one.

@define["Record Type"]{@name{<key-provider>}}
@desc{An abstruct record type of key provider. Users must inherit this
record type to create a custom key provider.

This record type has a filed named @code{key-retrievers}.
}

@define[Function]{@name{key-provider?} @args{obj}}
@desc{Returns #t if the @var{obj} is a key provider
otherwise #f.}

@define[Function]{@name{key-provider-key-retrievers} @args{key-provider}}
@desc{Returns the value of field @code{key-retrievers} of the key provider
@var{key-provider}.

This procedure is not meant to be used by a user of HTTP client.
}

@define[Function]{@name{keystore-key-provider?} @args{obj}}
@desc{Returns #t if the @var{obj} is a keystore key provider
otherwise #f.}


@define[Function]{@name{make-keystore-key-provider} 
 @args{keystore password alias-provider}}
@desc{@var{Keystore} must be a keystore object of @code{(security keystore)}.
@var{password} must be a string of valid key password.
@var{alias-provider} must be a procedure which accepts one argument,
socket parameter, and returns either string of key alias or #f.

Creates a key provider with one key retriever.}

@define[Function]{@name{keystore-key-provider-add-key-retriever!}
 @args{keystore-key-provider password alias-provider}}
@desc{Add a key retriever to the @var{keystore-key-provider} and returns
@var{keystore-key-provider}.

This procedure is convenient if you have multiple keys in one keystore.
}

