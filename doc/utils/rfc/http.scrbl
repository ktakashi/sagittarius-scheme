@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.http"]{(rfc http) - HTTP client}

@define[Library]{@name{(rfc http)}}
@desc{This library provides simple HTTP client defined in 
@hyperlink[:href "https://www.ietf.org/rfc/rfc2616.txt"]{RFC 2616}.

The library only talks to HTTP 1.1 and provides part of the specified features.
We may implement complete specification in future.
}

Following is the simple example to send GET method to a HTTP server.
@codeblock{
(import (rfc http))

(define url "http://example.com/path")

(let-values (((server path) (url-server&path url)))
  (http-get server path))
;; -> return 3 values
;; status
;; header
;; body
}

@define[Condition]{@name{&http-error}}
@desc{HTTP error condition. }
@define[Function]{@name{http-error?} @args{obj}}
@desc{Return #t if given @var{obj} is HTTP condition, otherwise #f.}

@; TODO http connection thing

@subsubsection{Request APIs}

@define[Function]{@name{http-get} @args{server path options @dots{}}}
@define[Function]{@name{http-head} @args{server path options @dots{}}}
@define[Function]{@name{http-post} @args{server path body options @dots{}}}
@define[Function]{@name{http-put} @args{server path body options @dots{}}}
@define[Function]{@name{http-delete} @args{server path options @dots{}}}
@desc{Sends HTTP request to given @var{path} on @var{server}. The using
methods are @code{GET}, @code{HEAD}, @code{POST}, @code{PUT}, and
@code{DELETE}, respectively.

The @var{body} argument of @code{http-post} and @code{http-put} can be
UTF-8 string, bytevector or list of body parameters. The parameters are
used for sending multipart form data. The detail parameter form is described
in the @code{http-compose-form-data}.

The keyword @var{:value} and @var{file} should not be represented 
simultaneously, if both keywords are found then @var{:file} is used.

Optional arguments @var{options} are passed to underling procedure
@code{http-request}.
}


@; TODO list all keyword arguments
@define[Function]{@name{http-request}
 @args{method server request-uri
      :key (no-redirect #f)
           (auth-handler #f)
           (auth-user #f)
	   (auth-password #f)
	   (user-agent (*http-user-agent*))
	   (secure #f)
	   (receiver (http-string-receiver))
	   (sender #f)
      :allow-other-keys opts}}
@desc{Sends HTTP request to @var{request-uri} on @var{server} with
@var{method}.

The keyword argument @var{receiver} is used to receive the response data.
The value must be a procedure which takes four arguments, status code,
headers, size of content, and thunk to retrieve remote binary port and its 
size.

The keyword argument @var{sender} is used for @code{POST} and @code{PUT}
HTTP method to send data. If it's specified then it must be a procedure
which takes three arguments, headers, encoding and header-sink.

NOTE: Users can define own receiver and sender however the API may change
in the future. So if the predefined ones are sufficient, it is safe to use
them.

The keyword arguments start with @var{auth-} handle authentication.
If the server respond status code @code{401} then those values are
used. @var{auth-handler} must be a procedure and takes five arguments
alist of connection-info, @var{auth-user}, @var{auth-password}, response
headers and response body. If the handler returns list of "authorization"
value then @code{http-request} sends it as the authentication data. For
example, if the server requires BASIC authentication then the procedure
should return something like following value;

@snipet{(("authorization" "Basic dXNlcjpwYXNz"))}

Following is the complete example of @var{auth-handler};

@codeblock{
(define (basic-auth-handler info user pass headers body)
  (let ((m (format "~a:~a" user pass)))
    `(("authorization" ,(format "Basic ~a" (base64-encode-string m))))))
}

@code{http-request} supports BASIC authentication and Digest authentication
by default. If you know that the server requires one of each then specifying
@var{auth-user} and @var{auth-password} is sufficient.

If keyword argument @var{secure} is true value then TLS socket is used for
physical connection.

The rest arguments @var{opts} is converted to request header.
}

@define[Parameter]{@name{*http-user-agent*}}
@desc{This parameter value is used for @code{user-agent} header.
}

@subsubsection{Senders and receivers}

@define[Function]{@name{http-null-sender}}
@desc{Creates a sender which sends nothing.}

@define[Function]{@name{http-string-sender} @args{str}}
@desc{Creates a sender which content is @var{str}.

The string will be converted to bytevector according to the
@var{encoding} argument when the sender is called.
}

@define[Function]{@name{http-blob-sender} @args{blob}}
@desc{@var{blob} must be a string or bytevector.

Creates a sender which content is @var{blob}. If the @var{blob} is string,
it will be converted to bytevector with @code{string->utf8}.
}

@define[Function]{@name{http-multipart-sender} @args{params}}
@desc{Creates a sender which send @code{multipart/form-data} with
content of @var{param}.

The content will be created by @code{http-compose-form-data} procedure
passing @var{param}.
}

@define[Function]{@name{http-string-receiver}}
@define[Function]{@name{http-binary-receiver}}
@define[Function]{@name{http-null-receiver}}
@desc{Creates a receiver which returning content is string, bytevecotor and
unspecified value, respectively.


@codeblock[=> "status headers and string representation of received content"]{
(http-get "google.com" "/" :receiver (http-string-receiver))
}

@codeblock[=> "status headers and bytevector representation of received content"]{
(http-get "google.com" "/" :receiver (http-binary-receiver))
}

@codeblock[=> "status headers and unspecified value"]{
(http-get "google.com" "/" :receiver (http-null-receiver))
}
}

@define[Function]{@name{http-oport-receiver} @args{sink flusher}}
@desc{The @var{sink} must be a binary output port, @var{flusher} must be
a procedure takes two arguments, @var{sink} and headers.

Creates a receiver which stores the content of response to @var{sink} and
returns the result of @var{flusher}.

@codeblock[=> "status headers and bytevector representation of received content"]{
(http-get "google.com" "/"
          :receiver (let-values (((port extract) 
                                  (open-bytevector-output-port)))
                      (http-oport-receiver port 
                                           (lambda (port size) (extract)))))
}
}

@define[Function]{@name{http-file-receiver}
 @args{filename :key (temporary? #f)}}
@desc{@var{filename} must be a string.

Creates a receiver which stores the content to a file. The receiver
returns the file name.

If keyword arguments @var{temporary?} specified with true value, then
the returning file name is temporary file.

If there is no response or @code{content-length} header contains non
number value, then the file will be cleaned.

@codeblock[=> "status headers and \"google.html\""]{
(http-get "google.com" "/" :receiver (http-file-receiver "google.html"))
}

}

@; TODO
@; @define[Macro]{@name{http-cond-receiver}

@subsubsection{Utilities}

@define[Function]{@name{http-compose-query}
 @args{path params :optional (encoding 'utf-8)}}
@desc{Composes query string.

If given @var{path} is #f then only composed query string is returned,
otherwise this returns @code{@var{path}?@var{composed query}} form.

@var{params} must be a list of name & value list or null list.
}

@define[Function]{@name{http-compose-form-data}
 @args{params port :optional (encoding 'utf-8)}}
@desc{Composes  multipart form data.

If @var{port} is #f then it returns composed string. if it's a port then
the result is stored in the given @var{port}.

The @var{params} must be following form;

@codeblock{
 <params> : (<params> ...)
 <param>  : (<name> <value>)
          | (<name> <key> <value> <key2> <value2> ...)
 <key>    : :value | :file | :content-type | :content-transfer-encoding
          | other keyword (used as a header name)
}

@var{<value>} is the content of @var{<name>} parameter. It can be any of
Scheme object however it is converted to string representation except
 bytevector.

If @var{:file} keyword is used then it read the content of @var{<value>}.
}

@define[Function]{@name{url-server&path} @args{url}}
@desc{Decompose the given @var{url} and returns auth part of URL and
path + query + fragment.
}