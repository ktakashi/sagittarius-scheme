@; -*- coding: utf-8 -*-
@subsection[:tag "utils.rpc"]{RPC support framework}

RPC is basically separated in 2 parts. One is message marshalling and other 
one is transporting. Following sections and libraries provide some framework
for generic use.

@subsubsection{Message framework}

@define[Library]{@name{(rpc message)}}
@desc{This library provides generic functions for marshalling message.}

@define[Generic]{@name{rpc-marshall-message}}
@define[Generic]{@name{rpc-unmarshall-message}}
@desc{These generic function will be used transport layer to marshall or
unmarshall messages.}

@subsubsection[:tag "utils.rpc.transport.http"]{Http transport}

Currently there is no generic transport framework and support HTTP transport.

For future, we may provide more generic way to send/receive request 
and response.

@define[Library]{@name{(rpc transport http)}}
@desc{This library provides procedures and generic function of 
HTTP transport for RCP}.

@define[Function]{@name{rpc-http-request}
 @args{url message :key (unmarshall? #t) :allow-other-keys options}}
@desc{Send given @var{message} to @var{url} and returns 3 values,
http status, header and response.

If the keyword argument @var{unmarshall?} is #f then the returning response
value will not be unmarshalled.

@var{options} will be passed to @code{http-request} procedure.
}

@sub*section{Http transport hook}

@define[Method]{@name{rpc-http-method} @args{request}}
@desc{Returns http method. Default implementation returns @code{POST}}.

@define[Method]{@name{rpc-http-content-type} @args{request}}
@desc{Returns content type. Default implementation returns 
@code{application/octet-stream}}.

@define[Method]{@name{rpc-http-sender} @args{request}}
@desc{Returns http sender. Default implementation returns
@code{http-blob-sender} with marshalled request}.

@define[Method]{@name{rpc-http-receiver} @args{request}}
@desc{Returns http receiver. Default implementation returns
@code{http-binary-receiver}}.

@define[Method]{@name{rpc-http-response-type} @args{request}}
@desc{Returns a marker type to be used @code{rpc-http-unmarshall-message}.
Default implementation returns given @var{request} itself}.

@define[Method]{@name{rpc-http-unmarshall-message} @args{type header body}}
@desc{Unmarshall the given @var{body} according to the given @var{type}.
Default implementation ignores @var{header} and passes @var{type} and
@var{body} to @code{rpc-unmarshall-message}.}

@include-section["utils/rpc/json.scrbl"]