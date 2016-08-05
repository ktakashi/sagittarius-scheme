@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.websocket"]{(rfc websocket) - WebSocket}

@define[Library]{@name{(rfc websocket)}}
@desc{This library provides WebSocket APIs defined by
@hyperlink[:href "https://tools.ietf.org/html/rfc6455"]{RFC 6455}.
}

Following is a simple example to use user level APIs.
@codeblock{
(import (rnrs) (rfc websocket))

;; Creates an WebSocket object
(define websocket (make-websocket "wss://echo.websocket.org"))

;; Sets text message event handler
(websocket-on-text-message websocket
  (lambda (ws text) (display text) (newline)))

;; Opens the WebSocket
(websocket-open websocket)

;; Sends a message to endpoint
(websocket-send websocket "Hello")

;; Closes the WebSocket
(websocket-close websocket)
}

@subsubsection{User level APIs}

@define[Function]{@name{make-websocket}
 @args{uri :key (protocols '()) (extensions '()) (engine 'http)}}
@desc{Creates an WebSocket object which communicate to given @var{uri}.

The URI reprented by @var{uri} must be a valid WebSocket URI. If it's
not a valid URI, then @code{&websocket-engine} or its sub condition
shall be raised.

The keyword argument @var{protocols} are a list of sub protocols of the
creating WebSocket. If this is set, then handshake will send it with
@code{Sec-WebSocket-Protocol} header or equivalent.

The keyword argument @var{extensions} are a list of extensions of the
creating WebSocket. If this is set, then handshake will send it with
@code{Sec-WebSocket-Extensions} header or equivalent.

The keyword argument @var{engine} is a type of handshake engine and
must be a symbol. This determines which handshake protocol it should
use. The default value is @code{http} which uses HTTP/1.1.

}

@define[Function]{@name{websocket?} @args{obj}}
@desc{Returns #t if given @var{obj} is an WebSocket object otherwise #f.}

@define[Function]{@name{websocket-open} @args{websocket}}
@desc{Operating handshake on given WebSocket object @var{websocket} if
it's not opened, yet. And returns @var{websocket}.

If handshake failed, then @code{&websocket-engine} or its sub condition
shall be raised.

After successfull call of this procedure, the @var{websocket} has a
message dispatcher thread.
}

@define[Function]{@name{websocket-close}
 @args{websocket :key (status #f) (message "") (timeout #f)}}
@desc{Closes the given @var{websocket} if it's open and returns the
@var{websocket}. This procedure also finishes the underlying thread
created by @code{websocket-open}.

The keyword argument @var{status} is specified, it must be a non
negative integer which has less then or equal to 16 bits length, then
the procedure sends it as a connection close code.

The keyword argument @var{message} is specified, must be a string, then
the procedure sends it as a connection close message. This is sent only
if the @var{status} is specified otherwise ignored.

The keyword argument @var{timeout} is used as a timeout period for waiting
underlying dispatcher thread. If the thread didn't finish in the specified
period, then it'd be terminated and @code{&websocket-close-timeout} is
raised.

If the underlying thread raised an @code{&uncaught-exception}, then the
procedure raises its reason.
}

@define[Function]{@name{websocket-send}
 @args{websocket data :optional start splitter}}
@desc{Sends given @var{data} to the @var{websocket} and returns
@var{websocket}. This procedure runs in atomic so the @var{data} is sent
in one fragmentation sequence.

The optional arguments are passed to underlying data transport procedures.
If the @var{data} is a string, then @code{websocket-send-text} is used.
If the @var{data} is a bytevector, then @code{websocket-send-binary} is used.
If the @var{data} is not one of the aboves, then @code{&assertion} is raised.
}

@define[Function]{@name{websocket-ping}
 @args{websocket data :optional (timeout #f) (timeout-value #f)}}
@desc{Send ping control frame whose data is @var{data} to @var{websocket}
and returns @var{websocket}.

The procedure waits until the endpoint returns pong control frame. If the
pong frame doesn't contain the same @var{data} in its payload, then
@code{&websocket-pong} is raised.

The optional argument @var{timeout} specifies how long the procedure waits
the pong response.

The optional argument @var{timeout-value} is an alternative value when
the endpoint didn't return pong in time.

CAUTION: this procedure depends on the endpoint's behaviour.
}


@sub*section{Event handling}

The user level APIs are event driven like JavaScript's WebSocket API.
Whenever an event is occured, then the configured event handler is
invoked.

There are 2 types of error handler invocations. One is calling active
procedures such as @code{websocket-send}. The other one is passive situation
such as receiving frame from endpoint. The first case, all active procedures
would catch @code{&websocket} and its sub conditions. If the condition is
@code{&websocket-engine} or its sub condition, then the condition is
re-raised. Other @code{&websocket} conditions are not re-raised by the
APIs. Other conditions are simply re-raised without invcating event handler.
On the second case, it's mostly the same, but when
@code{&websocket-closed} is raised, then event handler won't be invoked.

@define[Function]{@name{websocket-on-text-message}
 @args{websocket event-handler}}
@define[Function]{@name{websocket-on-binary-message}
 @args{websocket event-handler}}
@desc{Sets @var{event-handler} for receiving text message and binary
message to @var{websocket}, respectively.

The @var{event-handler} must be a procedure which accepts 2 arguments,
WebSocket object and response data, string or bytevector.

If the @var{event-handler} raises an error, and it's not
a @code{&websocket}, then the dispatcher thread opened by
@code{websocket-open} stops running.

The procedure returns @var{websocket}.
}

@define[Function]{@name{websocket-on-open} @args{websocket event-handler}}
@define[Function]{@name{websocket-on-close} @args{websocket event-handler}}
@desc{Sets @var{event-handler} for open and close event to @var{websocket},
respectively.

The @var{event-handler} must be a procedure which accepts 1 argument,
WebSocket object.

The procedure returns @var{websocket}.
}

@define[Function]{@name{websocket-on-error} @args{websocket event-handler}}
@desc{Sets @var{event-handler} for error situation to @var{websocket}.

The @var{event-handler} must be a procedure which accepts 2 arguments,
WebSocket object and a captured error. 

If the @var{event-handler} raises an error, then it would be propagated
caller of the user level APIs.

The procedure returns @var{websocket}.
}

@subsubsection{Low level APIs}

The low level APIs can be used when users want to implement WebSocket process
in programatic way. The user level APIs are implemented on this APIs.

@sub*section{WebSocket Connection}

@define[Function]{@name{make-websocket-connection}
 @args{uri :optional (engine 'http)}}
@desc{Creates WebSocket connection object. @var{uri} and @var{engine} are
the same as @var{make-websocket}.
}
 
@define[Function]{@name{websocket-connection?} @args{obj}}
@desc{Returns #t if given @var{obj} is WebSocket connection object
otherwise #f.}

@define[Function]{@name{websocket-connection-handshake!}
 @args{connection :optional (protocols '()) (extensions '()) :rest}}
@desc{Processes handshake on given @var{connection}. All the optional
arguments are passed to underlying handshake engine.
}

@define[Function]{@name{websocket-connection-close!} @args{connection}}
@desc{Closes given @var{connection}. This procedure simply closes
socket connection.}

@define[Function]{@name{websocket-connection-closed?} @args{connection}}
@desc{Returns #t if the @var{connection} is not connected otherwise #f.}

@define[Function]{@name{websocket-connection-pong-queue} @args{connection}}
@desc{Returns a shared queue which stores pong data from the endpoint of
the @var{connection}.}

@sub*section{WebSocket Messages}

@define[Function]{@name{websocket-send-text}
 @args{connection data :optional (start 0) split}}
@define[Function]{@name{websocket-send-binary}
 @args{connection data :optional (start 0) split}}
@desc{Sends text or binary frame of @var{data} to given @var{connection}.

If the first procedure is called, then @var{data} must be a string.

If the second procedure is called, then @var{data} must be a bytevector.

The optional argument start specifies from which point of @var{data} to send.

The optional argument @var{split} is specified, it must be an non negative
integer or procedure which accepts 2 arguments, @var{data} and start, then
the @var{data} is sent fragmented.
}

@define[Function]{@name{websocket-send-close}
 @args{connection :optional (data #vu8()) (wait? #t)}}
@desc{Sends close control frame to given @var{connection}.

If the optional argument @var{data} is specified, it must be a properly
coded connection close status, the it's sent as a payload of close frame.

If the optional argument @var{wait?} is true value, then the procedure
waits until the endpoint sends close frame back.

If @var{wait?} is #f, then it is users responsibility to receive the
response close frame and closes the connection.
}

@define[Function]{@name{websocket-send-ping}
 @args{connection :optional (data #vu8())}}
@define[Function]{@name{websocket-send-pong}
 @args{connection :optional (data #vu8())}}
@desc{Sends ping and pong frame to given @var{connection} respectively.

The optional argument @var{data} is sent as its payload.

NB: the payload must be less than 125 bytes.
}

@define[Function]{@name{websocket-receive}
 @args{connection :key (push-pong? #f)}}
@desc{Receives a data frame and return 2 values, opcode and payload.

If the frame is text, then returning value is a string. Otherwise
returning value is a bytevector.

If the keyword argument @var{push-pong?} is true value, then
payload of pong control frame will be pushed in to the @code{pong-queue}
of the @var{connection}.

This procedure doesn't return ping and pong control frame.
}

@define[Function]{@name{websocket-receive-fragments}
 @args{connection proc :key (push-pong? #f)}}
@desc{Receives a data fragments until its end and return the result of
the last call of @var{proc}.

The @var{proc} must accept 3 values, @var{finished?}, @var{opcode} and
@var{data}.

This procedure is useful to receive a huge frame without concising.

The @code{websocket-receive} is implemented on top of this procedure
like this:
@codeblock{
(define (websocket-receive conn :key (push-pong? #f))
  (define (convert opcode data)
    (if (eqv? opcode +websocket-text-frame+)
	(values opcode (utf8->string data))
	(values opcode data)))
  (let-values (((out extract) (open-bytevector-output-port)))
    (websocket-receive-fragments conn
     (lambda (fin? opcode data)
       (put-bytevector out data)
       (if fin? (convert opcode (extract)) (values opcode #f)))
     :push-pong? push-pong?)))
}
}

@define[Function]{@name{websocket-compose-close-status}
 @args{status :optional message}}
@desc{@var{status} must be an non negative integer whose bit length
must be less than or equal to 16 bits.

If optional argument @var{message} is specified, then it must be a
string.

Composes a payload of close frame.
}
@define[Function]{@name{websocket-parse-close-status} @args{data}}
@desc{@var{data} must be a payload of close frame.

Returns 2 values, status and mesasge of the close frame payload.
}

@define[Constants]{@name{+websocket-text-frame+}}
@define[Constants]{@name{+websocket-binary-frame+}}
@desc{WebSocket frame type constants. It represents text frame and
binary frame, respectively.}

@define[Constants]{@name{+websocket-close-frame+}}
@define[Constants]{@name{+websocket-ping-frame+}}
@define[Constants]{@name{+websocket-pong-frame+}}
@desc{WebSocket control frame type constants. It represents cloe frame,
ping frame, and pong frame, respectively.}

@subsubsection{Conditions}

@code{&websocket} is a base condition of the WebSocket library's condition.
The hierarchy is the following:
@codeblock{
+ &error
  + &websocket
    + &websocket-engine
      + &websocket-engine-not-found
         - engine
	 - reason
      + &websocket-engine-scheme
         - scheme
      + &websocket-engine-connection
         - host
	 - port
    + &websocket-closed
       - status
       - message
    + &websocket-pong
       - pong-data
    + &websocket-close-timeout
}

The condition types are not exported by the library.

@define[Function]{@name{websocket-error?} @args{obj}}}
@desc{Returns #t if @var{obj} is an instance of @code{&websocket} otherwise #f}

@define[Function]{@name{websocket-engine-error? @args{obj}}}
@desc{Returns #t if @var{obj} is an instance of @code{&websocket-engine}
otherwise #f}

@define[Function]{@name{websocket-engine-not-found-error? @args{obj}}}
@desc{Returns #t if @var{obj} is an instance of
@code{&websocket-engine-not-found} otherwise #f}
@define[Function]{@name{websocket-error-engine} @args{condition}}
@desc{Returns value of @code{engine} field of
@code{&websocket-engine-not-found}.}
@define[Function]{@name{websocket-error-reason} @args{condition}}
@desc{Returns value of @code{reason} field of
@code{&websocket-engine-not-found}.}

@define[Function]{@name{websocket-engine-scheme-error? @args{obj}}}
@desc{Returns #t if @var{obj} is an instance of
@code{&websocket-engine-scheme} otherwise #f}
@define[Function]{@name{websocket-error-scheme} @args{condition}}
@desc{Returns value of @code{scheme} field of
@code{&websocket-engine-scheme}.}

@define[Function]{@name{websocket-engine-connection-error? @args{obj}}}
@desc{Returns #t if @var{obj} is an instance of
@code{&websocket-engine-connection} otherwise #f}
@define[Function]{@name{websocket-error-host} @args{condition}}
@desc{Returns value of @code{host} field of
@code{&websocket-engine-connection}.}
@define[Function]{@name{websocket-error-port} @args{condition}}
@desc{Returns value of @code{port} field of
@code{&websocket-engine-connection}.}

@define[Function]{@name{websocket-closed-error? @args{obj}}}
@desc{Returns #t if @var{obj} is an instance of
@code{&websocket-closed} otherwise #f}
@define[Function]{@name{websocket-error-status} @args{condition}}
@desc{Returns value of @code{stauts} field of
@code{&websocket-cloded}.}
@define[Function]{@name{websocket-error-message} @args{condition}}
@desc{Returns value of @code{message} field of
@code{&websocket-cloded}.}

@define[Function]{@name{websocket-pong-error? @args{obj}}}
@desc{Returns #t if @var{obj} is an instance of
@code{&websocket-pong} otherwise #f}
@define[Function]{@name{websocket-error-pong-data} @args{condition}}
@desc{Returns value of @code{pong-data} field of
@code{&websocket-pong}.}

@define[Function]{@name{websocket-close-timeout-error? @args{obj}}}
@desc{Returns #t if @var{obj} is an instance of
@code{&websocket-close-timeout} otherwise #f}

@; TBD writing engine and lower level APIs such as
@;     websocket-recv-frame and so