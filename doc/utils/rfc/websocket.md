[§2] (rfc websocket) - WebSocket {#rfc.websocket}
-------------

###### [!Library] `(rfc websocket)` 

This library provides WebSocket APIs defined by
[RFC 6455](https://tools.ietf.org/html/rfc6455).


Following is a simple example to use user level APIs.

``````````scheme
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
``````````

### [§3] User level APIs

###### [!Function] `make-websocket`  _uri_ _:key_ _(protocols_ _'())_ _(extensions_ _'())_ _(engine_ _'http)_

Creates an WebSocket object which communicate to given _uri_.

The URI reprented by _uri_ must be a valid WebSocket URI. If it's
not a valid URI, then `&websocket-engine` or its sub condition
shall be raised.

The keyword argument _protocols_ are a list of sub protocols of the
creating WebSocket. If this is set, then handshake will send it with
`Sec-WebSocket-Protocol` header or equivalent.

The keyword argument _extensions_ are a list of extensions of the
creating WebSocket. If this is set, then handshake will send it with
`Sec-WebSocket-Extensions` header or equivalent.

The keyword argument _engine_ is a type of handshake engine and
must be a symbol. This determines which handshake protocol it should
use. The default value is `http` which uses HTTP/1.1.



###### [!Function] `websocket?`  _obj_

Returns #t if given _obj_ is an WebSocket object otherwise #f.

###### [!Function] `websocket-open`  _websocket_

Operating handshake on given WebSocket object _websocket_ if
it's not opened, yet. And returns _websocket_.

If handshake failed, then `&websocket-engine` or its sub condition
shall be raised.

After successfull call of this procedure, the _websocket_ has a
message dispatcher thread.


###### [!Function] `websocket-close`  _websocket_ _:key_ _(status_ _#f)_ _(message_ _"")_ _(timeout_ _#f)_

Closes the given _websocket_ if it's open and returns the
_websocket_. This procedure also finishes the underlying thread
created by `websocket-open`.

The keyword argument _status_ is specified, it must be a non
negative integer which has less then or equal to 16 bits length, then
the procedure sends it as a connection close code.

The keyword argument _message_ is specified, must be a string, then
the procedure sends it as a connection close message. This is sent only
if the _status_ is specified otherwise ignored.

The keyword argument _timeout_ is used as a timeout period for waiting
underlying dispatcher thread. If the thread didn't finish in the specified
period, then it'd be terminated and `&websocket-close-timeout` is
raised.

If the underlying thread raised an `&uncaught-exception`, then the
procedure raises its reason.


###### [!Function] `websocket-send`  _websocket_ _data_ _:optional_ _start_ _splitter_

Sends given _data_ to the _websocket_ and returns
_websocket_. This procedure runs in atomic so the _data_ is sent
in one fragmentation sequence.

The optional arguments are passed to underlying data transport procedures.
If the _data_ is a string, then `websocket-send-text` is used.
If the _data_ is a bytevector, then `websocket-send-binary` is used.
If the _data_ is not one of the aboves, then `&assertion` is raised.


###### [!Function] `websocket-ping`  _websocket_ _data_ _:optional_ _(timeout_ _#f)_ _(timeout-value_ _#f)_

Send ping control frame whose data is _data_ to _websocket_and returns _websocket_.

The procedure waits until the endpoint returns pong control frame. If the
pong frame doesn't contain the same _data_ in its payload, then
`&websocket-pong` is raised.

The optional argument _timeout_ specifies how long the procedure waits
the pong response.

The optional argument _timeout-value_ is an alternative value when
the endpoint didn't return pong in time.

CAUTION: this procedure depends on the endpoint's behaviour.


#### [§4] Event handling

The user level APIs are event driven like JavaScript's WebSocket API.
Whenever an event is occured, then the configured event handler is
invoked.

There are 2 types of error handler invocations. One is calling active
procedures such as `websocket-send`. The other one is passive situation
such as receiving frame from endpoint. The first case, all active procedures
would catch `&websocket` and its sub conditions. If the condition is
`&websocket-engine` or its sub condition, then the condition is
re-raised. Other `&websocket` conditions are not re-raised by the
APIs. Other conditions are simply re-raised without invcating event handler.
On the second case, it's mostly the same, but when
`&websocket-closed` is raised, then event handler won't be invoked.

###### [!Function] `websocket-on-text-message`  _websocket_ _event-handler_
###### [!Function] `websocket-on-binary-message`  _websocket_ _event-handler_

Sets _event-handler_ for receiving text message and binary
message to _websocket_, respectively.

The _event-handler_ must be a procedure which accepts 2 arguments,
WebSocket object and response data, string or bytevector.

If the _event-handler_ raises an error, and it's not
a `&websocket`, then the dispatcher thread opened by
`websocket-open` stops running.

The procedure returns _websocket_.


###### [!Function] `websocket-on-open`  _websocket_ _event-handler_
###### [!Function] `websocket-on-close`  _websocket_ _event-handler_

Sets _event-handler_ for open and close event to _websocket_,
respectively.

The _event-handler_ must be a procedure which accepts 1 argument,
WebSocket object.

The procedure returns _websocket_.


###### [!Function] `websocket-on-error`  _websocket_ _event-handler_

Sets _event-handler_ for error situation to _websocket_.

The _event-handler_ must be a procedure which accepts 2 arguments,
WebSocket object and a captured error. 

If the _event-handler_ raises an error, then it would be propagated
caller of the user level APIs.

The procedure returns _websocket_.


### [§3] Low level APIs

The low level APIs can be used when users want to implement WebSocket process
in programatic way. The user level APIs are implemented on this APIs.

#### [§4] WebSocket Connection

###### [!Function] `make-websocket-connection`  _uri_ _:optional_ _(engine_ _'http)_

Creates WebSocket connection object. _uri_ and _engine_ are
the same as _make-websocket_.


 
###### [!Function] `websocket-connection?`  _obj_

Returns #t if given _obj_ is WebSocket connection object
otherwise #f.

###### [!Function] `websocket-connection-handshake!`  _connection_ _:optional_ _(protocols_ _'())_ _(extensions_ _'())_ _:rest_

Processes handshake on given _connection_. All the optional
arguments are passed to underlying handshake engine.


###### [!Function] `websocket-connection-close!`  _connection_

Closes given _connection_. This procedure simply closes
socket connection.

###### [!Function] `websocket-connection-closed?`  _connection_

Returns #t if the _connection_ is not connected otherwise #f.

###### [!Function] `websocket-connection-pong-queue`  _connection_

Returns a shared queue which stores pong data from the endpoint of
the _connection_.

#### [§4] WebSocket Messages

###### [!Function] `websocket-send-text`  _connection_ _data_ _:optional_ _(start_ _0)_ _split_
###### [!Function] `websocket-send-binary`  _connection_ _data_ _:optional_ _(start_ _0)_ _split_

Sends text or binary frame of _data_ to given _connection_.

If the first procedure is called, then _data_ must be a string.

If the second procedure is called, then _data_ must be a bytevector.

The optional argument start specifies from which point of _data_ to send.

The optional argument _split_ is specified, it must be an non negative
integer or procedure which accepts 2 arguments, _data_ and start, then
the _data_ is sent fragmented.


###### [!Function] `websocket-send-close`  _connection_ _:optional_ _(data_ _#vu8())_ _(wait?_ _#t)_

Sends close control frame to given _connection_.

If the optional argument _data_ is specified, it must be a properly
coded connection close status, the it's sent as a payload of close frame.

If the optional argument _wait?_ is true value, then the procedure
waits until the endpoint sends close frame back.

If _wait?_ is #f, then it is users responsibility to receive the
response close frame and closes the connection.


###### [!Function] `websocket-send-ping`  _connection_ _:optional_ _(data_ _#vu8())_
###### [!Function] `websocket-send-pong`  _connection_ _:optional_ _(data_ _#vu8())_

Sends ping and pong frame to given _connection_ respectively.

The optional argument _data_ is sent as its payload.

NB: the payload must be less than 125 bytes.


###### [!Function] `websocket-receive`  _connection_ _:key_ _(push-pong?_ _#f)_

Receives a data frame and return 2 values, opcode and payload.

If the frame is text, then returning value is a string. Otherwise
returning value is a bytevector.

If the keyword argument _push-pong?_ is true value, then
payload of pong control frame will be pushed in to the `pong-queue`of the _connection_.

This procedure doesn't return ping and pong control frame.


###### [!Function] `websocket-receive-fragments`  _connection_ _proc_ _:key_ _(push-pong?_ _#f)_

Receives a data fragments until its end and return the result of
the last call of _proc_.

The _proc_ must accept 3 values, _finished?_, _opcode_ and
_data_.

This procedure is useful to receive a huge frame without concising.

The `websocket-receive` is implemented on top of this procedure
like this:

``````````scheme
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
``````````



###### [!Function] `websocket-compose-close-status`  _status_ _:optional_ _message_

_status_ must be an non negative integer whose bit length
must be less than or equal to 16 bits.

If optional argument _message_ is specified, then it must be a
string.

Composes a payload of close frame.


###### [!Function] `websocket-parse-close-status`  _data_

_data_ must be a payload of close frame.

Returns 2 values, status and mesasge of the close frame payload.


###### [!Constants] `+websocket-text-frame+` 
###### [!Constants] `+websocket-binary-frame+` 

WebSocket frame type constants. It represents text frame and
binary frame, respectively.

###### [!Constants] `+websocket-close-frame+` 
###### [!Constants] `+websocket-ping-frame+` 
###### [!Constants] `+websocket-pong-frame+` 

WebSocket control frame type constants. It represents cloe frame,
ping frame, and pong frame, respectively.

### [§3] Conditions

`&websocket` is a base condition of the WebSocket library's condition.
The hierarchy is the following:

``````````scheme
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
``````````

The condition types are not exported by the library.

###### [!Function] `websocket-error?`  _obj_
