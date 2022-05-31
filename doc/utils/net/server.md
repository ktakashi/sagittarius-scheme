[§2] (net server) - Simple server framework {#net.server}
-------------

###### [!Library] `(net server)` 

This library provides simple server framework.

Following example describes how to write a simple echo server with the APIs
this library provides.

``````````scheme
(import (net server) (sagittarius socket))

(define (handler server socket)
  ;; echo message is limited to 255 bytes in this example
  (let ((r (socket-recv socket 255)))
    ;; socket will be closed by the framework
    (socket-send socket r)))

(define server (make-simple-server "5000" handler))

(server-start! server)
``````````

Above example creates only one thread and if there are more than one
connection, then the latter one needs to wait until first one is done.
The library also provides mult threading server. Following example describes
how to make multi threading server.

``````````scheme
(import (net server) (sagittarius socket))

;; specifies maximum thread number
(define server-config (make-server-config :max-thread 5))

(define (handler server socket)
  (let ((r (socket-recv socket 255)))
    ;; socket will be closed by the framework
    (socket-send socket r)))

(define server (make-simple-server "5000" handler :config server-config))

(server-start! server)
``````````

If the server gets more than 5 connection simultaneously, then it tries to
wait until one of the connection's task finishes. If it doesn't finish in
time, then connection will be refused.

If clients keep the connection but server wants to handle requests more than
configured thread number, then specify _non-blocking?_ keyword argument
with #t.

``````````scheme
(import (net server) (sagittarius socket))

;; specifies maximum thread number
(define server-config (make-server-config :max-thread 5 :non-blocking? #t))

(define (handler server socket)
  (let ((r (socket-recv socket 255)))
    (if (eof-object? r)
        ;; close the socket after the process
        (socket-close socket)
        (socket-send socket r))))

(define server (make-simple-server "5000" handler :config server-config))

(server-start! server)
``````````

Above server example creates 5 threads and accept all requests. The requests
are dispatched to the least busy thread. There are couple of restrictions
to use this server. See the descirption of _non-blocking?_ keyword
argument.


### [§3] Server

###### [!Class] `<simple-server>` 

Simple server class.

###### [!Function] `server?`  _obj_

Returns #t if the _obj_ is an instance of `<simple-server>`,
otherwise #f.

###### [!Function] `make-simple-server`  _service_ _handler_ _:key_ _server-class_ _config_ _:allow-other-keys_

Creates a server object.

_service_ must be a string and indicates the service name or port number.

_handler_ must be a procedure accepts 2 arguments, server object 
_server_ created with this procedure and socket object _socket_.

Keyword argument _server-class_ is specified, it must be a class
inherits `<simple-server>`, then the procedure uses the class to
instantiate. And during instantiation, given other keys are passed.

Keyword argument _config_ is specified, it must be an instance
of _\<server-config>_ or subclass of it, then the server is created
according to the configuration.


###### [!Function] `server-config`  _server_

Returns configuration object used to create given server object
_server_.


###### [!Function] `server-stopped?`  _server_

Returns #t if given server is stopped.

NOTE: this also returns #t if the server is not started.


###### [!Function] `server-start!`  _server_ _:key_ _background_ _:allow-other-keys_

Starts the given _server_.

Keyword argument _background_ is true value then the server is started
background. By default it's #f.

The rest of keywords are passed to `on-server-start!`.

NOTE: Server object is not reusable thus once server is started, it is
impossible to restart the server.


###### [!Function] `server-stop!`  _server_ _:allow-other-keys_

Stops the given _server_.

The rest of keywords are passed to `on-server-stop!`.


###### [!Function] `wait-server-stop!`  _server_ _:optional_ _timeout_

Waits until the _server_ stops.

The _server_ must be stopping by accessing shutdown port otherwise
this procedure waits forever/for _timeout_ period.

Optional argument _timeout_ must be #f, time object or real number.
If the value is #f then this procedure waits forever until the _server_stops. By default #f.


###### [!Method] `on-server-start!`  _(server_ _<simple-server>)_ _rest_ _..._
###### [!Method] `on-server-stop!`  _(server_ _<simple-server>)_ _rest_ _..._

Hook methods for subclasses.

The first method is called when server is starting.

The second method is called after server is stopped.


### [§3] Configuration

###### [!Class] `<server-config>` 

Server configuration class.

###### [!Function] `server-config?`  _obj_

Returns #t if the _obj_ is an instance of `<server-config>`,
otherwise #f.

###### [!Function] `make-server-config`  _:key_ _shutdown-port_ _shutdown-handler_ _exception-handler_ _max-thread_ _
_ _max-retry_ _use-ipv6?_ _secure?_ _certificates_

Creates a server config object.

Following is the description of keyword arguments.

_shutdown-port_
: Specifying shutdown port. The value must be a string. If this is not
  specified, then the server doesn't have shutdown port.

_shutdown-handler_
: This is only used then _shutdown-port_ is specified. The value
  must be a procedure takes 2 arguments, _server_ and _socket_.
  When the procedure returns true value then server will be stopped.
  By default, it's a procedure always returns #t.

_exception-handler_
: Specifying exception handler. The value must be a procedure accepts
  3 arguments, _server_, _socket_ and _condition_. This
  is called when the server _handler_ raises an error. 
  NOTE: The passing _socket_ is **not** closed so that the handler can
  send messages to client socket.

_max-thread_
: Specifying max thread count. Default value is 1.

_max-retry_
: Specifying max retry count. When connection reached _max-thread_,
  then the server waits if the one of the connections finishes. The
  waiting period is half second (500 ms) and this value specifies
  how many times server waits.
  Default value is 10.

_non-blocking?_
: Creating non blocking server.
  If the server is non blocking server, then the server _handler_    must follow the following rules:
  - the _handler_ process must not block/stop even if the
            given socket is active.
  - the _handler_ process must close the socket when it's
            not needed.
  When handler raises an error and _exception-handler_ is specified,
  then the given socket won't be closed. So _exception-handler_ needs
  to decide whether the exception is continuable or not. Otherwise, server
  closes the socket.
  Specifying this keyword argument makes server ignore _max-retry_.

_use-ipv6?_
: Specifying whether or not the server uses IPv6.
  Default value is #f. (only IPv4)

_secure?_
_certificates_
: If _secure?_ is true value and _certificates_ is a list of
  X509 certificates, then the server uses TLS.

_private-key_
: If the server uses TLS, then this keyword argument is passed to
  `make-server-tls-socket`. It is strongly recommended to
  specify this keyword argument, otherwise key exchange is done
  anonymously, means no signature is sent.



### [§3] Socket detaching

Non blocking server manages sockets per threads. This feature is useful if
the server handler is reused per socket. However this prevents users to
write asynchronous call. The following procedure allow users to detach
sockets from the server.

###### [!Function] `server-detach-socket!`  _server_ _socket_

Detaches the given _socket_.

If the socket is detached, then all resource managements, including closing
socket, become users' responsibility.

This procedure is only available on non blocking server and can be called
inside of server handler. If the condition is not met, then `&assertion`is signaled.


