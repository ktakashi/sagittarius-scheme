[§2] (sagittarius socket) - socket library {#lib.sagittarius.socket}
-------------

This section describes low level socket API on Sagittarius. The APIs are mostly
the same signature as Ypsilon and mosh. The following example is simple echo
server, it receives input from a client and just returns it to the client.

The example program is from example/socket/echo.scm.

``````````scheme
(import (rnrs) (sagittarius socket))
;; creates echo server socket with port number 5000
(define echo-server-socket (make-server-socket "5000"))
;; addr is client socket
(let loop ((addr (socket-accept echo-server-socket)))
  (call-with-socket addr
   (lambda (sock)
     ;; socket-port creates binary input/output port
     ;; make it transcoded port for convenience.
     (let ((p (transcoded-port (socket-port sock)
			       ;; on Sagittarius Scheme native-transcoder
			       ;; uses utf8 codec for ASCII compatibility.
			       ;; For socket programming it might be better
			       ;; to specify eol-style with crlf.
			       ;; But this sample just shows how it goes.
			       (native-transcoder))))
       (call-with-port p
	(lambda (p)
	  (put-string p "please type something\n\r")
	  (put-string p "> ")
	  ;; gets line from client.
	  (let lp2 ((r (get-line p)))
	    (unless (eof-object? r)
	      (print "received: " r)
	      ;; just returns message from client.
	      ;; NB: If client type nothing, it'll throw assertion-violation.
	      (put-string p r)
	      (put-string p "\r\n> ")
	      ;; waits for next input.
	      (lp2 (get-line p)))))))))
  ;; echo server waits next connection.
  (loop (socket-accept echo-server-socket)))
``````````

###### [!Library] `(sagittarius socket)` 

This library provides procedures for socket programming.

###### [!Function] `make-client-socket`  _node_ _srvice_ _:opational_ _(ai_family_ `AF_INET` _)_ _
_ _(ai_socktype_ `SOCK_STREAM` _)_ _
_ _(ai_flags_ _(+_ `AI_V4MAPPED` `AI_ADDRCONFIG` _))_ _(ai_protocol_ _0)_

_Node_ and _service_ must be string or #f. Other optional arguments
must be exact integer.

Returns a client socket connected to an Internet address. The Internet address
is identified by _node_ and _service_. The `make-client-socket`uses `getaddrinfo(3)` to look it up. The arguments _node_, _service_,
_ai-family_, _ai-socktype_, _ai-flags_ and _ai-protocol_ are
passed to `getaddrinfo(3)` as corresponding parameters. For more detail,
see reference of `getaddrinfo(3)`.

_Node_ is a network address, ex) "www.w3.org", "localhost", "192.168.1.1". 

_Service_ is a network service, ex) "http", "ssh", "80", "22".

_Ai-family_ is an address family specifier. Predefined specifiers are listed
below.

- AF_INET
- AF_INET6
- AF_UNSPEC

_Ai-sockettype_ is a socket type specifier. Predefined specifiers are listed
below.

- SOCK_STREAM
- SOCK_DGRAM
- SOCK_RAW

_Ai-flags_ is an additional options specifier. Predefined specifiers are listed
below.

- AI_ADDRCONFIG
- AI_ALL
- AI_CANONNAME
- AI_NUMERICHOST
- AI_NUMERICSERV
- AI_PASSIVE
- AI_V4MAPPED

_Ai-protocol_ is a protocol specifier. Predefined specifiers are listed below.

- IPPROTO_TCP
- IPPROTO_UDP
- IPPROTO_RAW



###### [!Function] `make-server-socket`  _service_ _:optional_ _(ai_family_ `AF_INET` _)_ _
_ _(ai_socktype_ `SOCK_STREAM` _)_ _
_ _(ai_protocol_ _0)_

_Service_ must be string or #f. Other optional arguments must be exact
integer. Returns a server socket waiting for connections. The argument details
are the same as the `make-client-socket`.


###### [!Function] `socket?`  _obj_

Returns #t if _obj_ is socket object, otherwise #f.

###### [!Function] `socket-port`  _socket_ _:optional_ _(close?_ _#t)_

_Socket_ must be a socket object. Returns a binary input/output port
associated with _socket_.

If optional argument _close?_ is #f then the port won't close socket
when port is closing or being GCed.


###### [!Function] `socket-input-port`  _socket_
###### [!Function] `socket-output-port`  _socket_

[SRFI-106] _Socket_ must be a socket object. Returns a binary input
and output port associated with _socket_, respectively.


###### [!Function] `call-with-socket`  _socket_ _proc_

_Socket_ must be a socket object. _Proc_ must accept one argument.

The `call-with-socket` calls a procedure with socket as an argument.

This procedure is analogy with `call-with-port`.


###### [!Function] `shutdown-port`  _port_ _how_

_Port_ must be associated with a socket.

Shutdowns associated port according to _how_.


###### [!Function] `shutdown-input-port`  _port_
###### [!Function] `shutdown-output-port`  _port_

_Port_ must be associated with a socket.

The `shutdown-output-port` and `shutdown-input-port` shutdown
output or input connection of a socket associated with _port_ respectively.


###### [!Function] `socket-accept`  _socket_

_Socket_ must be a socket object created by
`make-server-socket`.

Wait for an incoming connection request and returns a fresh connected client
socket.

This procedures is a thin wrapper of POSIX's `accept(2)`.

If the calling thread is interrupted by `thread-interrupt!`, then
the procedure returns #f.


###### [!Function] `socket-recv`  _socket_ _size_ _:optional_ _(flags_ _0)_

_Socket_ must be a socket object.

Receives a binary data block from given socket. If zero length bytevector is
returned, it means the peer connection is closed.

This procedures is a thin wrapper of POSIX's `recv(2)`.


###### [!Function] `socket-send`  _socket_ _bytevector_ _:optional_ _(flags_ _0)_

_Socket_ must be a socket object.

Sends a binary data block to given socket and returns the sent data size.

This procedures is a thin wrapper of POSIX's `send(2)`.


###### [!Function] `socket-shutdown`  _socket_ _how_

_Socket_ must be a socket object. _How_ must be one of the
`SHUT_RD`, `SHUT_WR` or `SHUT_RDWR`.

The `socket-shutdown` shutdowns socket.

SHUT_RD
: shutdowns input.

SHUT_WR
: shutdowns output.

SHUT_RDWR
: shutdowns input and output.



###### [!Function] `socket-close`  _socket_

_Socket_ must be a socket object. Closes _socket_.

### [§3] Socket information

###### [!Class] `<socket-info>` 

The socket information immutable class. This class has 3 slots

###### [!Slot] `hostname` 

This slot has string value represents own or peer socket host.

###### [!Slot] `ip-address` 

This slot has ip-address object of own or peer socket.

###### [!Slot] `port` 

This slot has integer value of own or peer socket port number.



###### [!Function] `socket-peer`  _socket_

_Socket_ must be a socket object.

Returns socket info object or #f. The socket info object contains hostname,
IP address and port number. These information is retrieved from getpeername(2).


###### [!Function] `socket-name`  _socket_

_Socket_ must be a socket object.

Returns the name string of socket or #f if the socket doesn't have name.


###### [!Function] `socket-node`  _socket_

_Socket_ must be a socket object.

Returns the node string of socket. Returns #f for server socket.


###### [!Function] `socket-service`  _socket_

_Socket_ must be a socket object.

Returns the service string of socket.


###### [!Function] `socket-info`  _socket_

_Socket_ must be a socket object.

Returns socket info object or #f. The socket info object contains hostname,
IP address and port number. These information is retrieved from getsockname(2).


###### [!Function] `socket-info-values`  _socket_ _:key_ _(type_ _'peer)_

_Socket_ must be a socket object.

Returns 3 values; hostname, IP address and port number. This procedures is
for convenience to handle socket info object.

The keyword argument specifies which socket info it should retrieve. If the
type is `peer` then it uses `socket-peer`. If it is `info`,
then it uses `socket-info`

### [§3] IP address operations

###### [!Function] `ip-address->string`  _ip_

_ip_ must be an IP address object returned from the second value 
of `socket-info-values`.

Converts given IP address object to human readable string.


###### [!Function] `ip-address->bytevector`  _ip_

_ip_ must be an IP address object returned from the second value 
of `socket-info-values`.

Converts given IP address object to bytevector.


### [§3] Low level APIs {#socket.low.level}

The low level socket APIs are almost compatible with BSD socket.

#### [§4] Socket

###### [!Function] `socket-sendto`  _socket_ _bvtevector_ _sockaddr_ _:optional_ _(flags_ _0)_

Sends a binary data block to given sockaddr and returns the
sent data size.

This procedures is a thin wrapper of POSIX's `sendto (2)`.


###### [!Function] `socket-recvfrom`  _socket_ _sockaddr_ _:optional_ _(flags_ _0)_

Receives a binary data block from given sockaddr. If zero length
bytevector is returned, it means the peer connection is closed.

This procedures is a thin wrapper of POSIX's `recvfrom (2)`.


###### [!Function] `make-socket`  _ai-family_ _ai-socktype_ _ai-protocol_

Creates socket object. The procedure returns #f if it couldn't create
a socket. `SO_NOSIGPIPE` socket option is set to the created socket.

This procedure is a thin wrapper of `socket (2)`.


###### [!Function] `socket-connect!`  _socket_ _addrinfo_
###### [!Function] `socket-connect!`  _socket_ _addrinfo_ _timeout_

Initiate connection on the given _socket_ with given
addrinfo _addrinfo_.

If the second form is used, then argument _timeout_ represents a
connection timeout. The value must be a timeout value of `socket-select`.

This procedure is a thin wrapper of `connect (2)`.


###### [!Function] `socket-bind!`  _socket_ _addrinfo_

Binds a name to the given socket _socket_ with given addrinfo
_addrinfo_.

This procedure is a thin wrapper of `bind (2)`.


###### [!Function] `socket-listen!`  _socket_ _backlog_

Listen for connections on the given socket _socket_.

This procedure is a thin wrapper of `listen (2)`.


###### [!Function] `socket-setsockopt!`  _socket_ _level_ _name_ _value_

Sets socket option on the given socket _socket_.

_level_ must be an integer and should be one of the followings:

- SOL_SOCKET
- SOL_TCP
- SOL_IP

_name_ must be an integer and should be one of the followings:

- SO_ACCEPTCONN
- SO_BINDTODEVICE
- SO_BROADCAST
- SO_DEBUG
- SO_DONTROUTE
- SO_ERROR
- SO_KEEPALIVE
- SO_LINGER
- SO_OOBINLINE
- SO_PASSCRED
- SO_PEERCRED
- SO_PRIORITY
- SO_RCVBUF
- SO_RCVLOWAT
- SO_RCVTIMEO
- SO_REUSEADDR
- SO_REUSEPORT
- SO_SNDBUF
- SO_SNDLOWAT
- SO_SNDTIMEO
- SO_TIMESTAMP
- SO_TYPE
- TCP_NODELAY
- TCP_MAXSEG
- TCP_CORK
- IP_OPTIONS
- IP_PKTINFO
- IP_RECVTOS
- IP_RECVTTL
- IP_RECVOPTS
- IP_TOS
- IP_TTL
- IP_HDRINCL
- IP_RECVERR
- IP_MTU_DISCOVER
- IP_MTU
- IP_ROUTER_ALERT
- IP_MULTICAST_TTL
- IP_MULTICAST_LOOP
- IP_ADD_MEMBERSHIP
- IP_DROP_MEMBERSHIP
- IP_MULTICAST_IF

The _value_ must be either bytevector or integer.

This procedure is a thin wrapper of `setsockopt (2)`.


###### [!Function] `socket-getsockopt`  _socket_ _level_ _name_ _size_

Gets socket option on the given socket _socket_.

The _level_ and _name_ are the same as `socket-setsockopt!`.

_size_ must be an integer. If the value is positive number, then the
returning value is a bytevector whose element count is _size_ and
contains the socket option converted to byte array. Otherwise it returns
an integer value.


###### [!Function] `socket-nonblocking!`  _socket_
###### [!Function] `socket-blocking!`  _socket_

Converts given socket to nonblocking socket and blocking socket,
respectively.


###### [!Function] `socket-set-read-timeout!`  _socket_ _timeout_

Sets read timeout of _timeout_ to the given _socket_.

The _timeout_ must be either an exact integer represents milliseconds,
or a time object.


###### [!Function] `socket-select`  _rfds_ _wfds_ _efds_ _timeout_
###### [!Function] `socket-select!`  _rfds_ _wfds_ _efds_ _timeout_

Monitor given fdset.

_rfds_, _wfds_ and _efds_ must be fdset object.

_timeout_ must be #f, integer, time object or pair of integers. If this
value is not #f, then the procedure waits only specified amount of time or
something interesting happens. Otherwise infinite time or something
interesting happens.

This procedure blocks the calling thread, and it can be interrupted by
`thread-interrupt!`.

This procedure is a thin wrapper of `select (2)`.


###### [!Function] `socket-read-select`  _timeout_ _sockets_ _..._
###### [!Function] `socket-write-select`  _timeout_ _sockets_ _..._
###### [!Function] `socket-error-select`  _timeout_ _sockets_ _..._

Waits until the given sockets _sockets_ have something interesting.
This is the convenient procedure for `socket-select`.

_timeout_ is the same as `socket-select`.

`socket-read-select` can be used to detect if the given sockets have
readable data.

`socket-write-select` can be used to detect if the given sockets are
still active.

`socket-error-select` can be used to detect if the given sockets are
readable data. This procedure might not be so interesting since it can be
done by `socket-read-select`.


#### [§4] Addrinfo

###### [!Function] `addrinfo?`  _obj_

Returns #t if given _obj_ is an addrinfo, otherwise #f.

###### [!Function] `make-addrinfo` 

Creates empty addrinfo object.

The object has the following slots:

- `family`
- `socktype`
- `flags`
- `protocol`
- `sockaddr`
- `next`



###### [!Function] `make-hint-addrinfo`  _family_ _socktype_ _flags_ _protocol_

Creates an addrinfo with given flags. This can be used as hint for
`get-addrinfo`.

###### [!Function] `get-addrinfo`  _addrinfo_

Gets addrinfo of given hint addrinfo.

When the procedure fails, then `&i/o` is raised.

This procedure is a thin wrapper of `getaddrinfo (3)`.


###### [!Function] `next-addrinfo`  _addrinfo_

Retrieves next addrinfo of given _addrinfo_ if availalbe, otherwise
returns #f.

###### [!Function] `addrinfo-sockaddr`  _addrinfo_

Retrieves `sockaddr` slot of given _addrinfo_.

The returning value can be used `socket-recvfrom` and `socket-sendto`.


###### [!Function] `sockaddr?`  _obj_

Returns #t if given _obj_ is an sockaddr object, otherwise #f.

#### [§4] FD sets

###### [!Function] `fdset?`  _obj_

Returns #t if given _obj_ is a fdset object, otherwise #f.

###### [!Function] `make-fdset` 

Creates a empty fdset object.

###### [!Function] `sockets->fdset`  _sockets_

Creates a fdset from given socket list _sockets_.

###### [!Function] `fdset-ref`  _fdset_ _socket_

Returns #t if the given _socket_ is set to _fdset_, otherwise #f.

###### [!Function] `fdset-set!`  _fdset_ _socket_ _flag_

Sets/unsets the given socket _socket_ on _fdset_.

If the _flags_ is #f, then the procedure unsets the _socket_.

If the _flags_ is #t, then the procedure sets the _socket_.


###### [!Function] `collect-sockets`  _fdset_

Returns a list of socket which are set on _fdset_.

#### [§4] Socket conditions

Above APIs may raise either `&socket` or `&host-not-found`. The first
condition is raised when socket related operation failed, for example
`socket-send`. The latter condition is raised when `get-addrinfo` is
failed.

NOTE: `make-client-socket` and `make-server-socket` may raise
`&host-not-found` when the given _node_ or _service_ is not a
valid value.

The condition hierarchy is the following:

``````````scheme
&i/o
 + &host-not-found (node service)
 + &socket (socket)
    + &socket-connection
    + &socket-closed
    + &socket-read-timeout
    + &socket-port (port)
``````````

###### [!Condition Type] `&host-not-found` 
###### [!Function] `host-not-found-error?`  _obj_
###### [!Function] `make-host-not-found-error`  _node_ _service_
###### [!Function] `host-not-found-error-node`  _host-not-found_
###### [!Function] `host-not-found-error-service`  _host-not-found_

This condition describes the combination of _node_ and _service_does not exist.


###### [!Condition Type] `&socket` 
###### [!Function] `socket-error?`  _obj_
###### [!Function] `make-socket-error`  _socket_
###### [!Function] `socket-error-socket`  _socket-error_

This condition describes general socket operation error.

###### [!Condition Type] `&socket-connection` 
###### [!Function] `socket-connection-error?`  _obj_
###### [!Function] `make-socket-connection-error`  _socket_

This condition describes socket connection error.

###### [!Condition Type] `&socket-closed` 
###### [!Function] `socket-closed-error?`  _obj_
###### [!Function] `make-socket-closed-error`  _socket_

This condition describes socket closed error.

###### [!Condition Type] `&socket-read-timeout` 
###### [!Function] `socket-closed-error?`  _obj_
###### [!Function] `make-socket-closed-error`  _socket_

This condition describes socket read timeout error.

###### [!Condition Type] `&socket-port` 
###### [!Function] `socket-port-error?`  _obj_
###### [!Function] `make-socket-port-error`  _socket_ _port_
###### [!Function] `socket-error-port`  _socket-port-error_

This condition describes error of socket port operation. Particularly,
when `port-ready` procedure is called on socket port and
`select (2)` failed.

NOTE: Read or write failure of socket port raises `&i/o-read` or
`&i/o-write` the same as other ports for compatibility.

NOTE2: This condition may be signalled when `get-bytevector-all` is
called on socket port since it checks whether or not the given port is ready. 


