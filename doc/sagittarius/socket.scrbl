@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "lib.sagittarius.socket"]{(sagittarius socket) - socket library}

This section describes low level socket API on Sagittarius. The APIs are mostly
the same signature as Ypsilon and mosh. The following example is simple echo
server, it receives input from a client and just returns it to the client.

The example program is from example/socket/echo.scm.

@codeblock{
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
}

@define[Library]{@name{(sagittarius socket)}}
@desc{This library provides procedures for socket programming.}

@define[Function]{@name{make-client-socket}
 @args{node srvice :opational (ai_family @code{AF_INET})
 (ai_socktype @code{SOCK_STREAM})
 (ai_flags (+ @code{AI_V4MAPPED} @code{AI_ADDRCONFIG})) (ai_protocol 0)}}
@desc{@var{Node} and @var{service} must be string or #f. Other optional arguments
must be exact integer.

Returns a client socket connected to an Internet address. The Internet address
is identified by @var{node} and @var{service}. The @code{make-client-socket}
uses @code{getaddrinfo(3)} to look it up. The arguments @var{node}, @var{service},
@var{ai-family}, @var{ai-socktype}, @var{ai-flags} and @var{ai-protocol} are
passed to @code{getaddrinfo(3)} as corresponding parameters. For more detail,
see reference of @code{getaddrinfo(3)}.

@var{Node} is a network address, ex) "www.w3.org", "localhost", "192.168.1.1". 

@var{Service} is a network service, ex) "http", "ssh", "80", "22".

@var{Ai-family} is an address family specifier. Predefined specifiers are listed
below.
@itemlist[
@item{AF_INET}
@item{AF_INET6}
@item{AF_UNSPEC}
]

@var{Ai-sockettype} is a socket type specifier. Predefined specifiers are listed
below.
@itemlist[
@item{SOCK_STREAM}
@item{SOCK_DGRAM}
@item{SOCK_RAW}
]

@var{Ai-flags} is an additional options specifier. Predefined specifiers are listed
below.
@itemlist[
@item{AI_ADDRCONFIG}
@item{AI_ALL}
@item{AI_CANONNAME}
@item{AI_NUMERICHOST}
@item{AI_NUMERICSERV}
@item{AI_PASSIVE}
@item{AI_V4MAPPED}
]

@var{Ai-protocol} is a protocol specifier. Predefined specifiers are listed below.
@itemlist[
@item{IPPROTO_TCP}
@item{IPPROTO_UDP}
@item{IPPROTO_RAW}
]
}

@define[Function]{@name{make-server-socket}
 @args{service :optional (ai_family @code{AF_INET})
 (ai_socktype @code{SOCK_STREAM})
 (ai_protocol 0)}}
@desc{@var{Service} must be string or #f. Other optional arguments must be exact
integer. Returns a server socket waiting for connections. The argument details
are the same as the @code{make-client-socket}.
}

@define[Function]{@name{socket?} @args{obj}}
@desc{Returns #t if @var{obj} is socket object, otherwise #f.}

@define[Function]{@name{socket-port} @args{socket :optional (close? #t)}}
@desc{@var{Socket} must be a socket object. Returns a binary input/output port
associated with @var{socket}.

If optional argument @var{close?} is #f then the port won't close socket
when port is closing or being GCed.
}

@define[Function]{@name{socket-input-port} @args{socket}}
@define[Function]{@name{socket-output-port} @args{socket}}
@desc{[SRFI-106] @var{Socket} must be a socket object. Returns a binary input
and output port associated with @var{socket}, respectively.
}

@define[Function]{@name{call-with-socket} @args{socket proc}}
@desc{@var{Socket} must be a socket object. @var{Proc} must accept one argument.

The @code{call-with-socket} calls a procedure with socket as an argument.

This procedure is analogy with @code{call-with-port}.
}

@define[Function]{@name{shutdown-port} @args{port how}}
@desc{@var{Port} must be associated with a socket.

Shutdowns associated port according to @var{how}.
}

@define[Function]{@name{shutdown-input-port} @args{port}}
@define[Function]{@name{shutdown-output-port} @args{port}}
@desc{@var{Port} must be associated with a socket.

The @code{shutdown-output-port} and @code{shutdown-input-port} shutdown
output or input connection of a socket associated with @var{port} respectively.
}

@define[Function]{@name{socket-accept} @args{socket}}
@desc{@var{Socket} must be a socket object created by @code{make-server-socket}.

Wait for an incoming connection request and returns a fresh connected client
socket.

This procedures is a thin wrapper of POSIX's @code{accept(2)}.
}
@define[Function]{@name{socket-recv} @args{socket size :optional (flags 0)}}
@desc{@var{Socket} must be a socket object.

Receives a binary data block from given socket. If zero length bytevector is
returned, it means the peer connection is closed.

This procedures is a thin wrapper of POSIX's @code{recv(2)}.
}
@define[Function]{@name{socket-send}
 @args{socket bytevector :optional (flags 0)}}
@desc{@var{Socket} must be a socket object.

Sends a binary data block to given socket and returns the sent data size.

This procedures is a thin wrapper of POSIX's @code{send(2)}.
}

@define[Function]{@name{socket-shutdown} @args{socket how}}
@desc{@var{Socket} must be a socket object. @var{How} must be one of the
@code{SHUT_RD}, @code{SHUT_WR} or @code{SHUT_RDWR}.

The @code{socket-shutdown} shutdowns socket.
@dl-list[
@dl-item["SHUT_RD"]{shutdowns input.}
@dl-item["SHUT_WR"]{shutdowns output.}
@dl-item["SHUT_RDWR"]{shutdowns input and output.}
]
}

@define[Function]{@name{socket-close} @args{socket}}
@desc{@var{Socket} must be a socket object. Closes @var{socket}.}

@subsubsection{Socket information}

@define[Class]{@name{<socket-info>}}
@desc{The socket information immutable class. This class has 3 slots

@define[Slot]{@name{hostname}}
@desc{This slot has string value represents own or peer socket host.}
@define[Slot]{@name{ip-address}}
@desc{This slot has ip-address object of own or peer socket.}
@define[Slot]{@name{port}}
@desc{This slot has integer value of own or peer socket port number.}
}

@define[Function]{@name{socket-peer} @args{socket}}
@desc{@var{Socket} must be a socket object.

Returns socket info object or #f. The socket info object contains hostname,
IP address and port number. These information is retrieved from getpeername(2).
}

@define[Function]{@name{socket-name} @args{socket}}
@desc{@var{Socket} must be a socket object.

Returns the name string of socket or #f if the socket doesn't have name.
}

@define[Function]{@name{socket-info} @args{socket}}
@desc{@var{Socket} must be a socket object.

Returns socket info object or #f. The socket info object contains hostname,
IP address and port number. These information is retrieved from getsockname(2).
}

@define[Function]{@name{socket-info-values} @args{socket :key (type 'peer)}}
@desc{@var{Socket} must be a socket object.

Returns 3 values; hostname, IP address and port number. This procedures is
for convenience to handle socket info object.

The keyword argument specifies which socket info it should retrieve. If the
type is @code{peer} then it uses @code{socket-peer}. If it is @code{info},
then it uses @code{socket-info}
}

@subsubsection{IP address operations}

@define[Function]{@name{ip-address->string} @args{ip}}
@desc{@var{ip} must be an IP address object returned from the second value 
of @code{socket-info-values}.

Converts given IP address object to human readable string.
}

@define[Function]{@name{ip-address->bytevector} @args{ip}}
@desc{@var{ip} must be an IP address object returned from the second value 
of @code{socket-info-values}.

Converts given IP address object to bytevector.
}

@subsubsection[:tag "socket.low.level"]{Low level APIs}

The low level socket APIs are almost compatible with BSD socket.

@sub*section{Socket}

@define[Function]{@name{socket-sendto}
 @args{socket bvtevector sockaddr :optional (flags 0)}}
@desc{Sends a binary data block to given sockaddr and returns the
sent data size.

This procedures is a thin wrapper of POSIX's @code{sendto (2)}.
}
@define[Function]{@name{socket-recvfrom}
 @args{socket sockaddr :optional (flags 0)}}
@desc{Receives a binary data block from given sockaddr. If zero length
bytevector is returned, it means the peer connection is closed.

This procedures is a thin wrapper of POSIX's @code{recvfrom (2)}.
}

@define[Function]{@name{make-socket}  @args{ai-family ai-socktype ai-protocol}}
@desc{Creates socket object. The procedure returns #f if it couldn't create
a socket. @code{SO_NOSIGPIPE} socket option is set to the created socket.

This procedure is a thin wrapper of @code{socket (2)}.
}

@define[Function]{@name{socket-connect!} @args{socket addrinfo}}
@desc{Initiate connection on the given @var{socket} with given
addrinfo @var{addrinfo}.

This procedure is a thin wrapper of @code{connect (2)}.
}

@define[Function]{@name{socket-bind!} @args{socket addrinfo}}
@desc{Binds a name to the given socket @var{socket} with given addrinfo
@var{addrinfo}.

This procedure is a thin wrapper of @code{bind (2)}.
}

@define[Function]{@name{socket-listen!} @args{socket backlog}}
@desc{Listen for connections on the given socket @var{socket}.

This procedure is a thin wrapper of @code{listen (2)}.
}

@define[Function]{@name{socket-setsockopt!}
 @args{socket level name value}}
@desc{Sets socket option on the given socket @var{socket}.

@var{level} must be an integer and should be one of the followings:
@itemlist{
  @item{SOL_SOCKET}
  @item{SOL_TCP}
  @item{SOL_IP}
}

@var{name} must be an integer and should be one of the followings:
@itemlist{
  @item{SO_ACCEPTCONN}
  @item{SO_BINDTODEVICE}
  @item{SO_BROADCAST}
  @item{SO_DEBUG}
  @item{SO_DONTROUTE}
  @item{SO_ERROR}
  @item{SO_KEEPALIVE}
  @item{SO_LINGER}
  @item{SO_OOBINLINE}
  @item{SO_PASSCRED}
  @item{SO_PEERCRED}
  @item{SO_PRIORITY}
  @item{SO_RCVBUF}
  @item{SO_RCVLOWAT}
  @item{SO_RCVTIMEO}
  @item{SO_REUSEADDR}
  @item{SO_REUSEPORT}
  @item{SO_SNDBUF}
  @item{SO_SNDLOWAT}
  @item{SO_SNDTIMEO}
  @item{SO_TIMESTAMP}
  @item{SO_TYPE}
  @item{TCP_NODELAY}
  @item{TCP_MAXSEG}
  @item{TCP_CORK}
  @item{IP_OPTIONS}
  @item{IP_PKTINFO}
  @item{IP_RECVTOS}
  @item{IP_RECVTTL}
  @item{IP_RECVOPTS}
  @item{IP_TOS}
  @item{IP_TTL}
  @item{IP_HDRINCL}
  @item{IP_RECVERR}
  @item{IP_MTU_DISCOVER}
  @item{IP_MTU}
  @item{IP_ROUTER_ALERT}
  @item{IP_MULTICAST_TTL}
  @item{IP_MULTICAST_LOOP}
  @item{IP_ADD_MEMBERSHIP}
  @item{IP_DROP_MEMBERSHIP}
  @item{IP_MULTICAST_IF} 
}

The @var{value} must be either bytevector or integer.

This procedure is a thin wrapper of @code{setsockopt (2)}.
}

@define[Function]{@name{socket-getsockopt}
 @args{socket level name size}}
@desc{Gets socket option on the given socket @var{socket}.

The @var{level} and @var{name} are the same as @code{socket-setsockopt!}.

@var{size} must be an integer. If the value is positive number, then the
returning value is a bytevector whose element count is @var{size} and
contains the socket option converted to byte array. Otherwise it returns
an integer value.
}

@define[Function]{@name{socket-nonblocking!} @args{socket}}
@define[Function]{@name{socket-blocking!} @args{socket}}
@desc{Converts given socket to nonblocking socket and blocking socket,
respectively.
}

@define[Function]{@name{socket-select} @args{rfds wfds efds timeout}}
@define[Function]{@name{socket-select!} @args{rfds wfds efds timeout}}
@desc{Monitor given fdset.

@var{rfds}, @var{wfds} and @var{efds} must be fdset object.

@var{timeout} must be #f, integer, time object or pair of integers. If this
value is not #f, then the procedure waits only specified amount of time or
something interesting happens. Otherwise infinite time or something
interesting happens.

This procedure blocks the thread and can be interrupted by
@code{thread-interrupt!}.

This procedure is a thin wrapper of @code{select (2)}.
}

@define[Function]{@name{socket-read-select} @args{timeout sockets @dots{}}}
@define[Function]{@name{socket-write-select} @args{timeout sockets @dots{}}}
@define[Function]{@name{socket-error-select} @args{timeout sockets @dots{}}}
@desc{Waits until the given sockets @var{sockets} have something interesting.
This is the convenient procedure for @code{socket-select}.

@var{timeout} is the same as @code{socket-select}.

@code{socket-read-select} can be used to detect if the given sockets have
readable data.

@code{socket-write-select} can be used to detect if the given sockets are
still active.

@code{socket-error-select} can be used to detect if the given sockets are
readable data. This procedure might not be so interesting since it can be
done by @code{socket-read-select}.
}

@sub*section{Addrinfo}

@define[Function]{@name{addrinfo?} @args{obj}}
@desc{Returns #t if given @var{obj} is an addrinfo, otherwise #f.}

@define[Function]{@name{make-addrinfo}}
@desc{Creates empty addrinfo object.

The object has the following slots:
@itemlist{
 @item{@code{family}}
 @item{@code{socktype}}
 @item{@code{flags}}
 @item{@code{protocol}}
 @item{@code{sockaddr}}
 @item{@code{next}}
}

}
@define[Function]{@name{make-hint-addrinfo}
 @args{family socktype flags protocol}}
@desc{Creates an addrinfo with given flags. This can be used as hint for
@code{get-addrinfo}.}

@define[Function]{@name{get-addrinfo} @args{addrinfo}}
@desc{Gets addrinfo of given hint addrinfo.

When the procedure fails, then @code{&i/o} is raised.

This procedure is a thin wrapper of @code{getaddrinfo (3)}.
}

@define[Function]{@name{next-addrinfo} @args{addrinfo}}
@desc{Retrieves next addrinfo of given @var{addrinfo} if availalbe, otherwise
returns #f.}

@define[Function]{@name{addrinfo-sockaddr} @args{addrinfo}}
@desc{Retrieves @code{sockaddr} slot of given @var{addrinfo}.

The returning value can be used @code{socket-recvfrom} and @code{socket-sendto}.
}

@define[Function]{@name{sockaddr?} @args{obj}}
@desc{Returns #t if given @var{obj} is an sockaddr object, otherwise #f.}

@sub*section{FD sets}

@define[Function]{@name{fdset?} @args{obj}}
@desc{Returns #t if given @var{obj} is a fdset object, otherwise #f.}

@define[Function]{@name{make-fdset}}
@desc{Creates a empty fdset object.}

@define[Function]{@name{sockets->fdset} @args{sockets}}
@desc{Creates a fdset from given socket list @var{sockets}.}

@define[Function]{@name{fdset-ref} @args{fdset socket}}
@desc{Returns #t if the given @var{socket} is set to @var{fdset}, otherwise #f.}

@define[Function]{@name{fdset-set!} @args{fdset socket flag}}
@desc{Sets/unsets the given socket @var{socket} on @var{fdset}.

If the @var{flags} is #f, then the procedure unsets the @var{socket}.

If the @var{flags} is #t, then the procedure sets the @var{socket}.
}

@define[Function]{@name{collect-sockets} @args{fdset}}
@desc{Returns a list of socket which are set on @var{fdset}.}


@sub*section{Socket conditions}

Above APIs may raise either @code{&socket} or @code{&host-not-found}. The first
condition is raised when socket related operation failed, for example
@code{socket-send}. The latter condition is raised when @code{get-addrinfo} is
failed.

NOTE: @code{make-client-socket} and @code{make-server-socket} may raise
@code{&host-not-found} when the given @var{node} or @var{service} is not a
valid value.

The condition hierarchy is the following:

@codeblock{
&i/o
 + &host-not-found (node service)
 + &socket (socket)
    + &socket-connection
    + &socket-closed
    + &socket-port (port)
}
@; TBD