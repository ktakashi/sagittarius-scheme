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

@define[Function]{@name{socket-port} @args{socket}}
@desc{@var{Socket} must be a socket object. Returns a binary input/output port
associated with @var{socket}.
}

@define[Function]{@name{call-with-socket} @args{socket proc}}
@desc{@var{Socket} must be a socket object. @var{Proc} must accept one argument.

The @code{call-with-socket} calls a procedure with socket as an argument.

This procedure is analogy with @code{call-with-port}.
}

@define[Function]{@name{shutdown-output-port} @args{port}}
@desc{@var{Port} must be associated with a socket.

The @code{shutdown-output-port} shutdowns output connection of a socket associated
with @var{port}.
}

@define[Function]{@name{socket-accept} @args{socket}}
@define[Function]{@name{socket-recv} @args{socket ei ei}}
@define[Function]{@name{socket-send} @args{socket bytevector ei}}
@desc{@var{Socket} must be a socket object. These procedures are thin wrapper of
@code{accept(2)}, @code{recv(2)} and @code{send(2)}, respectively. For more detail,
see these manuals.
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