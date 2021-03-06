@; -*- mode:scribble; coding:utf-8; -*-

@subsection[:tag "net.server"]{(net server) - Simple server framework}

@define[Library]{@name{(net server)}}
@desc{This library provides simple server framework.}

Following example describes how to write a simple echo server with the APIs
this library provides.
@codeblock{
(import (net server) (sagittarius socket))

(define (handler server socket)
  ;; echo message is limited to 255 bytes in this example
  (let ((r (socket-recv socket 255)))
    ;; socket will be closed by the framework
    (socket-send socket r)))

(define server (make-simple-server "5000" handler))

(server-start! server)
}

Above example creates only one thread and if there are more than one
connection, then the latter one needs to wait until first one is done.
The library also provides mult threading server. Following example describes
how to make multi threading server.
@codeblock{
(import (net server) (sagittarius socket))

;; specifies maximum thread number
(define server-config (make-server-config :max-thread 5))

(define (handler server socket)
  (let ((r (socket-recv socket 255)))
    ;; socket will be closed by the framework
    (socket-send socket r)))

(define server (make-simple-server "5000" handler :config server-config))

(server-start! server)
}
If the server gets more than 5 connection simultaneously, then it tries to
wait until one of the connection's task finishes. If it doesn't finish in
time, then connection will be refused.

If clients keep the connection but server wants to handle requests more than
configured thread number, then specify @var{non-blocking?} keyword argument
with #t.
@codeblock{
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
}
Above server example creates 5 threads and accept all requests. The requests
are dispatched to the least busy thread. There are couple of restrictions
to use this server. See the descirption of @var{non-blocking?} keyword
argument.


@subsubsection{Server}

@define[Class]{@name{<simple-server>}}
@desc{Simple server class.}

@define[Function]{@name{server?} @args{obj}}
@desc{Returns #t if the @var{obj} is an instance of @code{<simple-server>},
otherwise #f.}

@define[Function]{@name{make-simple-server}
 @args{service handler :key server-class config :allow-other-keys}}
@desc{Creates a server object.

@var{service} must be a string and indicates the service name or port number.

@var{handler} must be a procedure accepts 2 arguments, server object 
@var{server} created with this procedure and socket object @var{socket}.

Keyword argument @var{server-class} is specified, it must be a class
inherits @code{<simple-server>}, then the procedure uses the class to
instantiate. And during instantiation, given other keys are passed.

Keyword argument @var{config} is specified, it must be an instance
of @var{<server-config>} or subclass of it, then the server is created
according to the configuration.
}

@define[Function]{@name{server-config} @args{server}}
@desc{Returns configuration object used to create given server object
@var{server}.
}

@define[Function]{@name{server-stopped?} @args{server}}
@desc{Returns #t if given server is stopped.

NOTE: this also returns #t if the server is not started.
}

@define[Function]{@name{server-start!}
 @args{server :key background :allow-other-keys}}
@desc{Starts the given @var{server}.

Keyword argument @var{background} is true value then the server is started
background. By default it's #f.

The rest of keywords are passed to @code{on-server-start!}.

NOTE: Server object is not reusable thus once server is started, it is
impossible to restart the server.
}

@define[Function]{@name{server-stop!}
 @args{server :allow-other-keys}}
@desc{Stops the given @var{server}.

The rest of keywords are passed to @code{on-server-stop!}.
}

@define[Function]{@name{wait-server-stop!} @args{server :optional timeout}}
@desc{Waits until the @var{server} stops.

The @var{server} must be stopping by accessing shutdown port otherwise
this procedure waits forever/for @var{timeout} period.

Optional argument @var{timeout} must be #f, time object or real number.
If the value is #f then this procedure waits forever until the @var{server}
stops. By default #f.
}

@define[Method]{@name{on-server-start!}
 @args{(server <simple-server>) rest @dots{}}}
@define[Method]{@name{on-server-stop!}
 @args{(server <simple-server>) rest @dots{}}}
@desc{Hook methods for subclasses.

The first method is called when server is starting.

The second method is called after server is stopped.
}

@subsubsection{Configuration}

@define[Class]{@name{<server-config>}}
@desc{Server configuration class.}
@define[Function]{@name{server-config?} @args{obj}}
@desc{Returns #t if the @var{obj} is an instance of @code{<server-config>},
otherwise #f.}

@define[Function]{@name{make-server-config}
 @args{:key shutdown-port shutdown-handler exception-handler max-thread
            max-retry use-ipv6? secure? certificates}}
@desc{Creates a server config object.

Following is the description of keyword arguments.
@dl-list[]{
  @dl-item[@var{shutdown-port}]{
    Specifying shutdown port. The value must be a string. If this is not
    specified, then the server doesn't have shutdown port.
  }
  @dl-item[@var{shutdown-handler}]{
    This is only used then @var{shutdown-port} is specified. The value
    must be a procedure takes 2 arguments, @var{server} and @var{socket}.
    When the procedure returns true value then server will be stopped.
    By default, it's a procedure always returns #t.
  }
  @dl-item[@var{exception-handler}]{
    Specifying exception handler. The value must be a procedure accepts
    3 arguments, @var{server}, @var{socket} and @var{condition}. This
    is called when the server @var{handler} raises an error. 

    NOTE: The passing @var{socket} is @b{not} closed so that the handler can
    send messages to client socket.
  }
  @dl-item[@var{max-thread}]{
    Specifying max thread count. Default value is 1.
  }
  @dl-item[@var{max-retry}]{
    Specifying max retry count. When connection reached @var{max-thread},
    then the server waits if the one of the connections finishes. The
    waiting period is half second (500 ms) and this value specifies
    how many times server waits.

    Default value is 10.
  }
  @dl-item[@var{non-blocking?}]{
    Creating non blocking server.

    If the server is non blocking server, then the server @var{handler}
    must follow the following rules:

    @itemlist{
      @item{the @var{handler} process must not block/stop even if the
            given socket is active.}
      @item{the @var{handler} process must close the socket when it's
            not needed.}
    }
    
    When handler raises an error and @var{exception-handler} is specified,
    then the given socket won't be closed. So @var{exception-handler} needs
    to decide whether the exception is continuable or not. Otherwise, server
    closes the socket.

    Specifying this keyword argument makes server ignore @var{max-retry}.
  }
  @dl-item[@var{use-ipv6?}]{
    Specifying whether or not the server uses IPv6.

    Default value is #f. (only IPv4)
  }
  @dl-itemx[2 @var{secure?} @var{certificates}]{
    If @var{secure?} is true value and @var{certificates} is a list of
    X509 certificates, then the server uses TLS.
  }
  @dl-item[@var{private-key}]{
    If the server uses TLS, then this keyword argument is passed to
    @code{make-server-tls-socket}. It is strongly recommended to
    specify this keyword argument, otherwise key exchange is done
    anonymously, means no signature is sent.
  }
}
}

@subsubsection{Socket detaching}

Non blocking server manages sockets per threads. This feature is useful if
the server handler is reused per socket. However this prevents users to
write asynchronous call. The following procedure allow users to detach
sockets from the server.

@define[Function]{@name{server-detach-socket!} @args{server socket}}
@desc{Detaches the given @var{socket}.

If the socket is detached, then all resource managements, including closing
socket, become users' responsibility.

This procedure is only available on non blocking server and can be called
inside of server handler. If the condition is not met, then @code{&assertion}
is signaled.
}

@; TBD
@;@subsubsection{Extending server}
