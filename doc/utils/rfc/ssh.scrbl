@; -*- coding:utf-8; -*-

@subsection[:tag "rfc.ssh"]{(rfc ssh) - SSH library}

@define[Library]{@name{rfc ssh}}
@desc{This library provides SSH programmatic operations. The library supports
only SSH2 defined in RFC 4250 - 4256.}

Following is the simple example to execute shell command via SSH:
@codeblock{
(import (rfc ssh))

(define transport 
  (open-client-ssh-transport! (make-client-ssh-transport "localhost" "22")))

(define user "guest1")
(define pass "pass1")

(ssh-authenticate transport +ssh-auth-method-password+ user pass)
(let-values (((status response) (ssh-execute-command transport "ls -l")))
  (print (utf8->string response)))
}

@subsubsection{SSH transport}

This represents SSH transport layer defined in RFC 4253.

@define[Function]{@name{ssh-transport?} @args{obj}}
@desc{Returns #t if the given @var{obj} is SSH transport, otherwise #f.}

@define[Function]{@name{make-client-ssh-transport} @args{server port}}
@desc{Creates a SSH transport which connects to given @var{server} on port
@var{port}. @var{server} and @var{port} must be srtings.

Creating SSH transport doesn't open socket. To connect to the @var{server}, 
@code{open-client-ssh-transport!} must be called.
}

@define[Function]{@name{open-client-ssh-transport!} @args{transport}}
@desc{Opens given @var{transport}. This procedure does the following steps:
@itemlist{
  @item{Make a client socket}
  @item{Version exchange}
  @item{Key exchange}
}
}

