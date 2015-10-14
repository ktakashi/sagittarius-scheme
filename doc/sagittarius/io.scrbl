@; -*- mode:scribble; coding: utf-8; -*-

@subsection[:tag "sagittarius.io"]{(sagittarius io) - Extra IO library}

@define[Library]{@name{(sagittarius io)}}
@desc{This library provided extra IO related procedures.}


@define[Function]{@name{with-input-from-port} @args{port thunk}}
@define[Function]{@name{with-output-to-port} @args{port thunk}}
@define[Function]{@name{with-error-to-port} @args{port thunk}}
@desc{Calls @var{thunk}. During evaluation of @var{thunk}, the current input
port, current output port, current error port are set to @var{port},
respectively.
}

@define[Function]{@name{call-with-input-string} @args{str proc}}
@define[Function]{@name{call-with-output-string} @args{proc}}
@define[Function]{@name{with-input-from-string} @args{str thunk}}
@define[Function]{@name{with-output-to-string} @args{thunk}}
@desc{These utility functions are trivially defined as follows;

@codeblock{
(define (call-with-input-string str proc)
  (proc (open-input-string str)))

(define (call-with-output-string proc)
  (let ((port (open-output-string)))
    (proc port)
    (get-output-string port)))

(define (with-input-from-string str thunk)
  (with-input-from-port (open-input-string str) thunk))

(define (with-output-to-string thunk)
  (let ((port (open-output-string)))
    (with-output-to-port port thunk)
    (get-output-string port)))
}
}

@define[Function]{@name{buffered-port} @args{port buffer-mode :key buffer}}
@define[Function]{@name{transcoded-port} @args{port transcoder}}
@desc{Re-export of @code{buffered-port} and @code{transcoded-port}.}

@subsubsection{Custom ports}

Sagittarius provides means to create user defined ports. One of the ways
is using R6RS custom port procedures. The other one is extending custom
port class. The followings show how to extend it.

@code{
;; example for input port
(import (rnrs) (sagittarius io) (clos user))

;; make a custom binary input port with 'read slot
(get-u8 (make <custom-binary-input-port>
          :read (lambda (bv start count)
                  (bytevector-u8-set! bv start 1)
                1)))
}

@code{
;; example for output port
(import (rnrs) (sagittarius io) (clos user))

;; user defined custom binary output port
(define-class <my-port> (<custom-binary-output-port>) 
  ;; this port has own buffer 
  ((buffer :init-form (make-bytevector 5 0))))

;; create the port
(let ((out (make <my-port>)))
  ;; set 'write slot
  (slot-set! out 'write
    (lambda (bv start count)
       ;; just get the first element of given bytevector
       ;; and set it to own buffer
       (bytevector-copy! bv start (slot-ref out 'buffer) 0 count)
       count))
  ;; 
  (put-bytevector out #vu8(1 2 3 4 5))
  (slot-ref out 'buffer))
;; -> #vu8(1 0 0 0 0)
}


@define[Class]{@name{<custom-binary-input-port>}}
@define[Class]{@name{<custom-binary-output-port>}}
@define[Class]{@name{<custom-binary-input/output-port>}}
@define[Class]{@name{<custom-textual-input-port>}}
@define[Class]{@name{<custom-textual-output-port>}}
@define[Class]{@name{<custom-textual-input/output-port>}}
@desc{Custom port classes. All of these classes have the following slots:

@define[Slot]{@name{id}}
@desc{Identifier of the port. Must be string is specified.}
@define[Slot]{@name{position}}
@define[Slot]{@name{set-position}}
@define[Slot]{@name{read}}
@define[Slot]{@name{write}}
@define[Slot]{@name{flush}}
@define[Slot]{@name{close}}
@desc{All of them must be either procedure or #f.

@code{position} procedure must accept 0 argument. The procedure should
return the position of the port.

@code{set-position} procedure must accept 2 argument, @var{position}
and@var{whence}. @var{Whence} shall be a symbol of @code{begin}, 
@code{current} or @code{end}. The procedure should set the position
of the port according to the given @var{whence} and @var{position}.

@code{read} procedure must accept 3 argument. @var{bv} or @var{string},
@var{start} and @var{count}. The first argument is decided by the port
type. If the port is binary port, then bytevector @var{bv} is passed.
If the port is textual port, then string @var{string} is passed.
The procedure should fill given @var{bv} or @var{string} in @var{count}
data elements starting @var{start}. And return number of data filled.

@code{write} procedure must accept 3 argument. @var{bv} or @var{string},
@var{start} and @var{count}. The first argument is decided by the port
type. If the port is binary port, then bytevector @var{bv} is passed.
If the port is textual port, then string @var{string} is passed.
The procedure should retrieve data from  given @var{bv} or @var{string}
upto @var{count} data elements starting @var{start}. And return number
of data read.

@code{ready} procedure must accept 0 argument. The procedure should
return true value if the port is ready to read. Otherwise #f.

@code{flush} procedure must accept 0 argument. The procedure should
flush the port.

@code{close} procedure must accept 0 argument. The procedure should
close the port.

If the creating port is input port, then @code{read} must be set before
any port operation. If the creating port is output port, then @code{write}
must be set before any port operation. Other slots are optional.
}
}