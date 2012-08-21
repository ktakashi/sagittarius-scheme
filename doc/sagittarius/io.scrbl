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

