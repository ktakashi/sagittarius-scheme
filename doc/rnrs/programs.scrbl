@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.programs.6"]{Command-line access and exit values}

@define[Library]{@name{(rnrs programs (6))}}
@desc{This library provides command-line arguments access and exit procedures.}

@define[Function]{@name{command-line}}
@desc{[R6RS] Returns a nonempty list of strings. The first element is the running
script file name or '() on REPL. The remaining elements are command-line arguments
according to the operating system's conventions.
}

@define[Function]{@name{exit} @args{:optional obj}}
@desc{[R6RS] Exits the running program and communicates an exit value to the
operating system. If no argument is supplied, the exit procedure should
communicate to the operating system that the program exited normally. If an
argument is supplied and if it is fixnum, the exit procedure translates it into
an appropriate exit value for the operating system. Otherwise the exit is
assumed to be abnormal.
}
