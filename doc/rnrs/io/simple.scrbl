@; -*- mode:scribble; coding: utf-8 -*-

@subsubsection[:tag "rnrs.io.simple.6"]{Simple I/O}

This section describes the @code{(rnrs io simple (6))}library, which provides
a somewhat more convenient interface for performing textual I/O on ports.

@define[Library]{@name{(rnrs io simple (6))}}
@desc{This library also exports the same procedures as @code{(rnrs io posts (6))}
library. I do not write the documentation of it, if you want to import only this
library, make sure which procedures are exported. You can see it on R6RS.
}

@define[Function]{@name{call-with-input-file} @args{filename proc}}
@define[Function]{@name{call-with-output-file} @args{filename proc}}
@desc{[R6RS] @var{Proc} should accept one argument. These procedures open the file
named by @var{filename} for input or for output, with no specified file options,
and call @var{proc} with the obtained port as an argument. If @var{proc} returns,
the port is closed automatically and the values returned by @var{proc} are
returned. If @var{proc} does not return, the port is not closed automatically,
unless it is possible to prove that the port will never again be used for an
I/O operation.
}

@define[Function]{@name{with-input-from-file} @args{filename thunk}}
@define[Function]{@name{with-output-to-file} @args{filename thunk}}
@desc{[R6RS] @var{Thunk} must be a procedure and must accept zero arguments. The
file is opened for input or output using empty file options, and @var{thunk} is
called with no arguments. These procedure replace current input/output port during
@var{thunk} is being called. When @var{thunk} returns, the port is closed
automatically. The values returned by thunk are returned.
}

@define[Function]{@name{open-input-file} @args{filename}}
@define[Function]{@name{open-output-file} @args{filename}}
@desc{[R6RS] Opens @var{filename} for input/output, with empty file options, and
returns the obtained port.
}

@define[Function]{@name{close-input-file} @args{port}}
@define[Function]{@name{close-output-file} @args{port}}
@desc{[R6RS] Closes @var{input-port} or @var{output-port}, respectively.
}

@define[Function]{@name{read-char} @args{:optional textual-input-port}}
@define[Function]{@name{peak-char} @args{:optional textual-input-port}}
@desc{[R6RS] These work the same as @code{get-char} and @code{lookahead-char}.
If @var{textual-input-port} is omitted, it defaults to the value returned by
@code{current-input-port}. 
}

@define[Function]{@name{read} @args{:optional textual-input-port}}
@desc{[R6RS] Reads an external representation from @var{textual-input-port}
and returns the datum it represents. The read procedure operates in the same
way as @var{get-datum}.

If @var{textual-input-port} is omitted, it defaults to the value returned by
@code{current-input-port}. 
}

@define[Function]{@name{write-char} @args{char :optional textual-input-port}}
@desc{[R6RS] Writes an encoding of the character char to the @var{textual-output-port},
and returns unspecified values.

If @var{textual-output-port} is omitted, it defaults to the value returned by
@code{current-output-port}.
}

@define[Function]{@name{newline} @args{:optional textual-input-port}}
@desc{[R6RS] This is equivalent to using @code{write-char} to write @code{#\linefeed}
to @var{textual-output-port}.

If @var{textual-output-port} is omitted, it defaults to the value returned by
@code{current-output-port}.
}

@define[Function]{@name{display} @args{obj :optional textual-input-port}}
@desc{[R6RS] Writes a representation of @var{obj} to the given 
@var{textual-output-port}. Strings that appear in the written representation
are not enclosed in double quotes, and no characters are escaped within those
strings. Character objects appear in the representation as if written by
@var{write-char} instead of by @var{write}. The display procedure returns
unspecified values.

If @var{textual-output-port} is omitted, it defaults to the value returned by
@code{current-output-port}.
}

@define[Function]{@name{write} @args{obj :optional textual-input-port}}
@desc{[R6RS] Writes the external representation of @var{obj} to
@var{textual-output-port}. The @code{write} procedure operates in the same way
as @code{put-datum}.

If @var{textual-output-port} is omitted, it defaults to the value returned by
@code{current-output-port}.
}