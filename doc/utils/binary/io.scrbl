@; -*- mode:scribble; coding: utf-8; -*-

@subsection[:tag "util.binary.io"]{(binary io) - Binary I/O utilities}

@define[Library]{@name{(binary data)}}
@desc{Binary I/O utility. In real world you sometimes want to treat
binary port like textual port (e.g. @code{get-line} for binary port).
This library exports those convenient procedures
}

@subsubsection{Binary I/O}

@define[Function]{@name{get-line} @args{in :key eol (transcoder #f)}}
@desc{@var{in} must be binary input port.

Reads a bytevector from @var{in} until it hits the @var{eol} data. @var{eol}
can be multiple length such as @code{#vu8(#x0d #x0a)}. Default value is
@code{#vu8(#x0a)}

If keyword argument @var{transcoder} is given, then returning value will be
converted to string.
}

@; TODO get-until
@; TODO lookahead-next-u8

@define[Function]{@name{put-u16} @args{out v endian}}
@define[Function]{@name{put-s16} @args{out v endian}}
@define[Function]{@name{put-u32} @args{out v endian}}
@define[Function]{@name{put-s32} @args{out v endian}}
@define[Function]{@name{put-u64} @args{out v endian}}
@define[Function]{@name{put-s64} @args{out v endian}}
@define[Function]{@name{put-f32} @args{out v endian}}
@define[Function]{@name{put-f64} @args{out v endian}}
@desc{@var{out} must be binary output port. @var{endian} must be a value
returned from @code{endianness} macro.

Write @var{v} to @var{out} as unsigned/signed 16/32/64 bit integer or
32/64 bit floating number.
}

@define[Function]{@name{get-u16} @args{in endian}}
@define[Function]{@name{get-s16} @args{in endian}}
@define[Function]{@name{get-u32} @args{in endian}}
@define[Function]{@name{get-s32} @args{in endian}}
@define[Function]{@name{get-u64} @args{in endian}}
@define[Function]{@name{get-s64} @args{in endian}}
@define[Function]{@name{get-f32} @args{in endian}}
@define[Function]{@name{get-f64} @args{in endian}}
@desc{@var{in} must be binary input port. @var{endian} must be a value
returned from @code{endianness} macro.

Read a number from @var{in} as unsigned/signed 16/32/64 bit integer or
32/64 bit floating number.
}

@subsubsection{Chunk buffered port}

Default binary input port requires bytevector however if users want to
handle bigger data then it would not be suitable. Chunk buffered port
is a buffered port which buffer is chunks of bytevector so that it doesn't
allocate huge memory.

@define[Function]{@name{->chunked-binary-input-port}
 @args{->chunks :key chunk-size}}
@desc{Creates chunk buffered port.

@var{->chunks} must be a procedure which takes one argument, @var{chunk-size}.
And must return a list of bytevectors which size is @var{chunk-size} except
the last element.

The keyword argument @var{chunk-size} is specified then it must be a positive
integer. By default @code{+default-chunk-size+} is used.

@define[Constant]{@name{+default-chunk-size+}}
@desc{Default chunk size of chunk buffered port.}

}

@define[Function]{@name{input-port->chunked-binary-input-port}
 @args{iport :key chunk-size threshold}}
@desc{Creates chunk buffered port from given @var{iport}.

@var{iport} must be a binary input port.

The keyword argument @var{threshold} is specified, it must be a positive
integer, then the procedure only reads the number of @var{threshold} bytes
from @var{iport}.
}
