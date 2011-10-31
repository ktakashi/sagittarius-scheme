@; -*- mode:scribble; coding: utf-8 -*-

@subsubsection[:tag "rnrs.io.ports.6"]{Port I/O}

@define[Library]{@name{(rnrs io ports (6))}}
@desc{The @code{(rnrs io ports (6))}library defines an I/O layer for conventional,
imperative buffered input and output. A @var{port} represents a buffered access
object for a data sink or source or both simultaneously. The library allows ports
to be created from arbitrary data sources and sinks.

The @code{(rnrs io ports (6))} library distinguishes between @var{input ports}
and @var{output ports}. An input port is a source for data, whereas an output
port is a sink for data. A port may be both an input port and an output port;
such a port typically provides simultaneous read and write access to a file or
other data.

The @code{(rnrs io ports (6))} library also distinguishes between @var{binary ports},
which are sources or sinks for uninterpreted bytes, and @var{textual ports},
which are sources or sinks for characters and strings.

This section uses @var{input-port}, @var{output-port}, @var{binary-port},
@var{textual-port}, @var{binary-input-port}, @var{textual-input-port},
@var{binary-output-port}, @var{textual-output-port}, and @var{port} as parameter
names for arguments that must be input ports (or combined input/output ports),
output ports (or combined input/output ports), binary ports, textual ports,
binary input ports, textual input ports, binary output ports, textual output
ports, or any kind of port, respectively.
}

@sub*section{Filename}

In this world, unfortunately there are a lot of operating systems. However, as
far as I know there are only two file separators, one is Windows style back
slash, the other is UNIX style slash. On Sagittarius both of it can be used as
file path. Inside of the resolution of file path, Sagittarius replaces those
file separators to OS specific one. Even though programmer does not have to care
about it, I think it's better to use slash as file separator on script files.

A @var{filename} parameter name means that the corresponding argument must be a
file name.

@sub*section{File options}

When opening a file, the various procedures in this library accept a
@code{file-options} object that encapsulates flags to specify how the file is to
be opened. A @code{file-options} object is an enum-set
(see @secref["rnrs.enums.6"]{(rnrs enums (6))}) over the symbols constituting
valid file options. A @var{file-options} parameter name means that the corresponding
argument must be a @var{file-options} object.

@define[Macro]{@name{file-options} @args{file-options-symbol @dots{}}}
@desc{[R6RS] Each @var{file-option-symbol} must be a symbol. The @var{file-options}
macro returns a @var{file-options} object that encapsulates the specified options.

When supplied to an operation that opens a file for output, the file-options object
returned by @code{(file-options)} specifies that the file is created if it does not
exist and an exception with condition type @code{&i/o-file-already-exists} is
raised if it does exist. The following standard options can be included to modify
the default behaviour.

@itemlist[
@item{@code{no-create} @p{If the file does not already exist, it is not created;
instead, an exception with condition type @code{&i/o-file-does-not-exist} is
raised. If the file already exists, the exception with condition type
@code{&i/o-file-already-exists} is not raised and the file is truncated to zero
length.}}
@item{@code{no-fail} @p{If the file already exists, the exception with condition
type @code{&i/o-file-already-exists} is not raised, even if @code{no-create} is
not included, and the file is truncated to zero length.}}
@item{@code{no-truncate} @p{If the file already exists and the exception with
condition type @code{&i/o-file-already-exists} has been inhibited by inclusion
of @code{no-create} or @code{no-fail}, the file is not truncated, but the port's
current position is still set to the beginning of the file.}}
]
}

@sub*section{Buffer modes}

Each port has an associated buffer mode. For an output port, the buffer mode
defines when an output operation flushes the buffer associated with the output
port. For an input port, the buffer mode defines how much data will be read to
satisfy read operations. The possible buffer modes are the symbols none for no
buffering, line for flushing upon line endings and reading up to line endings,
or other implementation-dependent behavior, and block for arbitrary buffering.
This section uses the parameter name @var{buffer-mode} for arguments that must
be buffer-mode symbols.

If two ports are connected to the same mutable source, both ports are unbuffered,
and reading a byte or character from that shared source via one of the two ports
would change the bytes or characters seen via the other port, a lookahead operation
on one port will render the peeked byte or character inaccessible via the other
port, while a subsequent read operation on the peeked port will see the peeked byte
or character even though the port is otherwise unbuffered.

In other words, the semantics of buffering is defined in terms of side effects on
shared mutable sources, and a lookahead operation has the same side effect on the
shared source as a read operation.

@define[Macro]{@name{buffer-mode} @args{buffer-mode-symbol}}
@desc{[R6RS] @var{Buffer-mode-symbol} must be a symbol whose name is one of
@code{none}, @code{line}, and @code{block}. The result is the corresponding
symbol, and specifies the associated buffer mode.
}

@define[Function]{@name{buffer-mode?} @args{obj}}
@desc{Returns #t if the argument is a valid buffer-mode symbol, and returns #f
otherwise.}

@sub*section{Transcoders}

Several different Unicode encoding schemes describe standard ways to encode
characters and strings as byte sequences and to decode those sequences. Within
this document, a codec is an immutable Scheme object that represents a Unicode
or similar encoding scheme.

An @var{end-of-line} style is a symbol that, if it is not none, describes how a
textual port transcodes representations of line endings.

A @var{transcoder} is an immutable Scheme object that combines a codec with an
end-of-line style and a method for handling decoding errors. Each transcoder
represents some specific bidirectional (but not necessarily lossless), possibly
stateful translation between byte sequences and Unicode characters and strings.
Every transcoder can operate in the input direction (bytes to characters) or in
the output direction (characters to bytes). A @var{transcoder} parameter name
means that the corresponding argument must be a transcoder.

A @var{binary port} is a port that supports binary I/O, does not have an associated
transcoder and does not support textual I/O. A @var{textual} port is a port that
supports textual I/O, and does not support binary I/O. A textual port may or may
not have an associated transcoder.

@define[Function]{@name{latin-1-codec}}
@define[Function]{@name{utf-8-codec}}
@define[Function]{@name{utf-16-codec}}
@desc{[R6RS] These are predefined codecs for the ISO 8859-1, UTF-8, and UTF-16
encoding schemes.

A call to any of these procedures returns a value that is equal in the sense
of @code{eqv?} to the result of any other call to the same procedure.}

@define[Macro]{@name{eol-style} @args{eol-style-symbol}}
@desc{[R6RS] @var{Eol-style-symbol} should be a symbol whose name is one of
@code{lf}, @code{cr}, @code{crlf}, @code{nel}, @code{crnel}, @code{ls}, and
@code{none}. The form evaluates to the corresponding symbol. If the name of
@var{eol-style-symbol} is not one of these symbols, it still returns given
symbol, however @code{make-transcoder} does not accept it.
}

@define[Function]{@name{native-eol-style}}
@desc{[R6RS] Returns the default end-of-line style of the underlying platform.}

@define["Condition Type"]{@name{&i/o-decoding}}
@define[Function]{@name{make-i/o-decoding-error} @args{port}}
@define[Function]{@name{i/o-decoding-error?} @args{obj}}
@desc{[R6RS] An exception with this type is raised when one of the operations for
textual input from a port encounters a sequence of bytes that cannot be translated
into a character or string by the input direction of the port's transcoder.

When such an exception is raised, the port's position is past the invalid encoding.
}

@define["Condition Type"]{@name{&i/o-encoding}}
@define[Function]{@name{make-i/o-encoding-error} @args{port}}
@define[Function]{@name{i/o-encoding-error?} @args{obj}}
@desc{[R6RS] An exception with this type is raised when one of the operations for
textual output to a port encounters a character that cannot be translated into
bytes by the output direction of the port's transcoder. Char is the character
that could not be encoded.}

@define[Macro]{@name{error-handling-mode} @args{error-handling-mode-symbol}}
@desc{[R6RS] @var{Error-handling-mode-symbol} should be a symbol whose name is
one of @code{ignore}, @code{raise}, and @code{replace}. The form evaluates to the
corresponding symbol.

The @code{error-handling} mode of a transcoder specifies the behavior of textual
I/O operations in the presence of encoding or decoding errors.

If a textual input operation encounters an invalid or incomplete character encoding,
and the error-handling mode is @code{ignore}, an appropriate number of bytes of the
invalid encoding are ignored and decoding continues with the following bytes. If the
error-handling mode is @code{replace}, the replacement character U+FFFD is injected
into the data stream, an appropriate number of bytes are ignored, and decoding
continues with the following bytes. If the error-handling mode is @code{raise}, an
exception with condition type @code{&i/o-decoding} is raised.

If a textual output operation encounters a character it cannot encode, and the
error-handling mode is @code{ignore}, the character is ignored and encoding
continues with the next character. If the error-handling mode is @code{replace},
a codec-specific replacement character is emitted by the transcoder, and encoding
continues with the next character. The replacement character is U+FFFD for
transcoders whose codec is one of the Unicode encodings, but is the @code{?}
character for the Latin-1 encoding. If the error-handling mode is @code{raise},
an exception with condition type @code{&i/o-encoding} is raised. 
}

@define[Function]{@name{make-transcoder} @args{codec :optional eol-style handling-mode}}
@desc{[R6RS] @var{Codec} must be a codec; @var{eol-style}, if present, an eol-style
symbol; and @var{handling-mode}, if present, an error-handling-mode symbol.
@var{Eol-style} may be omitted, in which case it defaults to the native end-of-line
style of the underlying platform. @var{Handling-mode} may be omitted, in which case
it defaults to @code{replace}. The result is a transcoder with the behaviour
specified by its arguments.
}

@define[Function]{@name{native-transcoder}}
@desc{[R6RS] Returns platform dependent transcoder.}

@define[Function]{@name{transcoder-codec} @args{transcoder}}
@define[Function]{@name{transcoder-eol-style} @args{transcoder}}
@define[Function]{@name{transcoder-error-handlingmode} @args{transcoder}}
@desc{[R6RS] These are accessors for transcoder objects; when applied to a transcoder
returned by @code{make-transcoder}, they return the @var{codec}, @var{eol-style},
and @var{handling-mode} arguments, respectively.
}

@define[Function]{@name{bytevector->string} @args{bytevector transcoder}}
@desc{[R6RS] Returns the string that results from transcoding the @var{bytevector}
according to the input direction of the transcoder.
}

@define[Function]{@name{string->bytevector} @args{string transcoder}}
@desc{[R6RS] Returns the bytevector that results from transcoding the @var{string}
according to the output direction of the transcoder.
}

@sub*section{End-of-file object}

The end-of-file object is returned by various I/O procedures when they reach end
of file.

@define[Function]{@name{eof-object}}
@desc{[R6RS] Returns the end-of-file object}

@define[Function]{@name{eof-object?} @args{obj}}
@desc{Returns #t if @var{obj} is the end-of-file object, #f otherwise.}

@sub*section{Input and output ports}

The operations described in this section are common to input and output ports,
both binary and textual. A port may also have an associated @var{position} that
specifies a particular place within its data sink or source, and may also provide
operations for inspecting and setting that place.

@define[Function]{@name{port?} @args{obj}}
@desc{Returns #t if the argument is a port, and returns #f otherwise.}

@define[Function]{@name{port-transcoder} @args{port}}
@desc{[R6RS] Returns the transcoder associated with @var{port} if port is textual
and has an associated transcoder, and returns #f if @var{port} is binary or does
not have an associated transcoder.
}

@define[Function]{@name{textual-port?} @args{port}}
@define[Function]{@name{binary-port?} @args{port}}
@desc{[R6RS] The @code{textual-port?} procedure returns #t if @var{port} is textual,
and returns #f otherwise.
The @code{binary-port?} procedure returns #t if port is binary, and returns #f
otherwise.}

@define[Function]{@name{transcoded-port} @args{binary-port transcoder}}
@desc{[R6RS] The @code{transcoded-port} procedure returns a new textual port with
the specified @var{transcoder}. Otherwise the new textual port's state is largely
the same as that of @var{binary-port}. If @var{binary-port} is an input port, the
new textual port will be an input port and will transcode the bytes that have not
yet been read from @var{binary-port}. If @var{binary-port} is an output port, the
new textual port will be an output port and will transcode output characters into
bytes that are written to the byte sink represented by @var{binary-port}.
}

@define[Function]{@name{port-has-port-position?} @args{port}}
@define[Function]{@name{port-position} @args{port}}
@desc{[R6RS] The @code{port-has-port-position?} procedure returns #t if the
@var{port} supports the port-position operation, and #f otherwise.

The @var{port-position} procedure returns the index of the position at which the
next position would be read from or written to the port as an exact non-negative
integer object.
}

@define[Function]{@name{port-has-set-port-position!?} @args{port}}
@define[Function]{@name{set-port-position!} @args{port pos}}
@desc{[R6RS] The @code{port-has-set-port-position!?} procedure returns #t if the
@var{port} supports the @code{set-port-position!} operation, and #f otherwise.

The @code{set-port-position!} procedure raises an exception with condition type
@code{&assertion} if the port does not support the operation, and an exception
with condition type @code{&i/o-invalid-position} if pos is not in the range of
valid positions of @var{port}. Otherwise, it sets the current position of the
@var{port} to @var{pos}. If @var{port} is an output port, @var{set-port-position!}
first flushes @var{port}.

On Sagittarius Scheme, binary port and string port have @code{port-position} but
transcoded file port does not support the @code{port-position} and
@code{set-port-position!}.
}

@define[Function]{@name{close-port} @args{port}}
@desc{[R6RS] Closes the @var{port}, rendering the @var{port} incapable of
delivering or accepting data. If @var{port} is an output port, it is flushed
before being closed. This has no effect if the port has already been closed. A
closed port is still a port. The @code{close-port} procedure returns unspecified
values.
}

@define[Function]{@name{call-with-port} @args{proc port}}
@desc{[R6RS] @var{Proc} must accept one argument. The @code{call-with-port}
procedure calls @var{proc} with @var{port} as an argument. If @var{proc} returns,
@var{port} is closed automatically and the values returned by @var{proc} are
returned. If @var{proc} does not return, @var{port} is not closed automatically,
except perhaps when it is possible to prove that @var{port} will never again be
used for an input or output operation. 
}

@sub*section{Input ports}

An input port allows the reading of an infinite sequence of bytes or characters
punctuated by end-of-file objects. An input port connected to a finite data
source ends in an infinite sequence of end-of-file objects.

It is unspecified whether a character encoding consisting of several bytes may
have an end of file between the bytes. If, for example, @code{get-char} raises
an @cpde{&i/o-decoding} exception because the character encoding at the port's
position is incomplete up to the next end of file, a subsequent call to
@code{get-char} may successfully decode a character if bytes completing the
encoding are available after the end of file.

@define[Function]{@name{input-port?} @args{obj}}
@desc{Returns #t if the argument is an input port (or a combined input and output
port), and returns #f otherwise.}

@define[Function]{@name{port-eof?} @args{input-port}}
@desc{[R6RS] Returns #t if the @code{lookahead-u8} procedure (if @var{input-port}
is a binary port) or the @code{lookahead-char} procedure (if @var{input-port} is
a textual port) would return the end-of-file object, and #f otherwise.
}

@define[Function]{@name{open-file-input-port}
 @args{filename :optiona file-options buffer-mode maybe-transcoder}}
@desc{[R6RS] @var{Maybe-transcoder} must be either a transcoder or #f.

The @var{file-options} argument, which may determine various aspects of the
returned port, defaults to the value of @code{(file-options)}.

The @var{buffer-mode} argument, if supplied, must be one of the symbols that
name a buffer mode. The @var{buffer-mode} argument defaults to @code{block}.

If @var{maybe-transcoder} is a transcoder, it becomes the transcoder associated
with the returned port.

If @var{maybe-transcoder} is #f or absent, the port will be a binary port,
otherwise the port will be textual port.
}

@define[Function]{@name{open-bytevector-input-port} @args{bytevector maybe-transcoder}}
@desc{[R6RS] @var{Maybe-transcoder} must be either a transcoder or #f.
The @code{open-bytevector-input-port} procedure returns an input port whose bytes
are drawn from @var{bytevector}. If @var{transcoder} is specified, it becomes the
transcoder associated with the returned port.

If @var{maybe-transcoder} is #f or absent, the port will be a binary port,
otherwise the port will be textual port.
}

@define[Function]{@name{open-string-input-port} @args{string}}
@desc{[R6RS] Returns a textual input port whose characters are drawn from
@var{string}.

These procedures reuse the given arguments, thus if @var{bytevector} is modified
after @code{open-bytevector-input-port} has been called, it affects the result of
the port. So does @code{open-string-input-port}.
}

@define[Function]{@name{standard-input-port}}
@desc{[R6RS] Returns a fresh binary input port connected to standard input.}

@define[Function]{@name{current-input-port} @args{:optional port}}
@desc{[R6RS+] If @var{port} is given, the @code{current-input-port} sets the 
@var{port} as a default port for input. Otherwise it returns a default input port.}

@define[Function]{@name{make-custom-binary-input-port}
 @args{id read! get-position set-position! close}}
@desc{[R6RS] Returns a newly created binary input port whose byte source is an
arbitrary algorithm represented by the read! procedure. @var{Id} must be a string
naming the new port, provided for informational purposes only. @var{Read!} must
be a procedure and should behave as specified below; it will be called by
operations that perform binary input.

Each of the remaining arguments may be #f; if any of those arguments is not #f,
it must be a procedure and should behave as specified below.

@itemlist[
@item{(@var{read!} @var{bytevector} @var{start} @var{count})
@p{@var{Start} will be a non-negative exact integer object, @var{count} will be
a positive exact integer object, and @var{bytevector} will be a bytevector whose
length is at least @var{start} + @var{count}. The @var{read!} procedure should
obtain up to @var{count} bytes from the byte source, and should write those bytes
into @var{bytevector} starting at index @var{start}. The @var{read!} procedure
should return an exact integer object. This integer object should represent the
number of bytes that it has read. To indicate an end of file, the @var{read!}
procedure should write no bytes and return 0.}}
@item{(@var{get-position})
@p{The @var{get-position} procedure (if supplied) should return an exact integer
object representing the current position of the input port. If not supplied, the
custom port will not support the @code{port-position} operation.}}
@item{(@var{set-position!} @var{pos})
@p{@var{Pos} will be a non-negative exact integer object. The @var{set-position!}
procedure (if supplied) should set the position of the input port to @var{pos}.
If not supplied, the custom port will not support the @var{set-port-position!}
operation.}}
@item{(@var{close})
@p{The @var{close} procedure (if supplied) should perform any actions that are
necessary when the input port is closed.}}
]
}

@define[Function]{@name{make-custom-textual-input-port}
 @args{id read! get-position set-position! close}}
@desc{[R6RS] Returns a newly created textual input port whose character source
is an arbitrary algorithm represented by the @var{read!} procedure. @var{Id}
must be a string naming the new port, provided for informational purposes only.
@var{Read!} must be a procedure and should behave as specified below; it will be
called by operations that perform textual input.

Each of the remaining arguments may be #f; if any of those arguments is not #f,
it must be a procedure and should behave as specified below.

@itemlist[
@item{(@var{read!} @var{string} @var{start} @var{count})
@p{@var{Start} will be a non-negative exact integer object, @var{count} will be
a positive exact integer object, and @var{string} will be a string whose length
is at least @var{start} + @var{count}. The @var{read!} procedure should obtain
up to @var{count} characters from the character source, and should write those
characters into @var{string} starting at index @var{start}. The @var{read!}
procedure should return an exact integer object representing the number of
characters that it has written. To indicate an end of file, the @var{read!}
procedure should write no bytes and return 0.}}
@item{(@var{get-position})
@p{The @var{get-position} procedure (if supplied) should return a single value.
The return value should represent the current position of the input port. If not
supplied, the custom port will not support the @code{port-position} operation.}}
@item{(@var{set-position!} @var{pos})
@p{The @var{set-position!} procedure (if supplied) should set the position of
the input port to @var{pos} if @var{pos} is the return value of a call to
@var{get-position}. If not supplied, the custom port will not support the
@code{set-port-position!} operation.}}
@item{(@var{close})
@p{The @var{close} procedure (if supplied) should perform any actions that are
necessary when the input port is closed.}}
]
}

@sub*section{Binary input}

@define[Function]{@name{get-u8} @args{binary-input-port}}
@desc{[R6RS] Reads from @var{binary-input-port}, blocking as necessary, until a
byte is available from @var{binary-input-port} or until an end of file is reached.
If a byte becomes available, @code{get-u8} returns the byte as an octet and
updates @var{binary-input-port} to point just past that byte. If no input byte
is seen before an end of file is reached, the end-of-file object is returned.
}

@define[Function]{@name{lookahead-u8} @args{binary-input-port}}
@desc{[R6RS] The @code{lookahead-u8} procedure is like @code{get-u8}, but it does
not update @var{binary-input-port} to point past the byte.}

@define[Function]{@name{get-bytevector-n} @args{binary-input-port count :optional reckless}}
@desc{[R6RS+] @var{Count} must be an exact, non-negative integer object representing
the number of bytes to be read.

The @code{get-bytevector-n} procedure reads from @var{binary-input-port}, blocking
as necessary, until @var{count} bytes are available from @var{binary-input-port}
or until an end of file is reached. If @var{count} bytes are available before an
end of file, @code{get-bytevector-n} returns a bytevector of size @var{count}.
If fewer bytes are available before an end of file, @code{get-bytevector-n}
returns a bytevector containing those bytes. In either case, the input port is
updated to point just past the bytes read. If an end of file is reached before
any bytes are available, @code{get-bytevector-n} returns the end-of-file object.
}

@define[Function]{@name{get-bytevector-n!}
 @args{binary-input-port bytevector start count :optional reckless}}
@desc{[R6RS+] @var{Count} must be an exact, non-negative integer object,
representing the number of bytes to be read. @var{bytevector} must be a
bytevector with at least @var{start} + @var{count} elements.

The @code{get-bytevector-n!} procedure reads from @var{binary-input-port},
blocking as necessary, until @var{count} bytes are available from
@var{binary-input-port} or until an end of file is reached. If @var{count} bytes
are available before an end of file, they are written into @var{bytevector}
starting at index @var{start}, and the result is @var{count}. If fewer bytes are
available before the next end of file, the available bytes are written into
@var{bytevector} starting at index @var{start}, and the result is a number object
representing the number of bytes actually read. In either case, the input port is
updated to point just past the bytes read. If an end of file is reached before
any bytes are available, @code{get-bytevector-n!} returns the end-of-file object.
}

@define[Function]{@name{get-bytevector-some} @args{binary-input-port :optional reckless}}
@desc{[R6RS+] Reads from @var{binary-input-port}, blocking as necessary, until
bytes are available from @var{binary-input-port} or until an end of file is
reached. If bytes become available, @code{get-bytevector-some} returns a freshly
allocated bytevector containing the initial available bytes (at least one and
maximum 512 bytes), and it updates @var{binary-input-port} to point just past
these bytes. If no input bytes are seen before an end of file is reached, the
end-of-file object is returned.
}

@define[Function]{@name{get-bytevector-all} @args{binary-input-port :optional reckless}}
@desc{[R6RS+] Attempts to read all bytes until the next end of file, blocking
as necessary. If one or more bytes are read, @code{get-bytevector-all} returns a
bytevector containing all bytes up to the next end of file. Otherwise,
@code{get-bytevector-all} returns the end-of-file object.

These procedures can take optional argument @var{reckless}. If this is given,
these procedures can read bytes from textual port. This optional argument is for
socket programming. Users needs to make sure that the given port can be read as
textual port after reading port recklessly.
}

@sub*section{Textual input}

@define[Function]{@name{get-char} @args{textual-input-port}}
@desc{[R6RS] Reads from @var{textual-input-port}, blocking as necessary, until a
complete character is available from @var{textual-input-port}, or until an end
of file is reached.

If a complete character is available before the next end of file, @code{get-char}
returns that character and updates the input port to point past the character.
If an end of file is reached before any character is read, @code{get-char}
returns the end-of-file object. 
}

@define[Function]{@name{lookahead-char} @args{textual-input-port}}
@desc{[R6RS] The @code{lookahead-char} procedure is like @code{get-char}, but it
does not update @var{textual-input-port} to point past the character.
}

@define[Function]{@name{get-string-n} @args{textual-input-port count}}
@desc{[R6RS] @var{Count} must be an exact, non-negative integer object,
representing the number of characters to be read.

The @code{get-string-n} procedure reads from @var{textual-input-port}, blocking
as necessary, until @var{count} characters are available, or until an end of
file is reached.

If @var{count} characters are available before end of file, @code{get-string-n}
returns a string consisting of those @var{count} characters. If fewer characters
are available before an end of file, but one or more characters can be read,
@code{get-string-n} returns a string containing those characters. In either case,
the input port is updated to point just past the characters read. If no characters
can be read before an end of file, the end-of-file object is returned. 
}

@define[Function]{@name{get-string-n!} @args{textual-input-port string start count}}
@desc{[R6RS] @var{Start} and @var{count} must be exact, non-negative integer
objects, with @var{count} representing the number of characters to be read.
@var{String} must be a string with at least @var{start} + @var{count} characters.

The @code{get-string-n!} procedure reads from @var{textual-input-port} in the
same manner as @code{get-string-n}. If @var{count} characters are available before
an end of file, they are written into @var{string} starting at index @var{start},
and @var{count} is returned. If fewer characters are available before an end of
file, but one or more can be read, those characters are written into string
starting at index @var{start} and the number of characters actually read is
returned as an exact integer object. If no characters can be read before an end
of file, the end-of-file object is returned.
}

@define[Function]{@name{get-string-all} @args{textual-input-port}}
@desc{[R6RS] Reads from @var{textual-input-port} until an end of file, decoding
characters in the same manner as @code{get-string-n} and @code{get-string-n!}.

If characters are available before the end of file, a string containing all the
characters decoded from that data are returned. If no character precedes the end
of file, the end-of-file object is returned. 
}

@define[Function]{@name{get-line} @args{textual-input-port}}
@desc{[R6RS] Reads from @var{textual-input-port} up to and including the linefeed
character or end of file, decoding characters in the same manner as
@code{get-string-n} and @code{get-string-n!}.

If a linefeed character is read, a string containing all of the text up to (but
not including) the linefeed character is returned, and the port is updated to
point just past the linefeed character. If an end of file is encountered before
any linefeed character is read, but some characters have been read and decoded
as characters, a string containing those characters is returned. If an end of
file is encountered before any characters are read, the end-of-file object is
returned.
}

@define[Function]{@name{get-datum} @args{textual-input-port}}
@desc{[R6RS] Reads an external representation from @var{textual-input-port} and
returns the datum it represents. The @code{get-datum} procedure returns the next
datum that can be parsed from the given @var{textual-input-port}, updating
@var{textual-input-port} to point exactly past the end of the external
representation of the object.

If a character inconsistent with an external representation is encountered in
the input, an exception with condition types @code{&lexical} and @code{&i/o-read}
is raised. Also, if the end of file is encountered after the beginning of an
external representation, but the external representation is incomplete and
therefore cannot be parsed, an exception with condition types @code{&lexical}
and @code{&i/o-read} is raised. 
}

@sub*section{Output ports}

An output port is a sink to which bytes or characters are written. The written
data may control external devices or may produce files and other objects that may
subsequently be opened for input.

@define[Function]{@name{output-port?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is an output port (or a combined input and
output port), #f otherwise.}

@define[Function]{@name{flush-output-port} @args{output-port}}
@desc{[R6RS] Flushes any buffered output from the buffer of @var{output-port} to
the underlying file, device, or object. The @code{flush-output-port} procedure
returns unspecified values.
}

@define[Function]{@name{output-port-buffer-mode} @args{output-port}}
@desc{[R6RS] Returns the symbol that represents the buffer mode of @var{output-port}.}

@define[Function]{@name{open-file-output-port}
 @args{filename :optional file-options buffer-mode maybe-transcoder}}
@desc{[R6RS] @var{Maybe-transcoder} must be either a transcoder or #f.

The @code{open-file-output-port} procedure returns an output port for the named
file.

The @var{file-options} argument, which may determine various aspects of the
returned port, defaults to the value of @code{(file-options)}.

The @var{buffer-mode} argument, if supplied, must be one of the symbols that
name a buffer mode. The @var{buffer-mode} argument defaults to @code{block}.

If @var{maybe-transcoder} is a transcoder, it becomes the transcoder associated
with the port.

If @var{maybe-transcoder} is #f or absent, the port will be a binary port,
otherwise the port will be textual port.
}

@define[Function]{@name{open-bytevector-output-port} @args{:optional maybe-transcoder}}
@desc{[R6RS] @var{Maybe-transcoder} must be either a transcoder or #f.

The @code{open-bytevector-output-port} procedure returns two values: an output
port and an extraction procedure. The output port accumulates the bytes written
to it for later extraction by the procedure.

If @var{maybe-transcoder} is a transcoder, it becomes the transcoder associated
with the port. If @var{maybe-transcoder} is #f or absent, the port will be a
binary port, otherwise the port will be textual port.

The extraction procedure takes no arguments. When called, it returns a bytevector
consisting of all the port's accumulated bytes (regardless of the port's current
position), removes the accumulated bytes from the port, and resets the port's
position.
}

@define[Function]{@name{call-with-bytevector-output-port}
 @args{proc :optional maybe-transcoder}}
@desc{[R6RS] @var{Proc} must accept one argument. @var{Maybe-transcoder} must be
either a transcoder or #f.

The @code{call-with-bytevector-output-port} procedure creates an output port that
accumulates the bytes written to it and calls @var{proc} with that output port as
an argument. Whenever @var{proc} returns, a bytevector consisting of all of the
port's accumulated bytes (regardless of the port's current position) is returned
and the port is closed.

The transcoder associated with the output port is determined as for a call to
@code{open-bytevector-output-port}.
}

@define[Function]{@name{open-string-output-port}}
@desc{[R6RS] Returns two values: a textual output port and an extraction procedure.
The output port accumulates the characters written to it for later extraction by
the procedure.

The extraction procedure takes no arguments. When called, it returns a string
consisting of all of the port's accumulated characters (regardless of the current
position), removes the accumulated characters from the port, and resets the port's
position.
}

@define[Function]{@name{call-with-string-output-port} @args{proc}}
@desc{[R6RS] @var{Proc} must accept one argument.

The @code{call-with-string-output-port} procedure creates a textual output port
that accumulates the characters written to it and calls @var{proc} with that
output port as an argument. Whenever proc returns, a string consisting of all of
the port's accumulated characters (regardless of the port's current position) is
returned and the port is closed.
}

@define[Function]{@name{standard-output-port}}
@define[Function]{@name{standard-error-port}}
@desc{[R6RS] Returns a fresh binary output port connected to the standard output
or standard error respectively.
}

@define[Function]{@name{current-output-port} @args{:optional port}}
@define[Function]{@name{current-error-port} @args{:optional port}}
@desc{[R6RS+] If @var{port} is given, these procedures set the @var{port} as a
default port for output and error. These return default ports for regular output
and error output.
}

@define[Function]{@name{make-custom-binary-output-port}
 @args{ id write! get-position set-position! close}}
@desc{[R6RS] Returns a newly created binary output port whose byte sink is an
arbitrary algorithm represented by the @var{write!} procedure. @var{Id} must be
a string naming the new port, provided for informational purposes only. @var{Write!}
must be a procedure and should behave as specified below; it will be called by
operations that perform binary output.

Each of the remaining arguments may be #f; if any of those arguments is not #f,
it must be a procedure and should behave as specified in the description of
@code{make-custom-binary-input-port}.

@itemlist[
@item{(@var{write!} @var{bytevector} @var{start} @var{count})
@p{@var{Start} and @var{count} will be non-negative exact integer objects, and
@var{bytevector} will be a bytevector whose length is at least @var{start} +
@var{count}. The @var{write!} procedure should write up to @var{count} bytes
from @var{bytevector} starting at index @var{start} to the byte sink. If 
@var{count} is 0, the @var{write!} procedure should have the effect of passing
an end-of-file object to the byte sink. In any case, the @var{write!} procedure
should return the number of bytes that it wrote, as an exact integer object.}}
]
}

@define[Function]{@name{make-custom-string-output-port}
 @args{ id write! get-position set-position! close}}
@desc{[R6RS] Returns a newly created textual output port whose byte sink is an
arbitrary algorithm represented by the @var{write!} procedure. @var{Id} must be
a string naming the new port, provided for informational purposes only. @var{Write!}
must be a procedure and should behave as specified below; it will be called by
operations that perform textual output.

Each of the remaining arguments may be #f; if any of those arguments is not #f,
it must be a procedure and should behave as specified in the description of
@code{make-custom-textual-input-port}.

@itemlist[
@item{(@var{write!} @var{string} @var{start} @var{count})
@p{@var{Start} and @var{count} will be non-negative exact integer objects, and
@var{string} will be a string whose length is at least @var{start} + @var{count}.
The @var{write!} procedure should write up to @var{count} characters from
@var{string} starting at index @var{start} to the character sink. If @var{count}
is 0, the @var{write!} procedure should have the effect of passing an end-of-file
object to the character sink. In any case, the @var{write!} procedure should
return the number of characters that it wrote, as an exact integer object.}}
]
}

@sub*section{Binary output}

@define[Function]{@name{put-u8} @args{binary-output-port octet}}
@desc{[R6RS] Writes @var{octet} to the output port and returns unspecified values.}

@define[Function]{@name{put-bytevector}
 @args{binary-output-port bytevector :optional start count}}
@desc{[R6RS] @var{Start} and @var{count} must be non-negative exact integer
objects that default to 0 and @code{(bytevector-length @var{bytevector})} - @var{start},
respectively. @var{Bytevector} must have a length of at least @var{start} + @var{count}.
The @code{put-bytevector} procedure writes the @var{count} bytes of the bytevector
@var{bytevector} starting at index @var{start} to the output port. The
@code{put-bytevector} procedure returns unspecified values.
}

@sub*section{Textual output}

@define[Function]{@name{put-char} @args{textual-output-port char}}
@desc{[R6RS] Writes @var{char} to the port and returns unspecified values.}

@define[Function]{@name{put-string}
 @args{textual-output-port string :optional start count}}
@desc{[R6RS] @var{Start} and @var{count} must be non-negative exact integer
objects. @var{String} must have a length of at least @var{start} + @var{count}.
@var{Start} defaults to 0. @var{Count} defaults to
@code{(string-length @var{string})} - @var{start}. The @code{put-string} procedure
writes the count characters of @var{string} starting at index @var{start} to the
port. The @code{put-string} procedure returns unspecified values.
}

@define[Function]{@name{put-datum} @args{textual-output-port datum}}
@desc{[R6RS] @var{Datum} should be a datum value. The @code{put-datum} procedure
writes an external representation of @var{datum} to @var{textual-output-port}.
}

@sub*section{Input/output ports}

@define[Function]{@name{open-file-input/output-port}
 @args{filename :optional file-options buffer-mode transcoder}}
@desc{[R6RS] Returns a single port that is both an input port and an output port
for the named file. The optional arguments default as described in the
specification of @code{open-file-output-port}. If the input/output port supports
@code{port-position} and/or @code{set-port-position!}, the same port position
is used for both input and output.
}

@define[Function]{@name{make-custom-binary-input/output-port}
 @args{id read! write! get-position set-position! close}}
@desc{[R6RS] Returns a newly created binary input/output port whose byte source
and sink are arbitrary algorithms represented by the @var{read!} and @var{write!}
procedures. @var{Id} must be a string naming the new port, provided for
informational purposes only. @var{Read!} and @var{write!} must be procedures,
and should behave as specified for the @code{make-custom-binary-input-port} and
@code{make-custom-binary-output-port} procedures.

Each of the remaining arguments may be #f; if any of those arguments is not #f,
it must be a procedure and should behave as specified in the description of
@code{make-custom-binary-input-port}.
}

@define[Function]{@name{make-custom-textual-input/output-port}
 @args id read! write! get-position set-position! close}}
@desc{[R6RS] Returns a newly created textual input/output port whose byte source
and sink are arbitrary algorithms represented by the @var{read!} and @var{write!}
procedures. @var{Id} must be a string naming the new port, provided for
informational purposes only. @var{Read!} and @var{write!} must be procedures,
and should behave as specified for the @code{make-custom-textual-input-port} and
@code{make-custom-textual-output-port} procedures.

Each of the remaining arguments may be #f; if any of those arguments is not #f,
it must be a procedure and should behave as specified in the description of
@code{make-custom-textual-input-port}.
}
