### [§3] Port I/O {#rnrs.io.ports.6}

###### [!Library] `(rnrs io ports (6))` 

The `(rnrs io ports (6))`library defines an I/O layer for
conventional, imperative buffered input and output. A _port_ represents
a buffered access object for a data sink or source or both simultaneously.
The library allows ports to be created from arbitrary data sources and sinks.

The `(rnrs io ports (6))` library distinguishes between _input ports_and _output ports_. An input port is a source for data, whereas an output
port is a sink for data. A port may be both an input port and an output port;
such a port typically provides simultaneous read and write access to a file or
other data.

The `(rnrs io ports (6))` library also distinguishes between
_binary ports_, which are sources or sinks for uninterpreted bytes,
and _textual ports_, which are sources or sinks for characters and strings.

This section uses _input-port_, _output-port_, _binary-port_,
_textual-port_, _binary-input-port_, _textual-input-port_,
_binary-output-port_, _textual-output-port_, and _port_ as parameter
names for arguments that must be input ports (or combined input/output ports),
output ports (or combined input/output ports), binary ports, textual ports,
binary input ports, textual input ports, binary output ports, textual output
ports, or any kind of port, respectively.


#### [§4] Filename

In this world, unfortunately there are a lot of operating systems. However, as
far as I know there are only two file separators, one is Windows style back
slash, the other is UNIX style slash. On Sagittarius both of it can be used as
file path. Inside of the resolution of file path, Sagittarius replaces those
file separators to OS specific one. Even though programmer does not have to care
about it, I think it's better to use slash as file separator on script files.

A _filename_ parameter name means that the corresponding argument must be a
file name.

#### [§4] File options

When opening a file, the various procedures in this library accept a
`file-options` object that encapsulates flags to specify how the file is to
be opened. A `file-options` object is an enum-set
(see [(rnrs enums (6))](#rnrs.enums.6)) over the symbols constituting
valid file options. A _file-options_ parameter name means that the
corresponding argument must be a _file-options_ object.

###### [!Macro] `file-options`  _file-options-symbol_ _..._

[R6RS+] Each _file-option-symbol_ must be a symbol. The
_file-options_ macro returns a _file-options_ object that encapsulates
the specified options.

When supplied to an operation that opens a file for output, the file-options
object returned by `(file-options)` specifies that the file is created if
it does not exist and an exception with condition type
`&i/o-file-already-exists` is raised if it does exist. The following
standard options can be included to modify the default behaviour.


- `no-create` If the file does not already exist, it is not created;
  instead, an exception with condition type `&i/o-file-does-not-exist` is
  raised. If the file already exists, the exception with condition type
  `&i/o-file-already-exists` is not raised and the file is truncated to zero
  length.
- `no-fail` If the file already exists, the exception with condition
  type `&i/o-file-already-exists` is not raised, even if `no-create` is
  not included, and the file is truncated to zero length.
- `no-truncate` If the file already exists and the exception with
  condition type `&i/o-file-already-exists` has been inhibited by inclusion
  of `no-create` or `no-fail`, the file is not truncated, but the port's
  current position is still set to the beginning of the file.
- `append` Among with `no-truncate`, set the opened port's 
  position the end of the file. This is not a part of R6RS specification.



#### [§4] Buffer modes

Each port has an associated buffer mode. For an output port, the buffer mode
defines when an output operation flushes the buffer associated with the output
port. For an input port, the buffer mode defines how much data will be read to
satisfy read operations. The possible buffer modes are the symbols none for no
buffering, line for flushing upon line endings and reading up to line endings,
or other implementation-dependent behavior, and block for arbitrary buffering.
This section uses the parameter name _buffer-mode_ for arguments that must
be buffer-mode symbols.

If two ports are connected to the same mutable source, both ports are
unbuffered, and reading a byte or character from that shared source via one of
the two ports would change the bytes or characters seen via the other port, a
lookahead operation on one port will render the peeked byte or character
inaccessible via the other port, while a subsequent read operation on the
peeked port will see the peeked byte or character even though the port is
otherwise unbuffered.

In other words, the semantics of buffering is defined in terms of side effects
on shared mutable sources, and a lookahead operation has the same side effect
on the shared source as a read operation.

###### [!Macro] `buffer-mode`  _buffer-mode-symbol_

[R6RS] _Buffer-mode-symbol_ must be a symbol whose name is one of
`none`, `line`, and `block`. The result is the corresponding
symbol, and specifies the associated buffer mode.


###### [!Function] `buffer-mode?`  _obj_

Returns #t if the argument is a valid buffer-mode symbol, and returns #f
otherwise.

#### [§4] Transcoders

Several different Unicode encoding schemes describe standard ways to encode
characters and strings as byte sequences and to decode those sequences. Within
this document, a codec is an immutable Scheme object that represents a Unicode
or similar encoding scheme.

An _end-of-line_ style is a symbol that, if it is not none, describes how a
textual port transcodes representations of line endings.

A _transcoder_ is an immutable Scheme object that combines a codec with an
end-of-line style and a method for handling decoding errors. Each transcoder
represents some specific bidirectional (but not necessarily lossless), possibly
stateful translation between byte sequences and Unicode characters and strings.
Every transcoder can operate in the input direction (bytes to characters) or in
the output direction (characters to bytes). A _transcoder_ parameter name
means that the corresponding argument must be a transcoder.

A _binary port_ is a port that supports binary I/O, does not have an
associated transcoder and does not support textual I/O. A _textual_ 
port is a port that supports textual I/O, and does not support binary I/O.
A textual port may or may not have an associated transcoder.

###### [!Function] `latin-1-codec` 
###### [!Function] `utf-8-codec` 
###### [!Function] `utf-16-codec` 

[R6RS] These are predefined codecs for the ISO 8859-1, UTF-8, and UTF-16
encoding schemes.

A call to any of these procedures returns a value that is equal in the sense
of `eqv?` to the result of any other call to the same procedure.

###### [!Macro] `eol-style`  _eol-style-symbol_

[R6RS] _Eol-style-symbol_ should be a symbol whose name is one of
`lf`, `cr`, `crlf`, `nel`, `crnel`, `ls`, and
`none`. The form evaluates to the corresponding symbol. If the name of
_eol-style-symbol_ is not one of these symbols, it still returns given
symbol, however `make-transcoder` does not accept it.


###### [!Function] `native-eol-style` 

[R6RS] Returns the default end-of-line style of the underlying platform.

###### [!Condition Type] `&i/o-decoding` 
###### [!Function] `make-i/o-decoding-error`  _port_
###### [!Function] `i/o-decoding-error?`  _obj_

[R6RS] An exception with this type is raised when one of the operations
for textual input from a port encounters a sequence of bytes that cannot be
translated into a character or string by the input direction of the port's
transcoder.

When such an exception is raised, the port's position is past the invalid
encoding.


###### [!Condition Type] `&i/o-encoding` 
###### [!Function] `make-i/o-encoding-error`  _port_
###### [!Function] `i/o-encoding-error?`  _obj_

[R6RS] An exception with this type is raised when one of the operations
for textual output to a port encounters a character that cannot be translated
into bytes by the output direction of the port's transcoder. Char is the
character that could not be encoded.

###### [!Macro] `error-handling-mode`  _error-handling-mode-symbol_

[R6RS] _Error-handling-mode-symbol_ should be a symbol whose name is
one of `ignore`, `raise`, and `replace`. The form evaluates to
the corresponding symbol.

The `error-handling` mode of a transcoder specifies the behavior of textual
I/O operations in the presence of encoding or decoding errors.

If a textual input operation encounters an invalid or incomplete character
encoding, and the error-handling mode is `ignore`, an appropriate number
of bytes of the invalid encoding are ignored and decoding continues with the
following bytes. If the error-handling mode is `replace`, the replacement
character U+FFFD is injected into the data stream, an appropriate number of
bytes are ignored, and decoding continues with the following bytes. If the
error-handling mode is `raise`, an exception with condition type
`&i/o-decoding` is raised.

If a textual output operation encounters a character it cannot encode, and the
error-handling mode is `ignore`, the character is ignored and encoding
continues with the next character. If the error-handling mode is `replace`,
a codec-specific replacement character is emitted by the transcoder, and
encoding continues with the next character. The replacement character is U+FFFD
for transcoders whose codec is one of the Unicode encodings, but is the `?`character for the Latin-1 encoding. If the error-handling mode is `raise`,
an exception with condition type `&i/o-encoding` is raised. 


###### [!Function] `make-transcoder`  _codec_ _:optional_ _eol-style_ _handling-mode_

[R6RS] _Codec_ must be a codec; _eol-style_, if present, an
eol-style symbol; and _handling-mode_, if present, an error-handling-mode
symbol. _Eol-style_ may be omitted, in which case it defaults to the native
end-of-line style of the underlying platform. _Handling-mode_ may be
omitted, in which case it defaults to `replace`. The result is a
transcoder with the behaviour specified by its arguments.


###### [!Function] `native-transcoder` 

[R6RS] Returns platform dependent transcoder.

###### [!Function] `transcoder-codec`  _transcoder_
###### [!Function] `transcoder-eol-style`  _transcoder_
###### [!Function] `transcoder-error-handlingmode`  _transcoder_

[R6RS] These are accessors for transcoder objects; when applied to a
transcoder returned by `make-transcoder`, they return the _codec_,
_eol-style_, and _handling-mode_ arguments, respectively.


###### [!Function] `bytevector->string`  _bytevector_ _transcoder_ _:optional_ _start_ _end_

[R6RS+] Returns the string that results from transcoding the
_bytevector_ according to the input direction of the transcoder.

If the optional argument _start_ is given, the procedure converts given
string from _start_ index (inclusive).

If the optional argument _end_ is given, the procedure converts given
string to _end_ index (exclusive).

These optional arguments must be fixnum if it's given.


###### [!Function] `string->bytevector`  _string_ _transcoder_ _:optional_ _start_ _end_

[R6RS] Returns the bytevector that results from transcoding the
_string_ according to the output direction of the transcoder.

If the optional argument _start_ is given, the procedure converts given
string from _start_ index (inclusive).

If the optional argument _end_ is given, the procedure converts given
string to _end_ index (exclusive).

These optional arguments must be fixnum if it's given.


#### [§4] End-of-file object

The end-of-file object is returned by various I/O procedures when they reach end
of file.

###### [!Function] `eof-object` 

[R6RS] Returns the end-of-file object

###### [!Function] `eof-object?`  _obj_

Returns #t if _obj_ is the end-of-file object, #f otherwise.

#### [§4] Input and output ports

The operations described in this section are common to input and output ports,
both binary and textual. A port may also have an associated _position_ that
specifies a particular place within its data sink or source, and may also
provide operations for inspecting and setting that place.

###### [!Function] `port?`  _obj_

Returns #t if the argument is a port, and returns #f otherwise.

###### [!Function] `port-transcoder`  _port_

[R6RS] Returns the transcoder associated with _port_ if port is
textual and has an associated transcoder, and returns #f if _port_ is
binary or does not have an associated transcoder.


###### [!Function] `textual-port?`  _obj_
###### [!Function] `binary-port?`  _obj_

[R6RS+] [R7RS] The `textual-port?` procedure returns #t if _obj_is textual port, otherwise #f.

The `binary-port?` procedure returns #t if _obj_ is binary port,
otherwise #f.

###### [!Function] `transcoded-port`  _binary-port_ _transcoder_

[R6RS] The `transcoded-port` procedure returns a new textual port
with the specified _transcoder_. Otherwise the new textual port's state is
largely the same as that of _binary-port_. If _binary-port_ is an input
port, the new textual port will be an input port and will transcode the bytes
that have not yet been read from _binary-port_. If _binary-port_ is an
output port, the new textual port will be an output port and will transcode
output characters into bytes that are written to the byte sink represented by
_binary-port_.


###### [!Function] `port-has-port-position?`  _port_
###### [!Function] `port-position`  _port_

[R6RS] The `port-has-port-position?` procedure returns #t if the
_port_ supports the port-position operation, and #f otherwise.

The _port-position_ procedure returns the index of the position at which the
next position would be read from or written to the port as an exact non-negative
integer object.


###### [!Function] `port-has-set-port-position!?`  _port_
###### [!Function] `set-port-position!`  _port_ _pos_ _:optional_ _(whence_ _'begin)_

[R6RS+] The `port-has-set-port-position!?` procedure returns #t if the
_port_ supports the `set-port-position!` operation, and #f otherwise.

The `set-port-position!` procedure raises an exception with condition type
`&assertion` if the port does not support the operation, and an exception
with condition type `&i/o-invalid-position` if pos is not in the range of
valid positions of _port_. Otherwise, it sets the current position of the
_port_ to _pos_. If _port_ is an output port,
_set-port-position!_ first flushes _port_.

The optional argument _whence_ must be one of the following symbols;

`begin`
: Set position from the beginning of the given _port_.

`current`
: Set position from the current position of the given _port_.

`end`
: Set position from the end of the given _port_.

NOTE: for R6RS custom port, the procedure doesn't accept the optional argument,
so it will be always considered `begin` even though user specified it
as `current` or `end`.


###### [!Function] `close-port`  _port_

[R6RS] Closes the _port_, rendering the _port_ incapable of
delivering or accepting data. If _port_ is an output port, it is flushed
before being closed. This has no effect if the port has already been closed. A
closed port is still a port. The `close-port` procedure returns unspecified
values.


###### [!Function] `call-with-port`  _port_ _proc_

[R6RS] _Proc_ must accept one argument. The `call-with-port`procedure calls _proc_ with _port_ as an argument. If _proc_returns, _port_ is closed automatically and the values returned by
_proc_ are returned. If _proc_ does not return, _port_ is not
closed automatically, except perhaps when it is possible to prove that
_port_ will never again be used for an input or output operation. 


#### [§4] Input ports

An input port allows the reading of an infinite sequence of bytes or characters
punctuated by end-of-file objects. An input port connected to a finite data
source ends in an infinite sequence of end-of-file objects.

It is unspecified whether a character encoding consisting of several bytes may
have an end of file between the bytes. If, for example, `get-char` raises
an `&i/o-decoding` exception because the character encoding at the port's
position is incomplete up to the next end of file, a subsequent call to
`get-char` may successfully decode a character if bytes completing the
encoding are available after the end of file.

###### [!Function] `input-port?`  _obj_

Returns #t if the argument is an input port (or a combined input and
output port), and returns #f otherwise.

###### [!Function] `port-eof?`  _input-port_

[R6RS] Returns #t if the `lookahead-u8` procedure (if
_input-port_ is a binary port) or the `lookahead-char` procedure (if
_input-port_ is a textual port) would return the end-of-file object, and
#f otherwise.


###### [!Function] `open-file-input-port`  _filename_ _:optiona_ _file-options_ _buffer-mode_ _maybe-transcoder_

[R6RS] _Maybe-transcoder_ must be either a transcoder or #f.

The _file-options_ argument, which may determine various aspects of the
returned port, defaults to the value of `(file-options)`.

The _buffer-mode_ argument, if supplied, must be one of the symbols that
name a buffer mode. The _buffer-mode_ argument defaults to `block`.

If _maybe-transcoder_ is a transcoder, it becomes the transcoder associated
with the returned port.

If _maybe-transcoder_ is #f or absent, the port will be a binary port,
otherwise the port will be textual port.


###### [!Function] `open-bytevector-input-port`  _bytevector_ _:optional_ _(transcoder_ _#f)_ _
_ _(start_ _0)_ _(end_ _(bytevector-length_ _bytevector))_

[R6RS+] _transcoder_ must be either a transcoder or #f.
The `open-bytevector-input-port` procedure returns an input port whose
bytes are drawn from _bytevector_. If _transcoder_ is specified, it
becomes the transcoder associated with the returned port.

If _transcoder_ is #f or absent, the port will be a binary port,
otherwise the port will be textual port.

Optional arguments _start_ and _end_ restricts the range of
input bytevector. It is almost the same as following code but doesn't
allocate extra memory;
``(open-bytevector-input-port (bytevector-copy bytevector start end))``



###### [!Function] `open-string-input-port`  _string_ _:optional_ _(start_ _0)_ _(end_ _(string-length_ _string))_

[R6RS+] Returns a textual input port whose characters are drawn from
_string_.

Optional arguments _start_ and _end_ restricts the range of
input string. It is almost the same as following code but doesn't
allocate extra memory;
``(open-string-input-port (substring string start end))``

These procedures reuse the given arguments, thus if _bytevector_ is modified
after `open-bytevector-input-port` has been called, it affects the result
of the port. So does `open-string-input-port`.


###### [!Function] `standard-input-port` 

[R6RS] Returns a fresh binary input port connected to standard input.

###### [!Function] `current-input-port`  _:optional_ _port_

[R6RS+] If _port_ is given, the `current-input-port` sets the 
_port_ as a default port for input. Otherwise it returns a default input
port.

###### [!Function] `make-custom-binary-input-port`  _id_ _read!_ _get-position_ _set-position!_ _close_ _:optional_ _(ready_ _#f)_

[R6RS+] Returns a newly created binary input port whose byte source is an
arbitrary algorithm represented by the read! procedure. _Id_ must be a
string naming the new port, provided for informational purposes only.
_Read!_ must be a procedure and should behave as specified below; it will
be called by operations that perform binary input.

Each of the remaining arguments may be #f; if any of those arguments is not #f,
it must be a procedure and should behave as specified below.


- (_read!_ _bytevector_ _start_ _count_)
  _Start_ will be a non-negative exact integer object, _count_ will be
  a positive exact integer object, and _bytevector_ will be a bytevector whose
  length is at least _start_ + _count_. The _read!_ procedure should
  obtain up to _count_ bytes from the byte source, and should write those
  bytes into _bytevector_ starting at index _start_. The _read!_procedure should return an exact integer object. This integer object should
  represent the number of bytes that it has read. To indicate an end of file, the
  _read!_ procedure should write no bytes and return 0.
- (_get-position_)
  The _get-position_ procedure (if supplied) should return an exact integer
  object representing the current position of the input port. If not supplied, the
  custom port will not support the `port-position` operation.
- (_set-position!_ _pos_)
  _Pos_ will be a non-negative exact integer object. The
  _set-position!_ procedure (if supplied) should set the position of the
  input port to _pos_. If not supplied, the custom port will not support the
  _set-port-position!_ operation.
- (_close_)
  The _close_ procedure (if supplied) should perform any actions that are
  necessary when the input port is closed.
- (_ready_)
  The _ready_ procedure (if supplied) should indicate the port data are
  ready or not.



###### [!Function] `make-custom-textual-input-port`  _id_ _read!_ _get-position_ _set-position!_ _close_ _:optional_ _(ready_ _#f)_

[R6RS+] Returns a newly created textual input port whose character source
is an arbitrary algorithm represented by the _read!_ procedure. _Id_must be a string naming the new port, provided for informational purposes only.
_Read!_ must be a procedure and should behave as specified below; it will be
called by operations that perform textual input.

Each of the remaining arguments may be #f; if any of those arguments is not #f,
it must be a procedure and should behave as specified below.


- (_read!_ _string_ _start_ _count_)
  _Start_ will be a non-negative exact integer object, _count_ will be
  a positive exact integer object, and _string_ will be a string whose length
  is at least _start_ + _count_. The _read!_ procedure should obtain
  up to _count_ characters from the character source, and should write those
  characters into _string_ starting at index _start_. The _read!_procedure should return an exact integer object representing the number of
  characters that it has written. To indicate an end of file, the _read!_procedure should write no bytes and return 0.
- (_get-position_)
  The _get-position_ procedure (if supplied) should return a single value.
  The return value should represent the current position of the input port. If not
  supplied, the custom port will not support the `port-position` operation.
- (_set-position!_ _pos_)
  The _set-position!_ procedure (if supplied) should set the position of
  the input port to _pos_ if _pos_ is the return value of a call to
  _get-position_. If not supplied, the custom port will not support the
  `set-port-position!` operation.
- (_close_)
  The _close_ procedure (if supplied) should perform any actions that are
  necessary when the input port is closed.
- (_ready_)
  The _ready_ procedure (if supplied) should indicate the port characters
  are ready or not.



#### [§4] Binary input

###### [!Function] `get-u8`  _binary-input-port_ _:optional_ _reckless_

[R6RS] Reads from _binary-input-port_, blocking as necessary, until a
byte is available from _binary-input-port_ or until an end of file is
reached.

If a byte becomes available, `get-u8` returns the byte as an octet and
updates _binary-input-port_ to point just past that byte. If no input byte
is seen before an end of file is reached, the end-of-file object is returned.


###### [!Function] `lookahead-u8`  _binary-input-port_ _:optional_ _reckless_

[R6RS] The `lookahead-u8` procedure is like `get-u8`, but it
does not update _binary-input-port_ to point past the byte.

###### [!Function] `get-bytevector-n`  _binary-input-port_ _count_ _:optional_ _reckless_

[R6RS+] _Count_ must be an exact, non-negative integer object
 representing the number of bytes to be read.

The `get-bytevector-n` procedure reads from _binary-input-port_,
blocking as necessary, until _count_ bytes are available from 
_binary-input-port_ or until an end of file is reached. If _count_bytes are available before an end of file, `get-bytevector-n` returns a
bytevector of size _count_.

If fewer bytes are available before an end of file, `get-bytevector-n`returns a bytevector containing those bytes. In either case, the input port is
updated to point just past the bytes read. If an end of file is reached before
any bytes are available, `get-bytevector-n` returns the end-of-file object.


###### [!Function] `get-bytevector-n!`  _binary-input-port_ _bytevector_ _start_ _count_ _:optional_ _reckless_

[R6RS+] _Count_ must be an exact, non-negative integer object,
representing the number of bytes to be read. _bytevector_ must be a
bytevector with at least _start_ + _count_ elements.

The `get-bytevector-n!` procedure reads from _binary-input-port_,
blocking as necessary, until _count_ bytes are available from
_binary-input-port_ or until an end of file is reached. If _count_ bytes
are available before an end of file, they are written into _bytevector_starting at index _start_, and the result is _count_. If fewer bytes are
available before the next end of file, the available bytes are written into
_bytevector_ starting at index _start_, and the result is a number object
representing the number of bytes actually read. In either case, the input port is
updated to point just past the bytes read. If an end of file is reached before
any bytes are available, `get-bytevector-n!` returns the end-of-file object.


###### [!Function] `get-bytevector-some`  _binary-input-port_ _:optional_ _reckless_

[R6RS+] Reads from _binary-input-port_, blocking as necessary, until
bytes are available from _binary-input-port_ or until an end of file is
reached. If bytes become available, `get-bytevector-some` returns a freshly
allocated bytevector containing the initial available bytes (at least one and
maximum 512 bytes), and it updates _binary-input-port_ to point just past
these bytes. If no input bytes are seen before an end of file is reached, the
end-of-file object is returned.


###### [!Function] `get-bytevector-all`  _binary-input-port_ _:optional_ _reckless_

[R6RS+] Attempts to read all bytes until the next end of file, blocking
as necessary. If one or more bytes are read, `get-bytevector-all` returns a
bytevector containing all bytes up to the next end of file. Otherwise,
`get-bytevector-all` returns the end-of-file object.

These procedures can take optional argument _reckless_. If this is given,
these procedures can read bytes from textual port. This optional argument is for
socket programming. Users needs to make sure that the given port can be read as
textual port after reading port recklessly.


#### [§4] Textual input

###### [!Function] `get-char`  _textual-input-port_

[R6RS] Reads from _textual-input-port_, blocking as necessary, until a
complete character is available from _textual-input-port_, or until an end
of file is reached.

If a complete character is available before the next end of file, `get-char`returns that character and updates the input port to point past the character.
If an end of file is reached before any character is read, `get-char`returns the end-of-file object. 


###### [!Function] `lookahead-char`  _textual-input-port_

[R6RS] The `lookahead-char` procedure is like `get-char`, but it
does not update _textual-input-port_ to point past the character.


###### [!Function] `get-string-n`  _textual-input-port_ _count_

[R6RS] _Count_ must be an exact, non-negative integer object,
representing the number of characters to be read.

The `get-string-n` procedure reads from _textual-input-port_, blocking
as necessary, until _count_ characters are available, or until an end of
file is reached.

If _count_ characters are available before end of file, `get-string-n`returns a string consisting of those _count_ characters. If fewer characters
are available before an end of file, but one or more characters can be read,
`get-string-n` returns a string containing those characters. In either case,
the input port is updated to point just past the characters read. If no characters
can be read before an end of file, the end-of-file object is returned. 


###### [!Function] `get-string-n!`  _textual-input-port_ _string_ _start_ _count_

[R6RS] _Start_ and _count_ must be exact, non-negative integer
objects, with _count_ representing the number of characters to be read.
_String_ must be a string with at least _start_ + _count_ characters.

The `get-string-n!` procedure reads from _textual-input-port_ in the
same manner as `get-string-n`. If _count_ characters are available before
an end of file, they are written into _string_ starting at index _start_,
and _count_ is returned. If fewer characters are available before an end of
file, but one or more can be read, those characters are written into string
starting at index _start_ and the number of characters actually read is
returned as an exact integer object. If no characters can be read before an end
of file, the end-of-file object is returned.


###### [!Function] `get-string-all`  _textual-input-port_

[R6RS] Reads from _textual-input-port_ until an end of file, decoding
characters in the same manner as `get-string-n` and `get-string-n!`.

If characters are available before the end of file, a string containing all the
characters decoded from that data are returned. If no character precedes the end
of file, the end-of-file object is returned. 


###### [!Function] `get-line`  _textual-input-port_

[R6RS] Reads from _textual-input-port_ up to and including the linefeed
character or end of file, decoding characters in the same manner as
`get-string-n` and `get-string-n!`.

If a linefeed character is read, a string containing all of the text up to (but
not including) the linefeed character is returned, and the port is updated to
point just past the linefeed character. If an end of file is encountered before
any linefeed character is read, but some characters have been read and decoded
as characters, a string containing those characters is returned. If an end of
file is encountered before any characters are read, the end-of-file object is
returned.


###### [!Function] `get-datum`  _textual-input-port_

[R6RS] Reads an external representation from _textual-input-port_ and
returns the datum it represents. The `get-datum` procedure returns the next
datum that can be parsed from the given _textual-input-port_, updating
_textual-input-port_ to point exactly past the end of the external
representation of the object.

If a character inconsistent with an external representation is encountered in
the input, an exception with condition types `&lexical` and `&i/o-read`is raised. Also, if the end of file is encountered after the beginning of an
external representation, but the external representation is incomplete and
therefore cannot be parsed, an exception with condition types `&lexical`and `&i/o-read` is raised. 


#### [§4] Output ports

An output port is a sink to which bytes or characters are written. The written
data may control external devices or may produce files and other objects that
may subsequently be opened for input.

###### [!Function] `output-port?`  _obj_

[R6RS] Returns #t if _obj_ is an output port (or a combined input and
output port), #f otherwise.

###### [!Function] `flush-output-port`  _output-port_

[R6RS+][R7RS] Flushes any buffered output from the buffer of
_output-port_ to the underlying file, device, or object. The
`flush-output-port` procedure returns unspecified values.

If the optional argument is omitted then `(current-output-port)` will be
used.


###### [!Function] `output-port-buffer-mode`  _output-port_

[R6RS] Returns the symbol that represents the buffer mode of
_output-port_.

###### [!Function] `open-file-output-port`  _filename_ _:optional_ _file-options_ _buffer-mode_ _maybe-transcoder_

[R6RS] _Maybe-transcoder_ must be either a transcoder or #f.

The `open-file-output-port` procedure returns an output port for the named
file.

The _file-options_ argument, which may determine various aspects of the
returned port, defaults to the value of `(file-options)`.

The _buffer-mode_ argument, if supplied, must be one of the symbols that
name a buffer mode. The _buffer-mode_ argument defaults to `block`.

If _maybe-transcoder_ is a transcoder, it becomes the transcoder associated
with the port.

If _maybe-transcoder_ is #f or absent, the port will be a binary port,
otherwise the port will be textual port.


###### [!Function] `open-bytevector-output-port`  _:optional_ _maybe-transcoder_

[R6RS] _Maybe-transcoder_ must be either a transcoder or #f.

The `open-bytevector-output-port` procedure returns two values: an output
port and an extraction procedure. The output port accumulates the bytes written
to it for later extraction by the procedure.

If _maybe-transcoder_ is a transcoder, it becomes the transcoder associated
with the port. If _maybe-transcoder_ is #f or absent, the port will be a
binary port, otherwise the port will be textual port.

The extraction procedure takes no arguments. When called, it returns a bytevector
consisting of all the port's accumulated bytes (regardless of the port's current
position), removes the accumulated bytes from the port, and resets the port's
position.


###### [!Function] `call-with-bytevector-output-port`  _proc_ _:optional_ _maybe-transcoder_

[R6RS] _Proc_ must accept one argument. _Maybe-transcoder_ must be
either a transcoder or #f.

The `call-with-bytevector-output-port` procedure creates an output port that
accumulates the bytes written to it and calls _proc_ with that output port as
an argument. Whenever _proc_ returns, a bytevector consisting of all of the
port's accumulated bytes (regardless of the port's current position) is returned
and the port is closed.

The transcoder associated with the output port is determined as for a call to
`open-bytevector-output-port`.


###### [!Function] `open-string-output-port` 

[R6RS] Returns two values: a textual output port and an extraction procedure.
The output port accumulates the characters written to it for later extraction by
the procedure.

The extraction procedure takes no arguments. When called, it returns a string
consisting of all of the port's accumulated characters (regardless of the current
position), removes the accumulated characters from the port, and resets the port's
position.


###### [!Function] `call-with-string-output-port`  _proc_

[R6RS] _Proc_ must accept one argument.

The `call-with-string-output-port` procedure creates a textual output port
that accumulates the characters written to it and calls _proc_ with that
output port as an argument. Whenever proc returns, a string consisting of all of
the port's accumulated characters (regardless of the port's current position) is
returned and the port is closed.


###### [!Function] `standard-output-port` 
###### [!Function] `standard-error-port` 

[R6RS] Returns a fresh binary output port connected to the standard output
or standard error respectively.


###### [!Function] `current-output-port`  _:optional_ _port_
###### [!Function] `current-error-port`  _:optional_ _port_

[R6RS+] If _port_ is given, these procedures set the _port_ as a
default port for output and error. These return default ports for regular output
and error output.


###### [!Function] `make-custom-binary-output-port`  _id_ _write!_ _get-position_ _set-position!_ _close_

[R6RS] Returns a newly created binary output port whose byte sink is an
arbitrary algorithm represented by the _write!_ procedure. _Id_ must be
a string naming the new port, provided for informational purposes only. _Write!_must be a procedure and should behave as specified below; it will be called by
operations that perform binary output.

Each of the remaining arguments may be #f; if any of those arguments is not #f,
it must be a procedure and should behave as specified in the description of
`make-custom-binary-input-port`.


- (_write!_ _bytevector_ _start_ _count_)
  _Start_ and _count_ will be non-negative exact integer objects, and
  _bytevector_ will be a bytevector whose length is at least _start_ +
  _count_. The _write!_ procedure should write up to _count_ bytes
  from _bytevector_ starting at index _start_ to the byte sink. If 
  _count_ is 0, the _write!_ procedure should have the effect of passing
  an end-of-file object to the byte sink. In any case, the _write!_ procedure
  should return the number of bytes that it wrote, as an exact integer object.



###### [!Function] `make-custom-string-output-port`  _id_ _write!_ _get-position_ _set-position!_ _close_

[R6RS] Returns a newly created textual output port whose byte sink is an
arbitrary algorithm represented by the _write!_ procedure. _Id_ must be
a string naming the new port, provided for informational purposes only. _Write!_must be a procedure and should behave as specified below; it will be called by
operations that perform textual output.

Each of the remaining arguments may be #f; if any of those arguments is not #f,
it must be a procedure and should behave as specified in the description of
`make-custom-textual-input-port`.


- (_write!_ _string_ _start_ _count_)
  _Start_ and _count_ will be non-negative exact integer objects, and
  _string_ will be a string whose length is at least _start_ + _count_.
  The _write!_ procedure should write up to _count_ characters from
  _string_ starting at index _start_ to the character sink. If _count_is 0, the _write!_ procedure should have the effect of passing an end-of-file
  object to the character sink. In any case, the _write!_ procedure should
  return the number of characters that it wrote, as an exact integer object.



#### [§4] Binary output

###### [!Function] `put-u8`  _binary-output-port_ _octet_

[R6RS] Writes _octet_ to the output port and returns unspecified values.

###### [!Function] `put-bytevector`  _binary-output-port_ _bytevector_ _:optional_ _start_ _count_

[R6RS] _Start_ and _count_ must be non-negative exact integer
objects that default to 0 and `(bytevector-length _bytevector_)` - _start_,
respectively. _Bytevector_ must have a length of at least _start_ + _count_.
The `put-bytevector` procedure writes the _count_ bytes of the bytevector
_bytevector_ starting at index _start_ to the output port. The
`put-bytevector` procedure returns unspecified values.


#### [§4] Textual output

###### [!Function] `put-char`  _textual-output-port_ _char_

[R6RS] Writes _char_ to the port and returns unspecified values.

###### [!Function] `put-string`  _textual-output-port_ _string_ _:optional_ _start_ _count_

[R6RS] _Start_ and _count_ must be non-negative exact integer
objects. _String_ must have a length of at least _start_ + _count_.
_Start_ defaults to 0. _Count_ defaults to
`(string-length _string_)` - _start_. The `put-string` procedure
writes the count characters of _string_ starting at index _start_ to the
port. The `put-string` procedure returns unspecified values.


###### [!Function] `put-datum`  _textual-output-port_ _datum_

[R6RS] _Datum_ should be a datum value. The `put-datum` procedure
writes an external representation of _datum_ to _textual-output-port_.


#### [§4] Input/output ports

###### [!Function] `open-file-input/output-port`  _filename_ _:optional_ _file-options_ _buffer-mode_ _transcoder_

[R6RS] Returns a single port that is both an input port and an output port
for the named file. The optional arguments default as described in the
specification of `open-file-output-port`. If the input/output port supports
`port-position` and/or `set-port-position!`, the same port position
is used for both input and output.


###### [!Function] `make-custom-binary-input/output-port`  _id_ _read!_ _write!_ _get-position_ _set-position!_ _close_ _:optional_ _(ready_ _#f)_

[R6RS+] Returns a newly created binary input/output port whose byte source
and sink are arbitrary algorithms represented by the _read!_ and
_write!_ procedures. _Id_ must be a string naming the new port,
provided for informational purposes only. _Read!_ and _write!_ must
be procedures, and should behave as specified for the
`make-custom-binary-input-port` and 
`make-custom-binary-output-port` procedures.

Each of the remaining arguments may be #f; if any of those arguments is not #f,
it must be a procedure and should behave as specified in the description of
`make-custom-binary-input-port`.


###### [!Function] `make-custom-textual-input/output-port`  _id_ _read!_ _write!_ _get-position_ _set-position!_ _close_ _:optional_ _(ready_ _#f)_

[R6RS+] Returns a newly created textual input/output port whose byte
source and sink are arbitrary algorithms represented by the _read!_and _write!_ procedures. _Id_ must be a string naming the new port,
provided for informational purposes only. _Read!_ and _write!_ must
be procedures, and should behave as specified for the
`make-custom-textual-input-port` and
`make-custom-textual-output-port` procedures.

Each of the remaining arguments may be #f; if any of those arguments is not #f,
it must be a procedure and should behave as specified in the description of
`make-custom-textual-input-port`.


