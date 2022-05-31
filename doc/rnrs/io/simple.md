### [§3] Simple I/O {#rnrs.io.simple.6}

This section describes the `(rnrs io simple (6))`library, which provides
a somewhat more convenient interface for performing textual I/O on ports.

###### [!Library] `(rnrs io simple (6))` 

This library also exports the same procedures as
`(rnrs io posts (6))` library. I do not write the documentation of it, if
you want to import only this library, make sure which procedures are exported.
You can see it on R6RS.


###### [!Function] `call-with-input-file`  _filename_ _proc_ _._ _opt_
###### [!Function] `call-with-output-file`  _filename_ _proc_ _._ _opt_

[R6RS+] _Proc_ should accept one argument.

These procedures open the file named by _filename_ for input or for output, 
with no specified file options, and call _proc_ with the obtained port as
an argument. If _proc_ returns, the port is closed automatically and the
values returned by _proc_ are returned. If _proc_ does not return, the
port is not closed automatically, unless it is possible to prove that the port
will never again be used for an I/O operation.

NOTE: _opt_ will be passed to `open-input-file` or
`open-output-file`.


###### [!Function] `with-input-from-file`  _filename_ _thunk_ _._ _opt_
###### [!Function] `with-output-to-file`  _filename_ _thunk_ _._ _opt_

[R6RS+] _Thunk_ must be a procedure and must accept zero arguments.

The file is opened for input or output using empty file options, and _thunk_is called with no arguments. These procedure replace current input/output port
during _thunk_ is being called. When _thunk_ returns, the port is closed
automatically. The values returned by thunk are returned.

NOTE: _opt_ will be passed to `open-input-file` or
`open-output-file`.


###### [!Function] `open-input-file`  _filename_ _:key_ _(transcoder_ _(native-transcoder))_
###### [!Function] `open-output-file`  _filename_ _:key_ _(transcoder_ _(native-transcoder))_

[R6RS+] Opens _filename_ for input/output, with empty file options,
and returns the obtained port.

If keyword argument _transcoder_ is given, it must be an transcoder or
#f and will be used to specify the transcoder to open input/output port. If it
is #f, then returned port will be binary port.


###### [!Function] `close-input-file`  _port_
###### [!Function] `close-output-file`  _port_

[R6RS] Closes _input-port_ or _output-port_, respectively.


###### [!Function] `read-char`  _:optional_ _textual-input-port_
###### [!Function] `peak-char`  _:optional_ _textual-input-port_

[R6RS] These work the same as `get-char` and `lookahead-char`.
If _textual-input-port_ is omitted, it defaults to the value returned by
`current-input-port`. 


###### [!Function] `read`  _:optional_ _textual-input-port_

[R6RS] Reads an external representation from _textual-input-port_and returns the datum it represents. The read procedure operates in the same
way as _get-datum_.

If _textual-input-port_ is omitted, it defaults to the value returned by
`current-input-port`. 


###### [!Function] `write-char`  _char_ _:optional_ _textual-input-port_

[R6RS] Writes an encoding of the character char to the
_textual-output-port_, and returns unspecified values.

If _textual-output-port_ is omitted, it defaults to the value returned by
`current-output-port`.


###### [!Function] `newline`  _:optional_ _textual-input-port_

[R6RS] This is equivalent to using `write-char` to write
`#\linefeed` to _textual-output-port_.

If _textual-output-port_ is omitted, it defaults to the value returned by
`current-output-port`.


###### [!Function] `display`  _obj_ _:optional_ _textual-input-port_

[R6RS] Writes a representation of _obj_ to the given 
_textual-output-port_. Strings that appear in the written representation
are not enclosed in double quotes, and no characters are escaped within those
strings. Character objects appear in the representation as if written by
_write-char_ instead of by _write_. The display procedure returns
unspecified values.

If _textual-output-port_ is omitted, it defaults to the value returned by
`current-output-port`.


###### [!Function] `write`  _obj_ _:optional_ _textual-input-port_

[R6RS] Writes the external representation of _obj_ to
_textual-output-port_. The `write` procedure operates in the same way
as `put-datum`.

If _textual-output-port_ is omitted, it defaults to the value returned by
`current-output-port`.


