[§2] (sagittarius pty) - Pseudo Terminal {#lib.sagittarius.pty}
-------------

###### [!Library] `(sagittarius pty)`

This library provides pseudo terminal capabilities.

###### [!Function] `pty?` _obj_

Returns `#t` if the _obj_ is pty object, otherwise `#f`.

###### [!Function] `make-pty`

Creates PTY object.

###### [!Function] `pty-closed?` (_pty_ `pty?`)

Returns `#t` if the _pty_ is closed, otherwise `#f`.

###### [!Function] `pty-spawn!` (_pty_ `pty?`) (_name_ `string?`) (_args_ `list?`) :key (_directory_ `#f`) (_user_ `#f`)

Spawn a process of _name_ command with _args_ in the _pty_.

_directory_ keyword argument specifies the process's working directory.  
_user_ keyword argument specifies the owner process.

###### [!Function] `pty-close!` (_pty_ `pty?`)

Closes the given _pty_.

###### [!Function] `pty-resize!` (_pty_ `pty?`) (_cols_ `fixnum?`) (_rows_ `fixnum?`)

Resize the _pty_ with _cols_ and _rows_ value.

###### [!Function] `pty-input-port` (_pty_ `pty?`)

Returns the binary input port of the _pty_.

Putting a value into this port means sending standard input value to the
underlying process of the _pty_.

###### [!Function] `pty-output-port` (_pty_ `pty?`)

Returns the binary output port of the _pty_.

Reading value from this port means, recieving standard out value from the
underlying process of the _pty_.

###### [!Function] `pty-pid` (_pty_ `pty?`)

Returns PID of the _pty_.

###### [!Function] `pty-tcsetattr!` (_pty_ `pty?`) (_termios_ `termios?`)

Sets the _termios_ into the _pty_.

###### [!Function] `pty-termios` (_pty_ `pty?`)

Retrieves the termios from the _pty_.
