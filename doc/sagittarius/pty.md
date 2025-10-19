[§2] (sagittarius pty) - Pseudo Terminal {#lib.sagittarius.pty}
-------------

###### [!Library] `(sagittarius pty)`

This library provides pseudo terminal capabilities.

###### [!Function] `pty?` _obj_
###### [!Function] `make-pty`
###### [!Function] `pty-closed?` (_pty_ `pty?`)
###### [!Function] `pty-spawn!` (_pty_ `pty?`) (_name_ `string?`) (_args_ `list?`) :key (_directory_ `#f`) (_user_ `#f`)
###### [!Function] `pty-close!` (_pty_ `pty?`)
###### [!Function] `pty-resize!` (_pty_ `pty?`) _cols_ _rows_
###### [!Function] `pty-input-port` (_pty_ `pty?`)
###### [!Function] `pty-output-port` (_pty_ `pty?`)
###### [!Function] `pty-pid` (_pty_ `pty?`)
###### [!Function] `pty-tcsetattr!` (_pty_ `pty?`) (_termios_ `termios?`)
###### [!Function] `pty-termios` (_pty_ `pty?`)

