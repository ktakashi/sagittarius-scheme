[§2] (sagittarius debug) - Debugging support {#lib.sagittarius.debug}
-------------

###### [!Library] `(sagittarius debug)` 

This library provides debugging support reader macro.

###### [!Reader Macro] `#?=`  _expr_

This reader macro reads the next expression as followings;

``(debug-print _expr_)``

`debug-print` is an internal macro of this library which prints the
read expression and its result.

Following example shows how to enable this;

``````````scheme
#!read-macro=sagittarius/debug
#!debug
(let ((a (+ 1 2)))
  #?=(expt a 2))

#|
#?=(expt a 2)
#?-    9
|#
``````````

`#!debug` enables the debug print.


###### [!Function] `macroexpand`  _expr_

Expands given _expr_. The returning value may or may not be used
as proper Scheme expression.


###### [!Function] `macroexpand-1`  _expr_
###### [!Function] `macroexpand-n`  _expr_ _n_

Expands given _expr_ _n_ times. The first form's _n_ is 1.

This procedure expands only globally defined macro and the result of expansion
is other macro such as next rule of `syntax-rules`. It doesn't consider
locally bound macros.

The returning value may or may not be used as proper Scheme expression.


Above 2 procedures are no more than debug aid. Depending on the result of
expansion is not guaranteed to work.

#### Remote debugger (experimental feature)

*CAVEAT* This functionality and interface is subjected to be changed in the
future.

When you want to debug a running script, you can use a remote debugger
provided this library. Below example shows how to use it.

```scheme
(import (rnrs)
        (sagittarius debug))

;; Using random port
(define remote-debugger (make-remote-debugger "0"))
(print "Debugger port: " (remote-debugger-port remote-debugger))

;; Do heavy process or multi threading program which hangs :)
```

To connect the remote debugger, you can use simpley use
`(sagittarius remote-repl)` towards the debugger node and port.

```
(import (rnrs)
        (sagittarius remote-repl))
(connect-remote-repl "localhost" "${port number shown on the console}")
```

###### [!Function] `remote-debugger?` _obj_

Returns `#t` if the given _obj_ is a remote debugger object.

###### [!Function] `make-remote-debugger` (_port_ `string?`)

Creates a remote debugger. The _port_ must be an available port number,
or `"0"` for random port.

###### [!Function] `remote-debugger-terminate!` (remote-debugger `remote-debugger?`)

Terminates the given _remote-debugger_.

###### [!Function] `remote-debugger-port` (remote-debugger `remote-debugger?`)

Returns the port number of the given _remote-debugger_.

