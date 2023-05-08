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

---

The bindings listed below are only available on the remote debugger's
REPL.

###### [!Function] `all-threads`

Get a list of threads created on Scheme world.

###### [!Function] `sleeping-threads` :optional (_timeout_ 0.01)

Get a list of sleeping threads created on Scheme world.

Ssleeping thread is a thread which can't suspend within the given _timeout_.

###### [!Function] `thread?` _obj_

[SRFI-18] Returns if the given _obj_ is a thread.

###### [!Function] `thread-name` (_thread_ `thread?`)

[SRFI-18] Returns the name of the given _thread_.

###### [!Function] `thread-specific` (_thread_ `thread?`)

[SRFI-18] Returns the specific value of the given _thread_.

###### [!Function] `thread->pretty-backtrace-string` (_thread_ `thread?`)

Returns a human readable string representation of the given *thread*'s 
backtrace.

###### [!Function] `thread-current-procedure` (_thread_ `thread?`)

Returns the current procedure of the _thread_.

###### [!Function] `thread-backtrace` (_thread_ `thread?`)

Returns the backtrace of the given _thread_.

Currently, a backtrace is a list, however it may change in the future,
to access the value of backtrace, use the  procedures listed below.

NOTE: A backtrace starts with `1`, not zero base.

###### [!Function] `thread-backtrace-type` _backtrace_ _n_

Returns the type of *n*th _backtrace_. The value is

`*cproc`
: For C procedure.

`*proc`
: For Scheme procedure.


###### [!Function] `thread-backtrace-procedure` _backtrace_  _n_

Returns the procedure of the *n*th _backtrace_.

###### [!Function] `thread-backtrace-source` _backtrace_  _n_

Returns the source, list of file and line number` of the *n*th _backtrace_,
if available.

###### [!Function] `thread-backtrace-arguments` _backtrace_  _n_

Returns alist of the arguments of the *n*th *backtrace*'s procedure.

For local variable, the key is `local`. For free variable, the key is `free`.

NOTE: `local` variable may contain more than the argument of the current
procedure. This is bacause it also retrieves the available local variable
of the current call frame.


###### [!Function] `thread-backtrace-local-variables` _backtrace_  _n_

Returns `local` part of the *n*th *backtrace* arguments.

###### [!Function] `thread-backtrace-free-variables` _backtrace_  _n_

Returns `free` part of the *n*th *backtrace* arguments.

###### [!Function] `thread-backtrace->pretty-string` _backtrace_

Returns a human readable string representation of the given _backtrace_.

###### [!Function] `slot-ref` _obj_ _slot_

Returns the _slot_ value of given _obj_.

###### [!Function] `inspect-object` _obj_

Returns the available slots of the given _obj_.
	    
###### [!Function] `print` _arg_ _..._
###### [!Function] `print/ss` _arg_ _..._

Prints the given *arg*s and newline at the end.
The first form uses `display` to print, the latter form uses `write/ss`.

###### [!Function] `string-prefix?` _s1_ _s2_

[SRFI-13] Returns `#t` if the given _s1_ is the prefix of _s2_.

###### [!Function] `string-suffix?` _s1_ _s2_

[SRFI-13] Returns `#t` if the given _s1_ is the suffix of _s2_.
