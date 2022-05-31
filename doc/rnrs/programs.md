[§2] Command-line access and exit values {#rnrs.programs.6}
-------------

###### [!Library] `(rnrs programs (6))` 

This library provides command-line arguments access and exit procedures.

###### [!Function] `command-line` 

[R6RS] Returns a nonempty list of strings. The first element is the running
script file name or '() on REPL. The remaining elements are command-line arguments
according to the operating system's conventions.


###### [!Function] `exit`  _:optional_ _obj_

[R6RS] Exits the running program and communicates an exit value to the
operating system. If no argument is supplied, the exit procedure should
communicate to the operating system that the program exited normally. If an
argument is supplied and if it is fixnum, the exit procedure translates it into
an appropriate exit value for the operating system. Otherwise the exit is
assumed to be abnormal.


