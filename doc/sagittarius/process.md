[§2] (sagittarius process) - Process  library {#lib.sagittarius.process}
-------------

In real world, there are a lot of useful programs and you want to re-use it
rather than re-write it in Scheme. For that purpose, this library can be useful.

The concept of this library is similar with Java's Process class. Users can
create process object and run/call whenever they want. However most of the time
process can be invoked immediately, so there are high level APIs for that
purpose.

This section describe from top to down.

###### [!Library] `(sagittarius process)` 

Exports high level APIs and low level APIs for operating process.

### [§3] High level APIs

###### [!Function] `run`  _name_ _arg1_ ...
###### [!Function] `call`  _name_ _arg1_ ...

_name_ must be string and indicate the process name be called.

_arg1_ and the rest must be string which will be passed to process.

The `run` procedure invokes _name_ process and waits until it ends.
Then returns process' exit status.

The `call` procedure invokes _name_ process and continue the Scheme
process, so it does not wait the called process. Then returns process object.
If you need to finish the process, make sure you call the `process-wait`procedure described below.

Both procedures' output will be redirects `current-output-port` and
`current-error-port`. If you need to redirect it other place use
`create-process` described below.


### [§3] Middle level APIs

###### [!Function] `create-process`  _name_ _args_ :key (_stdout_ `#f`) (_stderr_ `#f`) (_call?_ `#t`) reader (_transcoder_ `#f`) (_token_ `#f`)

_name_ must be string and indicate a process name.

_args_ must be list of string will be passed to the process.

The `create-process` procedure creates and invokes a process indicated
_name_. Keyword arguments decide how to invoke and where to redirect the
outputs.

If _stdout_ is #f or non output-port and _call?_ is #f then
`create-process` raises `&assertion`.

_stdout_ keyword argument indicates the port where to redirect the standard
output of the process. This can be either binary output port or textual output
port.

_stderr_ keyword argument indicates the port where to redirect the standard
error of the process. This can be either binary output port or textual output
port. If this argument is #f, then _stdout_ will be used.

_call?_ keyword argument decides the default behaviour. If this is #t and
_reader_ is not a procedure, then the `create-process` uses
`async-process-read`. If this is #f and _reader_ is not a procedure,
then it uses `sync-process-read`. If _reader_ is provided, then it
uses given _reader_.

_reader_ keyword argument must be procedure which takes 4 arguments,
process object, redirection of standard output and error, and transcoder
respectively. This procedure decides how to handle the output. 

Note: on Windows, both standard output end error has limitation. So if you
replace the default behaviour, make sure you must read the output from the
process, otherwise it can cause deat lock.

_transcoder_ keyword argument must be transcoder or #f. This can be used in
the procedure which specified _reader_ keyword argument.

_token_ keyword argument must be auth token or #f. If this keyword is specified
then the creating process will be executed unnder the user of the _token_.

The procedure `create-process` creates a process and call it. The
returning value is depending on the above keyword parameters. If _reader_and _stdout_ is provided, then the result value is the value returned from
_reader_ procedure. Otherwise the created process object.

**CAVEAT**  
_token_ will use either `setuid` or `CreateProcessAsUser`, both of the
C function require specific user permission. If the permission is not
granted, then the process creation fails.


###### [!Function] `async-process-read`  _process_ _stdout_ _stderr_ _transcoder_

Process output reader. This reader creates 2 threads to read standard
ouput and standard error. The reader returns immediately after the threads are
executed.


###### [!Function] `sync-process-read`  _process_ _stdout_ _stderr_ _transcoder_

Process output reader. This reader creates 2 threads to read standard
ouput and standard error. The reader waits until the given process is
finished.

### [§3] Low level APIs

This section describe low level APIs however some of these might be used even
if you use `call` described above.

###### [!Function] `process?`  _obj_

Returns #f if _obj_ is process object, otherwise #f.

###### [!Function] `make-process`  _name_ _args_

_name_ must be string and indicates the process name which will be
invoked.

_args_ must be empty list or list of strings and will be passed to the
process.

Creates a process object.


###### [!Function] `process-input-port`  _process_

_process_ must be a process object.

Returns the binary output port which is redirected to the process'
standard input.


###### [!Function] `process-output-port`  _process_

_process_ must be a process object.

Returns the binary input port which is redirected to the process'
standard output.


###### [!Function] `process-error-port`  _process_

_process_ must be a process object.

Returns the binary input port which is redirected to the process'
standard error.


###### [!Function] `process-run`  _process_

_process_ must be a process object.

Invokes the _process_ and wait until it ends.

On POSIX envionment this procesure returns the result status of the process.


###### [!Function] `process-call`  _process_

_process_ must be a process object.

Invokes the _process_ and continue the Scheme program.


###### [!Function] `process-wait`  _process_ _:key_ _timeout_

_process_ must be a process object.

Wait the given process until it ends and returns the exit status of the given
process.

If the keyword argument _timeout_ is specified, then it must be an
integer represents second or time object represents absolute time, then
the procedure waits either the given process is finished or until the
specified _timeout_ period is passed. When the _timeout_ period
has passed and yet the process is not finished, then the procedure returns
`#f`.

NOTE: The exit status are platform dependent. On Windows, the value will be
32 bit integer. On POSIX, the value will be 8 bit unsigned integer.

NOTE: On POSIX environment, _timeout_ only works if the given
_process_ is created by `make-process` related procedures. If the
process is created by `pid->process`, then it raises an error with
`ECHILD`.


###### [!Function] `process-kill`  _process_ _:key_ _children?_

_process_ must be a process object.

Kill the given process and returns the exit status of the given
process. If the process is already terminated before the `process-kill`is called, then returning value is its status code. Otherwise -1.

If the keyword argument _children?_ is given and if it's true value, then
the procedure kills the child processes. The process of killing child processes
is not the same between Windows and POSIX. On Windows, the process seeks all
possible child processes. On POSIX, it simply calls `killpg (2)`.


###### [!Function] `process-active?`  _process_

_process_ must be a process object.

Return #t if the given _process_ is still active. Otherwise #f.

On Windows, the procedure uses `GetExitCodeProcess` which means
if the process returns `STILL_ACTIVE(259)`, then this procedure
return #t even if the process itself is already terminated.

On POSIX, the procedure uses `kill (2)` sending 0 to check the
existance of the process.


###### [!Function] `getpid` 

Returns pid of current Sagittarius process. The returning value
is an integer.


###### [!Function] `pid->process`  _pid_

_pid_ must be an integer represents process id.

Creates a process form given _pid_. 

NOTE: the created process doesn't have any ports. Those values are set to #f.


### [§3] Inter-process communication (IPC)

Users can choose how to communicate processes. One of the typical ways is
using socket. `(sagittarius process)` provides shared memory for
simple IPC.

###### [!Function] `shared-memory?`  _obj_

Returns #t if given _obj_ is a shared memory object, otherwise #f.

###### [!Function] `open-shared-memory`  _name_ _size_ _:optional_ _option_

Creates or opens shared memory named _name_. 

_name_ must be an string and must be a valid shared memory name. If there
is already a shared memory with the same name, then this procedure maps to it
and ignores the _size_ argument.

_size_ must be an integer. When a new shared memory is created, then
its size is restricted to the given _size_.

Optional argument _option_ must be an enumeration which created by
_file-options_. If `no-create` is specified, and there is
no shared memory with given _name_, then `&i/o-file-does-not-exist`is raised. If `no-truncate` is specified, then the created shared
memory is intact, otherwise it is truncted.



###### [!Function] `close-shared-memory`  _shared-memory_

Closes given _shared-memory_ and invalidate the allocated
memory.

This procedure also removes the given _shared-memory_. On some platform,
for example Linux, if shared memory is not explicitly unliked, then it stays
until the OS is restarted. To avoid it, users need to call this procedure.

NOTE: invalidation means that the bytevector returned by 
`shared-memory->bytevector` will be 0 length bytevector.


###### [!Function] `shared-memory->bytevector`  _shared-memory_

Returns actual instance of shared memory as a bytevector.

Modifying the returning bytevector also modifies the actual shared memory.

To do synchronisation of this, use semaphore provided by
`(sagittarius threads)`.


