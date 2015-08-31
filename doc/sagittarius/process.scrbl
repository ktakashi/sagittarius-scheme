@; -*- coding: utf-8; -*-
@subsection[:tag "lib.sagittarius.process"]{(sagittarius process) - Process  library}

In real world, there are a lot of useful programs and you want to re-use it
rather than re-write it in Scheme. For that purpose, this library can be useful.

The concept of this library is similar with Java's Process class. Users can
create process object and run/call whenever they want. However most of the time
process can be invoked immediately, so there are high level APIs for that
purpose.

This section describe from top to down.

@define[Library]{@name{(sagittarius process)}}
@desc{Exports high level APIs and low level APIs for operating process.}

@subsubsection{High level APIs}

@define[Function]{@name{run} @args{name arg1 @dots{}}}
@define[Function]{@name{call} @args{name arg1 @dots{}}}
@desc{@var{name} must be string and indicate the process name be called.

@var{arg1} and the rest must be string which will be passed to process.

The @code{run} procedure invokes @var{name} process and waits until it ends.
Then returns process' exit status.

The @code{call} procedure invokes @var{name} process and continue the Scheme
process, so it does not wait the called process. Then returns process object.
If you need to finish the process, make sure you call the @code{process-wait}
procedure described below.

Both procedures' output will be redirects @code{current-output-port} and
@code{current-error-port}. If you need to redirect it other place use
@code{create-process} described below.
}

@subsubsection{Middle level APIs}

@define[Function]{@name{create-process}
 @args{name args
  :key (stdout #f) (stderr #f) (call? #t) reader (transcoder #f)}}
@desc{@var{name} must be string and indicate a process name.

@var{args} must be list of string will be passed to the process.

The @code{create-process} procedure creates and invokes a process indicated
@var{name}. Keyword arguments decide how to invoke and where to redirect the
outputs.

If @var{stdout} is #f or non output-port and @var{call?} is #f then
@code{create-process} raises @code{&assertion}.

@var{stdout} keyword argument indicates the port where to redirect the standard
output of the process. This can be either binary output port or textual output
port.

@var{stderr} keyword argument indicates the port where to redirect the standard
error of the process. This can be either binary output port or textual output
port. If this argument is #f, then @var{stdout} will be used.

@var{call?} keyword argument decides the behaviour. If this is #t, then the
process will be waited until its end. Otherwise just invokes it and returns
process object.

@var{reader} keyword argument must be procedure which takes 4 arguments,
process object, redirection of standard output and error, and transcoder
respectively. This procedure decides how to handle the output. the default
implementation is like this:
@codeblock{
(define (async-process-read process stdout stderr transcoder)
  (define (pipe-read in out reader converter)
    (let loop ((r (reader in)))
      (unless (eof-object? r)
	(display (converter r) out)
	(loop (reader in)))))
  (let ((out-thread (make-thread
		     (lambda ()
		       (let ((in (process-output-port process)))
			 (if transcoder
			     (pipe-read (transcoded-port in transcoder)
					stdout
					get-char
					(lambda (x) x))
			     (pipe-read in stdout get-u8 integer->char))))))
	(err-thread (make-thread
		     (lambda ()
		       (let ((in (process-error-port process)))
			 (if transcoder
			     (pipe-read (transcoded-port in transcoder)
					stderr
					get-char
					(lambda (x) x))
			     (pipe-read in stderr get-u8 integer->char)))))))
    (thread-start! out-thread)
    (thread-start! err-thread)))
}
This might not be efficient, so user can specify how to behave.

Note: on Windows, both standard output end error has limitation. So if you
replace the default behaviour, make sure you must read the output from the
process, otherwise it can cause deat lock.

@var{transcoder} keyword argument must be transcoder or #f. This can be used in
the procedure which specified @var{reader} keyword argument.
}

@subsubsection{Low level APIs}

This section describe low level APIs however some of these might be used even
if you use @code{call} described above.

@define[Function]{@name{process?} @args{obj}}
@desc{Returns #f if @var{obj} is process object, otherwise #f.}

@define[Function]{@name{make-process} @args{name args}}
@desc{@var{name} must be string and indicates the process name which will be
invoked.

@var{args} must be empty list or list of strings and will be passed to the
process.

Creates a process object.
}

@define[Function]{@name{process-input-port} @args{process}}
@desc{@var{process} must be a process object.

Returns the binary output port which is redirected to the process'
standard input.
}

@define[Function]{@name{process-output-port} @args{process}}
@desc{@var{process} must be a process object.

Returns the binary input port which is redirected to the process'
standard output.
}

@define[Function]{@name{process-error-port} @args{process}}
@desc{@var{process} must be a process object.

Returns the binary input port which is redirected to the process'
standard error.
}

@define[Function]{@name{process-run} @args{process}}
@desc{@var{process} must be a process object.

Invokes the @var{process} and wait until it ends.

On POSIX envionment this procesure returns the result status of the process.
}

@define[Function]{@name{process-call} @args{process}}
@desc{@var{process} must be a process object.

Invokes the @var{process} and continue the Scheme program.
}

@define[Function]{@name{process-wait} @args{process}}
@desc{@var{process} must be a process object.

Wait the given process until it ends and returns the exit status of the given
process.

NOTE: The exit status are platform dependent. On Windows, the value will be
32 bit integer. On POSIX, the value will be 8 bit unsigned integer.
}

@define[Function]{@name{process-kill} @args{process}}
@desc{@var{process} must be a process object.

Kill the given process and returns the exit status of the given
process. If the process is already terminated before the @code{process-kill}
is called, then returning value is its status code. Otherwise -1.
}

@define[Function]{@name{process-active?} @args{process}}
@desc{@var{process} must be a process object.

Return #t if the given @var{process} is still active. Otherwise #f.

On Windows, the procedure uses @code{GetExitCodeProcess} which means
if the process returns @code{STILL_ACTIVE(259)}, then this procedure
return #t even if the process itself is already terminated.

On POSIX, the procedure uses @code{kill (2)} sending 0 to check the
existance of the process.
}


@define[Function]{@name{getpid} @args{}}
@desc{Returns pid of current Sagittarius process. The returning value
is an integer.
}

@define[Function]{@name{pid->process} @args{pid}}
@desc{@var{pid} must be an integer represents process id.

Creates a process form given @var{pid}. 

NOTE: the created process doesn't have any ports. Those values are set to #f.
}

@; IPC
@subsubsection{Inter-process communication (IPC)}

Users can choose how to communicate processes. One of the typical ways is
using socket. @code{(sagittarius process)} provides shared memory for
simple IPC.

@define[Function]{@name{shared-memory?} @args{obj}}
@desc{Returns #t if given @var{obj} is a shared memory object, otherwise #f.}

@define[Function]{@name{open-shared-memory} @args{name size :optional option}}
@desc{Creates or opens shared memory named @var{name}. 

@var{name} must be an string and must be a valid shared memory name. If there
is already a shared memory with the same name, then this procedure maps to it
and ignores the @var{size} argument.

@var{size} must be an integer. When a new shared memory is created, then
its size is restricted to the given @var{size}.

Optional argument @var{option} must be an enumeration which created by
@var{file-options}. If @code{no-create} is specified, and there is
no shared memory with given @var{name}, then @code{&i/o-file-does-not-exist}
is raised. If @code{no-truncate} is specified, then the created shared
memory is intact, otherwise it is truncted.

}

@define[Function]{@name{close-shared-memory} @args{shared-memory}}
@desc{Closes given @var{shared-memory} and invalidate the allocated
memory.

This procedure also removes the given @var{shared-memory}. On some platform,
for example Linux, if shared memory is not explicitly unliked, then it stays
until the OS is restarted. To avoid it, users need to call this procedure.

NOTE: invalidation means that the bytevector returned by 
@code{shared-memory->bytevector} will be 0 length bytevector.
}

@define[Function]{@name{shared-memory->bytevector} @args{shared-memory}}
@desc{Returns actual instance of shared memory as a bytevector.

Modifying the returning bytevector also modifies the actual shared memory.

To do synchronisation of this, use semaphore provided by
@code{(sagittarius threads)}.
}