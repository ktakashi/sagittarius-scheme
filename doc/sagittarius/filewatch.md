[§2] (sagittarius filewatch) - Monitoring filesystem {#lib.sagittarius.filewatch}
-------------

###### [!Library] `(sagittarius filewatch)` 
 

Monitoring filesystem cannot be done efficiently without support of
underlying operating system. This library provides unified interface of the
mechanism.


The following simple `tail (1)` like script shows how it works:

* @[[Tail](../../example/filewatch/tail.scm)]

###### [!Function] `make-filesystem-watcher`  _:key_ _error-handler_

Creates and returns filesystem watcher object.

The keyword argument _error-handler_ is specified, which must be a
procedure accepts one argument, then it is called with a condition 
when monitoring handler raised an error.

###### [!Function] `release-filesystem-watcher!`  _watcher_

Releasing the _watcher_.

Released filesystem watcher can not be reused.


###### [!Function] `filesystem-watcher?`  _o_

Returns #t if the given _o_ is a filesystem watcher object, otherwise
#f.

###### [!Function] `filesystem-watcher-add-path!`  _watcher_ _path_ _flags_ _monitoring-handler_

Adds monitoring targets to the _watcher_.

The _path_ must be a string and indicating existing path.

The _flags_ must be one of the following symbols or list of the symbols:

`access`
: Checks if the _path_ is accessed.

`modify`
: Checks if the _path_ is modified.

`delete`
: Checks if the _path_ is deleted.

`move`
: Checks if the _path_ is moved.

`attribute`
: Checks if the _path_'s attribute is changed.

NOTE: The flags might not be supported depending on the platform. See 
[implementation limitation](#filewatch.limit) section for more details.

The _monitoring-handler_ must be a procedure accepts 2 arguments. The
procedure is called if the _path_ gets an event specified _flags_.
When the _monitoring-handler_ is invoked, then the path and a symbol 
of the invoking event are passed respectively. The possible event symbols 
are the followings:

`accessed`
: Checks if the _path_ is accessed.

`modified`
: Checks if the _path_ is modified.

`deleted`
: Checks if the _path_ is deleted.

`moved`
: Checks if the _path_ is moved.

`attribute`
: Checks if the _path_'s attribute is changed.

The procedure _filesystem-watcher-add-path!_ returns the _watcher_.

If the _watcher_ started monitoring, then the procedure raises
`&assertion`.


###### [!Function] `filesystem-watcher-remove-path!`  _watcher_ _path_

Removes given _path_ from the _watcher_. And returns 
_watcher_,

If the _watcher_ started monitoring, then the procedure raises
`&assertion`.


###### [!Function] `filesystem-watcher-start-monitoring!`  _watcher_ _:key_ _(background_ _#t)_

Starts monitoring filesystem on given _watcher_.

If the keyword argument _background_ is true value, then the procedure
creates a thread and let the thread monitor the filesystem. (So the procedure
returns after the thread invocation.) Otherwise, the procedure blocks and
wait until other thread calls `filesystem-watcher-stop-monitoring!`.


###### [!Function] `filesystem-watcher-stop-monitoring!`  _watcher_

Stops monitoring of given _watcher_.

If the _watcher_ is started on background, then the monitoring thread
may not stop immediately.


### [§3] Implementation limitation {#filewatch.limit}

Even the library provides unified APIs however users still should know the
limitations per operating system to avoid unexpected behaviours. The following
sections describes the known limitations.

#### [§4] Linux

On Linux, the library is constructed on top of `inotify (7)` and
`poll (2)`. If users add too many paths, then it may reach the
maximum number of watch descriptor.

The `IN_MOVED_FROM` and `IN_MOVED_TO` flags are passed as
`moved`. So it is users responsibility to detect which file is
_moved from_ and which file is _moved to_.

#### [§4] BSD Unix {#bsd.limitation}

On BSD Unix, the library is constructed on top of `kqueue (2)`. This
implementation contains 3 major issues. Possibility of number of file
descriptor explosion, not `access` flag support, and no support of
directory monitoring.

The `kqueue` requires file descriptor per monitoring path. Thus if
the number of paths is large, then it reaches the maxinum number of file
descriptors. (NB: `kern.maxfiles` on FreeBSD).

`kqueue` doesn't support path access monitoring (e.g. `IN_ACCESS` 
on `inotify`). So it is impossible to monitor file access.

Current implementation of `(sagittarius filewatch)` using `kqueue`doesn't allow users to monitor directory. This is because by default,
`kqueue` doesn't provide facility to detect which file is added.
To do it, we need manual management. To keep our code as simple as possible,
we decided not to do it for now. This decision may be changed if there's
enough demands.

#### [§4] OS X

On OS X, the library is constructed on top of `kqueue`, thus the
same limitation as [BSD Unix](#bsd.limitation) is applied.



#### [§4] Windows

Staring Windows Vista, Microsoft decided not to change timestamp just accessing
the file or directory by default. So `access` flag may or may not work on
Windows depending on the configuration of the platform.

Due to the lack of deletion detect, `delete` and `move` work the
same. Thus the monitoring handler may get both `deleted` and `moved`even though it's only specified `delete` or `move`.
