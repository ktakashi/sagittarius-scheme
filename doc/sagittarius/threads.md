[§2] (sagittarius threads) - Thread {#lib.sagittarius.threads}
-------------

###### [!Library] `(sagittarius threads)` 

The library provides thread related procedures. The procedures provided
this library is based on [SRFI-18 Multithreading support](http://srfi.schemers.org/srfi-18/srfi-18.html) and Sagittarius specific procedures.


### [§3] Thread APIs

###### [!Function] `thread?`  _obj_

[SRFI-18] Returns #t if given _obj_ is a thread, otherwise #f.

###### [!Function] `current-thread` 

[SRFI-18] Returns the current thread.

###### [!Function] `make-thread`  _thunk_ _:optional_ _name_

[SRFI-18] Returns a new thread. The created thread is not executed. 
To run it, users must explicitly call the `thread-start!` procedure.

The optional argument _name_ gives the thread a name. The name can be
retrieved calling `thread-name` procedure. If the argument is not given,
then the `make-thread` procedures creates an unique name.


###### [!Function] `thread-name`  _thread_

[SRFI-18] Returns the name of _thread_.

###### [!Function] `thread-state`  _thread_

Returns the current state of _thread_.

###### [!Function] `thread-specific`  _thread_

[SRFI-18] Returns the content of the _thread_'s specific slot.

###### [!Function] `thread-specific-set!`  _thread_ _obj_

[SRFI-18] Stores _obj_ into the _thread_'s specific slot and
returns unspecified value.

###### [!Function] `thread-start!`  _thread_

[SRFI-18] Executes the given _thread_ and returns _thread_

###### [!Function] `thread-yield!` 

[SRFI-18] The current thread exits the running state if its quantum
had expired.

###### [!Function] `thread-sleep!`  _timeout_

[SRFI-18] The current thread waits until the timeout is reached.

_timeout_ must be either a time object or an exact integer. The first case,
it represents absolute time of the future. The latter case represents second
from current time.

###### [!Function] `thread-terminate!`  _thread_

[SRFI-18] Causes an abnormal termination of the _thread_. If 
the _thread_ is not already terminated, all mutexes owned by the 
_thread_ become unlocked/abandoned and a "terminated thread exception"
object is stored in the _thread_'s end-exception field. If _thread_is the current thread, `thread-terminate!` does not return. Otherwise 
`thread-terminate!` returns an unspecified value; the termination of 
the thread will occur before `thread-terminate!` returns.


###### [!Function] `thread-join!`  _thread_ _:optional_ _timeout_ _timeout-val_

[SRFI-18] The current thread waits until the _thread_ terminates 
(normal or not) or until the _timeout_ is reached if _timeout_ is 
specified. If the _timeout_ is reached, `thread-join!` returns 
_timeout-val_ if it is supplied, otherwise a "join timeout exception" 
is raised. If the _thread_ terminated normally, the content of the 
end-result field is returned, otherwise the content of the end-exception
field is raised.

###### [!Function] `thread-suspend!`  _thread_ _:optional_ _timeout_ _timeout-val_

Suspends execution of the _thread_. Users can resume the _thread_by calling `thread-resume!`.


###### [!Function] `thread-resume!`  _thread_

Resumes execution of the _thread_.

If the caller thread is not the one stopped the target thread, then
the procedure raises an error.


###### [!Function] `thread-interrupt!`  _thread_

Interrupts blocking system call.

This procedure causes `EINTR` and cancels blocking system call such 
as `select (2)`. Currently the only relevant procedure for this is
`socket-select` related procedures. See
[socket library - Low level APIs](#socket.low.level).

Currently the procedure uses `SIGALRM` on POSIX environment. This
might be changed in future, so do not depend on the signal to interrupt
the call from outside of Sagittarius process.

On Windows, the procedure uses combination of `WSAEventSelect` and
`WaitForMultipleObjects`. So there is no way to interrupt from
outside of Sagittarius process.


### [§3] Mutex APIs

###### [!Function] `mutex?`  _obj_

[SRFI-18] Returns #t if given _obj_ is a mutex, otherwise #f.

###### [!Function] `make-mutex`  _:optional_ _name_

[SRFI-18] Returns a new mutex. 

The optional argument _name_ gives the mutex a name. If it's not specified,
then the procedure makes an unique name.

###### [!Function] `mutex-name`  _mutex_

[SRFI-18] Returns the name of given _mutex_.

###### [!Function] `mutex-specific`  _mutex_

[SRFI-18] Returns the content of specific slot of given _mutex_.

###### [!Function] `mutex-specific-set!`  _mutex_ _obj_

[SRFI-18] Stores the _obj_ to given _mutex_'s specific slot.

###### [!Function] `mutex-state`  _mutex_ _obj_

[SRFI-18] Returns the state of given _mutex_.

###### [!Function] `mutex-lock!`  _mutex_ _:optional_ _timeout_ _thread_

[SRFI-18] Locks the given _mutex_. If the _mutex_ is currently
locked, the current thread waits the _mutex_ is unlocked or until the
_timeout_ is reached. If _timeout_ is reached, the procedure returns
#f.



###### [!Function] `mutex-unlock!`  _mutex_ _:optional_ _cv_ _timeout_

[SRFI-18] Unlocks the given _mutex_. If condition variable _cv_is specified, the current thread is blocked and added to the _cv_ before
unlocking _mutex_, the thread can unblock at any time but no later than
when an appropriate call to `condition-variable-signal!` or
`condition-variable-broadcast!` is performed, and no later than the 
_timeout_, if it's given.



### [§3] Condition variable APIs

###### [!Function] `condition-variable?`  _obj_

[SRFI-18] Returns #t if given _obj_ is a condition variable,
otherwise #f.

###### [!Function] `make-condition-variable`  _:optional_ _name_

[SRFI-18] Returns a new condition variable.

The optional argument _name_ gives the condition variable a name. If 
it's not specified, then the procedure makes an unique name.

###### [!Function] `condition-variable-name`  _cv_

[SRFI-18] Returns the name of given _cv_.

###### [!Function] `condition-variable-specific`  _cv_

[SRFI-18] Returns the content of specific slot of given _cv_.

###### [!Function] `condition-variable-specific-set!`  _cv_ _obj_

[SRFI-18] Stores the _obj_ to given _cv_'s specific slot.

###### [!Function] `condition-variable-signal`  _cv_

[SRFI-18] If there are thread blocked on _cv_, the scheduler selects
a thread and unblocks it.

###### [!Function] `condition-variable-broadcast!`  _cv_

[SRFI-18] Unblocks all the threads blocked on the _cv_.

### [§3] Semaphore APIs

###### [!Function] `semaphore?`  _obj_

Returns #t if given _obj_ is a semaphore, otherwise #f.

###### [!Function] `make-semaphore`  _name_ _initial_

Creates a new semaphore with initial count _initial__name_ must be either #f or string which represents semaphore name.
If the value is #f, then the returning semaphore is memory based semaphore.

_initial_ must be non negative integer.

If there is already the semaphore has the same name, then this procedure
returns that semaphore instead of creating new one.


###### [!Function] `open-semaphore`  _name_

Opens semaphore which has _name_.

If there is no such semaphore, then the procedure raises
`&i/o-file-does-not-exist`.


###### [!Function] `semaphore-name`  _semaphore_

Returns the name of given _semaphore_.

###### [!Function] `semaphore-wait!`  _semaphore_ _:optional_ _timeout_

Locks the _semaphore_. If the current count of _semaphore_is 0, then the procedure waits.

The optional argument _timeout_ is specified, which it must be #f, integer
or time object, then the procedure only waits the given _timeout_ amount.
#f means inifinite.


###### [!Function] `semaphore-post!`  _semaphore_

Unlock the _semaphore_. This procedure increase the count
of _semaphore_.

###### [!Function] `semaphore-close!`  _semaphore_

Closes the semaphore.

###### [!Function] `semaphore-destroy!`  _semaphore_

Removes the semaphore.

NOTE: the `semaphore-close!` and `semaphore-destroy!` behaves the
same on Windows.


