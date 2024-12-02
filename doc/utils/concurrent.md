[§2] (util concurrent) - Concurrency utilities {#concurrent}
-------------

###### [!Library] `(util concurrent)` 

This library provides high level concurrency APIs.

Using low level thread and mutex sometimes causes dead lock or incomprehensible
code. This library provides frameworks of common use cases. This library is
built on top of SRFI-18.

### [§3] Future

###### [!Library] `(util concurrent future)` 

A sub library of `(util concurrent)`. This library provides
future related APIs.

A future is an object that has a task which is a thunk and will be executed
in future. In this implementation, future is an interface and its execution
is depending on the sub class. The default implementation this library
provides uses a thread per future. If users don't have to manage number
of threads, then using this is sufficient.

``````````scheme
(import (rnrs) (util concurrent))

;; creates 5 futures
(define futures
  (map (lambda (i) (future (* i i))) '(1 2 3 4 5)))

;; wait and retrieve the results
(map future-get futures)
``````````
=> ``(1 4 9 16 25)``

###### [!Record] `<future>` 

The interface of all futures.

###### [!Function] `future?`  _obj_

Returns #t if given _obj_ is a future object, otherwise #f.

###### [!Macro] `future`  _expr_ _..._
###### [!Macro] `future`  _(class_ _record)expr_ _..._

Creates a future which executes _expr_. The type of the
returning future is _record_.

The first form is equivalent with the following:
`(future (class <simple-future>) _expr ..._)`

###### [!Function] `future-get`  _future_

Retrieves the result of the given _future_.

This procedure waits if the execution of the _future_ isn't finished yet.


###### [!Function] `future-cancel`  _future_

Cancels the execution of _future_.

This procedure may or may not cancel the execution depending on the
implementation of _future_. The `<simple-future>` provides by
this library won't disturb the execution. Thus calling this procedure
doesn't do anything but changing the future's state.

NOTE: once this procedure is called, then calling `future-get`with _future_ raises a `&future-terminated`.

``````````scheme
(import (rnrs) (util concurrent))

(define f (future (display "cancelled") (newline)))
(future-cancel f)
(future-get f)
``````````
=> ``&future-terminated``

The above example may or may not print `"cancelled"`.


###### [!Function] `future-done?`  _future_

Returns #t if given execution of _future_ is finished, otherwise #f.

###### [!Function] `future-cancelled?`  _future_

Returns #t if given execution of _future_ is terminated by
`future-cancel`, otherwise #f.

###### [!Function] `future-state`  _future_

Returns current state of the given _future_.

The returning state is depending on the implementation of _future_.
Only 2 states are defined in this library, `done` and `terminated`.

`done` is set when `future-get` is called.

`terminated` is set when `future-cancel` is called.


###### [!Condition Type] `&future-terminated` 

This type describes when a future is terminated but users try to
retrieve its result.

###### [!Function] `future-terminated?`  _obj_

Returns #t if given _obj_ is object of `&future-terminated`.


###### [!Function] `terminated-future`  _condition_

_condition_ must be a `&future-terminated` condition.

Retrieve terminated future from _condition_.


#### [§4] Simple future

Simple future is a future implementation which executes the task
on a thread immediately.

###### [!Record] `<simple-future>` 

Default `<future>` implementation of this library.

###### [!Function] `simple-future?`  _obj_

Returns #t if given _obj_ is a simple future, otherwise #f.

###### [!Function] `make-simple-future`  _thunk_

Creates a simple future which executes _thunk_.

### [§3] Executor

###### [!Library] `(util concurrent executor)` 

A sub library of `(util concurrent)`. This library provides
executor related APIs.

A executor is an object that executes submitted futures. The idea is taken
from `java.util.concurrent` package. The library provides 2 types
of executors, thread pool executor and fork join executor. The first
one uses thread pool, described below section, and the latter one
just creates a thread per task. The following is an example how to use
the executors:

``````````scheme
(import (rnrs) (util concurrent))

;; creates executor which uses 5 threads and push all tasks
(define executor 
  (make-thread-pool-executor 5 push-future-handler))

;; creates 10 futures
(define futures 
  (map (lambda (i) 
         (future (class <executor-future>)
           (* i i)))
       '(1 2 3 4 5 6 7 8 9 10)))

;; execute futures
(for-each (lambda (future) (execute-future! executor future)) futures)

;; wait/retrieve the results
(map future-get futures)
``````````
=> ``(1 4 9 16 25 36 49 64 81 100)``

The thread pool executor with `push-future-handler` waits until the
previous taskes are finished. 

#### [§4] Generic Executor APIs

Executor provided by this library is an extensible. So the most commonly
used procedures are generic.

###### [!Record] `<executor>` 

The interface of executor.

This record only has one field, `state`.


###### [!Function] `executor?`  _obj_

Returns #t if given _obj_ is an executor, otherwise #f.

###### [!Function] `executor-state`  _executor_

Returns `state` field of the _executor_.

The behaviour of the folowing procedures depend on its implementation.

###### [!Function] `executor-available?`  _executor_

Returns #t if the given _executor_ is available, otherwise #f.

###### [!Function] `execute-future!`  _executor_ _future_

Executes given _future_ on _executor_.

###### [!Function] `shutdown-executor!`  _executor_

Shutdowns the given _executor_. 

This procedure may or may not affect the managed futures on the _executor_.


###### [!Function] `executor-submit!`  _executor_ _thunk_

Converts _thunk_ to a future and execute it on given _executor_,
then returns the future. This procedure is defined as follows:

``````````scheme
(define (executor-submit! e thunk)
  (let ((f (make-executor-future thunk)))
    (execute-future! e f)
    f))
``````````



#### [§4] Thread pool executor

Thread pool executor uses `(util concurrent thread-pool)` as its
underlying thread managing. So once the threads are created then the
thread holds its environment until the executor is shutdown. In other
words, if a task changes the dynamic environment, then the next task
uses the changed dynamic environment. The following example describes
how dynamic environments works on this executor:

``````````scheme
(import (rnrs) (util concurrent) (srfi :39))

(define *one* (make-parameter 1))

(define executor (make-thread-pool-executor 1))

(let ((f1 (make-executor-future (lambda () (*one* 2) 1)))
      (f2 (make-executor-future (lambda () (*one*)))))
  (execute-future! executor f1)
  (future-get f1)
  (execute-future! executor f2)
  (future-get f2))
``````````
=> ``2``

NOTE: parameter objects are thread safe in general, thus if a thread is
created per future, then the parameter `*one*` is initialised with
the initial value `1` during thread creation.

###### [!Record] `<thread-pool-executor>` 

Record type of thread pool executor. This record type inherits
`<executor>`.

###### [!Function] `thread-pool-executor?`  _obj_

Returns #t if given _obj_ is a thread pool executor, otherwise #f.

###### [!Function] `make-thread-pool-executor`  _max-thread_ _:optional_ _reject-handler_

Creates a thread pool executor with thread count _max-thread_.

If optional argument _reject-handler_ is specified, then the specified
handler is used. Otherwise, `abort-rejected-handler` is used.


###### [!Function] `thread-pool-executor-pool-size`  _thread-pool-executor_

Returns number of futures currently executing on the given thread pool
executor.

This number would be greater than the thread count if `push-future-handler`is specified during the executor creation.


###### [!Function] `thread-pool-executor-max-pool-size`  _thread-pool-executor_

Returns number of thread count of the given thread pool executor.


###### [!Function] `thread-pool-executor-available?`  _thread-pool-executor_

Return #t if the number of executing future is less than the number of
thread count.

NOTE: this procedure may return #f even tasks can be pushed to the executor
if `push-future-handler` is specified.


###### [!Function] `thread-pool-executor-execute-future!`  _thread-pool-executor_ _future_

Executes the given _future_ on _thread-pool-executor_.

###### [!Function] `thread-pool-executor-shutdown!`  _thread-pool-executor_

Shutdown the given _thread-pool-executor_.

Builtin reject handlers.

Reject handler is a procedure called when thread pool executor is not
available to decide how the executor should treat the given future.

###### [!Function] `abort-rejected-handler` 

Reject the future and raises `&rejected-execution-error`.

This is the default handler.


###### [!Function] `terminate-oldest-handler` 

Terminates the oldest future.

When this handler is called, the thread which target future is running is
also terminated. Thus the dynamic environment is also reset.


###### [!Function] `wait-finishing-handler`  _wait-retry_

Creats a reject handler which waits until one of the thread
is available.

The _wait-retry_ is a number of retry count. If none of future task
is finished by this counter, then `abort-rejected-handler` is called.


###### [!Function] `push-future-handler` 

Pushes the task to the least used thread.

###### [!Condition Type] `&rejected-execution-error` 

A condition describes when a future is rejected by an executor.

###### [!Function] `rejected-execution-error?`  _obj_

Returns #t if given _obj_ is `&rejected-execution-error` object.
Otherwise #f.

###### [!Function] `rejected-future`  _condition_
###### [!Function] `rejected-executor`  _condition_

_condition_ must be a `&rejected-execution-error` object.

Retrieves the rejected future and executor, respectively.


#### [§4] Fork join executor

Fork join executor is an executor which uses fork join pool as its underlying
thread pool.

###### [!Record] `<fork-join-executor>` 

Record type of fork join executor. This record type inherits
`<executor>`.

###### [!Function] `fork-join-executor?`  _obj_

Returns #t if given _obj_ is a fork join executor, otherwise #f.

###### [!Function] `make-fork-join-executor` 
###### [!Function] `make-fork-join-executor` _parallelism_
###### [!Function] `make-fork-join-executor` _parallelism_ _parameter_

Creates a fork join executor. 

If the second form is used, then it uses the given _parallelism_ as its
parallelism.

If the third form is used, then the _parameter_ must be
`fork-join-pool-parameter` described in below section and the
procedure passes the given _parameter_ to fork join thread pool
creation.

###### [!Function] `fork-join-executor-max-thread-count` _fork-join-executor_

Returns max thread count of the given _fork-join-executor_.

###### [!Function] `fork-join-executor-thread-count` _fork-join-executor_

Returns current active thread count of the given _fork-join-executor_.

###### [!Function] `fork-join-executor-available?` _fork-join-executor_

Returns `#t`, if the underlying thread pool is not shutdown, otherwise `#f`.

###### [!Function] `fork-join-executor-execute-future!`  _fork-join-executor_ _future_

Executes the given _future_ on _fork-join-executor_.


###### [!Function] `fork-join-executor-shutdown!`  _fork-join-executor_

Shutdowns the given _fork-join-executor_. The procedure also shutdowns the
underlying fork join pool.

#### [§4] Executor future

An executor future is an future object which can be used on executor.

###### [!Record] `<executor-future>` 

Record type of `<executor-future>`. This type inherits 
`<future>`.


###### [!Function] `executor-future?`  _obj_

Returns #t if the given _obj_ is an executor future, otherwise #f.

###### [!Function] `make-executor-future`  _thunk_

Creates an executor future object.

### [§3] Thread pool

###### [!Library] `(util concurrent thread-pool)` 

A sub library of `(util concurrent)`. This library provides
thread pool APIs.

Creating a thread is not cheap on Sagittarius. If users want to reuse threads,
then this library can be used.

```scheme
(import (rnrs) (util concurrent))

;; pooling 5 thread
(define thread-pool (make-thread-pool 5))

(for-each (lambda (i) (thread-pool-push-task! thread-pool (lambda () (* i i))))
          '(1 2 3 4 5 6 7 8 9 10))

;; waits until all tasks are done
(thread-pool-wait-all! thread-pool)

;; release thread-pool
(thread-pool-release! thread-pool)
```

###### [!Record type] `<thread-pool>`

Record type of thread pool.

###### [!Function] `thread-pool?` _obj_

Returns `#t` if given _obj_ is a thread-pool, otherwise `#f`.

###### [!Function] `make-thread-pool` _thread-count_ :optional _error-handler_

Creates a thread pool with _thread-count_ of threads. 

If the optional argument _error-handler_ is given, it must be a
procedure which accept one argument, then the procedure is called
when the pushed task raised an error.


###### [!Function] `thread-pool-size` _thread-pool_

Returns number of threads on the given _thread-pool_.

###### [!Function] `thread-pool-push-task!` _thread-pool_ _thunk_

Push the given _thunk_ to least used _thread-pool_'s thread.
And returns the id of the pushed thread. This id can be used to retrive
the actual thread calling `thread-pool-thread` procedure.


###### [!Function] `thread-pool-wait-all!` _thread-pool_

Waits all the tasks pushed into the given _thread-pool_.

The return value of the tasks are discarded.

###### [!Function] `thread-pool-release!` _thread-pool_ :optional _how_

Joins all the thread on the given _thread-pool_.

If optional argument how is specified `terminate`, then the procedure
terminates the thread instead of joining.

NOTE: terminating a thread is very dangerous operation, so don't use casually.


###### [!Function] `thread-pool-thread` _thread-pool_ _id_

Retrieves the pooled thread associated with given _id_ from
given _thread-pool_.

It signals an error if the given _id_ is not a thread id.


###### [!Function] `thread-pool-thread-id`  _thread-pool_ _thread_

Retrieves the pooled thread id associated with given _thread_ from
given _thread-pool_. The procedure takes O(n) where n is number of threads
managed by the _thread-pool_. It might be better to use
`(thread-pool-current-thread-id)` procedure to retrieve thread id from
managed threads.

It signals an error if the given _thread_ is not a managed thread.

NOTE: if the thread is terminated, then the procedure also signals an error.


###### [!Function] `(thread-pool-current-thread-id)` 

Retrieves thread id of current thread. If the current thread is not
a managed thread, then #f is returned.


###### [!Function] `thread-pool-thread-terminate!`  _thread-pool_ _id_

Terminates the pooled thread associated with given _id_ and recreate
a new thread into _thread-pool_.

NTOE: this is a dangerous operation. Don't use it casually.


###### [!Function] `thread-pool-thread-task-running?`  _thread-pool_ _id_

Returns #t if the given _id_ of thread in the _thread-pool_ is 
running. Otherwise #f.


###### [!Library] `(util concurrent fork-join-pool)`

A sub library of `(util concurrent)`. This library provides
fork join pool APIs.

On Sagittarius, fork join pool means work stealing pool. The pool
takes core number of threads and it may creates ephemeral threads
until it reaches the max thread number.

CAVEAT: The implementation increases threads rather quick, which
means it reaches the max thread number very easily if the thread
pool receives large number of tasks. This behaviour may change
in the near future not to make threads too soon.

###### [!Record type] `<fork-join-pool>`

Record type of fork join pool.

###### [!Function] `fork-join-pool?` _obj_

Returns `#t` if the given _obj_ is a fork-join-pool, otherwise `#f`.

###### [!Function] `make-fork-join-pool` _core-threads_ :optional _parameter_

Creates a fork join pool with core thread count of _core-threads_.

If the optional argument _parameter_ is given, then it must be a
fork join pool parameter. The _parameter_ controls creating fork
join pool.

###### [!Function] `fork-join-pool-thread-count` _fork-join-pool_

Returns number of threads currently the given _fork-join-pool_ is having.

###### [!Function] `fork-join-pool-max-threads` _fork-join-pool_

Returns max thread number of the given _fork-join-pool_.

###### [!Function] `fork-join-pool-push-task!` _fork-join-pool_ _thunk_

Pushes the given _thunk_ into the _fork-join-pool_. The _thunk_ will be
executed on the _fork-join-pool_ when there's an available thread.

###### [!Function] `fork-join-pool-wait-all!` _fork-join-pool_ :optional _timeout_

Waits _fork-join-pool_ to finish all tasks. The procedure blocks the
calling thread and may not return if there's a task which hanged.

Optional argument _timeout_ specifies the timeout. It can be either
an integer represents milliseconds or absolute time.

The procedure returns `#t` if all the core threads are freed.
otherwise `#f`. (e.g. timeout)

NOTE: At this moment, this procedure doesn't guarantee the tasks are finished,
if it's running on a spawned thread.

###### [!Function] `fork-join-pool-shutdown!` _fork-join-pool_

Shutdowns the given _fork-join-pool_.  
This procedure discards all the threads. After this procedure is called,
then the given _fork-join-pool_ is no longer available.

###### [!Function] `fork-join-pool-available?` _fork-join-pool_

Returns `#t` if the given _fork-join-pool_ is available.

### [§3] Shared queues

###### [!Library] `(util concurrent shared-queue)` 

A sub library of `(util concurrent)`. This library provides
shared queue APIs.

A shared queue is a queue whose operations are done atomically.

``````````scheme
(import (rnrs) (util concurrent) (srfi :18))

(define shared-queue (make-shared-queue))

(define thread 
  (thread-start!
    (make-thread
      (lambda ()
        ;; waits until the queue has an element
        (let ((value (shared-queue-get! shared-queue)))
          (* value value))))))

(shared-queue-put! share-queue 5)

(thread-join! thread)
``````````
=> ``25``

###### [!Record] `<shared-queue>` 

Record type of shared queue.

###### [!Function] `shared-queue?`  _obj_

Returns #t if given _obj_ is shared queue, otherwise #f.

###### [!Function] `make-shared-queue`  _:optional_ _(max-length_ _-1)_

Creates a shared queue.

If optional argument _max-length_ is 0, then the queue can be used as
synchronised queue. If the value is positive number, then the queue can 
only have specified number of elements. If it overflows, then it waits 
until the number of elements is less than _max-length_.


###### [!Function] `shared-queue-empty?`  _shared-queue_

Returns #t if the number of elements inside of _shared-queue_ is 0.
Otherwise #f.

###### [!Function] `shared-queue-size`  _shared-queue_

Returns the number of elements inside of _shared-queue_.

###### [!Function] `shared-queue-max-length`  _shared-queue_

Returns max length of _shared-queue_.

###### [!Function] `shared-queue-overflows?`  _shared-queue_ _count_

Returns #t if putting _count_ of element into _shared-queue_overflows. Otherwise #f.

###### [!Function] `shared-queue-get!`  _shared-queue_ _:optional_ _(timeout_ _#f)_ _(timeout-val_ _#f)_

Retrieves the first element from _shared-queue_.

If the queue is empty and optional argument _timeout_ is #f, then 
this procedure waits until the queue gets something.

If the optional argument _timeout_ is specified, then the procedure
only waits specified amount of time. _timeout_ can be either integer
or time object defined in SRFI-19. If the queue didn't get any object within
the _timeout_, then _timeout-val_ is returned.


###### [!Function] `shared-queue-put!`  _shared-queue_ _obj_ _:optional_ _(timeout_ _#f)_ _(timeout-val_ _#f)_

Puts _obj_ into _shared-queue_ and returns _obj_.

If the queue has `max-length` and overflows, then it wait until
it's available.

If the optional argument _timeout_ is specified, then the procedure
only waits specified amount of time. _timeout_ can be either integer
or time object defined in SRFI-19. If the queue didn't get any object within
the _timeout_, then _timeout-val_ is returned.


###### [!Function] `shared-queue-remove!`  _shared-queue_ _obj_ _:optional_ _(=_ _equal?)_

Removes the given _obj_ from the _shared-queue_. The procedure
returns #t if _obj_ is removed, otherwise #f.

Optional argument _=_, must be a comparison procedure, specifies how to
compare the element of the _shared-queue_ and given _obj_. Default
value is `equal?`.


###### [!Function] `shared-queue-clear!`  _shared-queue_

Clears all element inside of _shared-queue_.

###### [!Function] `shared-queue-find`  _shared-queue_ _pred_

Finds an elements which satisfies _pred_. This operations locks the
given _shared-queue_.

###### [!Function] `shared-queue-locked?`  _shared-queue_ _:optional_ _(wait?_ _#f)_

Returns #t if the given _shared-queue_ is locked by other thread,
otherwise #f.

If the optional argument _wait?_ is given, then the procedure waits
until the queue is available and returns #f.


### [§3] Actor

###### [!Library] `(util concurrent actor)` 

A sub library of `(util concurrent)`. This library provides
actor model like APIs.

An actor is an object which contains thread, input receiver and output sender.
This is based on the Actor model. Communication between an actor and outside of
the actor can only be done via input receiver or output sender. From here, we
call them channel. The following is a simple bank account example using actor.

``````````scheme
(import (rnrs) (util concurrent actor) (match))

(define (open-account initial-amount)
  (define balance initial-amount)
  (make-shared-queue-channel-actor
   (lambda (input-receiver output-sender)
     (let loop ()
       (match (input-receiver)
	 (('withdrow how-much)
	  (if (< balance how-much)
	      (output-sender "invalid amount")
	      (begin
		(set! balance (- balance how-much))
		(output-sender (cons how-much balance))))
	  (loop))
	 (('deposite a)
	  (if (negative? a)
	      (output-sender "invalid amount")
	      (begin
		(set! balance (+ balance a))
		(output-sender (cons 0 balance))))
	  (loop))
	 (('close) #t)
	 (else "invalid message"))))))

(define client (open-account 1000))

(actor-send-message! client '(withdrow 100))
(actor-send-message! client '(deposite 100))
(actor-send-message! client '(close))

(actor-receive-message! client) ;; => (100 . 900)
(actor-receive-message! client) ;; => (0 . 1000)
``````````

###### [!Function] `actor?`  _obj_

Returns #t if the given _obj_ is an actor, otherwise #f.

###### [!Function] `make-shared-queue-channel-actor`  _task_
###### [!Function] `make-shared-priority-queue-channel-actor`  _task_ _compare_

Creates actors with shared-queue or shared-priority-queue as underlying
channel implementation, respectively.

If the `make-shared-priority-queue-channel-actor` is used, then the
_compare_ must be a procedure which takes 2 arguments and returns the
comparison result of given 2 arguments. The value should be, -1, 0 or 1.

_task_ must be an procedure accepts 2 argument, _input-receiver_ and
_output-sender_. The procedures' signatures are the followings:
###### [!Function] `input-receiver`  _:optional_ _timeout_ _timeout-val_
###### [!Function] `output-sender`  _messge_ _:optional_ _timeout_ _timeout-val_
The _input-receiver_ receives a message from outside of the actor.

The _output-sender_ sends a message _message_ to outside of the actor.

Messages can be sent to an actor via `actor-send-message!`, and be
received from an actor via `actor-receive-message!`.

The optional arguments _timeout_ and _timeout-val_ are given, it shall
behave the same as `shared-queue-get!` or `shared-queue-put!`.


###### [!Function] `actor-send-message!`  _actor_ _message_ _:optional_ _timeout_ _timeout-val_

Sends the given _message_ to the _actor_. The operation may block
the caller thread depending on the underlying channel implementation.

If optional argument _timeout_ and _timeout-val_ are given, it shall
behave the same as `shared-queue-put!`.


###### [!Function] `actor-receive-message!`  _actor_ _:optional_ _timeout_ _timeout-val_

Receives a message from given _actor_. The operation may block the
caller thread depending on the underlying channel implementation.

If optional argument _timeout_ and _timeout-val_ are given, it shall
behave the same as `shared-queue-get!`.


###### [!Function] `actor-running?`  _actor_

Return #t if the given _actor_ is running, otherwise #f.

###### [!Function] `actor-wait!`  _actor_ _:optional_ _timeout_ _timeout-val_

Waits until the given _actor_ is finished.

The optional arguments works the same as `thread-join!`.


###### [!Function] `actor-terminate!`  _actor_

Terminates the given _actor_.

NOTE: This operation is not safe. It is users' responsibility to release
resource if it's needed.


###### [!Library] `(util concurrent completable-future)` 

A sub library of `(util concurrent)`. This library provides
Java's CompletableFuture like interface

###### [!Function] `thunk->future`  _thunk_
###### [!Function] `thunk->future`  _thunk_ _executor_

Provide a future whose value supplier is the _thunk_.

If the second form is used, then the execution will be done by the
given _executor_ otherwise `*completable-future:default-executor*`will be used.


###### [!Function] `future-map`  _proc_ _future_ _..._

_proc_ must accept the same number of arguments as the given *future*s

Apply the procedure _proc_ to the result of the *future*s.
And return a newly created future.

These procedures return immediately and the computation of
_proc_ will be done in some future.


###### [!Function] `future-map/executor` _executor_ _proc_ _future_ _..._

The same as `future-map`, the only diffrence is that it takes
_exeuctor_ as its execution environment.

###### [!Function] `future-flatmap`  _proc_ _future_ _..._

_proc_ must accept the same number of arguments as the given *future*s,
and return a future.

Apply the procedure _proc_ to the result of the *future*s.
And return a newly created future which returns the result of
the future returned by the _proc_.

These procedures return immediately and the computation of
_proc_ will be done in some future.


###### [!Function] `future-flatmap/executor` _executor_  _proc_ _future_ _..._

The same as `future-flatmap`, the only diffrence is that it takes
_exeuctor_ as its execution environment.

###### [!Function] `future-guard`  _proc_ _future_

Guards the _future_ and apply the raised condition to _proc_.

``````````scheme
(future-get (future-guard (lambda (e) 'ok)
                          (thunk->future (lambda () (raise 'boo)))))
``````````
=> ``'ok``

These procedures return immediately and the computation of
_proc_ will be done in some future.


