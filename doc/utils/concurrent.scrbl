@; -*- coding: utf-8 -*-
@subsection[:tag "concurrent"]{(util concurrent) - Concurrency utilities}

@define[Library]{@name{(util concurrent)}}
@desc{This library provides high level concurrency APIs.}

Using low level thread and mutex sometimes causes dead lock or incomprehensible
code. This library provides frameworks of common use cases. This library is
built on top of SRFI-18.

@subsubsection{Future}

@define[Library]{@name{(util concurrent future)}}
@desc{A sub library of @code{(util concurrent)}. This library provides
future related APIs.}

A future is an object that has a task which is a thunk and will be executed
in future. In this implementation, future is an interface and its execution
is depending on the sub class. The default implementation this library
provides uses a thread per future. If users don't have to manage number
of threads, then using this is sufficient.

@codeblock[=> (1 4 9 16 25)]{
(import (rnrs) (util concurrent))

;; creates 5 futures
(define futures
  (map (lambda (i) (future (* i i))) '(1 2 3 4 5)))

;; wait and retrieve the results
(map future-get futures)
}

@define[Record]{@name{<future>}}
@desc{The interface of all futures.}

@define[Function]{@name{future?} @args{obj}}
@desc{Returns #t if given @var{obj} is a future object, otherwise #f.}

@define[Macro]{@name{future} @args{expr @dots{}}}
@define[Macro]{@name{future} @args{(class record)expr @dots{}}}
@desc{Creates a future which executes @var{expr}. The type of the
returning future is @var{record}.

The first form is equivalent with the following:
@code{(future (class <simple-future>) @var{expr @dots{}})}
}

@define[Function]{@name{future-get} @args{future}}
@desc{Retrieves the result of the given @var{future}.

This procedure waits if the execution of the @var{future} isn't finished yet.
}

@define[Function]{@name{future-cancel} @args{future}}
@desc{Cancels the execution of @var{future}.

This procedure may or may not cancel the execution depending on the
implementation of @var{future}. The @code{<simple-future>} provides by
this library won't disturb the execution. Thus calling this procedure
doesn't do anything but changing the future's state.

NOTE: once this procedure is called, then calling @code{future-get}
with @var{future} raises a @code{&future-terminated}.

@codeblock[=> "&future-terminated"]{
(import (rnrs) (util concurrent))

(define f (future (display "cancelled") (newline)))
(future-cancel f)
(future-get f)
}

The above example may or may not print @code{"cancelled"}.
}

@define[Function]{@name{future-done?} @args{future}}
@desc{Returns #t if given execution of @var{future} is finished, otherwise #f.}

@define[Function]{@name{future-cancelled?} @args{future}}
@desc{Returns #t if given execution of @var{future} is terminated by
@code{future-cancel}, otherwise #f.}

@define[Function]{@name{future-state} @args{future}}
@desc{Returns current state of the given @var{future}.

The returning state is depending on the implementation of @var{future}.
Only 2 states are defined in this library, @code{done} and @code{terminated}.

@code{done} is set when @code{future-get} is called.

@code{terminated} is set when @code{future-cancel} is called.
}

@define["Condition Type"]{@name{&future-terminated}}
@desc{This type describes when a future is terminated but users try to
retrieve its result.}

@define[Function]{@name{future-terminated?} @args{obj}}
@desc{Returns #t if given @var{obj} is object of @code{&future-terminated}.
}

@define[Function]{@name{terminated-future} @args{condition}}
@desc{@var{condition} must be a @code{&future-terminated} condition.

Retrieve terminated future from @var{condition}.
}

@sub*section{Simple future}

Simple future is a future implementation which executes the task
on a thread immediately.

@define[Record]{@name{<simple-future>}}
@desc{Default @code{<future>} implementation of this library.}

@define[Function]{@name{simple-future?} @args{obj}}
@desc{Returns #t if given @var{obj} is a simple future, otherwise #f.}

@define[Function]{@name{make-simple-future} @args{thunk}}
@desc{Creates a simple future which executes @var{thunk}.}


@subsubsection{Executor}

@define[Library]{@name{(util concurrent executor)}}
@desc{A sub library of @code{(util concurrent)}. This library provides
executor related APIs.}

A executor is an object that executes submitted futures. The idea is taken
from @code{java.util.concurrent} package. The library provides 2 types
of executors, thread pool executor and fork join executor. The first
one uses thread pool, described below section, and the latter one
just creates a thread per task. The following is an example how to use
the executors:

@codeblock[=> (1 4 9 16 25 36 49 64 81 100)]{
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
}

The thread pool executor with @code{push-future-handler} waits until the
previous taskes are finished. 

@sub*section{Generic Executor APIs}

Executor provided by this library is an extensible. So the most commonly
used procedures are generic.

@define[Record]{@name{<executor>}}
@desc{The interface of executor.

This record only has one field, @code{state}.
}

@define[Function]{@name{executor?} @args{obj}}
@desc{Returns #t if given @var{obj} is an executor, otherwise #f.}

@define[Function]{@name{executor-state} @args{executor}}
@desc{Returns @code{state} field of the @var{executor}.}


The behaviour of the folowing procedures depend on its implementation.

@define[Function]{@name{executor-available?} @args{executor}}
@desc{Returns #t if the given @var{executor} is available, otherwise #f.}

@define[Function]{@name{execute-future!} @args{executor future}}
@desc{Executes given @var{future} on @var{executor}.}

@define[Function]{@name{shutdown-executor!} @args{executor}}
@desc{Shutdowns the given @var{executor}. 

This procedure may or may not affect the managed futures on the @var{executor}.
}

@define[Function]{@name{executor-submit!} @args{executor thunk}}
@desc{Converts @var{thunk} to a future and execute it on given @var{executor},
then returns the future. This procedure is defined as follows:

@codeblock{
(define (executor-submit! e thunk)
  (let ((f (make-executor-future thunk)))
    (execute-future! e f)
    f))
}
}

@sub*section{Thread pool executor}

Thread pool executor uses @code{(util concurrent thread-pool)} as its
underlying thread managing. So once the threads are created then the
thread holds its environment until the executor is shutdown. In other
words, if a task changes the dynamic environment, then the next task
uses the changed dynamic environment. The following example describes
how dynamic environments works on this executor:

@codeblock[=> 2]{
(import (rnrs) (util concurrent) (srfi :39))

(define *one* (make-parameter 1))

(define executor (make-thread-pool-executor 1))

(let ((f1 (make-executor-future (lambda () (*one* 2) 1)))
      (f2 (make-executor-future (lambda () (*one*)))))
  (execute-future! executor f1)
  (future-get f1)
  (execute-future! executor f2)
  (future-get f2))
}

NOTE: parameter objects are thread safe in general, thus if a thread is
created per future, then the parameter @code{*one*} is initialised with
the initial value @code{1} during thread creation.

@define[Record]{@name{<thread-pool-executor>}}
@desc{Record type of thread pool executor. This record type inherits
@code{<executor>}.}

@define[Function]{@name{thread-pool-executor?} @args{obj}}
@desc{Returns #t if given @var{obj} is a thread pool executor, otherwise #f.}

@define[Function]{@name{make-thread-pool-executor}
 @args{max-thread :optional reject-handler}}
@desc{Creates a thread pool executor with thread count @var{max-thread}.

If optional argument @var{reject-handler} is specified, then the specified
handler is used. Otherwise, @code{abort-rejected-handler} is used.
}

@define[Function]{@name{thread-pool-executor-pool-size}
 @args{thread-pool-executor}}
@desc{Returns number of futures currently executing on the given thread pool
executor.

This number would be greater than the thread count if @code{push-future-handler}
is specified during the executor creation.
}

@define[Function]{@name{thread-pool-executor-max-pool-size}
 @args{thread-pool-executor}}
@desc{Returns number of thread count of the given thread pool executor.
}

@define[Function]{@name{thread-pool-executor-available?}
 @args{thread-pool-executor}}
@desc{Return #t if the number of executing future is less than the number of
thread count.

NOTE: this procedure may return #f even tasks can be pushed to the executor
if @code{push-future-handler} is specified.
}

@define[Function]{@name{thread-pool-executor-execute-future!}
 @args{thread-pool-executor future}}
@desc{Executes the given @var{future} on @var{thread-pool-executor}.}

@define[Function]{@name{thread-pool-executor-shutdown!}
 @args{thread-pool-executor}}
@desc{Shutdown the given @var{thread-pool-executor}.}

Builtin reject handlers.

Reject handler is a procedure called when thread pool executor is not
available to decide how the executor should treat the given future.

@define[Function]{@name{abort-rejected-handler}}
@desc{Reject the future and raises @code{&rejected-execution-error}.

This is the default handler.
}

@define[Function]{@name{terminate-oldest-handler}}
@desc{Terminates the oldest future.

When this handler is called, the thread which target future is running is
also terminated. Thus the dynamic environment is also reset.
}

@define[Function]{@name{wait-finishing-handler} @args{wait-retry}}
@desc{Creats a reject handler which waits until one of the thread
is available.

The @var{wait-retry} is a number of retry count. If none of future task
is finished by this counter, then @code{abort-rejected-handler} is called.
}

@define[Function]{@name{push-future-handler}}
@desc{Pushes the task to the least used thread.}

@define["Condition Type"]{@name{&rejected-execution-error}}
@desc{A condition describes when a future is rejected by an executor.}

@define[Function]{@name{rejected-execution-error?} @args{obj}}
@desc{Returns #t if given @var{obj} is @code{&rejected-execution-error} object.
Otherwise #f.}

@define[Function]{@name{rejected-future} @args{condition}}
@define[Function]{@name{rejected-executor} @args{condition}}
@desc{@var{condition} must be a @code{&rejected-execution-error} object.

Retrieves the rejected future and executor, respectively.
}

@sub*section{Fork join executor}

Fork join executor is an executor which simply creats a thread per future.
It's almost the same as @code{<simple-future>}.


@define[Record]{@name{<fork-join-executor>}}
@desc{Record type of fork join executor. This record type inherits
@code{<executor>}.}

@define[Function]{@name{fork-join-executor?} @args{obj}}
@desc{Returns #t if given @var{obj} is a fork join executor, otherwise #f.}

@define[Function]{@name{make-fork-join-executor}}
@desc{Creates a fork join executor. }

@define[Function]{@name{fork-join-executor-available?}
 @args{fork-join-executor}}
@desc{Returns #t.}

@define[Function]{@name{fork-join-executor-execute-future!}
 @args{fork-join-executor future}}
@desc{Executes the given @var{future} on @var{fork-join-executor}.

NOTE: the executor doesn't manage anything so the future is actually not
related to the executor.
}

@define[Function]{@name{fork-join-executor-shutdown!}
 @args{fork-join-executor}}
@desc{Sets executor's state @code{shutdown}.}

@sub*section{Executor future}

An executor future is an future object which can be used on executor.

@define[Record]{@name{<executor-future>}}
@desc{Record type of @code{<executor-future>}. This type inherits 
@code{<future>}.
}

@define[Function]{@name{executor-future?} @args{obj}}
@desc{Returns #t if the given @var{obj} is an executor future, otherwise #f.}

@define[Function]{@name{make-executor-future} @args{thunk}}
@desc{Creates an executor future object.}

@; TODO creating custom executor and future.

@subsubsection{Thread pool}

@define[Library]{@name{(util concurrent thread-pool)}}
@desc{A sub library of @code{(util concurrent)}. This library provides
thread pool APIs.}

Creating a thread is not cheap on Sagittarius. If users want to reuse threads,
then this library can be used.

@codeblock{
(import (rnrs) (util concurrent))

;; pooling 5 thread
(define thread-pool (make-thread-pool 5))

(for-each (lambda (i) (thread-pool-push-task! thread-pool (lambda () (* i i))))
	  '(1 2 3 4 5 6 7 8 9 10))

;; waits until all tasks are done
(thread-pool-wait-all! thread-pool)

;; release thread-pool
(thread-pool-release! thread-pool)
}

@define[Record]{@name{<thread-pool>}}
@desc{Record type of thread pool.}

@define[Function]{@name{thread-pool?} @args{obj}}
@desc{Returns #t if given @var{obj} is a thread-pool, otherwise #f.}

@define[Function]{@name{make-thread-pool}
 @args{thread-count :optional error-handler}}
@desc{Creates a thread pool with @var{thread-count} of threads. 

If the optional argument @var{error-handler} is given, it must be a
procedure which accept one argument, then the procedure is called
when the pushed task raised an error.
}

@define[Function]{@name{thread-pool-size} @args{thread-pool}}
@desc{Returns number of threads on the given @var{thread-pool}.}

@define[Function]{@name{thread-pool-push-task!} @args{thread-pool thunk}}
@desc{Push the given @var{thunk} to least used @var{thread-pool}'s thread.
And returns the id of the pushed thread. This id can be used to retrive
the actual thread calling @code{thread-pool-thread} procedure.
}

@define[Function]{@name{thread-pool-wait-all!} @args{thread-pool}}
@desc{Waits all the tasks pushed into the given @var{thread-pool}.

The return value of the tasks are discarded.}

@define[Function]{@name{thread-pool-release!} @args{thread-pool :optional how}}
@desc{Joins all the thread on the given @var{thread-pool}.

If optional argument how is specified @code{terminate}, then the procedure
terminates the thread instead of joining.

NOTE: terminating a thread is very dangerous operation, so don't use casually.
}

@define[Function]{@name{thread-pool-thread} @args{thread-pool id}}
@desc{Retrieves the pooled thread associated with given @var{id} from
given @var{thread-pool}.

It signals an error if the given @var{id} is not a thread id.
}

@define[Function]{@name{thread-pool-thread-id} @args{thread-pool thread}}
@desc{Retrieves the pooled thread id associated with given @var{thread} from
given @var{thread-pool}. The procedure takes O(n) where n is number of threads
managed by the @var{thread-pool}. It might be better to use
@code{(thread-pool-current-thread-id)} procedure to retrieve thread id from
managed threads.

It signals an error if the given @var{thread} is not a managed thread.

NOTE: if the thread is terminated, then the procedure also signals an error.
}

@define[Function]{@name{(thread-pool-current-thread-id)}}
@desc{Retrieves thread id of current thread. If the current thread is not
a managed thread, then #f is returned.
}

@define[Function]{@name{thread-pool-thread-terminate!} @args{thread-pool id}}
@desc{Terminates the pooled thread associated with given @var{id} and recreate
a new thread into @var{thread-pool}.

NTOE: this is a dangerous operation. Don't use it casually.
}

@define[Function]{@name{thread-pool-thread-task-running?} @args{thread-pool id}}
@desc{Returns #t if the given @var{id} of thread in the @var{thread-pool} is 
running. Otherwise #f.
}

@subsubsection{Shared queues}

@define[Library]{@name{(util concurrent shared-queue)}}
@desc{A sub library of @code{(util concurrent)}. This library provides
shared queue APIs.}

A shared queue is a queue whose operations are done atomically.

@codeblock[=> 25]{
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
}

@define[Record]{@name{<shared-queue>}}
@desc{Record type of shared queue.}

@define[Function]{@name{shared-queue?} @args{obj}}
@desc{Returns #t if given @var{obj} is shared queue, otherwise #f.}

@define[Function]{@name{make-shared-queue} @args{:optional (max-length -1)}}
@desc{Creates a shared queue.

If optional argument @var{max-length} is 0, then the queue can be used as
synchronised queue. If the value is positive number, then the queue can 
only have specified number of elements. If it overflows, then it waits 
until the number of elements is less than @var{max-length}.
}

@define[Function]{@name{shared-queue-empty?} @args{shared-queue}}
@desc{Returns #t if the number of elements inside of @var{shared-queue} is 0.
Otherwise #f.}

@define[Function]{@name{shared-queue-size} @args{shared-queue}}
@desc{Returns the number of elements inside of @var{shared-queue}.}

@define[Function]{@name{shared-queue-max-length} @args{shared-queue}}
@desc{Returns max length of @var{shared-queue}.}

@define[Function]{@name{shared-queue-overflows?} @args{shared-queue count}}
@desc{Returns #t if putting @var{count} of element into @var{shared-queue}
overflows. Otherwise #f.}

@define[Function]{@name{shared-queue-get!}
 @args{shared-queue :optional (timeout #f) (timeout-val #f)}}
@desc{Retrieves the first element from @var{shared-queue}.

If the queue is empty and optional argument @var{timeout} is #f, then 
this procedure waits until the queue gets something.

If the optional argument @var{timeout} is specified, then the procedure
only waits specified amount of time. @var{timeout} can be either integer
or time object defined in SRFI-19. If the queue didn't get any object within
the @var{timeout}, then @var{timeout-val} is returned.
}

@define[Function]{@name{shared-queue-put!}
 @args{shared-queue obj :optional (timeout #f) (timeout-val #f)}}
@desc{Puts @var{obj} into @var{shared-queue} and returns @var{obj}.

If the queue has @code{max-length} and overflows, then it wait until
it's available.

If the optional argument @var{timeout} is specified, then the procedure
only waits specified amount of time. @var{timeout} can be either integer
or time object defined in SRFI-19. If the queue didn't get any object within
the @var{timeout}, then @var{timeout-val} is returned.
}

@define[Function]{@name{shared-queue-remove!}
 @args{shared-queue obj :optional (= equal?)}}
@desc{Removes the given @var{obj} from the @var{shared-queue}. The procedure
returns #t if @var{obj} is removed, otherwise #f.

Optional argument @var{=}, must be a comparison procedure, specifies how to
compare the element of the @var{shared-queue} and given @var{obj}. Default
value is @code{equal?}.
}

@define[Function]{@name{shared-queue-clear!} @args{shared-queue}}
@desc{Clears all element inside of @var{shared-queue}.}

@define[Function]{@name{shared-queue-find} @args{shared-queue pred}}
@desc{Finds an elements which satisfies @var{pred}. This operations locks the
given @var{shared-queue}.}

@define[Function]{@name{shared-queue-locked?}
 @args{shared-queue :optional (wait? #f)}}
@desc{Returns #t if the given @var{shared-queue} is locked by other thread,
otherwise #f.

If the optional argument @var{wait?} is given, then the procedure waits
until the queue is available and returns #f.
}


@; TODO shared priority queue

@subsubsection{Actor}

@define[Library]{@name{(util concurrent actor)}}
@desc{A sub library of @code{(util concurrent)}. This library provides
actor model like APIs.}

An actor is an object which contains thread, input receiver and output sender.
This is based on the Actor model. Communication between an actor and outside of
the actor can only be done via input receiver or output sender. From here, we
call them channel.

@define[Function]{@name{actor?} @args{obj}}
@desc{Returns #t if the given @var{obj} is an actor, otherwise #f.}

@define[Function]{@name{actor-send-message!}
 @args{actor message :optional timeout timeout-val}}
@desc{Sends the given @var{message} to the @var{actor}. The operation may block
the caller thread depending on the underlying channel implementation.

If optional argument @var{timeout} and @var{timeout-val} are given, it shall
behave the same as @code{shared-queue-put!}.
}
