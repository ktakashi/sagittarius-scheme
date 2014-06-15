@; -*- coding: utf-8 -*-
@subsection[:tag "util.queue"]{(util queue) - Queue}

@; mostly from Gauche's document

@define[Library]{@name{(util queue)}}
@desc{This library provides queue (FIFO) data structure and its operations.

You can create a simple queue, which is not thread-safe, or an TM queue, a
thread-safe queue. Basic queue operations work on both type of queues. When
a mtqueue is passed to the procedures listed in this section, each operation
is done in atomic way, unless otherwise noted.

There are also a set of procedures for mtqueues that can be used for thread
synchronisation; for example, you can let the consumer thread block if an
mtqueue is empty, and/or the producer thread block if the number of items in
the mtqueue reaches a specified limit. Using these procedures allows the
program to use an mtqueue as a @var{channel}.

The simple queue API is a super set of SLIB's queue implementation.
}

@define[Class]{@name{<queue>}}
@desc{A class of simple queue. }

@define[Class]{@name{<mtqueue>}}
@desc{A class of mtqueue. Inherits @code{<queue>}.}

@define[Function]{@name{make-queue}}
@desc{Creates and return an empty simple queue.}

@define[Function]{@name{make-mtqueue} @args{:key max-length}}
@desc{Creates and return an empty mtqueue.

The keyword argument @var{max-length} specifies the maximum entry count of
the queue. Negative number indicates unlimited number of entry. If the given
number is zero then the queue cannot hold any item.
}

@define[Function]{@name{queue?} @args{obj}}
@desc{Returns #t if @var{obj} is a queue (either a simple queue or an mtqueue).}

@define[Function]{@name{queue?} @args{obj}}
@desc{Returns #t if @var{obj} is an mtqueue.}

@define[Function]{@name{queue-empty?} @args{queue}}
@desc{Returns #t if @var{queue} is an empty queue.}

@define[Function]{@name{queue-length} @args{queue}}
@desc{Returns the number of the items in the queue.}

@define[Function]{@name{mtqueue-max-length} @args{mtqueue}}
@desc{Returns the maximum number of items @var{mtqueue} can hold. 
#f indicates unlimited.}

@define[Function]{@name{mtqueue-room} @args{mtqueue}}
@desc{Returns the number of elements @var{mtqueue} can accept at this moment
before it hits its maximum length. If the queue has unlimited capacity then
the procedure returns @code{+inf.0}.}

@define[Function]{@name{copy-queue} @args{queue}}
@desc{Returns a copy of @var{queue}.}

@define[Function]{@name{enqueue!} @args{queue obj more-objs @dots{}}}
@desc{Adds @var{obj} to the end of  @var{queue}. You may give more than
one object, and each of them are enqueued in order.

If @var{queue} is an mtqueue, all the objects are enqueued atomically; no
other objects from other threads can be inserted between the objects given
to a single @code{enqueue!} call. Besides, if the value of the result of
@code{mtqueue-max-length} is positive, and adding @var{objs} makes the
number of element in @var{queue} exceeds it, an error is raised and 
@var{queue} won't be modified. (If the maximum length is zero, this procedure
always fail. Use @code{enqueue/wait!} below.)
}

@define[Function]{@name{queue-push!} @args{queue obj more-objs @dots{}}}
@desc{Adds @var{obj} to in front of @var{queue}. You may give more than
one object, and each of them are pushed in order.

Like @code{enqueue!}, when @var{queue} is an mtqueue, all objects are added
atomically, and the value of max length is checked. See @code{enqueue!} above
for more detail.
}

@define[Function]{@name{dequeue!} @args{queue :optional fallback}}
@define[Function]{@name{queue-pop!} @args{queue :optional fallback}}
@desc{Take one object from the front of @var{queue} and return it. Both 
function work the same, but @code{queue-pop!} may be used to emphasize it
works with @code{queue-push!}.

If @var{queue} is empty, @var{fallback} is returned if give, otherwise an 
error is raised.

If @var{queue} is mtqueue and its max length is zero, then the queue is
always empty. Use @code{dequeue/wait!} to use such a queue as a 
synchronisation device.
}

@define[Function]{@name{dequeue-all!} @args{queue}}
@desc{Returns the whole content of @var{queue} by a list, with emptying 
@var{queue}. If @var{queue} is empty, returns an empty list. See also
@code{queue->list} below.
}

@define[Function]{@name{queue-front} @args{queue :optional fallback}}
@define[Function]{@name{queue-rear} @args{queue :optional fallback}}
@desc{Peek the head or the tail of @var{queue} and return the object, 
respectively.

If @var{queue} is empty, @var{fallback} is returned if give, otherwise an 
error is raised.
}

@define[Function]{@name{list->queue} @args{list :optional class :rest initargs}}
@desc{Returns a new queue which content is the elements in @var{list}, in
the given order.

By default the created queue is a simple queue, but you can create mtqueue
or instance of other subclass @code{<queue>} by giving the class to the
optional @var{class} arguments. The optional @var{initargs} arguments are
passed to the constructor of @var{class}.
}

@define[Function]{@name{queue->list} @args{queue}}
@desc{Returns a list whose content is the items in @var{queue} in order.
Unlike @code{dequeue-all!}, the content of @var{queue} remains intact.
The returning list is a copy of the content. So modifying the list won't
affect @var{queue}.
}

@define[Function]{@name{find-in-queue} @args{pred queue}}
@desc{Returns the first item in @var{queue} that satisfies a predicate
@var{pred}.
}

@define[Function]{@name{any-in-queue} @args{pred queue}}
@desc{Apply @var{pred} on each item in @var{queue} until it evaluates true,
and returns that true value. If no item satisfies @var{pred}, @f is returned.
}

@define[Function]{@name{every-in-queue} @args{pred queue}}
@desc{Apply @var{pred} on each item in @var{queue}. If @var{pred} returns #f,
stops iteration and returns #f immediately. Otherwise, returns the result of
@var{pred} on the last item of @var{queue}. If the queue is empty, #t is
returned.
}

@define[Function]{@name{remove-from-queue!} @args{pred queue}}
@desc{Removes all items in @var{queue} that satisfies @var{pred}. Returns
#t if any item is removed. Otherwise #f.
}

@define[Function]{@name{enqueue/wait!}
 @args{mtqueue obj :optional timeout timeout-val}}
@define[Function]{@name{queue-push/wait!}
 @args{mtqueue obj :optional timeout timeout-val}}
@define[Function]{@name{dequeue/wait!}
 @args{mtqueue :optional timeout timeout-val}}
@define[Function]{@name{queue-pop/wait!}
 @args{mtqueue :optional timeout timeout-val}}
@desc{These synchronising variants work on an mtqueue and make the caller
thread block when the mtqueue has reached its maximum length (for
@code{enqueue/wait!} and @var{queue-push/wait!}), or the mtqueue is empty
(for @code{dequeue/wait!} and @var{queue-pop/wait!}). The blocked caller
thread is unblocked either the blocking condition is resolved, or the
timeout condition is met.

The optional @var{timeout} argument specifies the timeout condition. If it
is #f, those procedure wait indefinitely. If it is a real number, they wait
at least the given number of seconds.
@; TODO <time> object.

In case the call is blocked then timed out, the value of @var{timeout-val} is
returned, which default value is #t.

When @code{enqueue/wait!} and @code{queue-push/wait!} succeeds without hitting
timeout, they return #t.
}