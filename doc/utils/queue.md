[§2] (util queue) - Queue {#util.queue}
-------------

###### [!Library] `(util queue)` 

This library provides queue (FIFO) data structure and its operations.

You can create a simple queue, which is not thread-safe, or an MT queue, a
thread-safe queue. Basic queue operations work on both type of queues. When
a mtqueue is passed to the procedures listed in this section, each operation
is done in atomic way, unless otherwise noted.

There are also a set of procedures for mtqueues that can be used for thread
synchronisation; for example, you can let the consumer thread block if an
mtqueue is empty, and/or the producer thread block if the number of items in
the mtqueue reaches a specified limit. Using these procedures allows the
program to use an mtqueue as a _channel_.

The simple queue API is a super set of SLIB's queue implementation.

NOTE: [(util deque)](#util.deque) is used as underlying library.


###### [!Class] `<queue>` 

A class of simple queue. 

###### [!Class] `<mtqueue>` 

A class of mtqueue. Inherits `<queue>`.

###### [!Function] `make-queue` 

Creates and return an empty simple queue.

###### [!Function] `make-mtqueue`  _:key_ _max-length_

Creates and return an empty mtqueue.

The keyword argument _max-length_ specifies the maximum entry count of
the queue. Negative number indicates unlimited number of entry. If the given
number is zero then the queue cannot hold any item.


###### [!Function] `queue?`  _obj_

Returns #t if _obj_ is a queue (either a simple queue or an mtqueue).

###### [!Function] `mtqueue?`  _obj_

Returns #t if _obj_ is an mtqueue.

###### [!Function] `queue-empty?`  _queue_

Returns #t if _queue_ is an empty queue.

###### [!Function] `queue-length`  _queue_

Returns the number of the items in the queue.

###### [!Function] `mtqueue-max-length`  _mtqueue_

Returns the maximum number of items _mtqueue_ can hold. 
#f indicates unlimited.

###### [!Function] `mtqueue-room`  _mtqueue_

Returns the number of elements _mtqueue_ can accept at this moment
before it hits its maximum length. If the queue has unlimited capacity then
the procedure returns `+inf.0`.

###### [!Function] `copy-queue`  _queue_

Returns a copy of _queue_.

###### [!Function] `enqueue!`  _queue_ _obj_ _more-objs_ _..._

Adds _obj_ to the end of  _queue_. You may give more than
one object, and each of them are enqueued in order.

If _queue_ is an mtqueue, all the objects are enqueued atomically; no
other objects from other threads can be inserted between the objects given
to a single `enqueue!` call. Besides, if the value of the result of
`mtqueue-max-length` is positive, and adding _objs_ makes the
number of element in _queue_ exceeds it, an error is raised and 
_queue_ won't be modified. (If the maximum length is zero, this procedure
always fail. Use `enqueue/wait!` below.)


###### [!Function] `queue-push!`  _queue_ _obj_ _more-objs_ _..._

Adds _obj_ to in front of _queue_. You may give more than
one object, and each of them are pushed in order.

Like `enqueue!`, when _queue_ is an mtqueue, all objects are added
atomically, and the value of max length is checked. See `enqueue!` above
for more detail.


###### [!Function] `enqueue-unique!`  _queue_ _eq-proc_ _obj_ _more-objs_ _..._
###### [!Function] `queue-push-unique!`  _queue_ _eq-proc_ _obj_ _more-objs_ _..._

Like `enqueue!` and `queue-push!`, respectively, except that
these don't modify _queue_ if it already contains _objs_ (elements are
compared by two-argument procedure _eq-proc_).

When _queue_ is an mtqueue, all objects are added atomically, and the max
length is checked. See `enqueue!` above for the detail.


###### [!Function] `dequeue!`  _queue_ _:optional_ _fallback_
###### [!Function] `queue-pop!`  _queue_ _:optional_ _fallback_

Take one object from the front of _queue_ and return it. Both 
function work the same, but `queue-pop!` may be used to emphasize it
works with `queue-push!`.

If _queue_ is empty, _fallback_ is returned if give, otherwise an 
error is raised.

If _queue_ is mtqueue and its max length is zero, then the queue is
always empty. Use `dequeue/wait!` to use such a queue as a 
synchronisation device.


###### [!Function] `dequeue-all!`  _queue_

Returns the whole content of _queue_ by a list, with emptying 
_queue_. If _queue_ is empty, returns an empty list. See also
`queue->list` below.


###### [!Function] `queue-front`  _queue_ _:optional_ _fallback_
###### [!Function] `queue-rear`  _queue_ _:optional_ _fallback_

Peek the head or the tail of _queue_ and return the object, 
respectively.

If _queue_ is empty, _fallback_ is returned if give, otherwise an 
error is raised.


###### [!Function] `list->queue`  _list_ _:optional_ _class_ _:rest_ _initargs_

Returns a new queue which content is the elements in _list_, in
the given order.

By default the created queue is a simple queue, but you can create mtqueue
or instance of other subclass `<queue>` by giving the class to the
optional _class_ arguments. The optional _initargs_ arguments are
passed to the constructor of _class_.


###### [!Function] `queue->list`  _queue_

Returns a list whose content is the items in _queue_ in order.
Unlike `dequeue-all!`, the content of _queue_ remains intact.
The returning list is a copy of the content. So modifying the list won't
affect _queue_.


###### [!Function] `find-in-queue`  _pred_ _queue_

Returns the first item in _queue_ that satisfies a predicate
_pred_.


###### [!Function] `any-in-queue`  _pred_ _queue_

Apply _pred_ on each item in _queue_ until it evaluates true,
and returns that true value. If no item satisfies _pred_, #f is returned.


###### [!Function] `every-in-queue`  _pred_ _queue_

Apply _pred_ on each item in _queue_. If _pred_ returns #f,
stops iteration and returns #f immediately. Otherwise, returns the result of
_pred_ on the last item of _queue_. If the queue is empty, #t is
returned.


###### [!Function] `remove-from-queue!`  _pred_ _queue_

Removes all items in _queue_ that satisfies _pred_. Returns
#t if any item is removed. Otherwise #f.


###### [!Function] `enqueue/wait!`  _mtqueue_ _obj_ _:optional_ _timeout_ _timeout-val_
###### [!Function] `queue-push/wait!`  _mtqueue_ _obj_ _:optional_ _timeout_ _timeout-val_
###### [!Function] `dequeue/wait!`  _mtqueue_ _:optional_ _timeout_ _timeout-val_
###### [!Function] `queue-pop/wait!`  _mtqueue_ _:optional_ _timeout_ _timeout-val_

These synchronising variants work on an mtqueue and make the caller
thread block when the mtqueue has reached its maximum length (for
`enqueue/wait!` and _queue-push/wait!_), or the mtqueue is empty
(for `dequeue/wait!` and _queue-pop/wait!_). The blocked caller
thread is unblocked either the blocking condition is resolved, or the
timeout condition is met.

The optional _timeout_ argument specifies the timeout condition. If it
is #f, those procedure wait indefinitely. If it is a real number, they wait
at least the given number of seconds.

In case the call is blocked then timed out, the value of _timeout-val_ is
returned, which default value is #t.

When `enqueue/wait!` and `queue-push/wait!` succeeds without hitting
timeout, they return #t.


