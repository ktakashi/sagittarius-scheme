[§2] (util deque) - Deque {#util.deque}
-------------

###### [!Library] `(util deque)` 

This library provides deque (double-ended queue) data structure and
its operations.

You can create a simple deque, which is not thread-safe, or an MT deque, a
thread-safe deque. Basic deque operations work on both type of deques. When
a mtdeque is passed to the procedures listed in this section, each operation
is done in atomic way, unless otherwise noted.

There are also a set of procedures for mtdeques that can be used for thread
synchronisation; for example, you can let the consumer thread block if an
mtdeque is empty, and/or the producer thread block if the number of items in
the mtdeque reaches a specified limit. Using these procedures allows the
program to use an mtdeque as a _channel_.

NOTE: [(util queue)](#util.queue) is implemented using this library.


###### [!Class] `<deque>` 

A class of simple deque. 

###### [!Class] `<mtdeque>` 

A class of mtdeque. Inherits `<deque>`.

###### [!Function] `make-deque` 

Creates and return an empty simple deque.

###### [!Function] `make-mtdeque`  _:key_ _max-length_

Creates and return an empty mtdeque.

The keyword argument _max-length_ specifies the maximum entry count of
the deque. Negative number indicates unlimited number of entry. If the given
number is zero then the deque cannot hold any item.


###### [!Function] `deque?`  _obj_

Returns #t if _obj_ is a deque (either a simple deque or an mtdeque).

###### [!Function] `deque?`  _obj_

Returns #t if _obj_ is an mtdeque.

###### [!Function] `deque-empty?`  _deque_

Returns #t if _deque_ is an empty deque.

###### [!Function] `deque-length`  _deque_

Returns the number of the items in the deque.

###### [!Function] `mtdeque-max-length`  _mtdeque_

Returns the maximum number of items _mtdeque_ can hold. 
#f indicates unlimited.

###### [!Function] `mtdeque-room`  _mtdeque_

Returns the number of elements _mtdeque_ can accept at this moment
before it hits its maximum length. If the deque has unlimited capacity then
the procedure returns `+inf.0`.

###### [!Function] `copy-deque`  _deque_

Returns a copy of _deque_.

###### [!Function] `deque-push!`  _deque_ _obj_ _more-objs_ _..._

Adds _obj_ to the end of  _deque_. You may give more than
one object, and each of them are pushed in order.

If _deque_ is an mtdeque, all the objects are pushed atomically; no
other objects from other threads can be inserted between the objects given
to a single `deque-push!` call. Besides, if the value of the result of
`mtdeque-max-length` is positive, and adding _objs_ makes the
number of element in _deque_ exceeds it, an error is raised and 
_deque_ won't be modified. (If the maximum length is zero, this procedure
always fail. Use `deque-push/wait!` below.)


###### [!Function] `deque-unshift!`  _deque_ _obj_ _more-objs_ _..._

Adds _obj_ to in front of _deque_. You may give more than
one object, and each of them are pushed in order.

Like `deque-push!`, when _deque_ is an mtdeque, all objects are added
atomically, and the value of max length is checked. See `deque-push!` above
for more detail.

The name unshift is taken from Perl.


###### [!Function] `deque-push-unique!`  _deque_ _eq-proc_ _obj_ _more-objs_ _..._
###### [!Function] `deque-unshift-unique!`  _deque_ _eq-proc_ _obj_ _more-objs_ _..._

Like `deque-push!` and `deque-unshift!`, respectively, except
that these don't modify _deque_ if it already contains _objs_ (elements
are compared by two-argument procedure _eq-proc_).

When _deque_ is an mtdeque, all objects are added atomically, and the max
length is checked. See `deque-push!` above for the detail.


###### [!Function] `deque-pop!`  _deque_ _:optional_ _fallback_
###### [!Function] `deque-shift!`  _deque_ _:optional_ _fallback_

Take one object from the end or the front of _deque_, respectively,
and return it.

If _deque_ is empty, _fallback_ is returned if give, otherwise an 
error is raised.

If _deque_ is mtdeque and its max length is zero, then the deque is
always empty. Use `deque-pop/wait!` or `deque-shift/wait!` to use
such a deque as a synchronisation device.

The name shift is take from Perl.


###### [!Function] `deque-pop-all!`  _deque_
###### [!Function] `deque-shift-all!`  _deque_

Returns the whole content of _deque_ by a list, with emptying 
_deque_. If _deque_ is empty, returns an empty list.

The the returning list of `deque-pop-all!` is constructed from the end
of _queue_ and `deque-shift-all!`'s one is constructed from the front
of _queue_.

See also `deque->list` below.


###### [!Function] `deque-front`  _deque_ _:optional_ _fallback_
###### [!Function] `deque-rear`  _deque_ _:optional_ _fallback_

Peek the head or the tail of _deque_ and return the object, 
respectively.

If _deque_ is empty, _fallback_ is returned if give, otherwise an 
error is raised.


###### [!Function] `list->deque`  _list_ _:optional_ _class_ _:rest_ _initargs_

Returns a new deque which content is the elements in _list_, in
the given order.

By default the created deque is a simple deque, but you can create mtdeque
or instance of other subclass `<deque>` by giving the class to the
optional _class_ arguments. The optional _initargs_ arguments are
passed to the constructor of _class_.


###### [!Function] `deque->list`  _deque_

Returns a list whose content is the items in _deque_ in order.
Unlike `deque-shift-all!`, the content of _deque_ remains intact.
The returning list is a copy of the content. So modifying the list won't
affect _deque_.


###### [!Function] `find-in-deque`  _pred_ _deque_

Returns the first item in _deque_ that satisfies a predicate
_pred_.


###### [!Function] `any-in-deque`  _pred_ _deque_

Apply _pred_ on each item in _deque_ until it evaluates true,
and returns that true value. If no item satisfies _pred_, #f is returned.


###### [!Function] `every-in-deque`  _pred_ _deque_

Apply _pred_ on each item in _deque_. If _pred_ returns #f,
stops iteration and returns #f immediately. Otherwise, returns the result of
_pred_ on the last item of _deque_. If the deque is empty, #t is
returned.


###### [!Function] `remove-from-deque!`  _pred_ _deque_

Removes all items in _deque_ that satisfies _pred_. Returns
#t if any item is removed. Otherwise #f.


###### [!Function] `deque-unshift/wait!`  _mtdeque_ _obj_ _:optional_ _timeout_ _timeout-val_
###### [!Function] `deque-push/wait!`  _mtdeque_ _obj_ _:optional_ _timeout_ _timeout-val_
###### [!Function] `deque-shift/wait!`  _mtdeque_ _:optional_ _timeout_ _timeout-val_
###### [!Function] `deque-pop/wait!`  _mtdeque_ _:optional_ _timeout_ _timeout-val_

These synchronising variants work on an mtdeque and make the caller
thread block when the mtdeque has reached its maximum length (for
`deque-unshift/wait!` and `deque-push/wait!`), or the mtdeque is empty
(for `deque-shift/wait!` and `deque-pop/wait!`). The blocked caller
thread is unblocked either the blocking condition is resolved, or the
timeout condition is met.

The optional _timeout_ argument specifies the timeout condition. If it
is #f, those procedure wait indefinitely. If it is a real number, they wait
at least the given number of seconds.

In case the call is blocked then timed out, the value of _timeout-val_ is
returned, which default value is #t.

When `deque-unshift/wait!` and `deque-push/wait!` succeeds without
hitting timeout, they return #t.


