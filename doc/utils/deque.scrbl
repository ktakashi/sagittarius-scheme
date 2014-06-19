@; -*- coding: utf-8 -*-
@subsection[:tag "util.deque"]{(util deque) - Deque}

@define[Library]{@name{(util deque)}}
@desc{This library provides deque (double-ended queue) data structure and
its operations.

You can create a simple deque, which is not thread-safe, or an MT deque, a
thread-safe deque. Basic deque operations work on both type of deques. When
a mtdeque is passed to the procedures listed in this section, each operation
is done in atomic way, unless otherwise noted.

There are also a set of procedures for mtdeques that can be used for thread
synchronisation; for example, you can let the consumer thread block if an
mtdeque is empty, and/or the producer thread block if the number of items in
the mtdeque reaches a specified limit. Using these procedures allows the
program to use an mtdeque as a @var{channel}.

NOTE: @secref["util.queue"]{(util queue)} is implemented using this library.
}

@define[Class]{@name{<deque>}}
@desc{A class of simple deque. }

@define[Class]{@name{<mtdeque>}}
@desc{A class of mtdeque. Inherits @code{<deque>}.}

@define[Function]{@name{make-deque}}
@desc{Creates and return an empty simple deque.}

@define[Function]{@name{make-mtdeque} @args{:key max-length}}
@desc{Creates and return an empty mtdeque.

The keyword argument @var{max-length} specifies the maximum entry count of
the deque. Negative number indicates unlimited number of entry. If the given
number is zero then the deque cannot hold any item.
}

@define[Function]{@name{deque?} @args{obj}}
@desc{Returns #t if @var{obj} is a deque (either a simple deque or an mtdeque).}

@define[Function]{@name{deque?} @args{obj}}
@desc{Returns #t if @var{obj} is an mtdeque.}

@define[Function]{@name{deque-empty?} @args{deque}}
@desc{Returns #t if @var{deque} is an empty deque.}

@define[Function]{@name{deque-length} @args{deque}}
@desc{Returns the number of the items in the deque.}

@define[Function]{@name{mtdeque-max-length} @args{mtdeque}}
@desc{Returns the maximum number of items @var{mtdeque} can hold. 
#f indicates unlimited.}

@define[Function]{@name{mtdeque-room} @args{mtdeque}}
@desc{Returns the number of elements @var{mtdeque} can accept at this moment
before it hits its maximum length. If the deque has unlimited capacity then
the procedure returns @code{+inf.0}.}

@define[Function]{@name{copy-deque} @args{deque}}
@desc{Returns a copy of @var{deque}.}

@; push
@define[Function]{@name{deque-push!} @args{deque obj more-objs @dots{}}}
@desc{Adds @var{obj} to the end of  @var{deque}. You may give more than
one object, and each of them are pushed in order.

If @var{deque} is an mtdeque, all the objects are pushed atomically; no
other objects from other threads can be inserted between the objects given
to a single @code{deque-push!} call. Besides, if the value of the result of
@code{mtdeque-max-length} is positive, and adding @var{objs} makes the
number of element in @var{deque} exceeds it, an error is raised and 
@var{deque} won't be modified. (If the maximum length is zero, this procedure
always fail. Use @code{deque-push/wait!} below.)
}

@define[Function]{@name{deque-unshift!} @args{deque obj more-objs @dots{}}}
@desc{Adds @var{obj} to in front of @var{deque}. You may give more than
one object, and each of them are pushed in order.

Like @code{deque-push!}, when @var{deque} is an mtdeque, all objects are added
atomically, and the value of max length is checked. See @code{deque-push!} above
for more detail.

The name unshift is taken from Perl.
}

@define[Function]{@name{deque-push-unique!}
 @args{deque eq-proc obj more-objs @dots{}}}
@define[Function]{@name{deque-unshift-unique!}
 @args{deque eq-proc obj more-objs @dots{}}}
@desc{Like @code{deque-push!} and @code{deque-unshift!}, respectively, except
that these don't modify @var{deque} if it already contains @var{objs} (elements
are compared by two-argument procedure @var{eq-proc}).

When @var{deque} is an mtdeque, all objects are added atomically, and the max
length is checked. See @code{deque-push!} above for the detail.
}

@define[Function]{@name{deque-pop!} @args{deque :optional fallback}}
@define[Function]{@name{deque-shift!} @args{deque :optional fallback}}
@desc{Take one object from the end or the front of @var{deque}, respectively,
and return it.

If @var{deque} is empty, @var{fallback} is returned if give, otherwise an 
error is raised.

If @var{deque} is mtdeque and its max length is zero, then the deque is
always empty. Use @code{deque-pop/wait!} or @code{deque-shift/wait!} to use
such a deque as a synchronisation device.

The name shift is take from Perl.
}

@define[Function]{@name{deque-pop-all!} @args{deque}}
@define[Function]{@name{deque-shift-all!} @args{deque}}
@desc{Returns the whole content of @var{deque} by a list, with emptying 
@var{deque}. If @var{deque} is empty, returns an empty list.

The the returning list of @code{deque-pop-all!} is constructed from the end
of @var{queue} and @code{deque-shift-all!}'s one is constructed from the front
of @var{queue}.

See also @code{deque->list} below.
}

@define[Function]{@name{deque-front} @args{deque :optional fallback}}
@define[Function]{@name{deque-rear} @args{deque :optional fallback}}
@desc{Peek the head or the tail of @var{deque} and return the object, 
respectively.

If @var{deque} is empty, @var{fallback} is returned if give, otherwise an 
error is raised.
}

@; convert
@define[Function]{@name{list->deque} @args{list :optional class :rest initargs}}
@desc{Returns a new deque which content is the elements in @var{list}, in
the given order.

By default the created deque is a simple deque, but you can create mtdeque
or instance of other subclass @code{<deque>} by giving the class to the
optional @var{class} arguments. The optional @var{initargs} arguments are
passed to the constructor of @var{class}.
}

@define[Function]{@name{deque->list} @args{deque}}
@desc{Returns a list whose content is the items in @var{deque} in order.
Unlike @code{deque-shift-all!}, the content of @var{deque} remains intact.
The returning list is a copy of the content. So modifying the list won't
affect @var{deque}.
}

@; iterate
@define[Function]{@name{find-in-deque} @args{pred deque}}
@desc{Returns the first item in @var{deque} that satisfies a predicate
@var{pred}.
}

@define[Function]{@name{any-in-deque} @args{pred deque}}
@desc{Apply @var{pred} on each item in @var{deque} until it evaluates true,
and returns that true value. If no item satisfies @var{pred}, #f is returned.
}

@define[Function]{@name{every-in-deque} @args{pred deque}}
@desc{Apply @var{pred} on each item in @var{deque}. If @var{pred} returns #f,
stops iteration and returns #f immediately. Otherwise, returns the result of
@var{pred} on the last item of @var{deque}. If the deque is empty, #t is
returned.
}

@define[Function]{@name{remove-from-deque!} @args{pred deque}}
@desc{Removes all items in @var{deque} that satisfies @var{pred}. Returns
#t if any item is removed. Otherwise #f.
}

@define[Function]{@name{deque-unshift/wait!}
 @args{mtdeque obj :optional timeout timeout-val}}
@define[Function]{@name{deque-push/wait!}
 @args{mtdeque obj :optional timeout timeout-val}}
@define[Function]{@name{deque-shift/wait!}
 @args{mtdeque :optional timeout timeout-val}}
@define[Function]{@name{deque-pop/wait!}
 @args{mtdeque :optional timeout timeout-val}}
@desc{These synchronising variants work on an mtdeque and make the caller
thread block when the mtdeque has reached its maximum length (for
@code{deque-unshift/wait!} and @code{deque-push/wait!}), or the mtdeque is empty
(for @code{deque-shift/wait!} and @code{deque-pop/wait!}). The blocked caller
thread is unblocked either the blocking condition is resolved, or the
timeout condition is met.

The optional @var{timeout} argument specifies the timeout condition. If it
is #f, those procedure wait indefinitely. If it is a real number, they wait
at least the given number of seconds.
@; TODO <time> object.

In case the call is blocked then timed out, the value of @var{timeout-val} is
returned, which default value is #t.

When @code{deque-unshift/wait!} and @code{deque-push/wait!} succeeds without
hitting timeout, they return #t.
}
