@; -*- coding: utf-8 -*-
@subsection[:tag "util.hashtables"]{(util hashtables) - Hashtable utilities}


@define[Library]{@name{(util hashtables)}}
@desc{This library provides extra utilities for hashtable operation.
}

@define[Function]{@name{hashtable-for-each} @args{proc hashtable}}
@define[Function]{@name{hashtable-map} @args{proc hashtable}}
@desc{@var{proc} must be a procedure which accepts 2 arguments.
@var{hashtable} must be a hashtable.

Iterates all keys and values in the given @var{hashtable} and passes them
to @var{proc} respectively.

The @code{hashtable-for-each} returns unspecified value. The 
@code{hashtable-map} returns a list of the @var{proc} result.

These procedures are analogous to @code{for-each} and @code{map} respectively.
}

@define[Function]{@name{hashtable-fold} @args{kons hashtable knil}}
@desc{@var{kons} must be a procedure which accepts 3 arguments.
@var{hashtable} must be a hashtable.

Iterates all keys and values in the given @var{hashtable} and passes them
and the result of @var{kons} to @var{kons} respectively. The first iteration
of the third argument is @var{knil}. The procedure returns the result of
all iterations.

Analogous to @code{fold}.
}

@define[Function]{@name{hashtable->alist} @args{hashtable}}
@desc{Converts to @var{hashtable} to an alist.}

@define[Function]{@name{alist->hashtable}
 @args{alist :key (compare eq?) (hasher symbol-hash)}}
@desc{Converts @var{alist} to hashtable.

The keyword arguments specify how to create the returning hashtable. By
default, it will use @code{make-eq-hashtable}. If it's specified then it
will use @code{make-hashtable} to create a hashtable.
}
