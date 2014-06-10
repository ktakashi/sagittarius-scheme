@; -*- coding: utf-8 -*-
@subsection[:tag "util.treemap"]{(util treemap) - Treemap utilities}


@define[Library]{@name{(util treemap)}}
@desc{This library provides treemap data structure and operations.

This section uses the following name convention;

@var{tm} - treemap

}

@subsubsection{Predicate and constructurs}

@define[Function]{@name{treemap?} @args{object}}
@desc{Returns #t when given @var{object} is a treemap.}

@define[Function]{@name{make-rb-treemap} @args{compare}}
@desc{@var{compare} must be a procedure which accepts 2 arguments and
returns integer indicating the order of given 2 arguments.

Creates red black treemap.
}

@subsubsection{Basic operations}

@define[Function]{@name{treemap-ref} @args{tm key :optional (fallback #f)}}
@desc{Returns a value associated with @var{key} in @var{tm}. If it doesn't
exist, then @var{fallback} will be returned.
}

@define[Function]{@name{treemap-contains?} @args{tm key}}
@desc{Returns #t if there is an entry associated to the given @var{key},
otherwise #f.
}

@define[Function]{@name{treemap-set!} @args{tm key value}}
@desc{Associate the @var{value} to @var{key} in @var{tm} and returns
unspecified value.}

@define[Function]{@name{treemap-update!} @args{tm key proc default}}
@desc{@var{proc} must be a procedure accepts one argument.

Updates the entry value associated to @var{key} with the returned value of 
@var{proc}. If the entry doesn't exist then @var{default} will be passed to
the @var{proc}.
}

@define[Function]{@name{treemap-delete!} @args{tm key}}
@desc{Deletes entry of @var{key} in @var{tm} and returns unspecified value.}

@define[Function]{@name{treemap-clear!} @args{tm}}
@desc{Removes all entries and returns unspecified value.}

@define[Function]{@name{treemap-copy} @args{tm}}
@desc{Returns the copy of given @var{tm}.}

@define[Function]{@name{treemap-size} @args{tm}}
@desc{Returns the number of entry in the given @var{tm}.}

@subsubsection{Conversions}

@define[Function]{@name{treemap->alist} @args{tm}}
@desc{Returns an alist of key and value in @var{tm}.}

@define[Function]{@name{alist->treemap} @args{alist compare}}
@desc{@var{alist} must be an alist.
@var{compare} must be a procedure which accepts 2 arguments.

Converts @var{alist} to a treemap. The car part of an element of @var{alist}
is a key, then cdr part of an element of @var{alist} is a value.
}

@subsubsection{Keys and values}

@define[Function]{@name{treemap-keys} @args{tm}}
@define[Function]{@name{treemap-keys-list} @args{tm}}
@desc{Returns a vector or a list of keys in @var{tm}, respectively.}

@define[Function]{@name{treemap-values} @args{tm}}
@define[Function]{@name{treemap-values-list} @args{tm}}
@desc{Returns a vector or a list of entry values in @var{tm}, respectively.}

@define[Function]{@name{treemap-entries} @args{tm}}
@define[Function]{@name{treemap-entries-list} @args{tm}}
@desc{Returns vectors or lists of entry key and values in @var{tm}, 
respectively. This procedure returns 2 values. }

@subsubsection{Iterations}

@define[Function]{@name{treemap-fold} @args{kons tm knil}}
@desc{@var{kons} must be a procedure which accepts 3 arguments.

Iterates all keys and values in the given @var{tm} and passes them
and the result of @var{kons} to @var{kons} respectively. The first iteration
of the third argument is @var{knil}. The procedure returns the result of
all iterations.

Analogous to @code{fold}.
}

@define[Function]{@name{treemap-map} @args{proc tm}}
@define[Function]{@name{treemap-for-each} @args{proc tm}}
@desc{@var{proc} must be a procedure which accepts 2 arguments.

Iterates all keys and values in the given @var{tm} and passes them
to @var{proc} respectively.

The @code{treemap-for-each} returns unspecified value. The 
@code{treemap-map} returns a list of the @var{proc} result.

These procedures are analogous to @code{for-each} and @code{map} respectively.
}
