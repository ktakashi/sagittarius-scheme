@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.sorting.6"]{Sorting}

@define[Library]{@name{(rnrs sorting (6))}}
@desc{The @code{(rnrs sorting (6))}library provides procedures for sorting lists
and vectors.}

@define[Function]{@name{list-sort} @args{proc list}}
@define[Function]{@name{vector-sort} @args{proc vector}}
@desc{[R6RS] @var{Proc} should accept any two elements of @var{list} or
@var{vector}, and should not have any side effects. @var{Proc} should return a
true value when its first argument is strictly less than its second, and #f
otherwise.

The @code{list-sort} and @code{vector-sort} procedures perform a stable sort
of @var{list} or @var{vector} in ascending order according to @var{proc}, without
changing @var{list} or @var{vector} in any way. The @code{list-sort} procedure
returns a list, and @code{vector-sort} returns a vector. The results may be
@code{eq?} to the argument when the argument is already sorted, and the result
of @code{list-sort} may share structure with a tail of the original list. The
sorting algorithm performs O(n lg n) calls to @var{proc} where n is the length
of @var{list} or @var{vector}, and all arguments passed to @var{proc} are elements
of the @var{list} or @var{vector} being sorted, but the pairing of arguments and
the sequencing of calls to @var{proc} are not specified. If multiple returns
occur from @code{list-sort} or @code{vector-sort}, the return values returned by
earlier returns are not mutated.
}

@define[Function]{@name{vector-sort!} @args{proc vector}}
@desc{[R6RS] @var{Proc} should accept any two elements of the @var{vector}, and
should not have any side effects. @var{Proc} should return a true value when its
first argument is strictly less than its second, and #f otherwise.

The @code{vector-sort!} procedure destructively sorts vector in ascending order
according to @var{proc}. The sorting algorithm performs O(n2) calls to @var{proc}
where n is the length of @var{vector}, and all arguments passed to @var{proc}
are elements of the @var{vector} being sorted, but the pairing of arguments and
the sequencing of calls to @var{proc} are not specified. The sorting algorithm
may be unstable. The procedure returns unspecified values.
}
