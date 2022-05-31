[§2] Sorting {#rnrs.sorting.6}
-------------

###### [!Library] `(rnrs sorting (6))` 

The `(rnrs sorting (6))`library provides procedures for sorting lists
and vectors.

###### [!Function] `list-sort`  _proc_ _list_
###### [!Function] `vector-sort`  _proc_ _vector_ _:optional_ _(start_ _0)_ _(end_ _(vector-length_ _vector))_

[R6RS+][SRFI-132] _Proc_ should accept any two elements of 
_list_ or _vector_. _Proc_ should return a true value when
its first argument is strictly less than its second, and #f otherwise.

The `list-sort` and `vector-sort` procedures perform a stable sort
of _list_ or _vector_ in ascending order according to _proc_, without
changing _list_ or _vector_ in any way. The `list-sort` procedure
returns a list, and `vector-sort` returns a vector. The results may be
`eq?` to the argument when the argument is already sorted, and the result
of `list-sort` may share structure with a tail of the original list. The
sorting algorithm performs O(n lg n) calls to _proc_ where n is the length
of _list_ or _vector_, and all arguments passed to _proc_ are 
elements of the _list_ or _vector_ being sorted, but the pairing of
arguments and the sequencing of calls to _proc_ are not specified. If 
multiple returns occur from `list-sort` or `vector-sort`, the 
return values returned by earlier returns are not mutated.

If the optional argument _start_ and _end_ for `vector-sort`is specified, then the sorting range is restricted by the given _start_(inclusive) and _end_ (exclusive).


###### [!Function] `vector-sort!`  _proc_ _vector_ _:optional_ _(start_ _0)_ _(end_ _(vector-length_ _vector))_

[R6RS+][SRFI-132] _Proc_ should accept any two elements of 
the _vector_, and should not have any side effects. _Proc_ 
should return a true value when its first argument is strictly less
than its second, and #f otherwise.

The `vector-sort!` procedure destructively sorts vector in 
ascending order according to _proc_. 

If the optional argument _start_ and _end_ is specified, then
the sorting range is restricted by the given _start_ (inclusive)
and _end_ (exclusive).


