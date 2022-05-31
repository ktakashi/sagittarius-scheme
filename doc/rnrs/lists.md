[§2] List utilities {#rnrs.lists.6}
-------------

###### [!Library] `(rnrs lists (6))` 

[R6RS] The `(rnrs lists (6))`library, which contains various useful
procedures that operate on lists.


###### [!Function] `find`  _proc_ _list_

[R6RS] _Proc_ should accept one argument and return a single value.
_Proc_ should not mutate list. The `find` procedure applies _proc_to the elements of _list_ in order. If _proc_ returns a true value for
an element, find immediately returns that element. If _proc_ returns #f for
all elements of the _list_, find returns #f.


###### [!Function] `for-all`  _pred_ _list1_ _list2_ _..._

[R6RS+] Applies _pred_ across each element of _lists_, and returns
#f as soon as _pred_ returns #f. If all application of _pred_ return a
non-false value, `for-all` returns the last result of the applications.


###### [!Function] `exists`  _pred_ _list1_ _list2_ _..._

[R6RS+] Applies _pred_ across each element of _lists_, and returns
as soon as _pred_ returns a non-false value. The return value of any is the
non-false value _pred_ returned. If lists are exhausted before _pred_returns a non-false value, #f is returned.

Note: R6RS requires the same length list for `for-all` and `exists`.
On Sagittarius, however, these can accept different length list and it will
finish to process when the shortest list is finish to process.


###### [!Function] `filter`  _proc_ _list_
###### [!Function] `partition`  _proc_ _list_

[R6RS] _Proc_ should accept one argument and return a single value.

The `filter` procedure applies _proc_ to each element of _list_ and
returns a list of the elements of _list_ for which _proc_ returned a true
value. The `partition` procedure also applies _proc_ to each element of
_list_, but returns two values, the first one a list of the elements of _list_for which _proc_ returned a true value, and the second a list of the elements
of _list_ for which _proc_ returned #f. In both cases, the elements of the
result list(s) are in the same order as they appear in the input list. If multiple
returns occur from `filter` or `partitions`, the return values returned
by earlier returns are not mutated.


###### [!Function] `fold-left`  _combine_ _nil_ _list1_ _list2_ _..._

[R6RS+] _Combine_ must be a procedure. It should accept one more argument
than there are _lists_ and return a single value. It should not mutate the
_list_ arguments. The `fold-left` procedure iterates the _combine_procedure over an accumulator value and the elements of the _lists_ from left
to right, starting with an accumulator value of _nil_. More specifically,
`fold-left` returns _nil_ if the _lists_ are empty. If they are not
empty, _combine_ is first applied to _nil_ and the respective first
elements of the _lists_ in order. The result becomes the new accumulator
value, and _combine_ is applied to the new accumulator value and the respective
next elements of the _list_. This step is repeated until the end of the
_list_ is reached; then the accumulator value is returned.


###### [!Function] `fold-right`  _combine_ _nil_ _list1_ _list2_ _..._

[R6RS+] _Combine_ must be a procedure. It should accept one more argument
than there are _lists_ and return a single value. _Combine_ should not
mutate the _list_ arguments. The `fold-right` procedure iterates the
_combine_ procedure over the elements of the _lists_ from right to left
and an accumulator value, starting with an accumulator value of _nil_. More
specifically, `fold-right` returns _nil_ if the lists are empty. If they
are not empty, _combine_ is first applied to the respective last elements of
the _lists_ in order and _nil_. The result becomes the new accumulator
value, and _combine_ is applied to the respective previous elements of the
_lists_ and the new accumulator value. This step is repeated until the beginning
of the _list_ is reached; then the accumulator value is returned.

Note: R6RS requires the same length list for `fold-left` and `fold-right`.
On Sagittarius, however, these can accept different length list and it will finish
to process when the shortest list is finish to process.


###### [!Function] `remp`  _proc_ _list_
###### [!Function] `remove`  _obj_ _list_
###### [!Function] `remv`  _obj_ _list_
###### [!Function] `remq`  _obj_ _list_

[R6RS] Proc should accept one argument and return a single value. _Proc_should not mutate _list_.

Each of these procedures returns a list of the elements of _list_ that do not
satisfy a given condition. The `remp` procedure applies _proc_ to each
element of _list_ and returns a list of the elements of _list_ for which
_proc_ returned #f. The `remove`, `remv`, and `remq` procedures
return a list of the elements that are not _obj_. The `remq` procedure
uses `eq?` to compare _obj_ with the elements of _list_, while
`remv` uses `eqv?` and `remove` uses `equal?`. The elements
of the result list are in the same order as they appear in the input list. If
multiple returns occur from `remp`, the return values returned by earlier
returns are not mutated.


###### [!Function] `memp`  _proc_ _list_
###### [!Function] `member`  _obj_ _list_ _:optional_ _=_
###### [!Function] `memv`  _obj_ _list_
###### [!Function] `memq`  _obj_ _list_

[R6RS+] _Proc_ should accept one argument and return a single value.
_Proc_ should not mutate _list_.

These procedures return the first sublist of _list_ whose car satisfies a
given condition, where the sublists of _lists_ are the lists returned by
`(list-tail _list_ _k_)` for _k_ less than the length of _list_.
The `memp` procedure applies _proc_ to the cars of the sublists of 
_list_ until it finds one for which _proc_ returns a true value. The
`member`, `memv`, and `memq` procedures look for the first
occurrence of _obj_. If list does not contain an element satisfying the
condition, then #f (not the empty list) is returned. The `member` procedure
uses `equal?` or if _=_ is given use it to compare _obj_ with the
elements of _list_, while `memv` uses `eqv?` and `memq` uses
`eq?`.


###### [!Function] `assp`  _proc_ _alist_
###### [!Function] `assc`  _obj_ _alist_ _:optional_ _=_
###### [!Function] `assv`  _obj_ _alist_
###### [!Function] `assq`  _obj_ _alist_

[R6RS+] _Alist_ (for "association list") should be a list of pairs.
_Proc_ should accept one argument and return a single value. _Proc_should not mutate _alist_.

These procedures find the first pair in _alist_ whose car field satisfies
a given condition, and returns that pair without traversing _alist_ further.
If no pair in _alist_ satisfies the condition, then #f is returned. The 
`assp` procedure successively applies _proc_ to the car fields of
_alist_ and looks for a pair for which it returns a true value. The
`assoc`, `assv`, and `assq` procedures look for a pair that has
_obj_ as its car. The `assoc` procedure uses `equal?` or if _=_is given use it to compare _obj_ with the car fields of the pairs in _alist_,
while `assv` uses `eqv?` and `assq` uses `eq?`.

Note: `member` and `assoc` procedures are the same behaviour as SRFI-1.


###### [!Function] `cons*`  _obj1_ _obj2_ _..._

[R6RS] Like `list`, but the last argument provides the tail of the
constructed list.

