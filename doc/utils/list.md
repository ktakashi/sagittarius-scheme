[§2] (util list) - Extra list utility library {#util.list}
-------------

###### [!Library] `(util list)` 

This library provides extra list utility procedures.


###### [!Function] `for-each-with-index`  _proc_ _list1_ _list2_ _..._
###### [!Function] `map-with-index`  _proc_ _list1_ _list2_ _..._

Like `for-each` and `map`, expect _proc_ receives the index
as the first argument.

``(map-with-index list '(a b c) '(e f g))`` => ``((0 a e) (1 b f) (2 c g))``



###### [!Function] `intersperse`  _item_ _list_

Inserts _item_ between elements in the _list_.

``(intersperse '+ '(1 2 3))`` => ``(1 + 2 + 3)``

``(intersperse '+ '(1))`` => ``(1)``

``(intersperse '+ '())`` => ``()``



###### [!Function] `slices`  _list_ _k_ _:optional_ _fill?_ _padding_

Splits _list_ into the sublists (slices) where the length of each
slice is _k_. If the length of _list_ is not multiple of _k_, the
last slice is dealt in the same way as `take*`; this is, it is shorter than
_k_ by default, or added _padding_ if _fill?_ is true.

``(slices '(a b c d e f g) 3)`` => ``((a b c) (d e f) (g))``

``(slices '(a b c d e f g) 3 #t 'z)`` => ``((a b c) (d e f) (g z z))``



###### [!Function] `split-at*`  _list_ _k_ _:optional_ _(fill?_ _#t)_ _(padding_ _#f)_

Splits the list _list_ at index _k_. This is more tolerant version
of `split-at` defined in SRFI-1 library. Returns the results of
`take*` and `drop*`.

``(split-at* '(a b c d) 6 #t 'z)`` => ``(a b c d z z) and ()``



###### [!Function] `take*`  _list_ _k_ _:optional_ _(fill?_ _#f)_ _(padding_ _#f)_
###### [!Function] `drop*`  _list_ _k_

More tolerant version of `take` and `drop` defined in SRFI-1
library. These won't raise an error when _k_ is larger than the size of the
given list.

If the list is shorter than _k_ elements, `take*` returns a copy of
_list_ by default. If _fill?_ is true, _padding_ is added to the
result to make its length _k_.

On the other hand, `drop*` just returns as empty list when the input list
is shorter than _k_ elements.

``(take* '(a b c d) 3)`` => ``(a b c)``

``(take* '(a b c d) 6)`` => ``(a b c d)``

``(take* '(a b c d) 6 #t)`` => ``(a b c d #f #f)``

``(take* '(a b c d) 6 #t 'z)`` => ``(a b c d z z)``

``(drop* '(a b c d) 3)`` => ``(d)``

``(drop* '(a b c d) 5)`` => ``()``



###### [!Macro] `cond-list`  _clause_ _..._

Construct a list by conditionally adding entries. Each _clause_ must
have a test and expressions. When its test yields true, then result of
associated expression is used to construct the resulting list. When the test 
yields false, nothing is inserted.

_Clause_ must either one of the following form:

_(test expr ...)_
: _Test_ is evaluated, and when it is true, _expr ..._ are
  evaluated, and the return value becomes a part of the result. If no
  _expr_ is given, the result of test is used if it is not false.

_(test => proc)_
: _Test_ is evaluated, and if it is true, _proc_ is called with
  the value, and the return value is used to construct the result

_(test   expr ...)_
: Like `(test expr ...)`, except that the result of the last
  _expr_ must be a list, and it is spliced into the resulting list,
  like unquote-splicing.

_(test =>   proc)_
: Like `(test => proc)`, except that the result of the last
  _proc_ must be a list, and it is spliced into the resulting list,
  like unquote-splicing.

``````````scheme
(let ((alist '((x 3) (y -1) (z 6))))
 (cond-list ((assoc 'x alist) 'have-x)
            ((assoc 'w alist) 'have-w)
            ((assoc 'z alist) => cadr)))
``````````
=> ``(have-x 6)``

``````````scheme
(let ((x 2) (y #f) (z 5))
  (cond-list (x   `(:x ,x))
             (y   `(:y ,y))
             (z   `(:z ,z))))
``````````
=> ``(:x 2 :z 5)``



