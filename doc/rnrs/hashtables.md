[§2] Hashtables {#rnrs.hashtables.6}
-------------

The `(rnrs hashtables (6))`library provides a set of operations on
hashtables. A _hashtable_ is a data structure that associates keys with
values. Any object can be used as a key, provided a _hash function_ and a
suitable _equivalence function_ is available. A hash function is a
procedure that maps keys to exact integer objects. It is the programmer's
responsibility to ensure that the hash function is compatible with the
equivalence function, which is a procedure that accepts two keys and returns
true if they are equivalent and #f otherwise. Standard hashtables for arbitrary
objects based on the `eq?` and `eqv?` predicates are provided. Also,
hash functions for arbitrary objects, strings, and symbols are provided.

This section uses the _hashtable_ parameter name for arguments that must be
hashtables, and the _key_ parameter name for arguments that must be
hashtable keys.

###### [!Library] `(rnrs hashtable (6))` 

[R6RS] This library exports a set of operations on hashtables.

### [§3] Constructors

###### [!Function] `make-eq-hashtable`  _:optional_ _k_ _weakness_
###### [!Function] `make-eqv-hashtable`  _:optional_ _k_ _weakness_

[R6RS+] Returns a newly allocated mutable hashtable that accepts arbitrary
objects as keys, and compares those keys with `eq?`(`make-eq-hashtable`) or `eqv?` (`make-eqv-hashtable`).

If optional argument _k_ is given, it must be exact non-negative integer or
`#f`. If it's `#f`, then the procedure picks up default initial
capacity, otherwise the initial capacity of the hashtable is set to
approximately _k_ elements.

If optional argument _weakness_ is given, then it must be one of the
symbols `key`, `value` or `both`, or `#f`. If the value is
one of the symbols, then the procedure creates weak hashtable of given symbol's
weakness. If the symbol is `key`, then entries whose keys are refered only
from this hashtable might be removed when garbage collection is
occurred. `value` is for entry values. `both` is for both.



###### [!Function] `make-hashtable`  _hash-function_ _equiv_ _:optional_ _k_ _weakness_

[R6RS] _Hash-function_ and _equiv_ must be procedures. 

_Hash-function_ should accept a key as an argument and should return a
non-negative exact integer object. _Equiv_ should accept two keys as
arguments and return a single value.

The `make-hashtable` procedure returns a newly allocated mutable hashtable
using _hash-function_ as the hash function and _equiv_ as the
equivalence function used to compare keys. 

If optional argument _k_ and _weakness_ are the same as
`make-eq-hashtable` and `make-eqv-hashtable`.



### [§3] Procedures

###### [!Function] `hashtable?`  _obj_

[R6RS] Returns #t if _obj_ is a hashtable, #f otherwise.

###### [!Function] `hashtable-size`  _hashtable_

[R6RS] Returns the number of keys contained in _hashtable_ as an exact
integer object.

###### [!Function] `hashtable-ref`  _hashtable_ _key_ _:optional_ _(default_ _#f)_

[R6RS+] Returns the value in _hashtable_ associated with _key_. If
_hashtable_ does not contain an association for _key_, _default_ is
returned.

Since Sagittarius version 0.3.4, this procedure's _default_ argument is
optional to implement SRFI-17 efficiently.


###### [!Function] `hashtable-set!`  _hashtable_ _key_ _obj_

[R6RS] Changes _hashtable_ to associate _key_ with _obj_,
adding a new association or replacing any existing association for _key_,
and returns unspecified values.


###### [!Function] `hashtable-delete!`  _hashtable_ _key_

[R6RS] Removes any association for _key_ within _hashtable_ and
returns unspecified values.


###### [!Function] `hashtable-contains?`  _hashtable_ _key_

[R6RS] Returns #t if _hashtable_ contains an association for _key_, #f
otherwise.

Note: On Sagittarius, `hashtable-ref` and `hashtable-contains?` do
not make any difference fot the performance.



###### [!Function] `hashtable-update!`  _hashtable_ _key_ _proc_ _default_
desc{[R6RS] _Proc_ should accept one argument, should return a single
value.

The `hashtable-update!` procedure applies _proc_ to the value in
_hashtable_ associated with _key_, or to _default_ if
_hashtable_ does not contain an association for _key_. The
_hashtable_ is then changed to associate _key_ with the value returned
by _proc_.
}

###### [!Function] `hashtable-copy`  _hashtable_ _:optional_ _mutable_

[R6RS] Returns a copy of _hashtable_. If the _mutable_ argument
is provided and is true, the returned hashtable is mutable; otherwise it is
immutable.


###### [!Function] `hashtable-clear`  _hashtable_ _:optional_ _k_

[R6RS] Removes all associations from _hashtable_ and returns
unspecified values.

If a second argument is given, the current capacity of the hashtable is reset
to approximately _k_ elements.  

###### [!Function] `hashtable-keys`  _hashtable_
###### [!Function] `hashtable-entries`  _hashtable_

[R6RS] Returns a vector of all keys or entries in _hashtable_,
respectively. The order of the vector is unspecified.


### [§3] Inspection

###### [!Function] `hashtable-equivalence-function`  _hashtable_
###### [!Function] `hashtable-hash-function`  _hashtable_

[R6RS] Returns the equivalence or hash function used by _hashtable_respectively.


###### [!Function] `hashtable-mutable?`  _hashtable_

[R6RS] Returns #t if _hashtable_ is mutable, otherwise #f.

### [§3] Hash functions

###### [!Function] `equal-hash`  _obj_
###### [!Function] `symbol-hash`  _symbol_

[R6RS] Returns hash value of given argument. Each procedures return
the hash values suitable for `equal?` and symbols.


###### [!Function] `string-hash`  _string_ _:optional_ _bound_ _start_ _end_
###### [!Function] `string-ci-hash`  _string_ _:optional_ _bound_ _start_ _end_

[R6RS+][SRFI-13] Returns hash value of given argument. Each procedures
 return the hash values suitable for `string=?` and `string-ci=?`.

If the optional argument _start_ and _end_ is given, then the given
string will be substringed with _start_ and _end_.

If the optional argument _bound_ is given, it must be exact integer and hash
function will also use the given value.


