[§2] (util hashtables) - Hashtable utilities {#util.hashtables}
-------------

###### [!Library] `(util hashtables)` 

This library provides extra utilities for hashtable operation.


###### [!Function] `hashtable-for-each`  _proc_ _hashtable_
###### [!Function] `hashtable-map`  _proc_ _hashtable_

_proc_ must be a procedure which accepts 2 arguments.
_hashtable_ must be a hashtable.

Iterates all keys and values in the given _hashtable_ and passes them
to _proc_ respectively.

The `hashtable-for-each` returns unspecified value. The 
`hashtable-map` returns a list of the _proc_ result.

These procedures are analogous to `for-each` and `map` respectively.


###### [!Function] `hashtable-fold`  _kons_ _hashtable_ _knil_

_kons_ must be a procedure which accepts 3 arguments.
_hashtable_ must be a hashtable.

Iterates all keys and values in the given _hashtable_ and passes them
and the result of _kons_ to _kons_ respectively. The first iteration
of the third argument is _knil_. The procedure returns the result of
all iterations.

Analogous to `fold`.


###### [!Function] `hashtable->alist`  _hashtable_

Converts to _hashtable_ to an alist.

###### [!Function] `alist->hashtable`  _alist_ _:key_ _(compare_ _eq?)_ _(hasher_ _symbol-hash)_

Converts _alist_ to hashtable.

The keyword arguments specify how to create the returning hashtable. By
default, it will use `make-eq-hashtable`. If it's specified then it
will use `make-hashtable` to create a hashtable.


