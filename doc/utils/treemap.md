[§2] (util treemap) - Treemap {#util.treemap}
-------------

###### [!Library] `(util treemap)` 

This library provides treemap data structure and operations.

This section uses the following name convention;

_tm_ - treemap



### [§3] Predicate and constructurs

###### [!Function] `treemap?`  _object_

Returns #t when given _object_ is a treemap.

###### [!Function] `make-rb-treemap`  _compare_

_compare_ must be a procedure which accepts 2 arguments and
returns integer indicating the order of given 2 arguments.

Creates red black treemap.


### [§3] Basic operations

###### [!Function] `treemap-ref`  _tm_ _key_ _:optional_ _(fallback_ _#f)_

Returns a value associated with _key_ in _tm_. If it doesn't
exist, then _fallback_ will be returned.


###### [!Function] `treemap-contains?`  _tm_ _key_

Returns #t if there is an entry associated to the given _key_,
otherwise #f.


###### [!Function] `treemap-set!`  _tm_ _key_ _value_

Associate the _value_ to _key_ in _tm_ and returns
unspecified value.

###### [!Function] `treemap-update!`  _tm_ _key_ _proc_ _default_

_proc_ must be a procedure accepts one argument.

Updates the entry value associated to _key_ with the returned value of 
_proc_. If the entry doesn't exist then _default_ will be passed to
the _proc_.


###### [!Function] `treemap-delete!`  _tm_ _key_

Deletes entry of _key_ in _tm_ and returns unspecified value.

###### [!Function] `treemap-clear!`  _tm_

Removes all entries and returns unspecified value.

###### [!Function] `treemap-copy`  _tm_

Returns the copy of given _tm_.

###### [!Function] `treemap-size`  _tm_

Returns the number of entry in the given _tm_.

### [§3] Conversions

###### [!Function] `treemap->alist`  _tm_

Returns an alist of key and value in _tm_.

###### [!Function] `alist->treemap`  _alist_ _compare_

_alist_ must be an alist.
_compare_ must be a procedure which accepts 2 arguments.

Converts _alist_ to a treemap. The car part of an element of _alist_is a key, then cdr part of an element of _alist_ is a value.


### [§3] Keys and values

###### [!Function] `treemap-keys`  _tm_
###### [!Function] `treemap-keys-list`  _tm_

Returns a vector or a list of keys in _tm_, respectively.

###### [!Function] `treemap-values`  _tm_
###### [!Function] `treemap-values-list`  _tm_

Returns a vector or a list of entry values in _tm_, respectively.

###### [!Function] `treemap-entries`  _tm_
###### [!Function] `treemap-entries-list`  _tm_

Returns vectors or lists of entry key and values in _tm_, 
respectively. This procedure returns 2 values. 

### [§3] Iterations

###### [!Function] `treemap-fold`  _kons_ _tm_ _knil_

_kons_ must be a procedure which accepts 3 arguments.

Iterates all keys and values in the given _tm_ and passes them
and the result of _kons_ to _kons_ respectively. The first iteration
of the third argument is _knil_. The procedure returns the result of
all iterations.

Analogous to `fold`.


###### [!Function] `treemap-map`  _proc_ _tm_
###### [!Function] `treemap-for-each`  _proc_ _tm_

_proc_ must be a procedure which accepts 2 arguments.

Iterates all keys and values in the given _tm_ and passes them
to _proc_ respectively.

The `treemap-for-each` returns unspecified value. The 
`treemap-map` returns a list of the _proc_ result.

These procedures are analogous to `for-each` and `map` respectively.


