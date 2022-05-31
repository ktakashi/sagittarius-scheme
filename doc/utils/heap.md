[§2] (util heap) - Heap {#util.heap}
-------------

###### [!Library] `(util heap)` 

This library provides heap data structure and its operations. The
implementation of heap is Fibonacci heap. Running time of heap operations
are followings;


- Insert(key, data): O(1)
- FindMin(): O(1)
- DeleteMin(): Amortized O(log n)
- Delete(node): Amortized O(log n)
- DecreaseKey(node): Amortized O(1)
- Merge(heap1, heap2): O(1)
- Search(key): O(n)



### [§3] Constructors, predicates and accessors

###### [!Class] `<heap>` 

A class of Fibonacci heap.

###### [!Function] `heap?`  _obj_

Return #t if _obj_ is a heap, otherwise #f.

###### [!Function] `make-heap`  _compare_

_compare_ must be a procedure which takes 2 argument and
returns an exact integer indicates the order of given arguments.

Creates a heap.


###### [!Function] `copy-heap`  _heap_

Creates copy of _heap_.

###### [!Function] `alist->heap`  _compare_ _alist_

_compare_ must be a procedure which takes 2 argument and
returns an exact integer indicates the order of given arguments.

Creates a heap whose keys and values are car/cdr part of _alist_.


###### [!Function] `heap-empty?`  _heap_

Returns #t if _heap_ is empty, otherwise #f.

###### [!Function] `heap-size`  _heap_

Returns the number of entries of _heap_.

###### [!Function] `heap-compare`  _heap_

Returns comparison procedure of _heap_.

###### [!Function] `heap-entry?`  _obj_

Returns #t if _obj_ is a heap entry, otherwise #f.

###### [!Function] `heap-entry-key`  _entry_
###### [!Function] `heap-entry-value`  _entry_

Returns key and value of _entry_, respectively.

###### [!Function] `heap-entry-value-set!`  _entry_ _value_

Sets _value_ as _entry_'s value.

### [§3] Heap operations

###### [!Function] `heap-min`  _heap_

Returns the smallest entry of _heap_. If _heap_ is empty,
then #f is returned.

To get the key and value from the returning entry, use `heap-entry-key`and `heap-entry-value` procedures, respectively.


###### [!Function] `heap-set!`  _heap_ _key_ _value_

Inserts an entry whose key is _key_, value is _value_ and
returns the entry.


###### [!Function] `heap-extract-min!`  _heap_

Removes smallest entry and returns it. It is an error if _heap_is empty.


###### [!Function] `heap-delete!`  _heap_ _entry/key_

Removes target _entry/key_ from _heap_ and returns the
removed entry.

If _entry/key_ is an entry then this operation is done in amortized
O(log n). If not, then O(n).

NOTE: If _entry/key_ is an entry, then it must exist in _heap_.
However the procedure won't check so it is user's responsibility to
make sure.


###### [!Function] `heap-delete!`  _heap_ _entry/key_ _new-key_

Change the key value of given _entry/key_ to _new-key_. The
_new-key_ must be smaller than current key in sense of the returning
value of `heap-compare`, otherwise it's an error.

If _entry/key_ is an entry then this operation is done in amortized
O(log n). If not, then O(n).

NOTE: If _entry/key_ is an entry, then it must exist in _heap_.
However the procedure won't check so it is user's responsibility to
make sure.


###### [!Function] `heap-clear!`  _heap_

Clears all entry of _heap_ and returns _heap_.

###### [!Function] `heap-merge!`  _heap1_ _heap2_

Merge _heap2_ into _heap1_ and empty it. Then returns _heap1_.

This procedure changes both heaps destructively, if you want to kepp
_heap2_ intact, use `merge-heaps` or `merge-heaps!` instead.


###### [!Function] `merge-heaps`  _heap1_ _heap2_ _more_ _..._
###### [!Function] `merge-heaps!`  _heap1_ _heap2_ _more_ _..._

Merges heap and returns merged heap.

If the first procedure is used then the returning heap is freshly created
according to _heap1_. The second form merged the rest of heaps to
_heap1_.

The running time of these procedures is O(nm) where m is number of heaps to
merge.


###### [!Function] `heap-ref`  _heap_ _key_ _:optional_ _(fallback_ _#f)_

Returns entry value associated with _key_ in _heap_. If there
is no entry, then _fallback_ is returned.

The running time of this procedures is O(n).


###### [!Function] `heap-update!`  _heap_ _key_ _proc_ _default_

_proc_ must be a procedure accepts one argument.

Updates the entry value associated to _key_ with the returned value of 
_proc_. If the entry doesn't exist then _default_ will be passed to
the _proc_.

The running time of this procedures is O(n).


###### [!Function] `heap-search`  _heap_ _key_ _:optional_ _finish_

Searches the entry associated to _key_ and returns it. If there is
no entry then #f is returned.

If optional argument _finish_ given then it must be a procedure which
takes one argument. The procedure is called when search process is finished.
The given argument is either an entry of #f.


