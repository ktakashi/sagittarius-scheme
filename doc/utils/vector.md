[§2] (util vector) - Extra vector utility library {#util.vector}
-------------

###### [!Library] `(util vector)` 

This library provides extra vector utility procedures which is not
provided neither `(srfi :43 vectors)` nor `(srfi :133 vectors)`.


###### [!Function] `vector-filter`  _pred_ _vec_
###### [!Function] `vector-remove`  _pred_ _vec_

Returns newly allocated vector which contains the elements from the
given _vec_ satisfied the given _pref_.

The `vector-filter` uses the elements which _pred_ returns true value.

The `vector-remove` removes the elements which _pred_ returns
true value.


###### [!Function] `vector-find`  _pred_ _vec_

Returns the element of the given _vec_ satisfies the _pred_or #f.

