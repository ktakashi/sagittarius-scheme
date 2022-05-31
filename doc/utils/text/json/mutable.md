[§2] (text json mutable) - Mutable JSON {#text.json.mutable}
-------------

###### [!Library] `(text json mutable)` 

This library provides an mutable JSON representation.

The following example shows how to use:

``````````scheme
(import (rnrs)
        (text json mutable))

(define json '#(("key" . "value")))
(define mutable-json (json->mutable-json json))

(mutable-json-object-set! mutable-json "key" '#(("value" . "as object")))

(mutable-json->json mutable-json)
;; -> #(("key" . #(("value" . "as object"))))
``````````

###### [!Function] `json->mutable-json`  _json_

Converts the given _json_ to a mutable JSON.

###### [!Function] `mutable-json->json`  _mutable-json_

Converts the given _mutable-json_ to a JSON.

###### [!Function] `mutable-json?`  _obj_

Returns `#t` if the given _obj_ is a mutable JSON,
otherwise `#f`.

Mutable JSON object consists with 2 containers

- Mutable JSON object
- Mutable JSON array

### [§3] Mutable JSON object

Mutable JSON object is a JSON object. The difference is a mutable JSON object
doesn't allow to have multiple identical keys while JSON object allows it.

###### [!Function] `mutable-json-object?`  _obj_

Returns `#t` if the given _obj_ is a mutable JSON object,
otherwise `#f`.

###### [!Function] `mutable-json-object-set!`  _mutable-json-object_ _key_ _value_

Associates the given _key_ and _value_ to the _mutable-json_.

###### [!Function] `mutable-json-object-merge!`  _mutable-json-object_ _mutable-json-object1_ _..._ _mutable-json-object*_

Merges the _mutable-json-object1_ and _mutable-json-object\*_ into
_mutable-json-object_.


###### [!Function] `mutable-json-object-delete!`  _mutable-json-object_ _key_

Remves the given _key_ from the _mutable-json-object_.

###### [!Function] `mutable-json-object-contains?`  _mutable-json-object_ _key_

Returns `#t` if the given _key_ exists in the
_mutable-json-object_, otherwise `#f`.

###### [!Function] `mutable-json-object-ref`  _mutable-json-object_ _key_

Returns the value associated with the given _key_ in the
_mutable-json-object_, otherwise returns `mutable JSON not found`object.


###### [!Function] `mutable-json-not-found?`  _obj_

Returns `#t` if the given _obj_ is a mutable JSON not found,
otherwise `#f`.

### [§3] Mutable JSON array

Mutable JSON array is a JSON array with capability of expansion or shrinking.

###### [!Function] `mutable-json-array?`  _obj_

Returns `#t` if the given _obj_ is a mutable JSON array
otherwise `#f`.

###### [!Function] `mutable-json-array-set!`  _mutable-json-array_ _index_ _value_

Sets the given value _value_ to the _mutable-json-array_on position of _index_.

###### [!Function] `mutable-json-array-insert!`  _mutable-json-array_ _index_ _value_

Insets the given value _value_ to the _mutable-json-array_on position of _index_.

###### [!Function] `mutable-json-array-delete!`  _mutable-json-array_ _index_

Deletes the value of given _mutable-json-array_on position of _index_ and shrink it.

###### [!Function] `mutable-json-array-ref`  _mutable-json-array_ _index_

Retrieves the value of _mutable-json-array_ on position of
_index_.

###### [!Function] `mutable-json-array-size`  _mutable-json-array_

Returns the size of the given _mutable-json-array_.

