[§2] (text json patch) - JSON Patch {#text.json.patch}
-------------

###### [!Library] `(text json pointer)` 

This library provides JSON Patch procedures.

The specification of JSON Patch is defined by the
[RFC 6902](https://tools.ietf.org/html/rfc6902).


The following example shows simple usage of the library:

This is an input file named `example.json`.

``````````scheme
{
  "id": 1234,
  "data": {
    "datum0": [0, 1, 2],
    "datum1": [3, 4]
  }
}
``````````

``````````scheme
(import (rnrs) (text json patch) (text json))

(define data-patcher
 (json-patcher 
  '(#(("op" . "replace") ("path" . "/data/datum0") ("value" . "ok"))
    #(("op" . "remove") ("path" . "/data/datum1")))))

(let ((json (call-with-input-file "example.json" json-read)))
  (data-patcher json))
``````````
=> ``#((id . 1234) (data . #((datum0 . ok))))``

###### [!Function] `json-patcher`  _patch_

Returns a procedure takes one argument which must be a list or vector
representing JSON object. The given _patch_ must be a list of vector
which represents JSON patch.


###### [!Function] `json-patch-error?`  _obj_

Returns #t if the given _obj_ is an instance of `&json-patch`condition.

`&json-patch` is the base condition of this library. The hierarchy is
the following:

``````````scheme
&json-patch
 + &json-patch:runtime (path)
   + &json-patch-path-not-found
   + &json-patch-illegal-type
 + &json-patch:compile (patch)
``````````



###### [!Function] `json-patch-compile-error?`  _obj_

Returns #t if the given _obj_ is an instance of
`&json-patch:compile` condition.

###### [!Function] `json-patch-error?`  _obj_

Returns #t if the given _obj_ is an instance of
`&json-patch:runtime` condition.

###### [!Function] `json-patch-path-not-found-error?`  _obj_

Returns #t if the given _obj_ is an instance of
`&json-patch-path-not-found` condition.

###### [!Function] `json-patch-path-illegal-type-error?`  _obj_

Returns #t if the given _obj_ is an instance of
`&json-patch-illegal-type` condition.

###### [!Function] `json-patch-error-path`  _json-patch-error_

Returns the `path` field of the given _json-patch-error_ if
the condition is type of `&json-patch-runtime`.

###### [!Function] `json-patch-error-patch`  _json-patch-error_

Returns the `patch` field of the given _json-patch-error_ if
the condition is type of `&json-patch-compile`.

###### [!Parameter] `*json-patcher:ignore-no-such-path*` 

Flag to supress no such path error. The value must be either a symbol
or a list of symbol of the name of the patch command. If the value is matched
with the patch command, then the runtime doesn't raise a condition. 

