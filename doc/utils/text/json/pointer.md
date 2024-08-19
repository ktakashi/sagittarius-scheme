[§2] (text json pointer) - JSON Pointer {#text.json.pointer}
-------------

###### [!Library] `(text json pointer)` 

This library provides JSON Pointer procedures.

The specification of JSON Pointer is defined by the
[RFC 6901](https://tools.ietf.org/html/rfc6901).


The following example shows simple usage of the library:

This is an input file named `example.json`.

```scheme
{
  "id": 1234,
  "data": {
    "datum0": [0, 1, 2],
    "datum1": [3, 4]
  }
}
```
(import (rnrs) (rfc json-pointer) (text json))

(define id-pointer (json-pointer "/id"))

(let ((json (call-with-input-file "example.json" json-read)))
  (id-pointer json))
``` ``1234``

```scheme
(import (rnrs) (rfc json-pointer) (text json))

(define data-pointer (json-pointer "/data"))

#|
example.json
|#
(let ((json (call-with-input-file "example.json" json-read)))
  ;; Retrievs /data/datum0
  ((json-pointer "/datum0" data-pointer) json))
```
=> ``'(0 1 2)``

###### [!Function] `json-pointer`  _pointer:_ _optional_ _parent_

Returns a procedure takes one which must be a list or vector
representing JSON object. The given _poinster_ must be a string or
textual input port which start with `/`.

The optional argument _parent_ is passed, then it must be a procedure
returned by the `json-pointer` and is called before the current
_pointer_ is processed.


**[@since] `0.9.12`**
The returning procedure is called with second argument, then it will be
the default value of not-found. This is useful when you are writing something
below:

```scheme
(define (wrap p)
  (lambda (json default)
    (let ((r (p json)))
      (if (json-pointer-not-found? r)
          default
          r))))
(define pointer (wrap (json-pointer "/foo")))
(pointer json #f)
```
The above can simply be like this:
```scheme
(define pointer (json-pointer "/foo"))
(pointer json #f)
```
