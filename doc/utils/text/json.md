[§2] (text json) -- JSON parser {#text.json}
-------------

###### [!Library] `(text json)` 

This library provides JSON parser and writer. 


###### [!Parameter] `*json-map-type*` 

Controls mapping of JSON map and array. The value must be either
`'vector` or `'alist`. The first one is compatible with Chicken
Scheme's json module, the latter one is compatible with Gauche's 
`rfc.json` module. By default, it's set to `vector` for backward
compatibility.

Conversion rules for `vector`:

``````````scheme
JSON array   <-> list
JSON map     <-> vector
JSON boolean <-> boolean
JSON null    <-> symbol `null`
``````````

Conversion rules for `alist`:

``````````scheme
JSON array   <-> vector
JSON map     <-> alist
JSON boolean <-> boolean
JSON null    <-> symbol `null`
``````````

This parameter affects the read and write procedures.


###### [!Function] `json-read`  _:optional_ _(port_ _(current-input-port))_

Reads JSON from given _port_ and returns representing S-expression.

###### [!Function] `json-write`  _json_ _:optional_ _(port_ _(current-output-port))_

Writes the given S-expression JSON representing object to given
_port_.


[§2] (text json object-builder) -- JSON object builder/serializer {#text.json.object-builder}
-------------

###### [!Library] `(text json object-builder)` 

This library provides Scheme object -> JSON string and vice versa
utilities.


### [§3] JSON object builder

JSON object builder is a Schem object which contains information to
construct a Scheme object from JSON representation. Currently this can be
created only via `json-object-builder` macro.

###### [!Function] `json:builder?`  _obj_

Returns #t if the given _obj_ is a JSON object builder.

###### [!Macro] `json-object-builder`  _ctr_ _spec_ _..._
###### [!Auxiliary syntax] `?` 
###### [!Auxiliary syntax] `@` 

A DSL which constructs JSON object builder.

The _spec_ must be one of the followings:

- `(@ ->array spec)`
- `(@ ->array)`
- `(ctr mapping ...)`
- `ctr/builder`

_->array_ must be a procedure which accepts variable length of
arguments, such as `list` or `vector`.

_ctr_ must be a procedure which accepts the same number of the
specified keys in the _mapping_ and constucts object.

_ctr/builder_ must be either object constructor described above
or JSON object builder created by the `json-object-builder`.

If the first 2 form is used, then the created builder handles JSON
array.

If the 3rd form is used, then the created builder handles JSON object
(a.k.a map).

If the lsst form is used, then the created builder handles simple
JSON values, such as JSON string and number.

The _mapping_ must be one of the followings:

- `(? key default spec)`
- `(? key default)`
- `(key spec)`
- `key`

_key_ must be a string represents the JSON object's key.

_default_ must be a Scheme object which is used when the _key_ is
absent.

The first 2 forms represetns optional values. If the JSON object key
_key_ is not present, then _default_ is mapped to the result
Scheme object.

Here are some of examples:

``````````scheme
(json-object-builder
 (make-image-holder
  ("Image"
  (make-image
   "Width"
   "Height"
   "Title"
   ("Thumbnail"
    (make-thumbnail
     "Url"
     "Height"
     "Width"))
   "Animated"
   ("IDs" (  list))))))
#|
Above construct Scheme object from JSON like the following:
{
  "Image": {
    "Width":  800,
    "Height": 600,
    "Title":  "View from 15th Floor",
    "Thumbnail": {
      "Url":    "http://www.example.com/image/481989943",
      "Height": 125,
      "Width":  100
  },
    "Animated" : false,
    "IDs": [116, 943, 234, 38793]
  }
}
|#
``````````

``````````scheme
(json-object-builder
 (  list
    (make-location
     "precision"
     "Latitude"
     "Longitude"
     (? "Address" #f)
     "City"
     "State"
     "Zip"
     "Country")))
#|
Above construct Scheme object from JSON like the following:
[
  {
    "precision": "zip",
    "Latitude":  37.7668,
    "Longitude": -122.3959,
    "Address":   "",
    "City":      "SAN FRANCISCO",
    "State":     "CA",
    "Zip":       "94107",
    "Country":   "US"
  },
  {
    "precision": "zip",
    "Latitude":  37.371991,
    "Longitude": -122.026020,
    "City":      "SUNNYVALE",
    "State":     "CA",
    "Zip":       "94085",
    "Country":   "US"
  }
]
|#
``````````



###### [!Function] `json->object`  _json_ _builder_ _:optional_ _missing-key-handler_
###### [!Function] `json-string->object`  _json-string_ _builder_ _:optional_ _missing-key-handler_
###### [!Function] `read-object-from-json`  _builder_ _:optional_ _(in-port_ _(current-input-port))_ _missing-key-handler_

Constructs Scheme object from given _json_, _json-string_or _in-port_, according to the given _builder_.

If the first form is used, then _json_ must be a vector type JSON
representation specified by the `*json-map-type*` parameter.

``````````scheme
(let ((json-string "{\"bar\": {\"buz\": 1}}"))
  (define-record-type foo
    (fields bar))
  (define-record-type bar
    (fields buz))
  (define bar-builder (json-object-builder (make-bar "buz")))
  (define builder (json-object-builder (make-foo ("bar" bar-builder))))

  (json-string->object json-string builder))
``````````
=> ``foo``

If _missing-key-handler_ is given, then it must be a procedure accepts 2
arguments. This procedure is called when the conversion procedure met keys
which is not defined in _builder_. The default behaviour is raising an
error.


###### [!Parameter] `*post-json-object-build*` 
###### [!Parameter] `*post-json-array-build*` 

These parameters hold a procedure which is called when an object is
constructed from JSON object (map) or JSON array, respectively.


### [§3] JSON object serializer

JSON object serializer is a Schem object which contains information to
construct a JSON representaion from Scheme object. Currently this can be
created only via `json-object-serializer` macro.

###### [!Function] `json:serializer?`  _obj_

Returns #t if the given _obj_ is a JSON object serializer.

###### [!Macro] `json-object-serializer`  _ctr_ _spec_ _..._
###### [!Auxiliary syntax] `?` 
###### [!Auxiliary syntax] `@` 
###### [!Auxiliary syntax] `->` 

A DSL which constructs JSON object serializer.

The _spec_ must be one of the followings:

- `(-> car cdr null? spec)`
- `(-> car cdr null?)`
- `(-> spec)`
- `(->)`
- `(@ ref length spec)`
- `(@ ref length)`
- `(@ spec)`
- `(@)`
- `(mapping mapping* ...)`
- `converter/serializer`

`->` indicates that the given object is a listlike object which can
be accessed sequentially. _car_, _cdr_ and _null?_ specifies
how to retrieve the car part and cdr part, and how to check if the object
is empty or not, respectively. If these are not given then the macro
uses `car`, `cdr` and `null?`.

`@` indicates that the given object is a vectorlike object which
can be accessed randomly. _ref_ and _length_ specifies how to access
the element of the object, and how to retrieve the length of the object,
respectively. If these are not given then the macro uses `vector-ref`,
and `vector-length`.

If both of the form don't have `spec`, then the macro uses the given
value.

`mapping` must be one of the followings:

- `(? name absent ref spec)`
- `(? name absent ref)`
- `(name ref spec)`
- `(name ref)`

`?` indicates that referencing object might be absent.

_name_ must be a string which represents JSON object's key.

_absent_ must be an object indicating absent value. If the converting
object is equal to this value in sense of `equal?`, then the constructed
JSON representaion don't have _name_.

_ref_ must be a accessor which is a procedure accepts one argument.

_converter/serializer_ must be either a JSON object serializer or
a procedure which accepts one argument and returns JSON representaion.

``````````scheme
(json-object-serializer
  (-> (("precision" location-precision)
       ("Latitude" location-latitude)
       ("Longitude" location-longitude)
       (? "Address" #f location-address)
       ("City" location-city)
       ("State" location-state)
       ("Zip" location-zip)
       ("Country" location-country))))

;; Above constructs JSON representaion from the following record type.
(define-record-type location
  (fields precision latitude longitude address city state zip country))
``````````

``````````scheme
(json-object-serializer
 (("Image" image-holder-image
   (("Width" image-width)
    ("Height" image-height)
    ("Title" image-title)
    ("Thumbnail" image-thumbnail
	(("Url" thumbnail-url)
	 ("Height" thumbnail-height)
	 ("Width" thumbnail-width)))
    ("Animated" image-animated)
    ("IDs" image-ids (->))))))

;; Above constructs JSON representaion from the following record type.
(define-record-type image-holder
  (fields image))
(define-record-type image
  (fields width height title thumbnail animated ids))
(define-record-type thumbnail
  (fields url height width))
``````````



###### [!Function] `object->json`  _obj_ _serializer_

Converts Scheme object to JSON representaion.

The converted JSON representaion is the same as `'vector` representaion.


###### [!Function] `object->json-string`  _obj_ _serializer_

Converts Scheme object to JSON string.

###### [!Function] `write-object-as-json`  _obj_ _serializer_ _:optional_ _(out-port_ _(current-output-port))_

Writes JSON string converted from _obj_ to _out-port_.

