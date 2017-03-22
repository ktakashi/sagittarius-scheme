@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "text.json"]{(text json) -- JSON parser}

@define[Library]{@name{(text json)}}
@desc{This library provides JSON parser and writer. 
}

@define[Parameter]{@name{*json-map-type*}}
@desc{Controls mapping of JSON map and array. The value must be either
@code{'vector} or @code{'alist}. The first one is compatible with Chicken
Scheme's json module, the latter one is compatible with Gauche's 
@code{rfc.json} module. By default, it's set to @code{vector} for backward
compatibility.

Conversion rules for @code{vector}:
@codeblock{
JSON array   <-> list
JSON map     <-> vector
JSON boolean <-> boolean
JSON null    <-> symbol @code{null}
}

Conversion rules for @code{alist}:
@codeblock{
JSON array   <-> vector
JSON map     <-> alist
JSON boolean <-> boolean
JSON null    <-> symbol @code{null}
}

This parameter affects the read and write procedures.
}

@define[Function]{@name{json-read} @args{:optional (port (current-input-port))}}
@desc{Reads JSON from given @var{port} and returns representing S-expression.}

@define[Function]{@name{json-write}
 @args{json :optional (port (current-output-port))}}
@desc{Writes the given S-expression JSON representing object to given
@var{port}.
}

@subsection[:tag "text.json.object-builder"]{(text json object-builder) -- JSON
object builder/serializer}

@define[Library]{@name{(text json object-builder)}}
@desc{This library provides Scheme object -> JSON string and vice versa
utilities.
}

@subsubsection{JSON object builder}

JSON object builder is a Schem object which contains information to
construct a Scheme object from JSON representation. Currently this can be
created only via @code{json-object-builder} macro.

@define[Function]{@name{json:builder?} @args{obj}}
@desc{Returns #t if the given @var{obj} is a JSON object builder.}

@define[Macro]{@name{json-object-builder} @args{ctr spec @dots{}}}
@define["Auxiliary syntax"]{@name{?}}
@define["Auxiliary syntax"]{@name{@atmark{}}}
@desc{A DSL which constructs JSON object builder.

The @var{spec} must be one of the followings:
@itemlist[
  @item{@code{(@atmark{} ->array spec)}}
  @item{@code{(@atmark{} ->array)}}
  @item{@code{(ctr mapping @dots{})}}
  @item{@code{ctr/builder}}
]
@var{->array} must be a procedure which accepts variable length of
arguments, such as @code{list} or @code{vector}.

@var{ctr} must be a procedure which accepts the same number of the
specified keys in the @var{mapping} and constucts object.

@var{ctr/builder} must be either object constructor described above
or JSON object builder created by the @code{json-object-builder}.

If the first 2 form is used, then the created builder handles JSON
array.

If the 3rd form is used, then the created builder handles JSON object
(a.k.a map).

If the lsst form is used, then the created builder handles simple
JSON values, such as JSON string and number.

The @var{mapping} must be one of the followings:
@itemlist[
  @item{@code{(? key default spec)}}
  @item{@code{(? key default)}}
  @item{@code{(key spec)}}
  @item{@code{key}}
]
@var{key} must be a string represents the JSON object's key.

@var{default} must be a Scheme object which is used when the @var{key} is
absent.

The first 2 forms represetns optional values. If the JSON object key
@var{key} is not present, then @var{default} is mapped to the result
Scheme object.

Here are some of examples:
@codeblock{
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
   ("IDs" (@ list))))))
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
}

@codeblock{
(json-object-builder
 (@ list
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
}

}

@define[Function]{@name{json->object}
 @args{json builder :optional missing-key-handler}}
@define[Function]{@name{json-string->object}
 @args{json-string builder :optional missing-key-handler}}
@define[Function]{@name{read-object-from-json}
 @args{builder :optional (in-port (current-input-port)) missing-key-handler}}
@desc{Constructs Scheme object from given @var{json}, @var{json-string}
or @var{in-port}, according to the given @var{builder}.

If the first form is used, then @var{json} must be a vector type JSON
representation specified by the @code{*json-map-type*} parameter.

@codeblock[=> foo]{
(let ((json-string "{\"bar\": {\"buz\": 1}}"))
  (define-record-type foo
    (fields bar))
  (define-record-type bar
    (fields buz))
  (define bar-builder (json-object-builder (make-bar "buz")))
  (define builder (json-object-builder (make-foo ("bar" bar-builder))))

  (json-string->object json-string builder))
}

If @var{missing-key-handler} is given, then it must be a procedure accepts 2
arguments. This procedure is called when the conversion procedure met keys
which is not defined in @var{builder}. The default behaviour is raising an
error.
}

@define[Parameter]{@name{*post-json-object-build*}}
@define[Parameter]{@name{*post-json-array-build*}}
@desc{These parameters hold a procedure which is called when an object is
constructed from JSON object (map) or JSON array, respectively.
}

@subsubsection{JSON object serializer}

JSON object serializer is a Schem object which contains information to
construct a JSON representaion from Scheme object. Currently this can be
created only via @code{json-object-serializer} macro.

@define[Function]{@name{json:serializer?} @args{obj}}
@desc{Returns #t if the given @var{obj} is a JSON object serializer.}

@define[Macro]{@name{json-object-serializer} @args{ctr spec @dots{}}}
@define["Auxiliary syntax"]{@name{?}}
@define["Auxiliary syntax"]{@name{@atmark{}}}
@define["Auxiliary syntax"]{@name{->}}
@desc{A DSL which constructs JSON object serializer.

The @var{spec} must be one of the followings:
@itemlist[
  @item{@code{(-> car cdr null? spec)}}
  @item{@code{(-> car cdr null?)}}
  @item{@code{(-> spec)}}
  @item{@code{(->)}}
  @item{@code{(@atmark{} ref length spec)}}
  @item{@code{(@atmark{} ref length)}}
  @item{@code{(@atmark{} spec)}}
  @item{@code{(@atmark{})}}
  @item{@code{(mapping mapping* @dots{})}}
  @item{@code{converter/serializer}}
]
@code{->} indicates that the given object is a listlike object which can
be accessed sequentially. @var{car}, @var{cdr} and @var{null?} specifies
how to retrieve the car part and cdr part, and how to check if the object
is empty or not, respectively. If these are not given then the macro
uses @code{car}, @code{cdr} and @code{null?}.

@code{@atmar{}} indicates that the given object is a vectorlike object which
can be accessed randomly. @var{ref} and @var{length} specifies how to access
the element of the object, and how to retrieve the length of the object,
respectively. If these are not given then the macro uses @code{vector-ref},
and @code{vector-length}.

If both of the form don't have @code{spec}, then the macro uses the given
value.

@code{mapping} must be one of the followings:
@itemlist[
  @item{@code{(? name absent ref spec)}}
  @item{@code{(? name absent ref)}}
  @item{@code{(name ref spec)}}
  @item{@code{(name ref)}}
]
@code{?} indicates that referencing object might be absent.

@var{name} must be a string which represents JSON object's key.

@var{absent} must be an object indicating absent value. If the converting
object is equal to this value in sense of @code{equal?}, then the constructed
JSON representaion don't have @var{name}.

@var{ref} must be a accessor which is a procedure accepts one argument.

@var{converter/serializer} must be either a JSON object serializer or
a procedure which accepts one argument and returns JSON representaion.

@codeblock{
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
}
@codeblock{
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
}
}

@define[Function]{@name{object->json} @args{obj serializer}}
@desc{Converts Scheme object to JSON representaion.

The converted JSON representaion is the same as @code{'vector} representaion.
}

@define[Function]{@name{object->json-string} @args{obj serializer}}
@desc{Converts Scheme object to JSON string.}

@define[Function]{@name{write-object-as-json}
 @args{obj serializer :optional (out-port (current-output-port))}}
@desc{Writes JSON string converted from @var{obj} to @var{out-port}.}