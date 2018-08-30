@; -*- coding: utf-8 -*-
@subsection[:tag "text.json.pointer"]{(text json pointer) - JSON Pointer}

@define[Library]{@name{(text json pointer)}}
@desc{This library provides JSON Pointer procedures.

The specification of JSON Pointer is defined by the
@hyperlink[:href "https://tools.ietf.org/html/rfc6901"]{RFC 6901}.
}

The following example shows simple usage of the library:

This is an input file named @code{example.json}.
@codeblock{
{
  "id": 1234,
  "data": {
    "datum0": [0, 1, 2],
    "datum1": [3, 4]
  }
}
}

@codeblock[=> 1234]{
(import (rnrs) (rfc json-pointer) (text json))

(define id-pointer (json-pointer "/id"))

(let ((json (call-with-input-file "example.json" json-read)))
  (id-pointer json))
}

@codeblock[=> '(0 1 2)]{
(import (rnrs) (rfc json-pointer) (text json))

(define data-pointer (json-pointer "/data"))

#|
example.json
|#
(let ((json (call-with-input-file "example.json" json-read)))
  ;; Retrievs /data/datum0
  ((json-pointer "/datum0" data-pointer) json))
}


@define[Function]{@name{json-pointer} @args{pointer: optional parent}}
@desc{Returns a procedure taks one argument which must be a list or vector
representing JSON object. The given @var{poinster} must be a string or
textual input port which start with @code{/}.

The optional argument @var{parent} is passed, then it must be a procedure
returned by the @code{json-pointer} and is called before the current
@var{pointer} is processed.
}
