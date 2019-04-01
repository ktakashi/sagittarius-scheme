@; -*- coding: utf-8 -*-
@subsection[:tag "text.json.patch"]{(text json patch) - JSON Patch}

@define[Library]{@name{(text json pointer)}}
@desc{This library provides JSON Patch procedures.

The specification of JSON Patch is defined by the
@hyperlink[:href "https://tools.ietf.org/html/rfc6902"]{RFC 6902}.
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

@codeblock[=> #(("id" . 1234) ("data" . #(("datum0" . "ok"))))]{
(import (rnrs) (text json patch) (text json))

(define data-patcher
 (json-patcher 
  '(#(("op" . "replace") ("path" . "/data/datum0") ("value" . "ok"))
    #(("op" . "remove") ("path" . "/data/datum1")))))

(let ((json (call-with-input-file "example.json" json-read)))
  (data-patcher json))
}

@define[Function]{@name{json-patcher} @args{patch}}
@desc{Returns a procedure takes one argument which must be a list or vector
representing JSON object. The given @var{patch} must be a list of vector
which represents JSON patch.
}

@define[Function]{@name{json-patch-error?} @args{obj}}
@desc{Returns #t if the given @var{obj} is an instance of @code{&json-patch}
condition.

@code{&json-patch} is the base condition of this library. The hierarchy is
the following:
@codeblock{
&json-patch
 + &json-patch:runtime (path)
   + &json-patch-path-not-found
   + &json-patch-illegal-type
 + &json-patch:compile (patch)
}
}
@define[Function]{@name{json-patch-compile-error?} @args{obj}}
@desc{Returns #t if the given @var{obj} is an instance of
@code{&json-patch:compile} condition.}
@define[Function]{@name{json-patch-error?} @args{obj}}
@desc{Returns #t if the given @var{obj} is an instance of
@code{&json-patch:runtime} condition.}
@define[Function]{@name{json-patch-path-not-found-error?} @args{obj}}
@desc{Returns #t if the given @var{obj} is an instance of
@code{&json-patch-path-not-found} condition.}
@define[Function]{@name{json-patch-path-illegal-type-error?} @args{obj}}
@desc{Returns #t if the given @var{obj} is an instance of
@code{&json-patch-illegal-type} condition.}

@define[Function]{@name{json-patch-error-path} @args{json-patch-error}}
@desc{Returns the @code{path} field of the given @var{json-patch-error} if
the condition is type of @code{&json-patch-runtime}.}
@define[Function]{@name{json-patch-error-patch} @args{json-patch-error}}
@desc{Returns the @code{patch} field of the given @var{json-patch-error} if
the condition is type of @code{&json-patch-compile}.}

@define[Parameter]{@name{*json-patcher:ignore-no-such-path*}}
@desc{Flag to supress no such path error. The value must be either a symbol
or a list of symbol of the name of the patch command. If the value is matched
with the patch command, then the runtime doesn't raise a condition. }
