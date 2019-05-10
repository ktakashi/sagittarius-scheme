@; -*- coding: utf-8 -*-
@subsection[:tag "text.json.mutable"]{(text json mutable) - Mutable JSON}

@define[Library]{@name{(text json mutable)}}
@desc{This library provides an mutable JSON representation.}

The following example shows how to use:
@codeblock{
(import (rnrs)
        (text json mutable))

(define json '#(("key" . "value")))
(define mutable-json (json->mutable-json json))

(mutable-json-object-set! mutable-json "key" '#(("value" . "as object")))

(mutable-json->json mutable-json)
;; -> #(("key" . #(("value" . "as object"))))
}


@define[Function]{@name{json->mutable-json} @args{json}}
@desc{Converts the given @var{json} to a mutable JSON.}
@define[Function]{@name{mutable-json->json} @args{mutable-json}}
@desc{Converts the given @var{mutable-json} to a JSON.}

@define[Function]{@name{mutable-json?} @args{obj}}
@desc{Returns @code{#t} if the given @var{obj} is a mutable JSON,
otherwise @code{#f}.}

Mutable JSON object consists with 2 containers
@itemlist[
  @item{Mutable JSON object}
  @item{Mutable JSON array}
]

@subsubsection{Mutable JSON object}

Mutable JSON object is a JSON object. The difference is a mutable JSON object
doesn't allow to have multiple identical keys while JSON object allows it.

@define[Function]{@name{mutable-json-object?} @args{obj}}
@desc{Returns @code{#t} if the given @var{obj} is a mutable JSON object,
otherwise @code{#f}.}

@define[Function]{@name{mutable-json-object-set!}
 @args{mutable-json-object key value}}
@desc{Associates the given @var{key} and @var{value} to the @var{mutable-json}.}
@define[Function]{@name{mutable-json-object-merge!}
 @args{mutable-json-object mutable-json-object1 @dots{} mutable-json-object*}}
@desc{Merges the @var{mutable-json-object1} and @var{mutable-json-object*} into
@var{mutable-json-object}.
}
@define[Function]{@name{mutable-json-object-delete!}
 @args{mutable-json-object key}}
@desc{Remves the given @var{key} from the @var{mutable-json-object}.}
@define[Function]{@name{mutable-json-object-contains?}
 @args{mutable-json-object key}}
@desc{Returns @code{#t} if the given @var{key} exists in the
@var{mutable-json-object}, otherwise @code{#f}.}
@define[Function]{@name{mutable-json-object-ref}
 @args{mutable-json-object key}}
@desc{Returns the value associated with the given @var{key} in the
@var{mutable-json-object}, otherwise returns @code{mutable JSON not found}
object.
}
@define[Function]{@name{mutable-json-not-found?} @args{obj}}
@desc{Returns @code{#t} if the given @var{obj} is a mutable JSON not found,
otherwise @code{#f}.}

@subsubsection{Mutable JSON array}

Mutable JSON array is a JSON array with capability of expansion or shrinking.

@define[Function]{@name{mutable-json-array?} @args{obj}}
@desc{Returns @code{#t} if the given @var{obj} is a mutable JSON array
otherwise @code{#f}.}

@define[Function]{@name{mutable-json-array-set!}
 @args{mutable-json-array index value}}
@desc{Sets the given value @var{value} to the @var{mutable-json-array}
on position of @var{index}.}
@define[Function]{@name{mutable-json-array-insert!}
 @args{mutable-json-array index value}}
@desc{Insets the given value @var{value} to the @var{mutable-json-array}
on position of @var{index}.}
@define[Function]{@name{mutable-json-array-delete!}
 @args{mutable-json-array index}}
@desc{Deletes the value of given @var{mutable-json-array}
on position of @var{index} and shrink it.}
@define[Function]{@name{mutable-json-array-ref}
 @args{mutable-json-array index}}
@desc{Retrieves the value of @var{mutable-json-array} on position of
@var{index}.}
@define[Function]{@name{mutable-json-array-size} @args{mutable-json-array}}
@desc{Returns the size of the given @var{mutable-json-array}.}
