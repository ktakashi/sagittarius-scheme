[§2] (text csv parser) - Delimitor Separated Values parser library {#text.dsv.parser}
------------------------------------------------------------------

###### [!Library] `(text dsv parser)` 

This library provides delimitor separated values parser and write procedures.

The implementation follows ABNF of RFC 4180 with come customization capability.

### [§3] High level APIs

###### [!Macro] `dsv-parser-options-builder`

Record builder macro to build `dsv-parser-options`.

The options are

`separator`
: Specifying delimitor value of the parser, default `#\,`

`dquote`
: Specifying the double quote character, aka `DQUOTE`, default `#\"`

`comment`
: Specifying the comment starting character, default `#\#`

`item-accumulator`
: Specifying the accumulator of the items of parsing values. Must be a procedure
  which accepts 3 arguments, _seed_, _item_ and _location_.

`item-seed`
: The initial _seed_ for the `item-accumulator`, default `()`


`line-accumulator`
: Specifying the accumulator of the items of parsing values. Must be a procedure
  which accepts 3 arguments, _seed_, _line_ and _location_.

`line-seed`
: The initial _seed_ for the `line-accumulator`, default `()`

For _item_ and _line_ arguments, these are the parsed value of the
DSV content. The accumulator must handle them to build the next _seed_.

The _location_ argument is either an integer, representing the index or
line number, or `#f`, representing the end of the line of content.


###### [!Function] `dsv-parser-options?` _obj_

Returns `#t` if the _obj_ is DSV parser options, otherwise `#f`.

###### [!Function] `make-dsv-parser` (_options_ `dsv-parser-options?`)

Make a DSV parser, which is a procedure accepting a textual port.

The simple CSV parser can be written like this

```scheme
(import (rnrs)
        (text dsv parser)
        ;; for reverse!
        (srfi :1 lists))
(define (csv-item-accumulator seed line loc)
  (let ((r (cons line seed)))
    (or (and loc r)
        (reverse! r))))

(define (csv-line-accumulator seed line loc)
  (if loc
      (cons line seed)
      (reverse! seed)))

(define csv-options
  (dsv-parser-options-builder 
    (item-accumulator csv-item-accumulator)
    (line-accumulator csv-line-accumulator)))

(define csv-parser (make-dsv-parser csv-options))

;; how to use
(call-with-input-file "data.csv" csv-parser)
;; -> list of list
```

### [§3] Low level APIs

###### [!Function] `make-item-reader` _separator_ _dquote_

Returns a procedure accepting a textual port.

The returning procedure reads one DSV item.

###### [!Function] `make-line-reader` _item-reader_ _comment_ _item-accumulator_ _item-seed_

Returns a procedure accepting a textual port.

The returning procedure reads one DSV line by accumulating the _seed_
and items read by _item-reader_.
