[§2] (binary pack) - Packing binary data {#util.binary.pack}
-------------

###### [!Library] `(binary pack)` 

This library provides an interface for packing and unpacking (writing
and reading) binary data with template. The functionality is inspired by
[Industria](https://weinholt.se/industria/)'s
`(weinholt struct pack)` library.


###### [!Macro] `pack`  _template_ _args_ _..._

_template_ must be a string.

Construct a bytevector with given _args_ according to the given
_template_. Template characters are described below.


###### [!Macro] `pack!`  _template_ _bv_ _offset_ _args_ _..._

_template_ must be a string. 

_bv_ must be a bytevector.

_offset_ must a non-negative exact integer.

Converts given _args_ and put it into _bv_ starting from _offset_.
The conversion is done according to the _template_ string.

The template characters are extensible so following description can only cover
predefined characters.

x: padding; c: s8; C: u8; s: s16; S: u16; l: s32; L: u32; q: s64; Q: u64;
f: ieee-single; d: ieee-double; ! or >: big-endian; \<: little-endian;
=: native-endian; u: disable natural alignment; a: enable natural alignment.
Whitespace is ignored.

``(pack "!c" 128)`` => ``#vu8(128)``

``(pack "s" 100)`` => ``#vu8(100 0)``

``(pack "!s" 100)`` => ``#vu8(0 100)``

``(pack "!d" 3.14)`` => ``#vu8(64 9 30 184 81 235 133 31)``

Fields are by default aligned to their natural alignment and NUL bytes are
inserted as necessary to have a field's index to be aligned to its size.

``(pack "!xd" 3.14)`` => ``#vu8(0 0 0 0 0 0 0 0 64 9 30 184 81 235 133 31)``

``(pack "!uxd" 3.14)`` => ``#vu8(0 64 9 30 184 81 235 133 31)``

Digits in front of the syntax characters means repetition. And `#\*` means
indefinite length repetition.

``(pack "3c" 1 2 3)`` => ``#vu8(1 2 3)``

``(pack "*c" 1 2 3 4)`` => ``#vu8(1 2 3 4)``

When the macro detects the given template is string, then it tries to expand
as much as possible. So it might raises the different condition even if the
template strings are the same.

``(pack "3c" 1 2 3 4)`` => ``&syntax``

``(pack (car '("3c")) 1 2 3 4)`` => ``&error``



###### [!Macro] `unpack`  _template_ _bv_
###### [!Macro] `unpack`  _template_ _bv_ _offset_

_template_ must be a string.

Unpack the given bytevector according to the given _template_ and returns
the values. The template syntax are the same as `pack!`.

If the second form is used, then unpacking is done from the given _offset_.

``(unpack "!SS" #vu8(0 1 0 2))`` => ``1 2``

``(unpack "!SS" #vu8(0 1 0 2 0 3) 1)`` => ``2 3``

``(unpack "!uSS" #vu8(0 1 0 2 0 3) 1)`` => ``256 512``



###### [!Macro] `get-unpack`  _port_ _template_

_template_ must be a string.

Utility unpacking macro for binary port.


###### [!Macro] `format-size`  _template_
###### [!Macro] `format-size`  _template_ _args_ _..._

_template_ must be a string.

Calculate the size of the result bytevector. If the second form is used, then
macro can calculate even if the template contains indefinite length syntax
`#\*`, otherwise #f is returned.

``(format-size "!xd")`` => ``16``

``(format-size "!uxd")`` => ``9``

``(format-size "*c")``

``(format-size "*c" 1 2 3 4)`` => ``4``



###### [!Macro] `define-`  _\*\*_ _-packer_ _(char_ _arg)_ _(_ `pack` _expr1_ _..._ _)_ _(_ `unpack` _expr2_ _..._ _)_

_char_ must character.

`pack` and `unpack` are syntactic keywords.

Defines packing extension to given _char_. This macro can not overwrite
the predefined characters. _\*\*_ can be followings;

`s8`, `u8`, `s16`, `u16`, `s32`, `u32`,
`s64`, `u64`, `f32`, and `f64`.

``````````scheme
;; defining char to u8 converter
(define-u8-packer (#\A v)
  (pack (char->integer v))
  (unpack (integer->char v)))
(pack "AA" #\a #\b)       ;; => #vu8(97 98)
(unpack "AA" #vu8(97 98)) ;; => #\a #\b
``````````



