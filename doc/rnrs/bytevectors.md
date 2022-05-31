[§2] Bytevectors {#rnrs.bytevectors.6}
-------------

Many applications deal with blocks of binary data by accessing them in various
ways-extracting signed or unsigned numbers of various sizes. Therefore, the 
`(rnrs bytevectors (6))`library provides a single type for blocks of binary
data with multiple ways to access that data. It deals with integers and
floating-point representations in various sizes with specified endianness.

Bytevectorsare objects of a disjoint type. Conceptually, a bytevector represents
a sequence of 8-bit bytes. The description of bytevectors uses the term byte for
an exact integer object in the interval { - 128, ..., 127} and the term octet for
an exact integer object in the interval {0, ..., 255}. A byte corresponds to its
two's complement representation as an octet.

The length of a bytevector is the number of bytes it contains. This number is
fixed. A valid index into a bytevector is an exact, non-negative integer object
less than the length of the bytevector. The first byte of a bytevector has index
0; the last byte has an index one less than the length of the bytevector.

Generally, the access procedures come in different flavors according to the size
of the represented integer and the endianness of the representation. The procedures
also distinguish signed and unsigned representations. The signed representations
all use two's complement.

Like string literals, literals representing bytevectors do not need to be quoted:
``#vu8(12 23 123)`` => ``#vu8(12 23 123)``

###### [!Library] `(rnrs bytevectors (6))` 

[R6RS] This library provides a single type for blocks of binary data with
multiple ways to access that data.


### [§3] General operations

###### [!Macro] `endianness`  _symbol_

[R6RS] The name of _symbol_ must be a symbol describing an endianness.
`(endianness _symbol_)` evaluates to the symbol named _symbol_.
Whenever one of the procedures operating on bytevectors accepts an endianness as
an argument, that argument must be one of these symbols. It is a syntax violation
for symbol to be anything other than an endianness symbol supported by the Sagittarius.

Currently, Sagittarius supports these symbols; `big`, `little`and `native`.


###### [!Function] `native-endianness` 

[R6RS] Returns the endianness symbol associated platform endianness.
This may be a symbol either big or little.


###### [!Function] `bytevector?`  _obj_

[R6RS] Returns #t if _obj_ is a bytevector, otherwise returns #f.

###### [!Function] `make-bytevector`  _k_ _:optional_ _fill_

[R6RS] Returns a newly allocated bytevector of _k_ bytes.

If the _fill_ argument is missing, the initial contents of the returned
bytevector are 0.

If the _fill_ argument is present, it must be an exact integer object in the
interval {-128, ..., 255} that specifies the initial value for the bytes of the
bytevector: If _fill_ is positive, it is interpreted as an octet; if it is
negative, it is interpreted as a byte.


###### [!Function] `bytevector-length`  _bytevector_

[R6RS] Returns, as an exact integer object, the number of bytes in
_bytevector_.

###### [!Function] `bytevector=?`  _bytevector1_ _bytevector2_

[R6RS] Returns #t if _bytevector1_ and _bytevector2_ are
equal-that is, if they have the same length and equal bytes at all valid
indices. It returns #f otherwise.


###### [!Function] `bytevector-fill!`  _bytevector_ _fill_ _:optional_ _start_ _end_

[R6RS+] The _fill_ argument is as in the description of the
`make-bytevector` procedure. The `bytevector-fill!` procedure stores
_fill_ in every element of _bytevector_ and returns unspecified values.
Analogous to `vector-fill!`.

If optional arguments _start_ or _end_ is given, then the procedure
restricts the range of filling from _start_ to _end_ (exclusive) index
of _bytevector_. When _end_ is omitted then it uses the length of the
given bytevector.


###### [!Function] `bytevector-copy!`  _source_ _source-start_ _target_ _target-start_ _k_

[R6RS] _Source_ and _target_ must be bytevectors. _Source-start_,
_target-start_, and _k_ must be non-negative exact integer objects that satisfy

0 \<= _source-start_ \<= _source-start_ + _k_ \<= _source-length_0 \<= _target-start_ \<= _target-start_ + _k_ \<= _target-length_where _source-length_ is the length of _source_ and _target-length_is the length of _target_.

The `bytevector-copy!` procedure copies the bytes from _source_ at indices

_source-start_, ... _source-start_ + _k_ - 1

to consecutive indices in _target_ starting at _target-index_.

This returns unspecified values.


###### [!Function] `bytevector-copy`  _bytevector_ _
_ _:optional_ _(start_ _0)_ _(end_ _-1)_

[R6RS+] Returns a newly allocated copy of _bytevector_.

If optional argument _start_ was given, the procedure copies from the given
_start_ index.

If optional argument _end_ was given, the procedure copies to the given
_end_ index (exclusive).


### [§3] Operation on bytes and octets

###### [!Function] `bytevector-u8-ref`  _bytevector_ _k_
###### [!Function] `bytevector-s8-ref`  _bytevector_ _k_

[R6RS] _K_ must be a valid index of _bytevector_.

The `bytevector-u8-ref` procedure returns the byte at index _k_ of
_bytevector_, as an octet.

The `bytevector-s8-ref` procedure returns the byte at index _k_ of
_bytevector_, as a (signed) byte.


###### [!Function] `bytevector-u8-set!`  _bytevector_ _k_ _octet_
###### [!Function] `bytevector-s8-set!`  _bytevector_ _k_ _byte_

[R6RS] _K_ must be a valid index of _bytevector_.

The `bytevector-u8-set!` procedure stores _octet_ in element _k_of _bytevector_.

The `bytevector-s8-set!` procedure stores the two's-complement
representation of _byte_ in element _k_ of _bytevector_.

Both procedures return unspecified values.


###### [!Function] `bytevector->u8-list`  _bytevector_
###### [!Function] `u8-list->bytevector`  _bytevector_

[R6RS] _List_ must be a list of octets.

The `bytevector->u8-list` procedure returns a newly allocated list of the
octets of _bytevector_ in the same order.

The `u8-list->bytevector` procedure returns a newly allocated bytevector
whose elements are the elements of list _list_, in the same order. It is
analogous to `list->vector`.


### [§3] Operations on integers of arbitary size

###### [!Function] `bytevector-uint-ref`  _bytevector_ _k_ _endianness_ _size_
###### [!Function] `bytevector-sint-ref`  _bytevector_ _k_ _endianness_ _size_
###### [!Function] `bytevector-uint-set!`  _bytevector_ _k_ _n_ _endianness_ _size_
###### [!Function] `bytevector-sint-set!`  _bytevector_ _k_ _n_ _endianness_ _size_

[R6RS] _Size_ must be a positive exact integer object.
_K_, ..., _k_ + _size_ - 1 must be valid indices of _bytevector_.

The `bytevector-uint-ref` procedure retrieves the exact integer object
corresponding to the unsigned representation of size _size_ and specified
by _endianness_ at indices _k_, ..., _k_ + _size_ - 1.

The `bytevector-sint-ref` procedure retrieves the exact integer object
corresponding to the two's-complement representation of size _size_ and
specified by _endianness_ at indices _k_, ..., _k_ + _size_ - 1.

For `bytevector-uint-set!`, _n_ must be an exact integer object in the
interval _{0, ..., 256 ^ "size" - 1}_The `bytevector-uint-set!` procedure stores the unsigned representation of
size _size_ and specified by _endianness_ into _bytevector_ at indices
_k_, ..., _k_ + _size_ - 1.

For `bytevector-sint-set!`, _n_ must be an exact integer object in the
interval _{-256 ^ "size" / 2, ..., 256 ^ "size" / 2 - 1}_.
`bytevector-sint-set!` stores the two's-complement representation of size
_size_ and specified by _endianness_ into _bytevector_ at indices
_k_, ..., _k_ + _size_ - 1.

The `...-set!` procedures return unspecified values.


###### [!Function] `bytevector->uint-list`  _bytevector_ _endianness_ _size_
###### [!Function] `bytevector->sint-list`  _bytevector_ _endianness_ _size_
###### [!Function] `uint-list->bytevector`  _list_ _endianness_ _size_
###### [!Function] `sint-list->bytevector`  _list_ _endianness_ _size_

[R6RS] _Size_ must be a positive exact integer object. For 
`uint-list->bytevector`, _list_ must be a list of exact integer objects
in the interval _{0, ..., 256 ^ "size" - 1}_. For `sint-list->bytevector`,
_list_ must be a list of exact integer objects in the interval
_{-256 ^ "size"/2, ..., 256 ^ "size"/2 - 1}_. The length of _bytevector_or, respectively, of _list_ must be divisible by _size_.

These procedures convert between lists of integer objects and their consecutive
representations according to _size_ and _endianness_ in the _bytevector_objects in the same way as `bytevector->u8-list` and `u8-list->bytevector`do for one-byte representations.


### [§3] Operation on 16-bit integers

###### [!Function] `bytevector-u16-ref`  _bytevector_ _k_ _endianness_
###### [!Function] `bytevector-s16-ref`  _bytevector_ _k_ _endianness_
###### [!Function] `bytevector-u16-native-ref`  _bytevector_ _k_
###### [!Function] `bytevector-s16-native-ref`  _bytevector_ _k_
###### [!Function] `bytevector-u16-set!`  _bytevector_ _k_ _n_ _endianness_
###### [!Function] `bytevector-s16-set!`  _bytevector_ _k_ _n_ _endianness_
###### [!Function] `bytevector-u16-native-set!`  _bytevector_ _k_ _n_
###### [!Function] `bytevector-s16-native-set!`  _bytevector_ _k_ _n_

[R6RS] _K_ must be a valid index of _bytevector_; so must _k_ + 1.
For `bytevector-u16-set!` and `bytevector-u16-native-set!`, _n_must be an exact integer object in the interval _{0, ..., 2 ^ 16 - 1}_.
For `bytevector-s16-set!` and `bytevector-s16-native-set!`, _n_must be an exact integer object in the interval _{-2 ^ 15, ..., 2 ^ 15 - 1}_.

These retrieve and set two-byte representations of numbers at indices _k_and _k_ + 1, according to the endianness specified by _endianness_.
The procedures with `u16` in their names deal with the unsigned representation;
those with `s16` in their names deal with the two's-complement representation.

The procedures with `native` in their names employ the native endianness,
and work only at aligned indices: _k_ must be a multiple of 2.

The `...-set!` procedures return unspecified values.


### [§3] Operation on 32-bit integers

###### [!Function] `bytevector-u32-ref`  _bytevector_ _k_ _endianness_
###### [!Function] `bytevector-s32-ref`  _bytevector_ _k_ _endianness_
###### [!Function] `bytevector-u32-native-ref`  _bytevector_ _k_
###### [!Function] `bytevector-s32-native-ref`  _bytevector_ _k_
###### [!Function] `bytevector-u32-set!`  _bytevector_ _k_ _n_ _endianness_
###### [!Function] `bytevector-s32-set!`  _bytevector_ _k_ _n_ _endianness_
###### [!Function] `bytevector-u32-native-set!`  _bytevector_ _k_ _n_
###### [!Function] `bytevector-s32-native-set!`  _bytevector_ _k_ _n_

[R6RS] _K_ must be a valid index of _bytevector_; so must _k_ + 3.
For `bytevector-u32-set!` and `bytevector-u32-native-set!`, _n_must be an exact integer object in the interval _{0, ..., 2 ^ 32 - 1}_.
For `bytevector-s32-set!` and `bytevector-s32-native-set!`, _n_must be an exact integer object in the interval _{-2 ^ 31, ..., 2 ^ 32 - 1}_.

These retrieve and set two-byte representations of numbers at indices _k_and _k_ + 3, according to the endianness specified by _endianness_.
The procedures with `u32` in their names deal with the unsigned representation;
those with `s32` in their names deal with the two's-complement representation.

The procedures with `native` in their names employ the native endianness,
and work only at aligned indices: _k_ must be a multiple of 4.

The `...-set!` procedures return unspecified values.


### [§3] Operation on 64-bit integers

###### [!Function] `bytevector-u64-ref`  _bytevector_ _k_ _endianness_
###### [!Function] `bytevector-s64-ref`  _bytevector_ _k_ _endianness_
###### [!Function] `bytevector-u64-native-ref`  _bytevector_ _k_
###### [!Function] `bytevector-s64-native-ref`  _bytevector_ _k_
###### [!Function] `bytevector-u64-set!`  _bytevector_ _k_ _n_ _endianness_
###### [!Function] `bytevector-s64-set!`  _bytevector_ _k_ _n_ _endianness_
###### [!Function] `bytevector-u64-native-set!`  _bytevector_ _k_ _n_
###### [!Function] `bytevector-s64-native-set!`  _bytevector_ _k_ _n_

[R6RS] _K_ must be a valid index of _bytevector_; so must _k_ + 7.
For `bytevector-u64-set!` and `bytevector-u64-native-set!`, _n_must be an exact integer object in the interval _{0, ..., 2 ^ 64 - 1}_.
For `bytevector-s64-set!` and `bytevector-s64-native-set!`, _n_must be an exact integer object in the interval _{-2 ^ 63, ..., 2 ^ 64 - 1}_.

These retrieve and set two-byte representations of numbers at indices _k_and _k_ + 7, according to the endianness specified by _endianness_.
The procedures with `u64` in their names deal with the unsigned representation;
those with `s64` in their names deal with the two's-complement representation.

The procedures with `native` in their names employ the native endianness,
and work only at aligned indices: _k_ must be a multiple of 8.

The `...-set!` procedures return unspecified values.


### [§3] Operation on IEEE-754 representations

###### [!Function] `bytevector-ieee-single-native-ref`  _bytevector_ _k_
###### [!Function] `bytevector-ieee-single-ref`  _bytevector_ _k_ _endianness_

[R6RS] _K_, …, _k_ + 3 must be valid indices of _bytevector_.
For `bytevector-ieee-single-native-ref`, _k_ must be a multiple of 4.

These procedures return the inexact real number object that best represents the
IEEE-754 single-precision number represented by the four bytes beginning at index
_k_.


###### [!Function] `bytevector-ieee-double-native-ref`  _bytevector_ _k_
###### [!Function] `bytevector-ieee-double-ref`  _bytevector_ _k_ _endianness_

[R6RS] _K_, …, _k_ + 7 must be valid indices of _bytevector_.
For `bytevector-ieee-double-native-ref`, _k_ must be a multiple of 8.

These procedures return the inexact real number object that best represents the
IEEE-754 double-precision number represented by the four bytes beginning at index
_k_.


###### [!Function] `bytevector-ieee-single-native-set!`  _bytevector_ _k_ _x_
###### [!Function] `bytevector-ieee-single-set!`  _bytevector_ _k_ _x_ _endianness_

[R6RS] _K_, …, _k_ + 3 must be valid indices of _bytevector_.
For `bytevector-ieee-single-native-set!`, _k_ must be a multiple of 4.

These procedures store an IEEE-754 single-precision representation of _x_into elements _k_ through _k_ + 3 of _bytevector_, and return
unspecified values.


###### [!Function] `bytevector-ieee-double-native-set!`  _bytevector_ _k_ _x_
###### [!Function] `bytevector-ieee-double-set!`  _bytevector_ _k_ _x_ _endianness_

[R6RS] _K_, …, _k_ + 7 must be valid indices of _bytevector_.
For `bytevector-ieee-double-native-set!`, _k_ must be a multiple of 8.

These procedures store an IEEE-754 double-precision representation of _x_into elements _k_ through _k_ + 7 of _bytevector_, and return
unspecified values.


### [§3] Operation on strings

This section describes procedures that convert between strings and bytevectors
containing Unicode encodings of those strings. When decoding bytevectors,
encoding errors are handled as with the replace semantics of textual I/O: If an
invalid or incomplete character encoding is encountered, then the replacement
character U+FFFD is appended to the string being generated, an appropriate number
of bytes are ignored, and decoding continues with the following bytes.

###### [!Function] `string->utf8`  _string_ _:optional_ _start_ _end_

[R6RS+] [R7RS] Returns a newly allocated (unless empty) bytevector that
contains the UTF-8 encoding of the given _string_.

If the optional argument _start_ is given, the procedure converts given
string from _start_ index (inclusive).

If the optional argument _end_ is given, the procedure converts given
string to _end_ index (exclusive).

These optional arguments must be fixnum if it's given.


###### [!Function] `string->utf16`  _string_ _:optional_ _endianness_

[R6RS] If _endianness_ is specified, it must be the symbol `big`or the symbol `little`. The `string->utf16` procedure returns a newly
allocated (unless empty) bytevector that contains the UTF-16BE or UTF-16LE
encoding of the given _string_ (with no byte-order mark). If _endianness_is not specified or is `big`, then UTF-16BE is used. If _endianness_ is
`little`, then UTF-16LE is used.


###### [!Function] `string->utf32`  _string_ _:optional_ _endianness_

[R6RS] If _endianness_ is specified, it must be the symbol `big`or the symbol `little`. The `string->utf32` procedure returns a newly
allocated (unless empty) bytevector that contains the UTF-32BE or UTF-32LE
encoding of the given _string_ (with no byte-order mark). If _endianness_is not specified or is `big`, then UTF-32BE is used. If _endianness_ is
`little`, then UTF-32LE is used.


###### [!Function] `utf8->string`  _bytevector_

[R6RS] Returns a newly allocated (unless empty) string whose character
sequence is encoded by the given _bytevector_.

If the optional argument _start_ is given, the procedure converts given
string from _start_ index (inclusive).

If the optional argument _end_ is given, the procedure converts given
string to _end_ index (exclusive).

These optional arguments must be fixnum if it's given.


###### [!Function] `utf16->string`  _bytevector_ _endianness_ _:optional_ _endianness-mandatory?_

[R6RS] _Endianness_ must be the symbol `big` or the symbol
`little`. The `utf16->string` procedure returns a newly allocated
(unless empty) string whose character sequence is encoded by the given
_bytevector_. _Bytevector_ is decoded according to UTF-16BE or UTF-16LE:
If _endianness-mandatory?_ is absent or #f, `utf16->string` determines
the endianness according to a UTF-16 BOM at the beginning of _bytevector_if a BOM is present; in this case, the BOM is not decoded as a character. Also
in this case, if no UTF-16 BOM is present, _endianness_ specifies the endianness
of the encoding. If _endianness-mandatory?_ is a true value, _endianness_specifies the endianness of the encoding, and any UTF-16 BOM in the encoding is
decoded as a regular character.


###### [!Function] `utf32->string`  _bytevector_ _endianness_ _:optional_ _endianness-mandatory?_

[R6RS] _Endianness_ must be the symbol `big` or the symbol
`little`. The `utf32->string` procedure returns a newly allocated
(unless empty) string whose character sequence is encoded by the given
_bytevector_. _Bytevector_ is decoded according to UTF-32BE or UTF-32LE:
If _endianness-mandatory?_ is absent or #f, `utf32->string` determines
the endianness according to a UTF-32 BOM at the beginning of _bytevector_if a BOM is present; in this case, the BOM is not decoded as a character. Also
in this case, if no UTF-32 BOM is present, _endianness_ specifies the endianness
of the encoding. If _endianness-mandatory?_ is a true value, _endianness_specifies the endianness of the encoding, and any UTF-32 BOM in the encoding is
decoded as a regular character.


