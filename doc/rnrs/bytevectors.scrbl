@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.bytevectors.6"]{Bytevectors}

Many applications deal with blocks of binary data by accessing them in various
ways—extracting signed or unsigned numbers of various sizes. Therefore, the 
@code{(rnrs bytevectors (6))}library provides a single type for blocks of binary
data with multiple ways to access that data. It deals with integers and
floating-point representations in various sizes with specified endianness.

Bytevectorsare objects of a disjoint type. Conceptually, a bytevector represents
a sequence of 8-bit bytes. The description of bytevectors uses the term byte for
an exact integer object in the interval { - 128, ..., 127} and the term octet for
an exact integer object in the interval {0, ..., 255}. A byte corresponds to its
two’s complement representation as an octet.

The length of a bytevector is the number of bytes it contains. This number is
fixed. A valid index into a bytevector is an exact, non-negative integer object
less than the length of the bytevector. The first byte of a bytevector has index
0; the last byte has an index one less than the length of the bytevector.

Generally, the access procedures come in different flavors according to the size
of the represented integer and the endianness of the representation. The procedures
also distinguish signed and unsigned representations. The signed representations
all use two’s complement.

Like string literals, literals representing bytevectors do not need to be quoted:
@snipet[=> #vu8(12 23 123)]{#vu8(12 23 123)}

@define[Library]{@name{(rnrs bytevectors (6))}}
@desc{[R6RS] This library provides a single type for blocks of binary data with
multiple ways to access that data.
}

@subsubsection{General operations}

@define[Macro]{@name{endianness} @args{symbol}}
@desc{[R6RS] The name of @var{symbol} must be a symbol describing an endianness.
@code{(endianness @var{symbol})} evaluates to the symbol named @var{symbol}.
Whenever one of the procedures operating on bytevectors accepts an endianness as
an argument, that argument must be one of these symbols. It is a syntax violation
for symbol to be anything other than an endianness symbol supported by the Sagittarius.

Currently, Sagittarius supports these symbols; @code{big}, @code{little}
and @code{native}.
}

@define[Function]{@name{native-endianness}}
@desc{[R6RS] Returns the endianness symbol associated platform endianness.
This may be a symbol either big or little.
}

@define[Function]{@name{bytevector?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is a bytevector, otherwise returns #f.}

@define[Function]{@name{make-bytevector} @args{k :optional fill}}
@desc{[R6RS] Returns a newly allocated bytevector of @var{k} bytes.

If the @var{fill} argument is missing, the initial contents of the returned
bytevector are 0.

If the @var{fill} argument is present, it must be an exact integer object in the
interval {-128, ..., 255} that specifies the initial value for the bytes of the
bytevector: If @var{fill} is positive, it is interpreted as an octet; if it is
negative, it is interpreted as a byte.
}

@define[Function]{@name{bytevector-length} @args{bytevector}}
@desc{[R6RS] Returns, as an exact integer object, the number of bytes in
@var{bytevector}.}

@define[Function]{@name{bytevector=?} @args{bytevector1 bytevector2}}
@desc{[R6RS] Returns #t if @var{bytevector1} and @var{bytevector2} are equal—that
is, if they have the same length and equal bytes at all valid indices. It
returns #f otherwise.
}

@define[Function]{@name{bytevector-fill!} @args{bytevector fill}}
@desc{[R6RS] The @var{fill} argument is as in the description of the
@code{make-bytevector} procedure. The @code{bytevector-fill!} procedure stores
@var{fill} in every element of @var{bytevector} and returns unspecified values.
Analogous to @code{vector-fill!}.
}

@define[Function]{@name{bytevector-copy!} @args{source source-start target target-start k}}
@desc{[R6RS] @var{Source} and @var{target} must be bytevectors. @var{Source-start},
@var{target-start}, and @var{k} must be non-negative exact integer objects that satisfy

0 ≤ @var{source-start} ≤ @var{source-start} + @var{k} ≤ @var{source-length}
0 ≤ @var{target-start} ≤ @var{target-start} + @var{k} ≤ @var{target-length}

where @var{source-length} is the length of @var{source} and @var{target-length}
is the length of @var{target}.

The @code{bytevector-copy!} procedure copies the bytes from @var{source} at indices

@var{source-start}, ... @var{source-start} + @var{k} - 1

to consecutive indices in @var{target} starting at @var{target-index}.

This returns unspecified values.
}

@define[Function]{@name{bytevector-copy} @args{bytevector}}
@desc{[R6RS] Returns a newly allocated copy of @var{bytevector}.}

@subsubsection{Operation on bytes and octets}

@define[Function]{@name{bytevector-u8-ref} @args{bytevector k}}
@define[Function]{@name{bytevector-s8-ref} @args{bytevector k}}
@desc{[R6RS] @var{K} must be a valid index of @var{bytevector}.

The @code{bytevector-u8-ref} procedure returns the byte at index @var{k} of
@var{bytevector}, as an octet.

The @code{bytevector-s8-ref} procedure returns the byte at index @var{k} of
@var{bytevector}, as a (signed) byte.
}

@define[Function]{@name{bytevector-u8-set!} @args{bytevector k octet}}
@define[Function]{@name{bytevector-s8-set!} @args{bytevector k byte}}
@desc{[R6RS] @var{K} must be a valid index of @var{bytevector}.

The @code{bytevector-u8-set!} procedure stores @var{octet} in element @var{k}
of @var{bytevector}.

The @code{bytevector-s8-set!} procedure stores the two’s-complement
representation of @var{byte} in element @var{k} of @var{bytevector}.

Both procedures return unspecified values.
}

@define[Function]{@name{bytevector->u8-list} @args{bytevector}}
@define[Function]{@name{u8-list->bytevector} @args{bytevector}}
@desc{[R6RS] @var{List} must be a list of octets.

The @code{bytevector->u8-list} procedure returns a newly allocated list of the
octets of @var{bytevector} in the same order.

The @code{u8-list->bytevector} procedure returns a newly allocated bytevector
whose elements are the elements of list @var{list}, in the same order. It is
analogous to @code{list->vector}.
}

@subsubsection{Operations on integers of arbitary size}

@define[Function]{@name{bytevector-uint-ref} @args{bytevector k endianness size}}
@define[Function]{@name{bytevector-sint-ref} @args{bytevector k endianness size}}
@define[Function]{@name{bytevector-uint-set!} @args{bytevector k n endianness size}}
@define[Function]{@name{bytevector-sint-set!} @args{bytevector k n endianness size}}
@desc{[R6RS] @var{Size} must be a positive exact integer object.
@var{K}, ..., @var{k} + @var{size} - 1 must be valid indices of @var{bytevector}.

The @code{bytevector-uint-ref} procedure retrieves the exact integer object
corresponding to the unsigned representation of size @var{size} and specified
by @var{endianness} at indices @var{k}, ..., @var{k} + @var{size} - 1.

The @code{bytevector-sint-ref} procedure retrieves the exact integer object
corresponding to the two’s-complement representation of size @var{size} and
specified by @var{endianness} at indices @var{k}, ..., @var{k} + @var{size} - 1.

For @code{bytevector-uint-set!}, @var{n} must be an exact integer object in the
interval @math{{0, ..., 256 ^ "size" - 1}}

The @code{bytevector-uint-set!} procedure stores the unsigned representation of
size @var{size} and specified by @var{endianness} into @var{bytevector} at indices
@var{k}, ..., @var{k} + @var{size} - 1.

For @code{bytevector-sint-set!}, @var{n} must be an exact integer object in the
interval @math{{-256 ^ "size" / 2, ..., 256 ^ "size" / 2 - 1}}.
@code{bytevector-sint-set!} stores the two’s-complement representation of size
@var{size} and specified by @var{endianness} into @var{bytevector} at indices
@var{k}, ..., @var{k} + @var{size} - 1.

The @code{...-set!} procedures return unspecified values.
}

@define[Function]{@name{bytevector->uint-list} @args{bytevector endianness size}}
@define[Function]{@name{bytevector->sint-list} @args{bytevector endianness size}}
@define[Function]{@name{uint-list->bytevector} @args{list endianness size}}
@define[Function]{@name{sint-list->bytevector} @args{list endianness size}}
@desc{[R6RS] @var{Size} must be a positive exact integer object. For 
@code{uint-list->bytevector}, @var{list} must be a list of exact integer objects
in the interval @math{{0, ..., 256 ^ "size" - 1}}. For @code{sint-list->bytevector},
@var{list} must be a list of exact integer objects in the interval
@math{{-256 ^ "size"/2, ..., 256 ^ "size"/2 - 1}}. The length of @var{bytevector}
or, respectively, of @var{list} must be divisible by @var{size}.

These procedures convert between lists of integer objects and their consecutive
representations according to @var{size} and @var{endianness} in the @var{bytevector}
objects in the same way as @code{bytevector->u8-list} and @code{u8-list->bytevector}
do for one-byte representations.
}

@subsubsection{Operation on 16-bit integers}

@define[Function]{@name{bytevector-u16-ref} @args{bytevector k endianness}}
@define[Function]{@name{bytevector-s16-ref} @args{bytevector k endianness}}
@define[Function]{@name{bytevector-u16-native-ref} @args{bytevector k}}
@define[Function]{@name{bytevector-s16-native-ref} @args{bytevector k}}
@define[Function]{@name{bytevector-u16-set!} @args{bytevector k n endianness}}
@define[Function]{@name{bytevector-s16-set!} @args{bytevector k n endianness}}
@define[Function]{@name{bytevector-u16-native-set!} @args{bytevector k n}}
@define[Function]{@name{bytevector-s16-native-set!} @args{bytevector k n}}
@desc{[R6RS] @var{K} must be a valid index of @var{bytevector}; so must @var{k} + 1.
For @code{bytevector-u16-set!} and @code{bytevector-u16-native-set!}, @var{n}
must be an exact integer object in the interval @math{{0, ..., 2 ^ 16 - 1}}.
For @code{bytevector-s16-set!} and @code{bytevector-s16-native-set!}, @var{n}
must be an exact integer object in the interval @math{{-2 ^ 15, ..., 2 ^ 15 - 1}}.

These retrieve and set two-byte representations of numbers at indices @var{k}
and @var{k} + 1, according to the endianness specified by @var{endianness}.
The procedures with @code{u16} in their names deal with the unsigned representation;
those with @code{s16} in their names deal with the two’s-complement representation.

The procedures with @code{native} in their names employ the native endianness,
and work only at aligned indices: @var{k} must be a multiple of 2.

The @code{...-set!} procedures return unspecified values.
}

@subsubsection{Operation on 32-bit integers}

@define[Function]{@name{bytevector-u32-ref} @args{bytevector k endianness}}
@define[Function]{@name{bytevector-s32-ref} @args{bytevector k endianness}}
@define[Function]{@name{bytevector-u32-native-ref} @args{bytevector k}}
@define[Function]{@name{bytevector-s32-native-ref} @args{bytevector k}}
@define[Function]{@name{bytevector-u32-set!} @args{bytevector k n endianness}}
@define[Function]{@name{bytevector-s32-set!} @args{bytevector k n endianness}}
@define[Function]{@name{bytevector-u32-native-set!} @args{bytevector k n}}
@define[Function]{@name{bytevector-s32-native-set!} @args{bytevector k n}}
@desc{[R6RS] @var{K} must be a valid index of @var{bytevector}; so must @var{k} + 3.
For @code{bytevector-u32-set!} and @code{bytevector-u32-native-set!}, @var{n}
must be an exact integer object in the interval @math{{0, ..., 2 ^ 32 - 1}}.
For @code{bytevector-s32-set!} and @code{bytevector-s32-native-set!}, @var{n}
must be an exact integer object in the interval @math{{-2 ^ 31, ..., 2 ^ 32 - 1}}.

These retrieve and set two-byte representations of numbers at indices @var{k}
and @var{k} + 3, according to the endianness specified by @var{endianness}.
The procedures with @code{u32} in their names deal with the unsigned representation;
those with @code{s32} in their names deal with the two’s-complement representation.

The procedures with @code{native} in their names employ the native endianness,
and work only at aligned indices: @var{k} must be a multiple of 4.

The @code{...-set!} procedures return unspecified values.
}

@subsubsection{Operation on 64-bit integers}

@define[Function]{@name{bytevector-u64-ref} @args{bytevector k endianness}}
@define[Function]{@name{bytevector-s64-ref} @args{bytevector k endianness}}
@define[Function]{@name{bytevector-u64-native-ref} @args{bytevector k}}
@define[Function]{@name{bytevector-s64-native-ref} @args{bytevector k}}
@define[Function]{@name{bytevector-u64-set!} @args{bytevector k n endianness}}
@define[Function]{@name{bytevector-s64-set!} @args{bytevector k n endianness}}
@define[Function]{@name{bytevector-u64-native-set!} @args{bytevector k n}}
@define[Function]{@name{bytevector-s64-native-set!} @args{bytevector k n}}
@desc{[R6RS] @var{K} must be a valid index of @var{bytevector}; so must @var{k} + 7.
For @code{bytevector-u64-set!} and @code{bytevector-u64-native-set!}, @var{n}
must be an exact integer object in the interval @math{{0, ..., 2 ^ 64 - 1}}.
For @code{bytevector-s64-set!} and @code{bytevector-s64-native-set!}, @var{n}
must be an exact integer object in the interval @math{{-2 ^ 63, ..., 2 ^ 64 - 1}}.

These retrieve and set two-byte representations of numbers at indices @var{k}
and @var{k} + 7, according to the endianness specified by @var{endianness}.
The procedures with @code{u64} in their names deal with the unsigned representation;
those with @code{s64} in their names deal with the two’s-complement representation.

The procedures with @code{native} in their names employ the native endianness,
and work only at aligned indices: @var{k} must be a multiple of 8.

The @code{...-set!} procedures return unspecified values.
}

@subsubsection{Operation on IEEE-754 representations}

@define[Function]{@name{bytevector-ieee-single-native-ref} @args{bytevector k}}
@define[Function]{@name{bytevector-ieee-single-ref} @args{bytevector k endianness}}
@desc{[R6RS] @var{K}, …, @var{k} + 3 must be valid indices of @var{bytevector}.
For @code{bytevector-ieee-single-native-ref}, @var{k} must be a multiple of 4.

These procedures return the inexact real number object that best represents the
IEEE-754 single-precision number represented by the four bytes beginning at index
@var{k}.
}

@define[Function]{@name{bytevector-ieee-double-native-ref} @args{bytevector k}}
@define[Function]{@name{bytevector-ieee-double-ref} @args{bytevector k endianness}}
@desc{[R6RS] @var{K}, …, @var{k} + 7 must be valid indices of @var{bytevector}.
For @code{bytevector-ieee-double-native-ref}, @var{k} must be a multiple of 8.

These procedures return the inexact real number object that best represents the
IEEE-754 double-precision number represented by the four bytes beginning at index
@var{k}.
}

@define[Function]{@name{bytevector-ieee-single-native-set!} @args{bytevector k x}}
@define[Function]{@name{bytevector-ieee-single-set!} @args{bytevector k x endianness}}
@desc{[R6RS] @var{K}, …, @var{k} + 3 must be valid indices of @var{bytevector}.
For @code{bytevector-ieee-single-native-set!}, @var{k} must be a multiple of 4.

These procedures store an IEEE-754 single-precision representation of @var{x}
into elements @var{k} through @var{k} + 3 of @var{bytevector}, and return
unspecified values.
}

@define[Function]{@name{bytevector-ieee-double-native-set!} @args{bytevector k x}}
@define[Function]{@name{bytevector-ieee-double-set!} @args{bytevector k x endianness}}
@desc{[R6RS] @var{K}, …, @var{k} + 7 must be valid indices of @var{bytevector}.
For @code{bytevector-ieee-double-native-set!}, @var{k} must be a multiple of 8.

These procedures store an IEEE-754 double-precision representation of @var{x}
into elements @var{k} through @var{k} + 7 of @var{bytevector}, and return
unspecified values.
}

@subsubsection{Operation on strings}

This section describes procedures that convert between strings and bytevectors
containing Unicode encodings of those strings. When decoding bytevectors,
encoding errors are handled as with the replace semantics of textual I/O: If an
invalid or incomplete character encoding is encountered, then the replacement
character U+FFFD is appended to the string being generated, an appropriate number
of bytes are ignored, and decoding continues with the following bytes.

@define[Function]{@name{string->utf8} @args{string}}
@desc{[R6RS] Returns a newly allocated (unless empty) bytevector that contains
the UTF-8 encoding of the given @var{string}.}

@define[Function]{@name{string->utf16} @args{string :optional endianness}}
@desc{[R6RS] If @var{endianness} is specified, it must be the symbol @code{big}
or the symbol @code{little}. The @code{string->utf16} procedure returns a newly
allocated (unless empty) bytevector that contains the UTF-16BE or UTF-16LE
encoding of the given @var{string} (with no byte-order mark). If @var{endianness}
is not specified or is @code{big}, then UTF-16BE is used. If @var{endianness} is
@code{little}, then UTF-16LE is used.
}

@define[Function]{@name{string->utf32} @args{string :optional endianness}}
@desc{[R6RS] If @var{endianness} is specified, it must be the symbol @code{big}
or the symbol @code{little}. The @code{string->utf32} procedure returns a newly
allocated (unless empty) bytevector that contains the UTF-32BE or UTF-32LE
encoding of the given @var{string} (with no byte-order mark). If @var{endianness}
is not specified or is @code{big}, then UTF-32BE is used. If @var{endianness} is
@code{little}, then UTF-32LE is used.
}

@define[Function]{@name{utf8->string} @args{bytevector}}
@desc{[R6RS] Returns a newly allocated (unless empty) string whose character
sequence is encoded by the given @var{bytevector}.
}

@define[Function]{@name{utf16->string} @args{bytevector endianness :optional endianness-mandatory?}}
@desc{[R6RS] @var{Endianness} must be the symbol @code{big} or the symbol
@code{little}. The @code{utf16->string} procedure returns a newly allocated
(unless empty) string whose character sequence is encoded by the given
@var{bytevector}. @var{Bytevector} is decoded according to UTF-16BE or UTF-16LE:
If @var{endianness-mandatory?} is absent or #f, @code{utf16->string} determines
the endianness according to a UTF-16 BOM at the beginning of @var{bytevector}
if a BOM is present; in this case, the BOM is not decoded as a character. Also
in this case, if no UTF-16 BOM is present, @var{endianness} specifies the endianness
of the encoding. If @var{endianness-mandatory?} is a true value, @var{endianness}
specifies the endianness of the encoding, and any UTF-16 BOM in the encoding is
decoded as a regular character.
}

@define[Function]{@name{utf32->string} @args{bytevector endianness :optional endianness-mandatory?}}
@desc{[R6RS] @var{Endianness} must be the symbol @code{big} or the symbol
@code{little}. The @code{utf32->string} procedure returns a newly allocated
(unless empty) string whose character sequence is encoded by the given
@var{bytevector}. @var{Bytevector} is decoded according to UTF-32BE or UTF-32LE:
If @var{endianness-mandatory?} is absent or #f, @code{utf32->string} determines
the endianness according to a UTF-32 BOM at the beginning of @var{bytevector}
if a BOM is present; in this case, the BOM is not decoded as a character. Also
in this case, if no UTF-32 BOM is present, @var{endianness} specifies the endianness
of the encoding. If @var{endianness-mandatory?} is a true value, @var{endianness}
specifies the endianness of the encoding, and any UTF-32 BOM in the encoding is
decoded as a regular character.
}
