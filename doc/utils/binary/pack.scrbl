@; -*- mode:scribble; coding: utf-8; -*-

@subsection[:tag "util.binary.pack"]{(binary pack) - Packing binary data}

@define[Library]{@name{(binary pack)}}
@desc{This library provides an interface for packing and unpacking (writing
and reading) binary data with template. The functionality is inspired by
@hyperlink[:href "https://weinholt.se/industria/"]{Industria}'s
@code{(weinholt struct pack)} library.
}

@define[Macro]{@name{pack} @args{template args @dots{}}}
@desc{@var{template} must be a string.

Construct a bytevector with given @var{args} according to the given
@var{template}. Template characters are described below.
}

@define[Macro]{@name{pack!} @args{template bv offset args @dots{}}}
@desc{@var{template} must be a string. 

@var{bv} must be a bytevector.

@var{offset} must a non-negative exact integer.

Converts given @var{args} and put it into @var{bv} starting from @var{offset}.
The conversion is done according to the @var{template} string.

The template characters are extensible so following description can only cover
predefined characters.

x: padding; c: s8; C: u8; s: s16; S: u16; l: s32; L: u32; q: s64; Q: u64;
f: ieee-single; d: ieee-double; ! or >: big-endian; <: little-endian;
=: native-endian; u: disable natural alignment; a: enable natural alignment.
Whitespace is ignored.

@snipet[=> "#vu8(128)"]{(pack "!c" 128)}
@snipet[=> "#vu8(100 0)"]{(pack "s" 100)}
@snipet[=> "#vu8(0 100)"]{(pack "!s" 100)}
@snipet[=> "#vu8(64 9 30 184 81 235 133 31)"]{(pack "!d" 3.14)}

Fields are by default aligned to their natural alignment and NUL bytes are
inserted as necessary to have a field's index to be aligned to its size.

@snipet[=> "#vu8(0 0 0 0 0 0 0 0 64 9 30 184 81 235 133 31)"]{(pack "!xd" 3.14)}
@snipet[=> "#vu8(0 64 9 30 184 81 235 133 31)"]{(pack "!uxd" 3.14)}

Digits in front of the syntax characters means repetition. And @code{#\*} means
indefinite length repetition.

@snipet[=> "#vu8(1 2 3)"]{(pack "3c" 1 2 3)}
@snipet[=> "#vu8(1 2 3 4)"]{(pack "*c" 1 2 3 4)}

When the macro detects the given template is string, then it tries to expand
as much as possible. So it might raises the different condition even if the
template strings are the same.

@snipet[=> "&syntax"]{(pack "3c" 1 2 3 4)}
@snipet[=> "&error"]{(pack (car '("3c")) 1 2 3 4)}

}

@define[Macro]{@name{unpack} @args{template bv}}
@define[Macro]{@name{unpack} @args{template bv offset}}
@desc{@var{template} must be a string.

Unpack the given bytevector according to the given @var{template} and returns
the values. The template syntax are the same as @code{pack!}.

If the second form is used, then unpacking is done from the given @var{offset}.

@snipet[=> "1 2"]{(unpack "!SS" #vu8(0 1 0 2))}
@snipet[=> "2 3"]{(unpack "!SS" #vu8(0 1 0 2 0 3) 1)}
@snipet[=> "256 512"]{(unpack "!uSS" #vu8(0 1 0 2 0 3) 1)}
}

@define[Macro]{@name{get-unpack} @args{port template}}
@desc{@var{template} must be a string.

Utility unpacking macro for binary port.
}

@define[Macro]{@name{format-size} @args{template}}
@define[Macro]{@name{format-size} @args{template args @dots{}}}
@desc{@var{template} must be a string.

Calculate the size of the result bytevector. If the second form is used, then
macro can calculate even if the template contains indefinite length syntax
@code{#\*}, otherwise #f is returned.

@snipet[=> 16]{(format-size "!xd")}
@snipet[=> 9]{(format-size "!uxd")}
@snipet[=> #f]{(format-size "*c")}
@snipet[=> 4]{(format-size "*c" 1 2 3 4)}
}

@define[Macro]{@name{define-@var{**}-packer}
 @args{(char arg) (@code{pack} expr1 @dots{}) (@code{unpack} expr2 @dots{})}}
@desc{@var{char} must character.

@code{pack} and @code{unpack} are syntactic keywords.

Defines packing extension to given @var{char}. This macro can not overwrite
the predefined characters. @var{**} can be followings;

@code{s8}, @code{u8}, @code{s16}, @code{u16}, @code{s32}, @code{u32},
@code{s64}, @code{u64}, @code{f32}, and @code{f64}.

@codeblock{
;; defining char to u8 converter
(define-u8-packer (#\A v)
  (pack (char->integer v))
  (unpack (integer->char v)))
(pack "AA" #\a #\b)       ;; => #vu8(97 98)
(unpack "AA" #vu8(97 98)) ;; => #\a #\b
}

}
