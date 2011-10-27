@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.unicode.6"]{Unicode}

@define[Library]{@name{(rnrs unicode (6))}}
@desc{[R6RS] The procedures exported by the @code{(rnrs unicode (6))}library
provide access to some aspects of the Unicode semantics for characters and strings:
category information, case-independent comparisons, case mappings, and normalization.

Some of the procedures that operate on characters or strings ignore the difference
between upper case and lower case. These procedures have “-ci” (for “case insensitive”)
embedded in their names.
}

@subsubsection{Characters}

@define[Function]{@name{char-upcase} @args{char}}
@define[Function]{@name{char-downcase} @args{char}}
@define[Function]{@name{char-titlecase} @args{char}}
@define[Function]{@name{char-foldcase} @args{char}}
@desc{[R6RS] These procedures take a character argument and return a character
result. If the argument is an upper-case or title-case character, and if there
is a single character that is its lower-case form, then @code{char-downcase}
returns that character. If the argument is a lower-case or title-case character,
and there is a single character that is its upper-case form, then @code{char-upcase}
returns that character. If the argument is a lower-case or upper-case character,
and there is a single character that is its title-case form, then @code{char-titlecase}
returns that character. If the argument is not a title-case character and there
is no single character that is its title-case form, then @code{char-titlecase}
returns the upper-case form of the argument. Finally, if the character has a
case-folded character, then @code{char-foldcase} returns that character. Otherwise
the character returned is the same as the argument. For Turkic characters İ (#\x130)
and ı (#\x131), @code{char-foldcase} behaves as the identity function; otherwise
@code{char-foldcase} is the same as @code{char-downcase} composed with 
@code{char-upcase}.
}

@define[Function]{@name{char-ci=?} @args{char1 char2 char3 @dots{}}}
@define[Function]{@name{char-ci>?} @args{char1 char2 char3 @dots{}}}
@define[Function]{@name{char-ci<?} @args{char1 char2 char3 @dots{}}}
@define[Function]{@name{char-ci>=?} @args{char1 char2 char3 @dots{}}}
@define[Function]{@name{char-ci<=?} @args{char1 char2 char3 @dots{}}}
@desc{[R6RS] These procedures are similar to @code{char=?}, etc., but operate on
the case-folded versions of the characters.
}

@define[Function]{@name{char-alphabetic?} @args{char}}
@define[Function]{@name{char-numeric?} @args{char}}
@define[Function]{@name{char-whitespace?} @args{char}}
@define[Function]{@name{char-upper-case?} @args{char}}
@define[Function]{@name{char-lower-case?} @args{char}}
@define[Function]{@name{char-title-case?} @args{char}}
@desc{[R6RS] These procedures return #t if their arguments are alphabetic, numeric,
whitespace, upper-case, lower-case, or title-case characters, respectively;
otherwise they return #f.

A character is alphabetic if it has the Unicode “Alphabetic” property. A character
is numeric if it has the Unicode “Numeric” property. A character is whitespace if
has the Unicode “White_Space” property. A character is upper case if it has the
Unicode “Uppercase” property, lower case if it has the “Lowercase” property, and
title case if it is in the Lt general category.
}

@define[Function]{@name{char-general-category} @args{char}}
@desc{[R6RS] Returns a symbol representing the Unicode general category of char,
one of Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Ps, Pe, Pi, Pf, Pd, Pc, Po,
Sc, Sm, Sk, So, Zs, Zp, Zl, Cc, Cf, Cs, Co, or Cn.
}

@subsubsection{Strings}

@define[Function]{@name{string-upcase} @args{string}}
@define[Function]{@name{string-downcase} @args{string}}
@define[Function]{@name{string-titlecase} @args{string}}
@define[Function]{@name{string-foldcase} @args{string}}
@desc{[R6RS] These procedures take a string argument and return a string result.
They are defined in terms of Unicode’s locale-independent case mappings from
Unicode scalar-value sequences to scalar-value sequences. In particular, the
length of the result string can be different from the length of the input string.
When the specified result is equal in the sense of @code{string=?} to the argument,
these procedures may return the argument instead of a newly allocated string.

The @code{string-upcase} procedure converts a string to upper case;
@code{string-downcase} converts a string to lower case. The @code{string-foldcase}
procedure converts the string to its case-folded counterpart, using the full
case-folding mapping, but without the special mappings for Turkic languages.
The @code{string-titlecase} procedure converts the first cased character of each
word via @code{char-titlecase}, and downcases all other cased characters.
}

@define[Function]{@name{string-ci=?} @args{string1 string2 string3 @dots{}}}
@define[Function]{@name{string-ci>?} @args{string1 string2 string3 @dots{}}}
@define[Function]{@name{string-ci<?} @args{string1 string2 string3 @dots{}}}
@define[Function]{@name{string-ci>=?} @args{string1 string2 string3 @dots{}}}
@define[Function]{@name{string-ci<=?} @args{string1 string2 string3 @dots{}}}
@desc{[R6RS] These procedures are similar to @code{string=?}, etc., but operate
on the case-folded versions of the strings.
}

@define[Function]{@name{string-nfd} @args{string}}
@define[Function]{@name{string-nfkd} @args{string}}
@define[Function]{@name{string-nfc} @args{string}}
@define[Function]{@name{string-nfkc} @args{string}}
@desc{[R6RS] These procedures take a string argument and return a string result,
which is the input string normalized to Unicode normalization form D, KD, C, or KC,
respectively. When the specified result is equal in the sense of @code{string=?}
to the argument, these procedures may return the argument instead of a newly
allocated string.
}
