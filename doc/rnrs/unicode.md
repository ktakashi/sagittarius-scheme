[§2] Unicode {#rnrs.unicode.6}
-------------

###### [!Library] `(rnrs unicode (6))` 

[R6RS] The procedures exported by the `(rnrs unicode (6))`library
provide access to some aspects of the Unicode semantics for characters and strings:
category information, case-independent comparisons, case mappings, and normalization.

Some of the procedures that operate on characters or strings ignore the difference
between upper case and lower case. These procedures have "-ci"
(for "case insensitive") embedded in their names.


### [§3] Characters

###### [!Function] `char-upcase`  _char_
###### [!Function] `char-downcase`  _char_
###### [!Function] `char-titlecase`  _char_
###### [!Function] `char-foldcase`  _char_

[R6RS] These procedures take a character argument and return a character
result. If the argument is an upper-case or title-case character, and if there
is a single character that is its lower-case form, then `char-downcase`returns that character. If the argument is a lower-case or title-case character,
and there is a single character that is its upper-case form, then `char-upcase`returns that character. If the argument is a lower-case or upper-case character,
and there is a single character that is its title-case form, then `char-titlecase`returns that character. If the argument is not a title-case character and there
is no single character that is its title-case form, then `char-titlecase`returns the upper-case form of the argument. Finally, if the character has a
case-folded character, then `char-foldcase` returns that character. Otherwise
the character returned is the same as the argument. For Turkic characters İ (#\\x130)
and ı (#\\x131), `char-foldcase` behaves as the identity function; otherwise
`char-foldcase` is the same as `char-downcase` composed with 
`char-upcase`.


###### [!Function] `char-ci=?`  _char1_ _char2_ _char3_ _..._
###### [!Function] `char-ci>?`  _char1_ _char2_ _char3_ _..._
###### [!Function] `char-ci<?`  _char1_ _char2_ _char3_ _..._
###### [!Function] `char-ci>=?`  _char1_ _char2_ _char3_ _..._
###### [!Function] `char-ci<=?`  _char1_ _char2_ _char3_ _..._

[R6RS] These procedures are similar to `char=?`, etc., but operate on
the case-folded versions of the characters.


###### [!Function] `char-alphabetic?`  _char_
###### [!Function] `char-numeric?`  _char_
###### [!Function] `char-whitespace?`  _char_
###### [!Function] `char-upper-case?`  _char_
###### [!Function] `char-lower-case?`  _char_
###### [!Function] `char-title-case?`  _char_

[R6RS] These procedures return #t if their arguments are alphabetic, numeric,
whitespace, upper-case, lower-case, or title-case characters, respectively;
otherwise they return #f.

A character is alphabetic if it has the Unicode "Alphabetic" property. A character
is numeric if it has the Unicode "Numeric" property. A character is whitespace if
has the Unicode "White_Space" property. A character is upper case if it has the
Unicode "Uppercase" property, lower case if it has the "Lowercase" property, and
title case if it is in the Lt general category.


###### [!Function] `char-general-category`  _char_

[R6RS] Returns a symbol representing the Unicode general category of char,
one of Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Ps, Pe, Pi, Pf, Pd, Pc, Po,
Sc, Sm, Sk, So, Zs, Zp, Zl, Cc, Cf, Cs, Co, or Cn.


### [§3] Strings

###### [!Function] `string-upcase`  _string_ _start_ _end_
###### [!Function] `string-downcase`  _string_ _start_ _end_
###### [!Function] `string-titlecase`  _string_ _start_ _end_
###### [!Function] `string-foldcase`  _string_ _start_ _end_

[R6RS+][SRFI-13] These procedures take a string argument and return a
string result. They are defined in terms of Unicode's locale-independent case
mappings from Unicode scalar-value sequences to scalar-value sequences. In
particular, the length of the result string can be different from the length of 
the input string. When the specified result is equal in the sense of
`string=?` to the argument, these procedures may return the argument
instead of a newly allocated string.

The `string-upcase` procedure converts a string to upper case;
`string-downcase` converts a string to lower case.

The `string-foldcase` procedure converts the string to its case-folded
counterpart, using the full case-folding mapping, but without the special
mappings for Turkic languages.

The `string-titlecase` procedure converts the first cased character of each
word via `char-titlecase`, and downcases all other cased characters.

If the optional argument _start_ and _end_ are given, these must be
exact integer and the procedures will first substring the given string with 
range _start_ and _end_ then convert it.


###### [!Function] `string-ci=?`  _string1_ _string2_ _string3_ _..._
###### [!Function] `string-ci>?`  _string1_ _string2_ _string3_ _..._
###### [!Function] `string-ci<?`  _string1_ _string2_ _string3_ _..._
###### [!Function] `string-ci>=?`  _string1_ _string2_ _string3_ _..._
###### [!Function] `string-ci<=?`  _string1_ _string2_ _string3_ _..._

[R6RS] These procedures are similar to `string=?`, etc., but operate
on the case-folded versions of the strings.


###### [!Function] `string-nfd`  _string_
###### [!Function] `string-nfkd`  _string_
###### [!Function] `string-nfc`  _string_
###### [!Function] `string-nfkc`  _string_

[R6RS] These procedures take a string argument and return a string result,
which is the input string normalized to Unicode normalization form D, KD, C, or KC,
respectively. When the specified result is equal in the sense of `string=?`to the argument, these procedures may return the argument instead of a newly
allocated string.


