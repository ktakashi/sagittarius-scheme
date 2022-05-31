[§2] (sagittarius regex) - regular expression library {#lib.sagittarius.regex}
-------------

As most of script language have own regular expression mechanism, Sagittarius
also has own regular expression library. It is influenced by Java's regular
expression, so there are a lot of differences between the most famous Perl
regular expression(perlre).

This feature may be changed, if R7RS large requires Perl like regular
expression.

Following examples show how to use Sagittarius's regular expression.

``````````scheme
;; For Perl like
(cond ((looking-at (regex "^hello\\s*(.+)") "hello world!")
        => (lambda (m) (m 1))))
``````````
=> ``world!``

``````````scheme
;; For Java like
(cond ((matches (regex "(\\w+?)\\s*(.+)") "123hello world!")) ;; this won't match
      (else "incovenient eh?"))
``````````

The `matches` procedure is total match, so it ignores boundary matcher
`'^'` and `'$'`. The `looking-at` procedure is partial match, so
it works as if perlre. 

###### [!Library] `(sagittarius regex)` 

This library provides Sagittarius regular expression procedures.

### [§3] User level APIs for regular expression

###### [!Function] `regex`  _string_ _:optional_ _flags_

_String_ must be regular expression. Returns compiled regular
expression. _Flags_' descriptions are the end of this section. The following
table is the supported regular expression constructs.


| Construct                          | Matches                                                                                                             |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| **Characters**                     | <                                                                                                                   |
| `x`                                | The character x                                                                                                     |
| `\\`                               | The backslash character                                                                                             |
| `\0n`                              | The character with octal value 0n (0 \<= n \<= 7)                                                                   |
| `\0nn`                             | The character with octal value 0nn (0 \<= n \<= 7)                                                                  |
| `\0mnn`                            | The character with octal value 0mnn (0 \<= m \<= 3, 0 \<= n \<= 7)                                                  |
| `\xhh`                             | The character with hexadecimal value 0xhh                                                                           |
| `\uhhhh`                           | The character with hexadecimal value 0xhhhh                                                                         |
| `\Uhhhhhhhh`                       | The character with hexadecimal value<br>0xhhhhhhhh. If the value exceed the maxinum fixnum value it rases an error. |
| `\t`                               | The tab character ('\\u0009')                                                                                       |
| `\n`                               | The newline (line feed) character ('\\u000A')                                                                       |
| `\r`                               | The carriage-return character ('\\u000D')                                                                           |
| `\f`                               | The form-feed character ('\\u000C')                                                                                 |
| `\a`                               | The alert (bell) character ('\\u0007')                                                                              |
| `\e`                               | The escape character ('\\u001B')                                                                                    |
| `\cx`                              | The control character corresponding to x                                                                            |
| **Character classes**              | <                                                                                                                   |
| `[abc]`                            | a, b, or c (simple class)                                                                                           |
| `[^abc]`                           | Any character except a, b, or c (negation)                                                                          |
| `[a-zA-Z]`                         | a through z or A through Z, inclusive (range)                                                                       |
| `[a-d[m-p]]`                       | a through d, or m through p: [a-dm-p] (union)                                                                       |
| `[a-z&&[def]]`                     | d, e, or f (intersection)                                                                                           |
| `[a-z&&[^bc]]`                     | a through z, except for b and c: [ad-z] (subtraction)                                                               |
| `[a-z&&[^m-p]]`                    | a through z, and not m through p: [a-lq-z](subtraction)                                                             |
| **Predefined character classes**   | <                                                                                                                   |
| `.`                                | Any character (may or may not match line terminators)                                                               |
| `\d`                               | A digit: [0-9]                                                                                                      |
| `\D`                               | A non-digit: [^0-9]                                                                                                 |
| `\s`                               | A whitespace character: [ \\t\\n\\x0B\\f\\r]                                                                        |
| `\S`                               | A non-whitespace character: [^\\s]                                                                                  |
| `\w`                               | A word character: [a-zA-Z_0-9]                                                                                      |
| `\W`                               | A non-word character: [^\\w]                                                                                        |
| **Boundary matchers**              | <                                                                                                                   |
| `^`                                | The beginning of a line                                                                                             |
| `$`                                | The end of a line                                                                                                   |
| `\b`                               | A word boundary                                                                                                     |
| `\B`                               | A non-word boundary                                                                                                 |
| `\A`                               | The beginning of the input                                                                                          |
| `\G`                               | The end of the previous match                                                                                       |
| `\Z`                               | The end of the input but for the final terminator, if any                                                           |
| `\z`                               | The end of the input                                                                                                |
| **Greedy quantifiers**             | <                                                                                                                   |
| `X?`                               | X, once or not at all                                                                                               |
| `X*`                               | X, zero or more times                                                                                               |
| `X+`                               | X, one or more times                                                                                                |
| `X{n}`                             | X, exactly n times                                                                                                  |
| `X{n,}`                            | X, at least n times                                                                                                 |
| `X{n,m}`                           | X, at least n but not more than m times                                                                             |
| **Reluctant quantifiers**          | <                                                                                                                   |
| `X??`                              | X, once or not at all                                                                                               |
| `X*?`                              | X, zero or more times                                                                                               |
| `X+?`                              | X, one or more times                                                                                                |
| `X{n}?`                            | X, exactly n times                                                                                                  |
| `X{n,}?`                           | X, at least n times                                                                                                 |
| `X{n,m}?`                          | X, at least n but not more than m times                                                                             |
| **Possessive quantifiers**         | <                                                                                                                   |
| `X?+`                              | X, once or not at all                                                                                               |
| `X*+`                              | X, zero or more times                                                                                               |
| `X++`                              | X, one or more times                                                                                                |
| `X{n}+`                            | X, exactly n times                                                                                                  |
| `X{n,}+`                           | X, at least n times                                                                                                 |
| `X{n,m}+`                          | X, at least n but not more than m times                                                                             |
| **Logical operators**              | <                                                                                                                   |
| `XY`                               | X followed by Y                                                                                                     |
| `X\|Y`                             | Either X or Y                                                                                                       |
| `(X)`                              | X, as a capturing group                                                                                             |
| **Back references**                | <                                                                                                                   |
| `\n`                               | Whatever the nth capturing group matched                                                                            |
| **Quotation**                      | <                                                                                                                   |
| `\`                                | Nothing, but quotes the following character                                                                         |
| `\Q`                               | Nothing, but quotes all characters until \\E                                                                        |
| `\E`                               | Nothing, but ends quoting started by \\Q                                                                            |
| **Special constructs (non-capturing)** | <                                                                                                                   |
| `(?:X)`                            | X, as a non-capturing group                                                                                         |
| `(?imsux-imsux)`                   | Nothing, but turns match flags on - off                                                                             |
| `(?imsux-imsux:X)`                 | X, as a non-capturing group with the given flags on - off                                                           |
| `(?=X)`                            | X, via zero-width positive lookahead                                                                                |
| `(?!X)`                            | X, via zero-width negative lookahead                                                                                |
| `(?<=X)`                           | X, via zero-width positive lookbehind                                                                               |
| `(?<!X)`                           | X, via zero-width negative lookbehind                                                                               |
| `(?>X)`                            | X, as an independent, non-capturing group                                                                           |
| `(?#...)`                          | comment.                                                                                                            |

Since version 0.2.3, `\p` and `\P` are supported. It is cooporated
with SRFI-14 charset. However it is kind of tricky. For example regex parser
can reads `\p{InAscii}` or `\p{IsAscii}` and search charset named
`char-set:ascii` from current library. It must have `In` or `Is`as its prefix.


###### [!Reader Macro] `#/-reader` 

This reader macro provides Perl like regular expression syntax.
It allows you to write regular expression like this `#/\w+?/i` instead of
like this `(regex "\\w+?" CASE-INSENSITIVE)`.


###### [!Function] `looking-at`  _regex_ _string_

_Regex_ must be regular expression object. Returns closure if
_regex_ matches input _string_.

The `matches` procedure attempts to match the entire input string against
the pattern of _regex_.

The `looking-at` procedure attempts to match the input string against the
pattern of _regex_.


###### [!Function] `regex-replace-first`  _pattern_ _text_ _replacement_
###### [!Function] `regex-replace-first`  _matcher_ _replacement_
###### [!Function] `regex-replace-all`  _pattern_ _text_ _replacement_
###### [!Function] `regex-replace-all`  _matcher_ _replacement_

_Pattern_ must be pattern object.

The first form of these procedures are for convenience. It is implemented like
this;

``````````scheme
(define (regex-replace-all pattern text replacement)
  (regex-replace-all (regex-matcher pattern text) replacement))
``````````

_Text_ must be string.

_Replacement_ must be either string or procedure which takes matcher object
and string port as its arguments respectively.

Replaces part of _text_ where _regex_ matches with _replacement_.

If _replacement_ is a string, the procedure replace _text_ with given
string. _Replacement_ can refer the match result with \``$_n_`\`.
_n_ must be group number of given _pattern_ or _matcher_.

If _replacement_ is a procedure, then it must accept either one or two
arguments. This is for backward compatibility.

The first argument is always current matcher.

If the procedure only accepts one argument, then it must return a string which
will be used for replacement value.

If the procedure accepts two arguments, then the second one is string output
port. User may write string to the given port and will be the replacement
string.

The `regex-replace-first` procedure replaces the first match.

The `regex-replace-all` procedure replaces the all matches.


###### [!Function] `string-split`  _text_ _pattern_

_text_ must be a string.

_pattern_ must be a string or regex-pattern object.

Split _text_ accoding to _pattern_.


### [§3] Low level APIs for regular expression

The above procedures are wrapped User level API. However, you might want to use
low level API directory when you re-use matcher and recursively find pattern from
input. For that purpose, you need to use low level APIs directly.

NOTE: This API might be changed in future depending on R7RS large.

###### [!Function] `regex-pattern?`  _obj_

Returns #f if _obj_ is regular expression object, otherwise #f.

###### [!Function] `regex-matcher?`  _obj_

Returns #f if _obj_ is matcher object, otherwise #f.

###### [!Function] `compile-regex`  _string_ _:optional_ _flags_

The same as `regex` procedure.

###### [!Function] `regex-matcher`  _regex_ _string_

_Regex_ must be regular expression object. Returns matcher object.

###### [!Function] `regex-matches`  _matcher_

_Matcher_ must be matcher object. Returns #t if _matcher_ matches
the entire input string against input pattern, otherwise #f.


###### [!Function] `regex-looking-at`  _matcher_

_Matcher_ must be matcher object. Returns #t if _matcher_ matches
the input string against input pattern, otherwise #f.


###### [!Function] `regex-find`  _matcher_ _:optional_ _start_

_Matcher_ must be matcher object. Resets _matcher_ and then
attempts to find the next subsequence of the input string that matches the
pattern, starting at the specified index if optional argument is given
otherwise from the beginning.


###### [!Function] `regex-group`  _matcher_ _index_

_Matcher_ must be matcher object. _Index_ must be non negative
exact integer.

Retrieve captured group value from _matcher_.


###### [!Function] `regex-capture-count`  _matcher_

_Matcher_ must be matcher object.

Returns number of captured groups.


### [§3] Regular expression flags

Regular expression compiler can take following flags.

CASE-INSENSITIVE
: Enables case-insensitive matching. `i` as a 
  flag

COMMENTS
: Permits whitespace and comments in pattern. `x` as a 
  flag

MULTILINE
: Enables multiline mode. `m` as a flag

LITERAL
: Enables literal parsing of the pattern.

DOTAIL
: Enables dotall mode. `s` as a flag

UNICODE
: Enables Unicode-aware case folding and pre defined charset.
  `u` as a flag.
  NOTE: when this flag is set then pre defined charset, such as `\d` or
  `\w`, expand it's content to Unicode characters. Following properties
  are applied to charsets.
  [[:lower:]]
  : The lower case charset contains `Ll` and `Other_Lowercase`.
  [[:upper:]]
  : The upper case charset contains `Lu` and `Other_Uppercase`.
  [[:title:]]
  : The upper case charset contains `Lt`.
  [[:alpha:]]
  : The upper case charset contains `L`, `Nl` and
  `Other_Alphabetic`.
  [[:numeric:]]
  : The upper case charset contains `Nd`.
  [[:punct:]]
  : The upper case charset contains `P`.
  symbol
  : The upper case charset contains `Sm`, `Sc`, `Sk` and
  `So`.
  [[:space:]]
  : The upper case charset contains `Zs`, `Zl` and `Zp`.
  [[:cntrl:]]
  : The upper case charset contains `Cc`, `Cf`, `Co`, `Cs`,
  and `Cn`.

### [§3] Regular expression for bytevectors

Above APIs can be used bytevectors as well. In this case, the regular 
expression engine treats given bytevectors as if it's ASCII strings.
If users want to use this feature, users must give bytevectors instead
of strings.
