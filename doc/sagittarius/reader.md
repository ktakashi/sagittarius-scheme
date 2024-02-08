[§2] (sagittarius reader) - reader macro library {#lib.sagittarius.reader}
-------------

Sagittarius provides functionalities to modify its reader like Common
Lisp. It makes the reader programable. However it has some restriction
to use. The following examples explain it.

Using reader macro

```scheme
#!read-macro=sagittarius/regex
(import (sagittarius regex)) ;; usual import for procedures
#/regex/i                    ;; (sagittarius regex) defines #/regex/ form
                             ;; reader macro in it. it converts it
                             ;; (comple-regex "regex" CASE-INSENSITIVE)
```

Writing reader macro on toplevel

```scheme
(import (rnrs) (sagittarius reader))
(set-macro-character #\$
 (lambda (port c) (error '$-reader "invliad close paren appeared")))
(set-macro-character #\! (lambda (port c) (read-delimited-list #\$ port)))
!define test !lambda !$ !display "hello reader macro"$$$
!test$ ;; prints "hello reader macro"
```

Writing reader macro in library and export it

```scheme
#!compatible ;; make sure Sagittarius can read keyword
(library (reader macro test)
    ;; :export-reader-macro keyword must be in export clause
    (export :export-reader-macro)
    (import (rnrs) (sagittarius reader))

  (define-reader-macro $-reader #\$
    (lambda (port c)
      (error '$-reader "unexpected close paren appeared")))

  (define-reader-macro !-reader #\!
    (lambda (port c)
      (read-delimited-list #\$ port)))
)

#!read-macro=reader/macro/test  ;; imports reader macro
!define test !lambda !$ !display "hello reader macro"$$$
!test$    ;; prints "hello reader macro"
```

If you need to use reader macro in your library code, you need to define it
outside of the library. The library syntax is just one huge list so Sagittarius
can not execute the definition of reader macro inside during reading it.

###### [!Library] `(sagittarius reader)` 

This library provides reader macro procedures and macros.

###### [!Macro] `define-reader-macro`  _char_ _(name_ _args_ _..._ _)_ _body_ _..._
###### [!Macro] `define-reader-macro`  _name_ _char_ _proc_
###### [!Macro] `define-reader-macro`  _name_ _char_ _proc_ _non-term?_

_Name_ must be self evaluated expression. _Proc_ must accept 2 or
3 arguments, the first one is a port, the second one is a character which is
defined as reader macro character, and the third one which is an optional
argument is a read context.

`define-reader-macro` macro associates _char_ and _proc_ as a
reader macro. Once it is associated and Sagittarius' reader reads it, then
dispatches to the _proc_ with 2 arguments.

If _non-term?_ argument is given and not #f, the _char_ is marked as
non terminated character. So reader reads as one identifier even it it contains
the given _char_ in it.

The first form is a convenient form. Users can write a reader macro without
explicitly writing `lambda`. The form is expanded to like this:

```scheme
(define-reader-macro #\$ ($-reader args ...) body ...)
;; -> (define-reader-macro $-reader #\$ (lambda (args ...) body ...))
```

Note: the _name_ is only for error message. It does not affect anything.


###### [!Macro] `define-dispatch-macro`  _name_ _char_ _subchar_ _proc_
###### [!Macro] `define-dispatch-macro`  _name_ _char_ _proc_ _subchar_ _non-term?_

_Name_ must be self evaluated expression.
_Proc_ must accept three arguments, the first one is a port, the second one 
is a character which is defined as reader macro character and the third one is
a macro parameter.

`define-dispatch-macro` creates macro dispatch macro character
_char_if there is not dispatch macro yet, and associates _subchar_ and
_proc_as a reader macro.

If _non-term?_ argument is given and not #f, the _char_ is marked as non
terminated character. So reader reads as one identifier even it it contains the 
given _char_ in it.

Note: the _name_ is only for error message. It does not affect anything.


###### [!Function] `get-macro-character`  _char_

Returns 2 values if _char_ is macro character; one is associated
procedure other one is boolean if the _char_ is terminated character or not.
Otherwise returns 2 #f.


###### [!Function] `set-macro-character`  _char_ _proc_ _:optional_ _non-term?_

Mark given _char_ as macro character and sets the _proc_ as its
reader.
If _non-term?_ is given and not #f, the _char_ will be marked as non
terminated macro character.


###### [!Function] `make-dispatch-macro-character`  _char_ _:optional_ _non-term?_

Creates a new dispatch macro character with given _char_ if it is not
a dispatch macro character yet.
If _non-term?_ is given and not #f, the _char_ will be marked as non
terminated macro character.


###### [!Function] `get-dispatch-macro-character`  _char_ _subchar_

Returns a procedure which is associated with _char_ and _subchar_as a
reader macro. If nothing is associated, it returns #f.


###### [!Function] `set-dispatch-macro-character`  _char_ _subchar_ _proc_

Sets _proc_ as a reader of _subchar_ under the dispatch macro 
character of _char_.


###### [!Function] `read-delimited-list`  _char_ _:optional_ _(port_ _(current-input-port))_

Reads a list until given _char_ appears.

### [§3] Predefined reader macros {#lib.sagittarius.reader.predefined}

The following table explains predefined reader macros.


| Macro character | Terminated    | Explanation                                                                                                                                                            |
| --------------- | ------------- | -----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| #\\(            | #t            | Reads a list until reader reads #\\).                                                                                                                                  |
| #\\[            | #t            | Reads a list until reader reads #\\].                                                                                                                                  |
| #\\)            | #t            | Raises read error.                                                                                                                                                     |
| #\\]            | #t            | Raises read error.                                                                                                                                                     |
| #\\\|           | #t            | Reads an escaped symbol until reader reads #\\\|.                                                                                                                      |
| #\\"            | #t            | Reads a string until reader reads #\\".                                                                                                                                |
| #\\'            | #t            | Reads a symbol until reader reads delimited character.                                                                                                                 |
| #\\;            | #t            | Discards read characters until reader reads a linefeed.                                                                                                                |
| #\\\`           | #t            | Reads a next expression and returns `(quasiquote _expr_)`                                                                                                              |
| #\\,            | #t            | Check next character if it is `@` and reads a next expression.<br><br> Returns `(unquote-splicing _expr_)` if next character was<br> `@`, otherwise `(unquote _expr_)` |
| #\\:            | #f            | Only compatible mode. Reads a next expression and returns a keyword.                                                                                                   |
| #\\#            | #t(R6RS mode) | Dispatch macro character.                                                                                                                                              |


| Sub character | Explanation                                                                                                                                                            |
| ------------- | -----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| #\\'          | Reads a next expression and returns `(syntax _expr_)`.                                                                                                                 |
| #\\\`         | Reads a next expression and returns `(quasisyntax _expr_)`                                                                                                             |
| #\\,          | Check next character if it is `@` and reads a next expression.<br><br>Returns `(unsyntax-splicing _expr_)` if next character was<br>`@`, otherwise `(unsyntax _expr_)` |
| #\\!          | Reads next expression and set flags.<br>     The details are described the below section                                                                               |
| #\\v          | Checks if the next 2 characters are `u` and `8` and reads<br> a bytevector.                                                                                            |
| #\\u          | Only compatible mode. Checks if the next character is `8` and reads<br> a bytevector.                                                                                  |
| #\\t and #\\T | Returns #t.                                                                                                                                                            |
| #\\f and #\\F | Returns #f.                                                                                                                                                            |
| #\\b and #\\B | Reads a binary number.                                                                                                                                                 |
| #\\o and #\\O | Reads a octet number.                                                                                                                                                  |
| #\\d and #\\D | Reads a decimal number.                                                                                                                                                |
| #\\x and #\\X | Reads a hex number.                                                                                                                                                    |
| #\\i and #\\I | Reads a inexact number.                                                                                                                                                |
| #\\e and #\\E | Reads a exact number.                                                                                                                                                  |
| #\\(          | Reads a next list and convert it to a vector.                                                                                                                          |
| #\\;          | Reads a next expression and discards it.                                                                                                                               |
| #\\\|         | Discards the following characters until reader reads `\|#`                                                                                                             |
| #\\\\         | Reads a character.                                                                                                                                                     |
| #\\=          | Starts reading SRFI-38 style shared object.                                                                                                                            |
| #\\#          | Refers SRFI-38 style shared object.                                                                                                                                    |

#### [§4] #! - Switching mode

Sagittarius has multiple reader and VM modes and users can switch these modes
with `#!`. Following describes details of those modes;

`#!r6rs`: R6RS mode
: Symbols are read according to R6RS specification and VM sets the
  `no-overwrite` and `nounbound` flag. With this mode, keywords are
  read as symbols; for example, `:key` is just a symbol and users can
  not use extended `lambda` syntax.
    

`#!r7rs`: R7RS mode
: The mode for new specification of Scheme. This mode is
  less strict than R6RS mode described above. The reader can read keyword and VM
  sets the `no-overwrite` flag.
    

`#!compatible`: Compatible mode
: This mode is least strict mode. In other words, it
  does not have any restrictions such as described above.

NOTE: If you import reader macro with `#!read-macro=` form and let
      reader reads above hash-bang, the read table will be reset. So
      following code will raise a read error;

```scheme
#!read-macro=sagittarius/regex
#!r6rs
#/regular expression/ ;; <- &lexical
```

### [§3] Replacing reader

Since 0.3.7, users can replace default reader. Following example describes how
to replace reader.

```scheme
#!reader=srfi/:49
define
  fac n
  if (zero? n) 1
    * n
      fac (- n 1)

(print (fac 10))
```

`#!reader=` specifies which reader will be used. For this example, it will
use the one defined in `(srfi :49)` library. For compatibility of the other
Scheme implementation, we chose not to use the library name itself but a bit
converted name.

This is the list of `#!` flags:

`#!r6rs`
: Switches to R6RS mode

`#!r7rs`
: Switches to R7RS mode

`#!compatible`
: Switches to compatible mode

`#!no-overwrite`
: Sets no-overwrite flag that does not allow user
  to overwrite exported variables.

`#!nocache`
: Sets disable cache flag on the current loading file

`#!deprecated`
: Display warning message of deprecated library.

`#!reader=name`
: Replace reader with library _name_. The _name_ must be converted
  with the naming convention described below. For more details, see
  [Naming convention](#sagittarius.name.convention)  

`#!read-macro=name`
: _name_ must be converted with the naming convention described below.
  For more details, see	
  [Naming convention](#sagittarius.name.convention)  

#### [§4] Naming convention {#sagittarius.name.convention}

The naming convention is really easy. For example, replacing with
`(srfi :49)`, first remove all parentheses or brackets then replace spaces
to `/`. Then you get `srfi/:49`.

###### [!Macro] `define-reader`  _name_ _expr_
###### [!Macro] `define-reader`  _(name_ _port)_ _expr_ _..._

This macro defines replaceable reader.

The forms are similar with `define`. However if you use the first form
then _expr_ must be `lambda` and it accept one argument.

The defined reader will be used on read time, so it needs to return valid
expression as a return value of the reader.

NOTE: Only one reader can be defined in one library. If you define more than
once the later one will be used.


NOTE: If you want to export user defined reader to other library, you need to
put `:export-reader` keyword to the library export clause.
