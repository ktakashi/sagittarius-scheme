[§1] R7RS Support
=============

Sagittarius supports the latest Scheme standard R7RS. Even though the R7RS
has incompatible from R6RS, however users can use both R6RS and R7RS libraries
seamlessly.

[§2] R7RS library system
-------------

On Sagittarius, user can call R7RS libraries on R6RS script and also other way
around. However syntaxes are different. User can choose which syntax use.

###### [!Syntax] `define-library`  _name_ _clauses_

[R7RS]_name_ must be a list which can only contains symbol or exact
integer.

_clauses_ may be one of these:

- (`export` _export-spec_ ...)
- (`import` _import-set_ ...)
- (`begin` _command-or-definition_ ...)
- (`include` _filenames_ ...)
- (`include-ci` _filenames_ ...)
- (`include-library-declarations` _filenames_ ...)
- (`cond-expand` _cond-expand-clause_ ...)

`export` and `import` are the same as R6RS. And on R7RS renaming
export syntax is different from R6RS, however on Sagittarius both can be
accepted.

`begin` starts the library contents. However it can appear more than once.

`include` and `include-ci` includes the files which are specified
_filenames_. _filenames_ must be string. This resolves file path from
current loading file. If the library file is /usr/local/lib/lib1.scm, then
search path will be /usr/local/lib. It can also take absolute path.
`include-ci` reads the file case insensitive.

`cond-exnpand` is the same as builtin syntax `cond-expand`. For more
detail, see [Builtin Syntax](#lib.sagittarius.builtin.syntax).


[§2] R7RS libraries
-------------

Sagittarius supports all R7RS libraries. However to avoid duplication,
this section only describes the different procedures from R6RS. The same
procedures are explicitly said 'the same as R6RS'.

### [§3] Base library

###### [!Library] `(scheme base)` 

[R7RS]R7RS base library.

These procedures/macros are the same as R6RS;

``````````scheme
 * + - ... / < <= = => > >= 

 abs and append apply assoc assq assv 

 begin binary-port? boolean=? boolean? 
 bytevector-copy bytevector-length bytevector-u8-ref
 bytevector-u8-set! bytevector?

 caar cadr call-with-current-continuation call-with-port
 call-with-values call/cc car case cdar cddr cdr ceiling
 char->integer char<=? char<? char=? char>=? char>? char?
 close-input-port close-output-port close-port complex?
 cond cons current-error-port current-input-port current-output-port
 
 define define-record-type define-syntax denominator do dynamic-wind

 else eof-object eof-object? eq? eqv? error even?
 exact exact-integer-sqrt exact? expt

 floor flush-output-port for-each

 gcd guard

 if inexact inexact? input-port? integer->char integer?

 lambda lcm length let let* let*-values let-values
 letrec letrec* list list->string list->vector
 list-ref list-tail list?

 make-bytevector make-string make-vector map max member memq memv min
 modulo

 negative? newline not null? number->string number? numerator

 odd? or output-port?

 pair? peek-char port? positive? procedure?

 quasiquote quote quotient

 raise raise-continuable rational? rationalize
 read-char real? remainder reverse round

 set! set-car! set-cdr!
 string string->list string->number string->symbol string->utf8
 string-append string-copy string-for-each string-length string-ref
 string-fill! string-set!
 string<=? string<? string=? string>=? string>? string? substring
 symbol->string symbol=? symbol? syntax-rules

 textual-port? truncate 

 unless unquote unquote-splicing utf8->string

 values vector vector->list vector-fill! vector-for-each vector-length
 vector-map vector-ref vector-set! vector? 

 when with-exception-handler write-char 

 zero?
``````````

This is defined in [(sagittarius)](#lib.sagittarius);

``````````scheme
cond-expand
``````````



#### [§4] Bytevectors

###### [!Function] `bytevector`  _bytes_ _..._

_bytes_ must be a list of non negative exact integer and its
value is up to 255.

Creates a bytevector whose content is values of _bytes_.


###### [!Function] `bytevector-append`  _bytevectors_ _..._

Returns a newly allocated bytevector whose contents are the
concatenation of the contents in _bytevectors_.

NOTE: R6RS also has the same named procedure and does the same however
the arguments lists are different.


###### [!Function] `bytevector-copy!`  _to_ _at_ _from_ _:optional_ _(start_ _0)_ _(end_ _(bytevector-length_ _from))_

_to_ and _from_ must be bytevectors. _at_ must be
a non negative exact integer in range of _to_ size. Optional arguments
_start_ and _end_ must be non negative exact integer in range of
_from_ and _end_ must be bigger than _end_.

Copies content of _from_ starting from _start_ until _end_ 
(exclusive) to _to_ starting _at_. The operation is done
destructively.


#### [§4] Lists

###### [!Function] `list-copy`  _obj_

Returns a newly allocated copy of _obj_ if it is a list. The
list elements stays intact. So changing element's content affects original
_obj_.


###### [!Function] `list-set!`  _list_ _index_ _value_

Stores _value_ in element _index_ of _list_.

###### [!Function] `make-list`  _k_ _:optional_ _fill_

Creates a list whose size is _k_.

If the optional argument _fill_ is given then returning list is filled
with _fill_, otherwise unspecified value is used.


 

#### [§4] Vectors

###### [!Function] `vector->string`  _vector_ _:optional_ _(start_ _0)_ _(end_ _(vector-length_ _vector))_

_vector_ must be a vector whose elements of range from _start_to _end_ (exclusive) are characters.

Returns a string constructed with the above range of characters.


###### [!Function] `string->vector`  _string_ _:optional_ _(start_ _0)_ _(end_ _(string-length_ _string))_

Returns a vector whose elements are characters of _string_ in range
of _start_ and _end_ (exclusive).


###### [!Function] `vector-append`  _vectors_ _..._

Returns a newly allocated vector whose contents are the
concatenation of the contents in _vectors_.

###### [!Function] `vector-copy`  _vector_ _:optional_ _(start_ _0)_ _(end_ _(vector-length_ _vector))_

Returns a vector whose elements are elements of given _vector_between _start_ and _end_.


###### [!Function] `vector-copy!`  _to_ _at_ _from_ _:optional_ _(start_ _0)_ _(end_ _(vector-length_ _from))_

_to_ and _from_ must be vectors. _at_ must be
a non negative exact integer in range of _to_ size. Optional arguments
_start_ and _end_ must be non negative exact integer in range of
_from_ and _end_ must be bigger than _end_.

Copies content of _from_ starting from _start_ until _end_ 
(exclusive) to _to_ starting _at_. The operation is done
destructively.


#### [§4] Strings

###### [!Function] `string-copy!`  _to_ _at_ _from_ _:optional_ _(start_ _0)_ _(end_ _(vector-length_ _from))_

_to_ and _from_ must be strings. _at_ must be
a non negative exact integer in range of _to_ size. Optional arguments
_start_ and _end_ must be non negative exact integer in range of
_from_ and _end_ must be bigger than _end_.

Copies content of _from_ starting from _start_ until _end_ 
(exclusive) to _to_ starting _at_. The operation is done
destructively.


###### [!Function] `string-map`  _proc_ _string_ _strings_ _..._

Calls _proc_ with given _strings_ elements and returns
a string whose content is constructed by the results of _proc_.


#### [§4] Parameters

###### [!Function] `make-parameter`  _init_ _:optional_ _converter_

[SRFI-39] Returns a parameter object, which is an applicable object
that can take zero or one argument. 

The initial value is `(_converter_ _init_)` if _converter_is given, otherwise _init_.

If the parameter object is applied without an argument, then it returns
the value associated with the parameter object.

If the parameter object is applied with an argument, then it changes the
associated value with the given value which may converted by _converter_if the parameter object is created with _converter_.



###### [!Macro] `parameterize`  _((param1_ _value1)_ _..._ _)_ _body_ _..._

[SRFI-39] A `parameterize` expression is used to change the
values returned by specified parameter objects during the evaluation of
_body_.

#### [§4] Inclusion

###### [!Syntax] `include`  _file_ _files_ _..._
###### [!Syntax] `include-ci`  _file_ _files_ _..._

_files_ must be strings.

Includes _files_ as if it's written there. `include-ci` reads
files case insensitively.


#### [§4] Multiple-value definition

###### [!Macro] `define-values`  _formals_ _expression_

_expression_ is evaluated and the _formals_ are bound to the
return values in the same way that the _formals_ in `lambda`expression are matched to the arguments in a procedure call.


#### [§4] Numerical operations

###### [!Function] `exact-integer?`  _obj_

Returns #t if _obj_ is exact integer. Otherwise #f.

###### [!Function] `floor/`  _n1_ _n2_
###### [!Function] `floor-quotient`  _n1_ _n2_
###### [!Function] `floor-remainder`  _n1_ _n2_
###### [!Function] `truncate/`  _n1_ _n2_
###### [!Function] `truncate-quotient`  _n1_ _n2_
###### [!Function] `truncate-remainder`  _n1_ _n2_

These procedures implement number-theoretic (integer) division.
It is an error if _n2_ is zero.

The procedure ending `/` returns two integers; the other procedures
return an integer. All the procedures compute a quotient _nq_ and
remainder _nr_ such that `_n1_ = _n2_*_nq_ + _nr_`.
for each of the division operators, there are three procedures defined as
follows;

``(<operator>/ _n1_ _n2_)`` => ``nq nr``

``(<operator>-quotient _n1_ _n2_)`` => ``nq``

``(<operator>-remainder _n1_ _n2_)`` => ``nr``

The remainder _nr_ is determined by the choice of integer
`_nq_:_nr_ = _n1_ - _n2_*_nq_`. Each set of
operations uses a different choice of _nq_:

`floor`: `_nq_ = [_n1_/_n2_]``truncate`: `_nq_ = truncate(_n1_/_n2_)`Examples;

``(floor/ 5 2)`` => ``2 1``

``(floor/ -5 2)`` => ``-3 1``

``(floor/ 5 -2)`` => ``-3 -1``

``(floor/ -5 -2)`` => ``2 -1``

``(truncate/ 5 2)`` => ``2 1``

``(truncate/ -5 2)`` => ``-2 -1``

``(truncate/ 5 -2)`` => ``-2 1``

``(truncate/ -5 -2)`` => ``2 -1``

``(truncate/ -5.0 -2)`` => ``2.0 -1.0``



###### [!Function] `square`  _z_

Returns the square of _z_.

#### [§4] Error objects

###### [!Macro] `syntax-error`  _msg_ _forms_ _..._

Raises an `&syntax` with `syntax-violation`.

The macro is defined like this;

``````````scheme
(define-syntax syntax-error
  (syntax-rules ()
    ((_ msg args ...)
     (syntax-violation 'syntax-error msg (quote args ...)))))
``````````



###### [!Function] `error-object?`  _obj_

Returns #t if _obj_ is condition object. This is a renaming
exported procedure of `consition?`.


###### [!Function] `error-object-irritants`  _err_

Returns irritants if the given _err_ satisfies 
`irritants-condition?`. Otherwise #f.


###### [!Function] `error-object-message`  _err_

Returns error message if the given _err_ satisfies
`message-condition?`. Otherwise #f.


#### [§4] I/O

###### [!Function] `file-error?`  _obj_

Returns #t if _obj_ is a file related condition, otherwise #f.


###### [!Function] `read-error?`  _obj_

Renaming export of `i/o-read-error?`.

###### [!Function] `input-port-open?`  _input-port_
###### [!Function] `output-port-open?`  _output-port_

Return #t if given _input-port_ or _output-port_ is open,
respectively. Otherwise #f.


###### [!Function] `u8-ready?`  _input-port_
###### [!Function] `char-ready?`  _input-port_

Renaming export of `port-ready?`.


###### [!Function] `peek-u8`  _:optional_ _(input-port_ _(current-input-port))_
###### [!Function] `read-u8`  _:optional_ _(input-port_ _(current-input-port))_

_input-port_ must be a binary input port.

Peeks or reads a byte from given _input-port_.


###### [!Function] `read-bytevector`  _len_ _:optional_ _(input-port_ _(current-input-port))_

_input-port_ must be a binary input port.

Reads _len_ from _input-port_ and returns a bytevector.


###### [!Function] `read-bytevector!`  _bv_ _:optional_ _(input-port_ _(current-input-port))_ _
_ _		_ _(start_ _0)_ _(end_ _(bytevector-length_ _bv))_

_input-port_ must be a binary input port.

Reads _end_ - _start_ size of data from _input-port_ and put it
into _bv_ from _start_ index.


###### [!Function] `write-u8`  _u8_ _:optional_ _(output-port_ _(current-output-port))_

_u8_ must be a byte. _output-port_ must be a binary output port.

Writes _u8_ to _output-port_.


###### [!Function] `write-bytevector`  _bv_ _:optional_ _(output-port_ _(current-output-port))_ _
_ _		_ _(start_ _0)_ _(end_ _(bytevector-length_ _bv))_

_output-port_ must be a binary output port.

Writes _bv_ from _start_ to _end_ (exclusive) to _output-port_.


###### [!Function] `read-line`  _:optional_ _(input-port_ _(current-input-port))_

_input-port_ must be a textual input port.

Reads line from _input-port_. For convenience, `\n`, `\r`and `\r\n` are considered end of line.


###### [!Function] `read-string`  _k_ _:optional_ _(input-port_ _(current-input-port))_

_input-port_ must be a textual input port.

Reads _k_ length of string from _input-port_.


###### [!Function] `write-string`  _str_ _:optional_ _(output-port_ _(current-output-port))_ _
_ _		_ _(start_ _0)_ _(end_ _(string-length_ _str))_

_output-port_ must be a binary output port.

Writes _bv_ from _start_ to _end_ (exclusive) to _output-port_.


###### [!Function] `open-input-bytevector`  _bv_
###### [!Function] `open-input-string`  _string_

Returns binary or textual input port whose source is _bv_or _string_, respectively.


###### [!Function] `open-output-bytevector` 
###### [!Function] `open-output-string` 

Returns binary or textual output port. The port is opened on memory.

###### [!Function] `get-output-bytevector`  _output-port_
###### [!Function] `get-output-string`  _output-port_

Retrieves buffered bytevector or string from given _output-ports_,
respectively.


#### [§4] System interface

###### [!Function] `features` 

Returns a list of the feature identifiers which `cond-expand`treas as true.


#### [§4] Differ from R6RS

The following procedures or macros are re-defined to strictly follow either
R6RS or R7RS. It is users' responsibility to make sure proper ones are used
in their script.

###### [!Function] `equal?`  _a_ _b_

Returns #t if _a_ and _b_ are equivalent.

The difference between the one from `(rnrs base)` is that this procedure
inspect record fields as well. For example:

``````````scheme
(import (rnrs))
(define-record-type (<pare> kons pare?)
  (fields (mutable a kar set-kar!)
	  (mutable d kdr set-kdr!)))

(let ((a (kons 'a 'b))
      (b (kons 'a 'b)))
  (equal? a b))
``````````

``````````scheme
(import (scheme base))
(define-record-type (<pare> kons pare?)
  (fields (mutable a kar set-kar!)
	  (mutable d kdr set-kdr!)))

(let ((a (kons 'a 'b))
      (b (kons 'a 'b)))
  (equal? a b))
``````````
=> ``#t``



###### [!Macro] `let-syntax`  _((var_ _trans)_ _..._ _)_ _expr_ _..._
###### [!Macro] `letrec-syntax`  _((var_ _trans)_ _..._ _)_ _expr_ _..._

Binds macro transformer _trans_ to _var_.

The difference between the one from `(rnrs base)` and these macros is
that these macro create scope. Thus the following is unbound variable error:

``````````scheme
(import (scheme base))
(let-syntax ()
  (define foo 'foo))
foo
``````````
=> ``&undefined``



### [§3] Case-lambda library

###### [!Library] `(scheme case-lambda)` 

[R7RS] This library exports the `case-lambda` syntax.

Exported macro is the same as R6RS;

`case-lambda`

### [§3] Char library

###### [!Library] `(scheme char)` 

[R7RS]This library exports procedures for dealing with Unicode character
operations.


These procedures are the same as R6RS;

``````````scheme
 char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
 char-downcase char-foldcase char-lower-case? char-numeric? char-upcase
 char-upper-case? char-whitespace? string-ci<=? string-ci<?
 string-ci=? string-ci>=? string-ci>? string-downcase string-foldcase
 string-upcase
``````````



###### [!Function] `digit-value`  _c_

[R7RS+] Converts given character to it's numeric value. If the given
character does not have numeric value, then it returns #f.

This procedure is extended from R7RS definition.

``(digit-value #\1)`` => ``1``

``(digit-value #\x0664)`` => ``4``

``(digit-value #\x0EA6)`` => ``#f``

``(digit-value #\xbc)`` => ``1/4``

``(digit-value #\xbf0)`` => ``10``



### [§3] Complex library

###### [!Library] `(scheme complex)` 

[R7RS]This library exports procedures for complex numbers.

These procedures are the same as R6RS;

`angle imag-part magnitude make-polar make-rectangular real-part`

### [§3] CxR library

###### [!Library] `(scheme cxr)` 

[R7RS]This library exports twenty-four procedures
which are the compositions of from three to four car and
cdr operations.

These procedures are the same as R6RS;

``````````scheme
caaaar caaadr
caaar caadar
caaddr caadr
cadaar cadadr
cadar caddar
cadddr caddr
cdaaar cdaadr
cdaar cdadar
cdaddr cdadr
cddaar cddadr
cddar cdddar
cddddr cdddr
``````````



### [§3] Eval library

###### [!Library] `(scheme eval)` 

[R7RS]This library exports exports procedures for evaluating
Scheme data as programs.

These procedures are the same as R6RS;

`environment eval`

### [§3] File library

###### [!Library] `(scheme file)` 

[R7RS]This library exports procedures for accessing files.

These procedures are the same as R6RS;

``````````scheme
 call-with-input-file call-with-output-file
 delete-file file-exists?
 open-input-file open-output-file
 with-input-from-file with-output-to-file
``````````



###### [!Function] `open-binary-input-file`  _file_
###### [!Function] `open-binary-output-file`  _file_

Returns file binary input or output port associated with given
_file_, respectively.


### [§3] Inexact library

###### [!Library] `(scheme inexact)` 

[R7RS]This library exports procedures for inexact value.

These procedures are the same as R6RS;

`acos asin atan cos exp finite? infinite? log nan? sin sqrt tan`

### [§3] Lazy library

###### [!Library] `(scheme lazy)` 

[R7RS]This library exports procedures and syntax
keywords for lazy evaluation.

These procedures/macros are the same as R6RS;

`delay force make-promise promise?`

###### [!Macro] `delay-force`  _expression_

The expression `(delay-force _expression_)` is conceptually
similar to `(delay (force _expression_))`, with the difference that
forcing the result of `delay-force` will in effect result in a tail call
to `(force _expression_)`, while forcing the result of
`(delay (force _expression_))` might not.


### [§3] Load library

###### [!Library] `(scheme load)` 

[R7RS]This library exports procedures for loading Scheme expressions
from files.


###### [!Function] `load`  _file_ _:optional_ _environment_

Reads expression in _file_ and evaluates it until it gets to
end of file. If _environment_ is passed, then the evaluation is done
in that environment. Otherwise it is done in current environment.


### [§3] Process-Context library

###### [!Library] `(scheme process-context)` 

[R7RS] This library exports procedures for accessing with the program's
calling context.

These procedures are the same as R6RS;
`command-line exit`

###### [!Function] `emergency-exit`  _:optional_ _obj_

Exist process without any cleanup. The optional argument _obj_is given then it's translated to proper return value of the process.


###### [!Function] `get-enviromnent-variable`  _name_

_name_ must be a string.

Retrieves environment variable associated to _name_.


###### [!Function] `get-enviromnent-variables` 

Returns alist of environment variables.

### [§3] Read library

###### [!Library] `(scheme read)` 

[R7RS]This library exports procedures for reading Scheme objects.


###### [!Function] `read`  _:optional_ _port_

Renaming export of `read/ss`.

### [§3] Repl library

###### [!Library] `(scheme repl)` 

[R7RS]This library library exports the
`interaction-environment` procedure.

###### [!Function] `interaction-environment` 

Returns interaction environment.

### [§3] Time library

###### [!Library] `(scheme time)` 

[R7RS]This library provides access to time-related values.


###### [!Function] `current-jiffy` 

Returns the number of jiffies as an exact integer.

###### [!Function] `jiffies-per-second` 

Returns an exact integer representing the number of jiffies per SI
second.

###### [!Function] `current-second` 

Returns an inexact number representing the current time on
International Atomic Time (TAI) scale.

### [§3] Write library

###### [!Library] `(scheme write)` 

[R7RS]This library exports procedures for writing Scheme objects.

This procedures is the same as R6RS
`display`

###### [!Function] `write`  _obj_ _:optional_ _(output-port_ _(current-output-port))_

Writes a representation of _obj_ to _output-port_.

If the _obj_ contains cyclic, the procedure writes with datum label.


###### [!Function] `write-shared`  _obj_ _:optional_ _(output-port_ _(current-output-port))_
###### [!Function] `write-simple`  _obj_ _:optional_ _(output-port_ _(current-output-port))_

Renaming export of `write/ss` and `write`, respectively.


### [§3] R5RS library

###### [!Library] `(scheme r5rs)` 

[R7RS]The (scheme r5rs) library provides the identifiers defined
by R5RS, except that `transcript-on` and `transcript-off` are
not present. Note that the `exact` and `inexact` procedures
appear under their R5RS names `inexact->exact` and 
`exact->inexact` respectively. However, if an implementation does
not provide a particular library such as the complex library, the
corresponding identifiers will not appear in this library either. This
library exports procedures for writing Scheme objects.


[§2] R7RS-large
-------------

R7RS-large makes Scheme specification more practical. It contains
lots of useful libraries as well. This section lists supporting
R7RS-large libraries.

### [§3] List library

###### [!Library] `(scheme list)` 

This library exports the procedures and macros defined in
SRFI-1 `(srfi :1)`.


### [§3] Vector library

###### [!Library] `(scheme vector)` 

This library exports the procedures and macros defined in
SRFI-133 `(srfi :133)`.


### [§3] Sorting library

###### [!Library] `(scheme sort)` 

This library exports the procedures and macros defined in
SRFI-132 `(srfi :132)`.


### [§3] Set library

###### [!Library] `(scheme set)` 

This library exports the procedures and macros defined in
SRFI-113 `(srfi :113)`.


### [§3] Character set library

###### [!Library] `(scheme charset)` 

This library exports the procedures and macros defined in
SRFI-14 `(srfi :14)`.


### [§3] Hashtable library

###### [!Library] `(scheme hash-table)` 

This library exports the procedures and macros defined in
SRFI-125 `(srfi :125)`.


### [§3] Immutable list library

###### [!Library] `(scheme ilist)` 

This library exports the procedures and macros defined in
SRFI-116 `(srfi :116)`.


### [§3] Random access list library

###### [!Library] `(scheme rlist)` 

This library exports the procedures and macros defined in
SRFI-101 `(srfi :101)` with the following exceptions:

`make-list` is renamed to `make-rlist`.

`random-access-list->linear-access-list` is renamed to `rlist->list`.

`linear-access-list->random-access-list` is renamed to `list->rlist`.

All other procedures are prefixed with `r`. For example, `pair?`exported from `(srfi :101)` is `rpair?`.


### [§3] Immutable deque library

###### [!Library] `(scheme ideque)` 

This library exports the procedures and macros defined in
SRFI-134 `(srfi :134)`.


### [§3] Immutable text library

###### [!Library] `(scheme text)` 

This library exports the procedures and macros defined in
SRFI-135 `(srfi :135)`.


### [§3] Generator library

###### [!Library] `(scheme generator)` 

This library exports the procedures and macros defined in
SRFI-158 `(srfi :158)`.


### [§3] Lazy sequence library

###### [!Library] `(scheme lazy-seq)` 

This library exports the procedures and macros defined in
SRFI-127 `(srfi :127)`.


### [§3] Stream library

###### [!Library] `(scheme stream)` 

This library exports the procedures and macros defined in
SRFI-41 `(srfi :41)`.


### [§3] Box library

###### [!Library] `(scheme box)` 

This library exports the procedures and macros defined in
SRFI-111 `(srfi :111)`.


### [§3] List queue library

###### [!Library] `(scheme list queue)` 

This library exports the procedures and macros defined in
SRFI-117 `(srfi :117)`.


### [§3] Ephemeron library

###### [!Library] `(scheme ephemeron)` 

This library exports the procedures and macros defined in
SRFI-124 `(srfi :124)`.


### [§3] Comparator library

###### [!Library] `(scheme comparator)` 

This library exports the procedures and macros defined in
SRFI-128 `(srfi :128)`.


### [§3] Division library

###### [!Library] `(scheme division)` 

This library exports the procedures and macros defined in
SRFI-141 `(srfi :141)`.


### [§3] Fixnum library

###### [!Library] `(scheme fixnum)` 

This library exports the procedures and macros defined in
SRFI-143 `(srfi :143)`.


### [§3] Flonum library

###### [!Library] `(scheme flonum)` 

This library exports the procedures and macros defined in
SRFI-144 `(srfi :144)`.


### [§3] Mapping library

###### [!Library] `(scheme mapping)` 

This library exports the procedures and macros defined in
SRFI-146 `(srfi :146)`.


### [§3] Hash mapping library

###### [!Library] `(scheme mapping hash)` 

This library exports the procedures and macros defined in
SRFI-146 `(srfi :146 hash)`.


### [§3] Bitwise library

###### [!Library] `(scheme bitwise)` 

This library exports the procedures and macros defined in
SRFI-151 `(srfi :151)`.


### [§3] Format library

###### [!Library] `(scheme format)` 

This library exports the procedures and macros defined in
SRFI-159 `(srfi :159)`.


### [§3] Homogeneous vector library base

###### [!Library] `(scheme vector base)` 

This library exports the procedures and macros defined in
SRFI-160 `(srfi :160 base)`.


### [§3] Homogeneous vector library per types

###### [!Library] `(scheme vector `  _@_ _)_

This library exports the procedures and macros defined in
SRFI-160 `(srfi :160 @)`. Where `@` is one of
`u8 s8 u16 s16 u32 s32 u64 s64 f32 f64 c64 c128`

### [§3] Bytevector library

###### [!Library] `(scheme bytevector)` 

This library exports the procedures and macros defined in
R6RS bytevectors library `(rnrs bytevectors)`.

NOTE: The exporting names may conflicts with the ones exported from R7RS
libraries (e.g. `bytevector-copy!` from `(scheme base)`).


