@; -*- mode:scribble; coding: utf-8 -*-

@section{R7RS Support}

Since R7RS has already had draft 5, Sagittarius also supports its syntax and
libraries. There are a lot of possibilites to be changed. So this document may
be changed.

@subsection{R7RS library system}

On Sagittarius, user can call R7RS libraries on R6RS script and also other way
around. However syntaxes are different. User can choose which syntax use.

@define[Syntax]{@name{define-library} @args{name clauses}}
@desc{[R7RS]@var{name} must be a list which can only contains symbol or exact integer.
The same as R6RS's library name.

@var{clauses} may be one of these:
@itemlist[
@item{(@code{export} @var{export-spec} @dots{})}
@item{(@code{import} @var{import-set} @dots{})}
@item{(@code{begin} @var{command-or-definition} @dots{})}
@item{(@code{include} @var{filenames} @dots{})}
@item{(@code{include-ci} @var{filenames} @dots{})}
@item{(@code{cond-expand} @var{cond-expand-clause} @dots{})}
]
@code{export} and @code{import} are the same as R6RS. And on R7RS renaming
export syntax is different from R6RS, however on Sagittarius both can be
accepted.

@code{begin} starts the library contents. However it can appear more than once.

@code{include} and @code{include-ci} includes the files which are specified
@var{filenames}. @var{filenames} must be string. This resolves file path from
current loading file. If the library file is /usr/local/lib/lib1.scm, then
search path will be /usr/local/lib. It can also take absolute path.
@code{include-ci} reads the file case insensitive.

@code{cond-exnpand} is the same as builtin syntax @code{cond-expand}. For more
detail, see @secref["lib.sagittarius.builtin.syntax"]{Builtin Syntax}.
}

@subsection{R7RS libraries}

Most of the procedures are the same as R6RS or SRFIs. So I just list up the
libraries and its exported procedures and describe only new comer procedures.

@define[Library]{@name{(scheme base)}}
@desc{[R7RS]R7RS base library.

exported procedures:
@code{* + - ... / < <= = => > >= abs and append apply assoc assq assv begin
   binary-port? boolean? bytevector-copy bytevector-copy!
   bytevector-copy-partial
   bytevector-copy-partial! bytevector-length bytevector-u8-ref
   bytevector-u8-set!
   bytevector? caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr
   cadar caddar cadddr caddr cadr call-with-current-continuation
   call-with-port call-with-values call/cc car case cdaaar cdaadr cdaar cdadar
   cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr cdr ceiling
   char->integer char-ready? char<=? char<? char=? char>=? char>? char?
   close-input-port close-output-port close-port complex? cond cond-expand cons
   current-error-port current-input-port current-output-port define
   define-record-type define-syntax define-values denominator do dynamic-wind
   else eof-object? eq? equal? eqv? error error-object-irritants
   error-object-message error-object? even? exact->inexact exact-integer-sqrt
   exact-integer? exact? expt floor flush-output-port for-each gcd
   get-output-bytevector get-output-string guard if import inexact->exact
   inexact? input-port? integer->char integer? lambda lcm length let let*
   let*-values let-syntax let-values letrec letrec* letrec-syntax list
   list->string
   list->vector list-copy list-ref list-set! list-tail list? make-bytevector
   make-list make-parameter make-string make-vector map max member memq memv min
   modulo negative? newline not null? number->string number? numerator odd?
   open-input-bytevector open-input-string open-output-bytevector
   open-output-string or output-port? pair? parameterize peek-char peek-u8
   port-open? port? positive? procedure? quasiquote quote quotient raise
   raise-continuable rational? rationalize read-bytevector read-bytevector!
   read-char read-line read-u8 real? remainder reverse round set! set-car!
   set-cdr! string string->list string->number string->symbol string->utf8
   string->vector string-append string-copy string-fill! string-for-each
   string-length string-map string-ref string-set! string<=? string<? string=?
   string>=? string>? string? substring symbol->string symbol? syntax-error
   syntax-rules textual-port? truncate u8-ready? unless unquote
   unquote-splicing utf8->string values vector vector->list
   vector->string vector-copy vector-fill! vector-for-each vector-length
   vector-map vector-ref vector-set! vector? when with-exception-handler
   write-bytevector write-char write-partial-bytevector write-u8 zero?}
}

@define[Macro]{@name{syntax-error} @args{msg args @dots{}}}
@desc{[R7RS]Raises @code{&syntax-violation}}

@define[Function]{@name{error-object?} @args{obj{}}}
@desc{[R7RS]Returns #t if @var{obj} is error object, otherwise #f.

For now error object is R6RS's condition. It might be changed in future.
}

@define[Function]{@name{error-object-message} @args{obj{}}}
@desc{[R7RS]Retrieve irritants from @var{obj} if it has it, otherwise #f.
}

@define[Function]{@name{bytevector-copy-partial!} @args{from start end to at}}
@desc{[R7RS]The same as the code below:
@snipet{(bytevector-copy! from start to at (- end start))}
}

@define[Function]{@name{list-set!} @args{list k obj}}
@desc{[R7RS]Sets @var{obj} to @var{k}-th index at @var{list} and returns
unspecified value.
}

@define[Function]{@name{make-list} @args{k :optional fill}}
@desc{[R7RS][SRFI-1+] Creates a new allocated list with length @var{k}. If
optional argument @var{fill} is given, the list will be filled @var{fill},
otherwise filled with unspecified value.
}

@define[Function]{@name{peek-u8} @args{:optional (port (current-input-port))}}
@desc{[R7RS]Look ahead octed from given port and returns it.

This does not change port position.
}

@define[Function]{@name{read-u8} @args{:optional (port (current-input-port))}}
@desc{[R7RS]Reads octed from given port and returns it.}

@define[Function]{@name{read-bytevector}
 @args{k :optional (port (current-input-port))}}
@desc{[R7RS]Reads @var{k} octets from @var{port} and return it as a bytevector.}

@define[Function]{@name{read-bytevector!}
 @args{bv start end :optional (port (current-input-port))}}
@desc{[R7RS]Read @var{end} - @var{start} length octets from port and store it
given @var{bv} starting index @var{start}}

@define[Function]{@name{write-bytevector}
 @args{bv :optional (port (current-input-port))}}
@desc{[R7RS]Writes bytevector @var{bv} to given port.}

@define[Function]{@name{write-partial-bytevector}
 @args{bv start end :optional (port (current-input-port))}}
@desc{[R7RS]Writes bytevector @var{bv} to given port starting index @var{start}
until index @var{end}.}

@define[Function]{@name{read-line}
 @args{:optional (port (current-input-port))}}
@desc{[R7RS]Read a line from @var{port}}

@define[Function]{@name{string->vector} @args{string}}
@desc{[R7RS]Converts given string @var{string} to a character vector.}

@define[Function]{@name{vector->string} @args{vector}}
@desc{[R7RS]Converts given a character vector @var{vector} to string.}

@define[Function]{@name{string-map} @args{proc string1 string2 @dots{}}}
@desc{[R7RS]The same as @code{map} but it requires string as its arguments.}

@define[Function]{@name{char-ready?}
 @args{:optional (port (current-input-port))}}
@define[Function]{@name{u8-ready?}
 @args{:optional (port (current-input-port))}}
@desc{[R7RS]Sagittarius does not support these procedures. It always returns #f.
}

@define[Library]{@name{(scheme inexact)}}
@desc{[R7RS]This library exports procedures for inexact value.

exported procedures:
@code{acos asin atan cos exp finite? log nan? sin sqrt tan}
}

@define[Library]{@name{(scheme complex)}}
@desc{[R7RS]This library exports procedures for complex numbers.

exported procedures:
@code{angle imag-part magnitude make-polar make-rectangular real-part}
}

@define[Library]{@name{(scheme division)}}
@desc{[R7RS]This library exports procedures for integer division.

exported procedures:
@code{ceiling-quotient ceiling-remainder ceiling/
centered-quotient centered-remainder centered/
euclidean-quotient euclidean-remainder euclidean/
floor-quotient floor-remainder floor/
round-quotient round-remainder round/
truncate-quotient truncate-remainder truncate/}
}
@;Todo write for procedures above
@define[Function]{@name{ceiling-quotient} @args{n1 n2}}
@define[Function]{@name{ceiling-remainder @args{n1 n2}}
@define[Function]{@name{ceiling/ @args{n1 n2}}
@define[Function]{@name{centered-quotient  @args{n1 n2}}
@define[Function]{@name{centered-remainder  @args{n1 n2}}
@define[Function]{@name{centered/ @args{n1 n2}}
@define[Function]{@name{euclidean-quotient  @args{n1 n2}}
@define[Function]{@name{euclidean-remainder  @args{n1 n2}}
@define[Function]{@name{euclidean/ @args{n1 n2}}
@define[Function]{@name{floor-quotient  @args{n1 n2}}
@define[Function]{@name{floor-remainder  @args{n1 n2}}
@define[Function]{@name{floor/ @args{n1 n2}}
@define[Function]{@name{round-quotient  @args{n1 n2}}
@define[Function]{@name{round-remainder  @args{n1 n2}}
@define[Function]{@name{round/ @args{n1 n2}}
@define[Function]{@name{truncate-quotient  @args{n1 n2}}
@define[Function]{@name{truncate-remainder  @args{n1 n2}}
@define[Function]{@name{truncate/ @args{n1 n2}}
@desc{[R7RS] These procedures implement integer division. It raises an
&assertion if @var{n2} is zero. The procedures ending in @code{/} return two
integers; the other procedures return an integer. All the procedures compute
a quotient @var{nq} and remainder @var{nr} such that
@code{@var{n1} = @var{n2}@var{nq} + @var{nr}}. For each of the six division
operators, there are three procedures defined as follows:

@snipet[=> @var{nq} @var{nr}]{@code{(<operator>/ @var{n1} @var{n2})}}
@snipet[=> @var{nq}]{@code{(<operator>-quotient @var{n1} @var{n2})}}
@snipet[=> @var{nr}]{@code{(<operator>-remainder @var{n1} @var{n2})}}

The remainder @var{nr} is determined by the choice of integer
@code{@var{nq} : @var{nr} = @var{n1} - @var{n2}@var{nq}}. Each set of operators
uses a different choice of @var{n1}:
@dl-list{
@dl-item[@code{ceiling}]{@code{@var{nq} = ceiling(@var{n1}/@var{n2})}}
@dl-item[@code{floor}]{@code{@var{nq} = floor(@var{n1}/@var{n2})}}
@dl-item[@code{truncate}]{@code{@var{nq} = truncate(@var{n1}/@var{n2})}}
@dl-item[@code{round}]{@code{@var{nq} = round(@var{n1}/@var{n2})}}
@dl-item[@code{euclidean}]{if @code{@var{n2} > 0}, 
@code{@var{nq} = floor(@var{n1}/@var{n2})};
if @code{@var{n2} < 0}, @code{@var{nq} = ceiling(@var{n1}/@var{n2})}}
@dl-item[@code{centered}]{choose @var{nq} such that
@code{-|@var{n2}/2| <= @var{nr} < |@var{n2}/2|}}
}
For any of the operators, and for integers @var{n1} and @var{n2} with @var{n2}
not equal to 0.
}

@define[Library]{@name{(scheme lazy)}}
@desc{[R7RS]This library exports procedures and macros for lazy evaluation.

exported procedures:
@code{delay eager force lazy}
}
@define[Function]{@name{eager} @args{obj}}
@desc{[R7RS] The @code{eager} procedure return a promise whith when for will
return @var{obj}. It is similar to @code{delay}, but does not delay its
argument.
}

@define[Library]{@name{(scheme process-context)}}
@desc{[R7RS] This library exports procedures for accessing with the program's
calling context.

exported procedures:
@code{command-line exit get-enviromnent-variable get-enviromnent-variables}
}
@define[Function]{@name{get-enviromnent-variable} @args{name}}
@desc{[R7RS][SRFI-98] The procedure @code{get-enviromnent-variable} returns the 
value of the environment variable @var{name}, or #f if the named environment
variable is not found.
}
@define[Function]{@name{get-enviromnent-variables}}
@desc{[R7RS][SRFI-98] Returns the names and values of all the environment
variables as an alist.
}

@define[Library]{@name{(scheme load)}}
@desc{[R7RS]This library exports procedures for loading Scheme expressions
from files.

exported procedures:
@code{load}
}
@define[Function]{@name{load} @args{filename :optional environment-specifier}}
@desc{[R7RS] The @code{load} procedure reads expressions and definitions from 
the file and evaluates them sequentially in the environment specified by
@var{environment-specifier}.

If @var{environment-specifier} is omitted, current environment is used.

Note: R7RS requires @code{interactive-environment} when
@var{environment-specifier} is omitted, however Sagittarius uses current
environment. And on Sagittarius environment is equal to library so make sure
when you use @var{load} in your library, the loaded value will be defined or
evaluated in the library.
}

@define[Library]{@name{(scheme file)}}
@desc{[R7RS]This library exports procedures for accessing files.

exported procedures:
@code{call-with-input-file call-with-output-file delete-file file-exists?
 open-binary-input-file open-binary-output-file open-input-file open-output-file
 with-input-from-file with-output-to-file}
}
@define[Function]{@name{open-binary-output-file} @args{filename}}
@define[Function]{@name{open-binary-input-file} @args{filename}}
@desc{[R7RS] Opens file input/output port with given @var{filename}. It is the
same result as
@snipet{(open-file-input-port @var{filename})}
or
@snipet{(open-file-output-port @var{filename})}
}

@define[Library]{@name{(scheme read)}}
@desc{[R7RS]This library exports procedures for reading Scheme objects.

exported procedures:
@code{read}
}
@define[Function]{@name{read} @args{:optional (port (current-input-port))}}
@desc{[R7RS] Synonym of SRFI-38's @code{read/ss}}

@define[Library]{@name{(scheme write)}}
@desc{[R7RS]This library exports procedures for writing Scheme objects.

exported procedures:
@code{display write write-simple}
}
@define[Function]{@name{write}
 @args{obj :optional (port (current-output-port))}}
@desc{[R7RS] Synonym of SRFI-38's @code{write/ss}}
@define[Function]{@name{write-simple}
 @args{obj :optional (port (current-output-port))}}
@desc{[R7RS] Synonym of R6RS's @code{write}}

@define[Library]{@name{(scheme char)}}
@desc{[R7RS]This library exports procedures for dealing with Unicode character
operations.

exported procedures:
@code{char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
 char-downcase char-foldcase char-lower-case? char-numeric? char-upcase
 char-upper-case? char-whitespace? digit-value string-ci<=? string-ci<?
 string-ci=? string-ci>=? string-ci>? string-downcase string-foldcase
 string-upcase}
}
@define[Function]{@name{digit-value} @args{char}}
@desc{[R7RS] This procedure returns the numeric value (0 to 9) of its
argument if it is a numeric digit (that is, if @code{char-numeric?}
returns #t), or #f on any other character.}

@define[Library]{@name{(scheme char normalization)}}
@desc{[R7RS]This library exports procedures for dealing with Unicode
normalization operations.

exported procedures:
@code{string-ni<=? string-ni<? string-ni=? string-ni>=? string-ni>?}
}
@define[Function]{@name{string-ni=?} @args{string1 string2 string3 @dots{}}}
@define[Function]{@name{string-ni<=? @args{string1 string2 string3 @dots{}}}
@define[Function]{@name{string-ni<?  @args{string1 string2 string3 @dots{}}}
@define[Function]{@name{string-ni>=? @args{string1 string2 string3 @dots{}}}
@define[Function]{@name{string-ni>?  @args{string1 string2 string3 @dots{}}}
@desc{[R7RS] Synonyms of R6RS's @code{string=?} @code{string<=?} @code{string<?}
@code{string>=?} @code{string>?}
respectively.
}

@define[Library]{@name{(scheme time)}}
@desc{[R7RS]This library exports procedures for accessing to the system time.

exported procedures:
@code{current-jiffy current-second jiffies-per-second}
}
@define[Function]{@name{current-jiffy}}
@desc{[R7RS] Returns an inexact number representing time on the International
Atomic Time (TAI) scale.}

@define[Function]{@name{current-second}}
@desc{[R7RS] Returns the number of jifies that have elapsed since an
arbitrary, implementation-defined epoch.}

@define[Function]{@name{jiffies-per-second}}
@desc{[R7RS] Returns an exact integer representing the number of jifies
per SI second.}