@; -*- mode:scribble; coding: utf-8 -*-

@section{R7RS Support}

Since R7RS has already had draft 5, Sagittarius also supports its syntax and
libraries. There are a lot of possibilites to be changed. So this document may
be changed.

@subsection{R7RS library system}

On Sagittarius, user can call R7RS libraries on R6RS script and also other way
around. However syntaxes are different. User can choose which syntax use.

@define[Syntax]{@name{define-library} @args{name clauses}}
@desc{@var{name} must be a list which can only contains symbol or exact integer.
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
@desc{R7RS base library.

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
@desc{Raises @code{&syntax-violation}}

@define[Function]{@name{error-object?} @args{obj{}}}
@desc{Returns #t if @var{obj} is error object, otherwise #f.

For now error object is R6RS's condition. It might be changed in future.
}

@define[Function]{@name{error-object-message} @args{obj{}}}
@desc{Retrieve irritants from @var{obj} if it has it, otherwise #f.
}

@define[Function]{@name{bytevector-copy-partial!} @args{from start end to at}}
@desc{The same as the code below:
@sniped{(bytevector-copy! from start to at (- end start))}
}

@define[Function]{@name{list-set!} @args{list k obj}}
@desc{Sets @var{obj} to @var{k}-th index at @var{list} and returns unspecified
value.
}

@define[Function]{@name{make-list} @args{k :optional fill}}
@desc{[SRFI-1+] Creates a new allocated list with length @var{k}. If optional
argument @var{fill} is given, the list will be filled @var{fill}, otherwise
filled with unspecified value.
}

@define[Function]{@name{peek-u8} @args{:optional (port (current-input-port))}}
@desc{Look ahead octed from given port and returns it.

This does not change port position.
}

@define[Function]{@name{read-u8} @args{:optional (port (current-input-port))}}
@desc{Reads octed from given port and returns it.}

@define[Function]{@name{read-bytevector}
 @args{k :optional (port (current-input-port))}}
@desc{Reads @var{k} octets from @var{port} and return it as a bytevector.}

@define[Function]{@name{read-bytevector!}
 @args{bv start end :optional (port (current-input-port))}}
@desc{Read @var{end} - @var{start} length octets from port and store it given 
@var{bv} starting index @var{start}}

@define[Function]{@name{write-bytevector}
 @args{bv :optional (port (current-input-port))}}
@desc{Writes bytevector @var{bv} to given port.}

@define[Function]{@name{write-partial-bytevector}
 @args{bv start end :optional (port (current-input-port))}}
@desc{Writes bytevector @var{bv} to given port starting index @var{start} until
index @var{end}.}

@define[Function]{@name{read-line}
 @args{:optional (port (current-input-port))}}
@desc{Read a line from @var{port}}

@define[Function]{@name{string->vector} @args{string}}
@desc{Converts given string @var{string} to a character vector.}

@define[Function]{@name{vector->string} @args{vector}}
@desc{Converts given a character vector @var{vector} to string.}

@define[Function]{@name{string-map} @args{proc string1 string2 @dots{}}}
@desc{The same as @code{map} but it requires string as its arguments.}

@define[Function]{@name{char-ready?}
 @args{:optional (port (current-input-port))}}
@define[Function]{@name{u8-ready?}
 @args{:optional (port (current-input-port))}}
@desc{Sagittarius does not support these procedures. It always returns #f.
}

@define[Library]{@name{(scheme inexact)}}
@desc{This library exports procedures for inexact value.

exported procedures:
@code{acos asin atan cos exp finite? log nan? sin sqrt tan}
}

@define[Library]{@name{(scheme complex)}}
@desc{This library exports procedures for complex numbers.

exported procedures:
@code{angle imag-part magnitude make-polar make-rectangular real-part}
}

@define[Library]{@name{(scheme division)}}
@desc{This library exports procedures for integer division.

exported procedures:
@code{ceiling-quotient ceiling-remainder ceiling/
centered-quotient centered-remainder centered/
euclidean-quotient euclidean-remainder euclidean/
floor-quotient floor-remainder floor/
round-quotient round-remainder round/
truncate-quotient truncate-remainder truncate/}
}

@define[Library]{@name{(scheme lazy)}}
@desc{This library exports procedures and macros for lazy evaluation.

exported procedures:
@code{delay eager force lazy}
}