@; -*- mode:scribble; coding: utf-8 -*-

@section{R7RS Support}

Since R7RS has already had draft 8, Sagittarius also supports its syntax and
libraries. There are a lot of possibilities to be changed. So this document may
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
@item{(@code{include-library-declarations} @var{filenames} @dots{})}
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

As long as R7RS is in draft state these libraries and procedures may be changed
in future. So we don't describe the procedures, yet. When it became final state,
I will add it.

@define[Library]{@name{(scheme base)}}
@desc{[R7RS]R7RS base library.

exported procedures:
@code{* + - ... / < <= = => > >=

     abs and append apply assoc assq assv

     begin binary-port? boolean=? boolean? bytevector
     bytevector-append bytevector-copy bytevector-copy! bytevector-length
     bytevector-u8-ref bytevector-u8-set! bytevector?

     caar cadr call-with-current-continuation call-with-port call-with-values
     call/cc car case cdar cddr cdr ceiling char->integer char-ready? char<=?
     char<? char=? char>=? char>? char? close-input-port
     close-output-port close-port complex? cond
     cond-expand cons current-error-port current-input-port current-output-port

     define define-record-type define-syntax define-values denominator
     do dynamic-wind

     else eof-object eof-object? eq? equal? eqv? error error-object-irritants
     error-object-message error-object? even? exact
     exact-integer-sqrt exact-integer? exact? expt

     (rename (cond-features features)) file-error?
     floor floor-quotient floor-remainder floor/ flush-output-port for-each

     gcd get-output-bytevector get-output-string guard

     if import include include-ci inexact inexact? input-port-open? input-port?
     integer->char integer?

     lambda lcm length let let* let*-values let-syntax let-values
     letrec letrec* letrec-syntax list list->string list->vector
     list-copy list-ref list-set! list-tail list?

     make-bytevector make-list make-parameter make-string make-vector map max
     member memq memv min modulo

     negative? newline not null? number->string number? numerator

     odd? open-input-bytevector open-input-string open-output-bytevector
     open-output-string or output-port-open? output-port?

     pair? parameterize peek-char peek-u8 port? positive? procedure? quasiquote
     quote quotient

     raise raise-continuable rational? rationalize
     read-bytevector read-bytevector! read-char read-error?
     read-line read-string read-u8 real? remainder reverse round set!

     set-car! set-cdr! square string string->list string->number
     string->symbol string->utf8 string->vector string-append
     string-copy string-copy! string-fill! string-for-each
     string-length string-map string-ref string-set!
     string<=? string<? string=? string>=? string>? string?
     substring symbol->string symbol=? symbol? syntax-error syntax-rules

     textual-port? truncate truncate-quotient truncate-remainder
     truncate/ u8-ready?

     unless unquote unquote-splicing utf8->string

     values vector vector->list vector->string vector-append vector-copy
     vector-copy! vector-fill! vector-for-each vector-length
     vector-map vector-ref vector-set! vector?

     when with-exception-handler
     write-bytevector write-char
     write-string write-u8

     zero?}
}

@define[Library]{@name{(scheme case-lambda)}}
@desc{[R7RS] This library exports the @code{case-lambda} syntax.

exported procedures:
@code{case-lambda}
}

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

@define[Library]{@name{(scheme complex)}}
@desc{[R7RS]This library exports procedures for complex numbers.

exported procedures:
@code{angle imag-part magnitude make-polar make-rectangular real-part}
}

@define[Library]{@name{(scheme cxr)}}
@desc{[R7RS]This library exports twenty-four procedures
which are the compositions of from three to four car and
cdr operations.

exported procedures:
@code{caaaar caaadr
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
cddddr cdddr}
}

@define[Library]{@name{(scheme eval)}}
@desc{[R7RS]This library exports exports procedures for evaluating
Scheme data as programs.

exported procedures:
@code{environment eval}
}


@define[Library]{@name{(scheme file)}}
@desc{[R7RS]This library exports procedures for accessing files.

exported procedures:
@code{call-with-input-file call-with-output-file delete-file file-exists?
 open-binary-input-file open-binary-output-file open-input-file open-output-file
 with-input-from-file with-output-to-file}
}

@define[Library]{@name{(scheme inexact)}}
@desc{[R7RS]This library exports procedures for inexact value.

exported procedures:
@code{acos asin atan cos exp finite? infinite? log nan? sin sqrt tan}
}

@define[Library]{@name{(scheme lazy)}}
@desc{[R7RS]This library exports procedures and syntax
keywords for lazy evaluation.

exported procedures:
@code{delay delay-force force make-promise promise?}
}

@define[Library]{@name{(scheme load)}}
@desc{[R7RS]This library exports procedures for loading Scheme expressions
from files.

exported procedures:
@code{load}
}

@define[Library]{@name{(scheme process-context)}}
@desc{[R7RS] This library exports procedures for accessing with the program's
calling context.

exported procedures:
@code{command-line exit emergency-exit get-enviromnent-variable
 get-enviromnent-variables}
}

@define[Library]{@name{(scheme read)}}
@desc{[R7RS]This library exports procedures for reading Scheme objects.

exported procedures:
@code{read}
}

@define[Library]{@name{(scheme repl)}}
@desc{[R7RS]This library library exports the
@code{interaction-environment} procedure.

exported procedures:
@code{interaction-environment}
}

@define[Library]{@name{(scheme time)}}
@desc{[R7RS]This library provides access to time-related
values.

exported procedures:
@code{current-jiffy current-second jiffies-per-second}
}

@define[Library]{@name{(scheme write)}}
@desc{[R7RS]This library exports procedures for writing Scheme objects.

exported procedures:
@code{display write write-shared write-simple}
}
