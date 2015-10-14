@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "lib.sagittarius.debug"]{(sagittarius debug) - Debugging support}

@define[Library]{@name{(sagittarius debug)}}
@desc{This library provides debugging support reader macro.}

@define["Reader Macro"]{@name{#?=} @args{expr}}
@desc{This reader macro reads the next expression as followings;

@snipet{(debug-print @var{expr})}

@code{debug-print} is an internal macro of this library which prints the
read expression and its result.

Following example shows how to enable this;

@codeblock{
#!read-macro=sagittarius/debug
#!debug
(let ((a (+ 1 2)))
  #?=(expt a 2))

#|
#?=(expt a 2)
#?-    9
|#
}

@code{#!debug} enables the debug print.
}

@define[Function]{@name{macroexpand} @args{expr}}
@desc{Expands given @var{expr}. The returning value may or may not be used
as proper Scheme expression.
}

@define[Function]{@name{macroexpand-1} @args{expr}}
@define[Function]{@name{macroexpand-n} @args{expr n}}
@desc{Expands given @var{expr} @var{n} times. The first form's @var{n} is 1.

This procedure expands only globally defined macro and the result of expansion
is other macro such as next rule of @code{syntax-rules}. It doesn't consider
locally bound macros.

The returning value may or may not be used as proper Scheme expression.
}

Above 2 procedures are no more than debug aid. Depending on the result of
expansion is not guaranteed to work.
