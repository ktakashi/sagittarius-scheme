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