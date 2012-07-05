@; -*- mode:scribble; coding: utf-8 -*-
@title{Sagittarius Users' Reference}

This document is a manual for Sagittarius, an R6RS Scheme implementation.
This is for version @eval{(sagittarius-version)}.

@table-of-contents[:id "table-of-contents"]

@section{Introduction}

This is a users' guide and reference manual of Sagittarius Scheme system. Here
I tried to describe the points which are not conformed to R6RS.

The target readers are those who already know Scheme and want to write useful
programs in Sagittarius.

This manual only deals with Scheme side of things. Sagittarius has another face,
a C interface. Details of it will be discussed in a separate document yet to be
written. Those who wants to use Sagittarius as an embedded language, or wants to
write an extension, need that volume.

@subsection{Overview of Sagittarius}

Sagittarius is a Scheme script engine; it reads Scheme programs, compiles it
on-the-fly and executes it on a virtual machine. Sagittarius @b{Mostly}
conforms the language standard "Revised^6 Report on the Algorithmic Language
Scheme" (R6RS), and supports various common libraries defined in 
"Scheme Requests for Implementation" (SRFI)s.

There are a lot of Scheme implementations and it has different strong and weak
point. Sagittarius focuses on "Flexibility" and "Easy to Use". R6RS specifies
strict requirements, but sometimes you may think this is too much. For that
purpose, Sagittarius has less strictness but it makes not to conform the
requirements. That's why it is Mostly R6RS.

To avoid to lose portability or write miss-working code, you may want to know
what are the non conformed points.

@dl-list[]{
@dl-item["Reader"]{
 Basically reader has 2 modes. One is R6RS mode and other one is compatible
 mode. Although, user can modify reader with reader macro. For detail, see
 @secref["lib.sagittarius.reader.predefined"]{Predefined reader macros}.
}
@dl-item["Macro expansion"]{
 On R6RS requires explicit macro expansion phase, however Sagittarius does
 not have it. A macro is expanded when programs are compiled.}
@dl-item["Unbound symbol"]{
 If you write unbound symbol in your code, however Sagittarius won't raise
 error until it really called. R6RS does not allow this behaviour. And also
 exported symbol. If you do not define exported symbol, Sagittarius, then, won't
 raise error until it will be called. I'm not sure if this is convenient or not.
 So this behaviour may be changed.}
@dl-item["Toplevel"]{
 Sagittarius does not require toplevel expression which is specified in R6RS.}
@dl-item["Miscellaneous"]{
 Redefinition of exported values are allowed. The value which imported at the
 last will be used.}
}

@subsection{Notations}

In this manual, each entry is represented like this.

@define[Category]{@name{foo} @args{arg1 arg2}}
@desc{[spec] Description foo @dots{}}

@var{Category} denotes category of the entry @b{foo}. The following category
will appear in this manual.

@dl-list[
@dl-item["Program"]{A command line program}
@dl-item["Function"]{A Scheme function}
@dl-item["Syntax"]{A syntax}
@dl-item["Auxiliary Syntax"]{A auxiliary syntax}
@dl-item["Macro"]{A macro}
@dl-item["Auxiliary Macro"]{A auxiliary macro}
@dl-item["Library"]{A library}
@dl-item["Condition Type"]{A condition type}
@dl-item["Reader Macro"]{A reader macro}
@dl-item["Class"]{A CLOS class}
@dl-item["Generic"]{A generic function}
@dl-item["Method"]{A method}
]

For functions, syntaxes, or macros, the the entry may be followed by one or more
arguments. In the arguments list, following notations may appear.

@dl-list[
@dl-item[@var{arg @dots{}}]{Indicates zero or more arguments}
@dl-itemx[2 @var{:optional x y z} 
	    @var{:optional (x x-default) (y y-default) (z z-default)}]{
Indicates is may take up to three optional arguments. The second form specifies
default values for x and y. This is either builtin functions or closures which
defined with define-optional macro.}
]

The description of the entry follows the entry line. If the specification of the
entry comes from some standard or implementation, its origin is noted in the
bracket at the beginning of the description. The following origins are noted:

@dl-list[
@dl-itemx[2 "[R6RS]" "[R6RS+]"]{
The entry works as specified in "Revised^6 Report on the Algorithmic Language
Scheme.". If it is marked as "[R6RS+]", the entry has additional functionality.}
@dl-item["[R7RS]"]{
The entry works as specified in "Revised^7 Report on the Algorithmic Language
Scheme."(draft 5).}
@dl-itemx[2 "[SRFI-n]" "[SRFI-n+]"]{The entry works as specified in SRFI-n. If
it is marked as "[SRFI-n+]", the entry has additional functionality.}
]

@section{Programming in Sagittarius}

@subsection{Invoking sash}

Sagittarius can be used either as an independent Schame interpreter or an
embedded Scheme library. The interpreter which comes with Sagittarius
destribution is a program named sash.

@define[Program]{@name{sash} @args{[options] scheme-file arg @dots{}}}
@desc{Invoking sash. If @var{scheme-file} is not given, it runs with interactive
mode.
Detail options are given with option @code{"-h"}.}

@subsection{Writing Scheme scripts}

When a Scheme file is given to @code{sash}, it bounds an internal variable to
list of the remaining command-line arguments which you can get with the 
@code{command-line} procedure, then loads the Scheme program. If the first line
of scheme-file begins with @code{"#!"}, then sash ignores the entire line. This 
is useful to write a Scheme program that works as an executable script in
unix-like systems.

Typical Sagittarius script has the first line like this:

@code{#!/usr/local/bin/sash}

or

@code{#!/bin/env sash}

The second form uses "shell trampoline" technique so that the script works as
far as sash is in the PATH.

After the script file is successfully loaded, then sash will process all
toplevel expression the same as Perl.

Now I show a simple example below. This script works like @code{cat(1)}, without
any command-line option processing and error handling.

@codeblock{
#!/usr/local/bin/sash
(import (rnrs))
(let ((args (command-line)))
  (unless (null? (cdr args))
    (for-each (lambda (file)
		(call-with-input-file file
		  (lambda (in)
		    (display (get-string-all in)))))
	      (cdr args)))
  0)
}
@subsection{Working on REPL}

If @code{sash} does not get any script file to process, then it will go in to
REPL (read-eval-print-loop). For developers' convenience, REPL imports some
libraries by default such as @code{(rnrs)}.

If @code{.sashrc} file is located in the directory indicated @code{HOME} or
@code{USERPROFILE} environment variable, then REPL reads it before evaluating
user input. So developer can pre-load some more libraries, instead of typing
each time.

NOTE: @code{.sashrc} is only for REPL, it is developers duty to load all
libraries on script file.


@include-section["r6rs.scrbl"]
@include-section["r7rs.scrbl"]
@include-section["clos.scrbl"]
@include-section["sagittarius.scrbl"]
@include-section["utils.scrbl"]
@include-section["ported.scrbl"]
@include-section["srfi.scrbl"]
@section[:appendix "A" :tag "index"]{Index}

@index-table[:id "index-table"]
@author["Takashi Kato"]