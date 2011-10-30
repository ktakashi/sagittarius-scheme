@; -*- mode:scribble; coding: utf-8 -*-
@title{Sagittarius Users' Reference}
@centered{@italic{version @eval{(sagittarius-version)}}}

This document is a manual for Sagittarius, a Mostly R6RS Scheme implementation.
This is for version @eval{(sagittarius-version)}.

@table-of-contents[]

@section{Introduction}

This is a users' guide and reference manual of Sagittarius Scheme system. Here
I tried to describe the points which are not conformed to R6RS.

The target readers are those who already know Scheme and want to write useful
program in Sagittarius.

This manual only deals with Scheme side of things. Sagittarius has another face,
a C interface. Details of it will be discussed in a separate document to be
written. Those who wants to use Sagittarius as an embedded language, or wants to
write an extension, need that volume.

@subsection{Overview of Sagittarius}

Sagittarius is a Scheme script engine; it reads Scheme programs, compiles it
on-the-fly and executes it on a virtual machine. Sagittarius @bold{Mostly}
conforms the language standard "Revised^6 Report on the Algorithmic Language
Scheme" (R6RS), and supports various common libraries defined in SRFIs.

There are a lot of Scheme implementations and it has different strong and weak
point. Frankly speaking, Sagittarius does not have much competitive point from
other implementation. The only thing it has “Flexibility”. R6RS specifies
strict requirements, but sometimes you may think this is too much. For that
purpose, Sagittarius has less strictness but it makes not to conform the
requirements. That's why it's Mostly R6RS.

To avoid to lose portability or write miss-working code, you may want to know
what are the non conformed points.

@table[]{
@item["Reader"]{
 Reader has two mode. One is R6RS mode and other one is compatible mode. Default
 mode is R6RS  you can switch with @code{#!compatible} hash-bang which describe
 later. In compatible mode, reader also can read keyword which start with
 @code{“:”} and more non R6RS feature will be available.}
@item["Macro expansion"]{
 On R6RS requires explicit macro expansion phase, however in Sagittarius we do
 not have it. A macro will expand when programs will be compiled.}
@item["Unbound symbol"]{
 If you write unbound symbol in your code, Sagittarius, however, won't raise
 error until it really called. R6RS does not allow this behaviour. And also
 export symbol. If you do not define exported symbol, Sagittarius, then, won't
 raise error until it will be called. I'm not sure if this is convenient or not.
 So this behaviour may be changed.}
@item["Toplevel"]{
 Sagittarius does not require toplevel expression which is specified in R6RS.}
@item["Miscellaneous"]{
 Redefinition of exported values are allowed. The value which imported at the
 last will be used.

 Almost everything is first class object. The syntax keyword @code{export} is
 not.}
}

@subsection{Notations}

In this manual, each entry is represented like this.

@define[Category]{@name{foo} @args{arg1 arg2}}
@desc{[spec] Description foo @dots{}}

@var{Category} denotes category of the entry @bold{foo}. The following category
will appear in this manual.

@itemlist[
@item{Program - A command line program}
@item{Function - A Scheme function}
@item{Syntax - A syntax}
@item{Macro - A macro}
@item{Library - A library}
]

For functions, syntaxes, or macros, the the entry may be followed by one or more
arguments. In the arguments list, following notations may appear.

@itemlist[
@item{@var{arg @dots{}} @p{Indicates zero or more arguments}}
@item{@var{:optional x y z} @var{:optional (x x-default) (y y-default) (z z-default)}
 @p{Indicates is may take up to three optional arguments. The second form
    specifies default values for x and y. This is either builtin functions or
    closures which defined with define-optional macro.}
}
]

The description of the entry follows the entry line. If the specification of the
entry comes from some standard or implementation, its origin is noted in the
bracket at the beginning of the description. The following origins are noted:

@itemlist[
@item{[R6RS] [R6RS+] @p{The entry works as specified in “Revised^6 Report of
 Algorithmic Language Scheme.”. If it is marked as "[R6RS+]", the entry has
 additional functionality.}}
@item{[SRFI-n] [SRFI-n+] @p{The entry works as specified in SRFI-n. If it is
 marked as "[SRFI-n+]", the entry has additional functionality.}}
]

@section{Programming in Sagittarius}

@subsection{Invoking sash}

Sagittarius can be used either as an independent Schame interpreter or an
embedded Scheme library. The interpreter which comes with Sagittarius destribution
is a program named sash.

@define[Program]{@name{sash} @args{[options] scheme-file arg @dots{}}}
@desc{Invoking sash. If @var{scheme-file} is not given, it runs with interactive mode.
Detail options are given with option @code{“-h”}.}

@subsection{Writing Scheme scripts}

When a Scheme file is given to sash, it bounds an internal variable to list of
the remaining command-line arguments which you can get with command-line procedure,
then loads the Scheme program. If the first line of scheme-file begins with
@code{“#!”}, then sash ignores the entire line. This is useful to write a Scheme
program that works as an executable script in unix-like systems.

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

@include-section["r6rs.scrbl"]