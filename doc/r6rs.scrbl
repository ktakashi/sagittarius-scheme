@; -*- mode:scribble; coding: utf-8 -*-

@section{R6RS Libraries}

Sagittarius works with library even toplevel expressions are belong to a library
named @code{“user”}. Here I list up all R6RS libraries. Some libraries contain
the same procedure ie. assoc which is in @code{(rnrs (6))} and @code{(srfi :1 lists)}.
In this case I will put a pointer to other library's section.

If library specifies its version, Sagittarius, however, ignores it. This behaviour
may change in future.

@subsection{Top library}

@define[Library]{@name{(rnrs (6))}}
@desc{[R6RS] The library @code{(rnrs (6))} is required by R6RS. It just export all
symbols from the libraries which are listed below.}

Most of these libraries documentations are from R6RS specification.
@itemlist[
 @item{@secref["rnrs.base.6"]{(rnrs base (6))}}
 @item{@secref["rnrs.unicode.6"]{(rnrs unicode (6))}}
 @item{@secref["rnrs.bytevector.6"]{(rnrs bytevector (6))}}
 @item{@secref["rnrs.lists.6"]{(rnrs lists (6))}}
 @item{@secref["rnrs.sorting.6"]{(rnrs sorting (6))}}
 @item{@secref["rnrs.control.6"]{(rnrs control (6))}}
 @item{@secref["rnrs.records.syntactic.6"]{(rnrs records syntactic (6))}}
 @item{@secref["rnrs.records.procedural.6"]{(rnrs records procedural (6))}}
 @item{@secref["rnrs.records.inspection.6"]{(rnrs records inspection (6))}}
 @item{@secref["rnrs.exceptions.6"]{(rnrs exceptions (6))}}
 @item{@secref["rnrs.conditions.6"]{(rnrs conditions (6))}}
 @item{@secref["rnrs.io.ports.6"]{(rnrs io ports (6))}}
 @item{@secref["rnrs.io.simple.6"]{(rnrs io simple (6))}}
 @item{@secref["rnrs.files.6"]{(rnrs files (6))}}
 @item{@secref["rnrs.programs.6"]{(rnrs programs (6))}}
 @item{@secref["rnrs.arithmetic.fixnums.6"]{(rnrs arithmetic fixnums (6))}}
 @item{@secref["rnrs.arithmetic.flonums.6"]{(rnrs arithmetic flonums (6))}}
 @item{@secref["rnrs.arithmetic.bitwise.6"]{(rnrs arithmetic bitwise (6))}}
 @item{@secref["rnrs.syntax-case.6"]{(rnrs syntax-case (6))}}
 @item{@secref["rnrs.hashtables.6"]{(rnrs hashtables (6))}}
 @item{@secref["rnrs.enums.6"]{(rnrs enums (6))}}
]

@include-section("rnrs/base.scrbl")
@include-section("rnrs/unicode.scrbl")
@include-section("rnrs/bytevector.scrbl")
@include-section("rnrs/lists.scrbl")
@include-section("rnrs/sorting.scrbl")
@include-section("rnrs/control.scrbl")
@include-section("rnrs/records/syntactic.scrbl")
@include-section("rnrs/records/procedural.scrbl")
@include-section("rnrs/records/inspection.scrbl")
@include-section("rnrs/exceptions.scrbl")
@include-section("rnrs/conditions.scrbl")
@include-section("rnrs/io/ports.scrbl")
@include-section("rnrs/io/simple.scrbl")
@include-section("rnrs/files.scrbl")
@include-section("rnrs/programs.scrbl")
@include-section("rnrs/arithmetic/fixnums.scrbl")
@include-section("rnrs/arithmetic/flonums.scrbl")
@include-section("rnrs/arithmetic/bitwise.scrbl")
@include-section("rnrs/syntax-case.scrbl")
@include-section("rnrs/hashtables.scrbl")
@include-section("rnrs/enums.scrbl")
