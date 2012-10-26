@; -*- mode:scribble; coding: utf-8 -*-

@section{R6RS Libraries}

Sagittarius works with library even toplevel expressions are belong to a library
named @code{"user"}. Here I list up all R6RS libraries. Some libraries contain
the same procedure ie. assoc which is in @code{(rnrs (6))} and
@code{(srfi :1 lists)}. In this case I will put a pointer to other library's
section.

If library specifies its version, Sagittarius, however, ignores it. This
behaviour may change in future.

@subsection{Library form}

@define[Syntax]{@name{library}
 @args{name export-clause import-clause body}}
@desc{Declare a library named @var{name}.

@var{name} uniquely identifies a library and is globally visible in the
@code{import} clauses of all other libraries. It must have the following form:
@snipet{(@var{identifier1 identifier2} @dots{} @var{version})}

where @var{version} is empty or has the following form:
@code{(@var{sub-version} @dots{})}

An @var{export-clause} names a set of imported and locally defined bindings to
be exported. It must have following form:
@snipet{(export @var{export-spec} @dots{})}

@var{export-spec} must have one of the following forms:
@itemlist[
@item{@code{@var{identifier}}}
@item{@code{(rename (@var{identifier1 identifier2}) @dots{})}}
]
In an @var{export-spec}, an @var{identifier} names a single binding defined
within or imported into the library, where the external name for the export is
the same as the name of the binding within the library. A @code{rename} spec
exports the binding named by @var{identifier1} in each
@code{(@var{identifier1 identifier2})} pairing, using @var{identifier2} as
the external name.

@var{import-clause} specifies a set of bindings to be imported into the
library. It must have the following form:
@snipet{(import @var{import-spec} @dots{})}

Each @var{import-spec} specifies a set of bindings to be imported into the
library. An @var{import-spec} must be one of the
following:
@itemlist[
@item{@var{import-set}}
@item{@code{(for @var{import-set} @var{import-level} @dots{})}}
]

An @var{import-level} is one of the following:
@itemlist[
@item{@code{run}}
@item{@code{expand}}
@item{@code{(meta @var{level})}}
]
where @var{level} represents an exact integer object.

Note: The levels will be ignored on Sagittarius. The macro expansion phase
will be automatically resolved. For portable code, it is better to specify
the proper level.

An @var{import-set} names a set of bindings from another library and
possibly specifies local names for the imported bindings. It must be one of the
following:
@itemlist[
@item{@var{reference}}
@item{@code{(library @var{reference})}}
@item{@code{(only @var{import-set} @var{identifier} @dots{})}}
@item{@code{(except @var{import-set} @var{identifier} @dots{})}}
@item{@code{(prefix @var{import-set} @var{identifier})}}
@item{@code{(rename @var{import-set} (@var{identifier1 identifier2}) @dots{})}}
]
A @var{reference} identifies a library by its name and optionally by its
version. It must have one of the following forms:
@itemlist[
@item{@code{(@var{identifier1 identifier2} @dots{})}}
@item{@code{(@var{identifier1 identifier2} @dots{} @var{version})}}
]
Note: Sagittarius ignores @var{version}.

A @var{reference} whose first @var{identifier} is
@code{for, library, only, except, prefix} or @code{rename} is permitted only
within a @code{library} @var{import-set}. The @var{import-set}
@code{(library @var{reference})} is otherwise equivalent to @var{reference}.

By default, all of an imported library's exported bindings are made visible
within an importing library using the names given to the bindings by the
imported library. The precise set of bindings to be imported and the names of
those bindings can be adjusted with the @code{only, except, prefix} and
@code{rename} forms described below.
@itemlist[
@item{
  An @code{only} form produces a subset of the bindings from another
  @var{import-set}, including only the listed @var{identifier}s. The included
  @var{identifier}s should be in the original @var{import-set}.
}
@item{
  An @code{except} form produces a subset of the bindings from another
  @var{import-set}, including all but the listed @var{identifier}s. All of the
  excluded @var{identifier}s should be in the original @var{import-set}.
}
@item{
  A @code{prefix} form adds the @var{identifier} prefix to each name from
  another @var{import-set}.
}
@item{
  A @code{rename} form @code{(rename @var{identifier1 identifier2} @dots{})},
  removes the bindings for @var{identifier1 @dots{}} to form an intermediate
  @var{import-set}, then adds the bindings back for the corresponding
  @var{identifier2 @dots{}} to form the final @var{import-set}. Each
  @var{identifier1} should be the original @var{import-set}, each
  @var{identifier2} should not be int the intermediate @var{import-set}, and
  the @var{identifier2}'s must be distinct.
}
]
Note: Sagittarius does not check importing or exporting non-existing or
duplicated bindings. So the following code is actually valid.
@codeblock{
(library (foo)
  (export bar)
  (import (rename (rnrs) (define def) (not-exist define) (define def)))
 (def bar)
)
}

}

@subsection{Top library}

@define[Library]{@name{(rnrs (6))}}
@desc{[R6RS] The library @code{(rnrs (6))} is required by R6RS. It just export
all symbols from the libraries which are listed below.}

Most of these libraries documentations are from R6RS specification.
@itemlist[
 @item{@secref["rnrs.base.6"]{(rnrs base (6))}}
 @item{@secref["rnrs.unicode.6"]{(rnrs unicode (6))}}
 @item{@secref["rnrs.bytevectors.6"]{(rnrs bytevectors (6))}}
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

@include-section["rnrs/base.scrbl"]
@include-section["rnrs/unicode.scrbl"]
@include-section["rnrs/bytevectors.scrbl"]
@include-section["rnrs/lists.scrbl"]
@include-section["rnrs/sorting.scrbl"]
@include-section["rnrs/control.scrbl"]
@include-section["rnrs/records/syntactic.scrbl"]
@include-section["rnrs/records/procedural.scrbl"]
@include-section["rnrs/records/inspection.scrbl"]
@include-section["rnrs/exceptions.scrbl"]
@include-section["rnrs/conditions.scrbl"]
@include-section["rnrs/io.scrbl"]
@include-section["rnrs/files.scrbl"]
@include-section["rnrs/programs.scrbl"]
@include-section["rnrs/arithmetic.scrbl"]
@include-section["rnrs/syntax-case.scrbl"]
@include-section["rnrs/hashtables.scrbl"]
@include-section["rnrs/enums.scrbl"]
@include-section["rnrs/composite.scrbl"]
