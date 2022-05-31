[§1] R6RS Libraries
=============

Sagittarius works with library even toplevel expressions are belong to a library
named `"user"`. Here I list up all R6RS libraries. Some libraries contain
the same procedure ie. assoc which is in `(rnrs (6))` and
`(srfi :1 lists)`. In this case I will put a pointer to other library's
section.

If library specifies its version, Sagittarius, however, ignores it. This
behaviour may change in future.

[§2] Library form
-------------

###### [!Syntax] `library`  _name_ _export-clause_ _import-clause_ _body_

Declare a library named _name_.

_name_ uniquely identifies a library and is globally visible in the
`import` clauses of all other libraries. It must have the following form:
``(_identifier1 identifier2_ ... _version_)``

where _version_ is empty or has the following form:
`(_sub-version_ ...)`An _export-clause_ names a set of imported and locally defined bindings to
be exported. It must have following form:
``(export _export-spec_ ...)``

_export-spec_ must have one of the following forms:

- `_identifier_`
- `(rename (_identifier1 identifier2_) ...)`

In an _export-spec_, an _identifier_ names a single binding defined
within or imported into the library, where the external name for the export is
the same as the name of the binding within the library. A `rename` spec
exports the binding named by _identifier1_ in each
`(_identifier1 identifier2_)` pairing, using _identifier2_ as
the external name.

_import-clause_ specifies a set of bindings to be imported into the
library. It must have the following form:
``(import _import-spec_ ...)``

Each _import-spec_ specifies a set of bindings to be imported into the
library. An _import-spec_ must be one of the
following:

- _import-set_
- `(for _import-set_ _import-level_ ...)`

An _import-level_ is one of the following:

- `run`
- `expand`
- `(meta _level_)`

where _level_ represents an exact integer object.

Note: The levels will be ignored on Sagittarius. The macro expansion phase
will be automatically resolved. For portable code, it is better to specify
the proper level.

An _import-set_ names a set of bindings from another library and
possibly specifies local names for the imported bindings. It must be one of the
following:

- _reference_
- `(library _reference_)`
- `(only _import-set_ _identifier_ ...)`
- `(except _import-set_ _identifier_ ...)`
- `(prefix _import-set_ _identifier_)`
- `(rename _import-set_ (_identifier1 identifier2_) ...)`

A _reference_ identifies a library by its name and optionally by its
version. It must have one of the following forms:

- `(_identifier1 identifier2_ ...)`
- `(_identifier1 identifier2_ ... _version_)`

Note: Sagittarius ignores _version_.

A _reference_ whose first _identifier_ is
`for, library, only, except, prefix` or `rename` is permitted only
within a `library` _import-set_. The _import-set_`(library _reference_)` is otherwise equivalent to _reference_.

By default, all of an imported library's exported bindings are made visible
within an importing library using the names given to the bindings by the
imported library. The precise set of bindings to be imported and the names of
those bindings can be adjusted with the `only, except, prefix` and
`rename` forms described below.

- An `only` form produces a subset of the bindings from another
  _import-set_, including only the listed _identifier_s. The included
  _identifier_s should be in the original _import-set_.
- An `except` form produces a subset of the bindings from another
  _import-set_, including all but the listed _identifier_s. All of the
  excluded _identifier_s should be in the original _import-set_.
- A `prefix` form adds the _identifier_ prefix to each name from
  another _import-set_.
- A `rename` form `(rename _identifier1 identifier2_ ...)`,
  removes the bindings for _identifier1 ..._ to form an intermediate
  _import-set_, then adds the bindings back for the corresponding
  _identifier2 ..._ to form the final _import-set_. Each
  _identifier1_ should be the original _import-set_, each
  _identifier2_ should not be int the intermediate _import-set_, and
  the _identifier2_'s must be distinct.

Note: Sagittarius does not check importing or exporting non-existing or
duplicated bindings. So the following code is actually valid.

``````````scheme
(library (foo)
  (export bar)
  (import (rename (rnrs) (define def) (not-exist define) (define def)))
 (def bar)
)
``````````



[§2] Top library
-------------

###### [!Library] `(rnrs (6))` 

[R6RS] The library `(rnrs (6))` is required by R6RS. It just export
all symbols from the libraries which are listed below.

Most of these libraries documentations are from R6RS specification.

- [(rnrs base (6))](#rnrs.base.6)
- [(rnrs unicode (6))](#rnrs.unicode.6)
- [(rnrs bytevectors (6))](#rnrs.bytevectors.6)
- [(rnrs lists (6))](#rnrs.lists.6)
- [(rnrs sorting (6))](#rnrs.sorting.6)
- [(rnrs control (6))](#rnrs.control.6)
- [(rnrs records syntactic (6))](#rnrs.records.syntactic.6)
- [(rnrs records procedural (6))](#rnrs.records.procedural.6)
- [(rnrs records inspection (6))](#rnrs.records.inspection.6)
- [(rnrs exceptions (6))](#rnrs.exceptions.6)
- [(rnrs conditions (6))](#rnrs.conditions.6)
- [(rnrs io ports (6))](#rnrs.io.ports.6)
- [(rnrs io simple (6))](#rnrs.io.simple.6)
- [(rnrs files (6))](#rnrs.files.6)
- [(rnrs programs (6))](#rnrs.programs.6)
- [(rnrs arithmetic fixnums (6))](#rnrs.arithmetic.fixnums.6)
- [(rnrs arithmetic flonums (6))](#rnrs.arithmetic.flonums.6)
- [(rnrs arithmetic bitwise (6))](#rnrs.arithmetic.bitwise.6)
- [(rnrs syntax-case (6))](#rnrs.syntax-case.6)
- [(rnrs hashtables (6))](#rnrs.hashtables.6)
- [(rnrs enums (6))](#rnrs.enums.6)

* @[[rnrs/base.md](rnrs/base.md)]
* @[[rnrs/unicode.md](rnrs/unicode.md)]
* @[[rnrs/bytevectors.md](rnrs/bytevectors.md)]
* @[[rnrs/lists.md](rnrs/lists.md)]
* @[[rnrs/sorting.md](rnrs/sorting.md)]
* @[[rnrs/control.md](rnrs/control.md)]
* @[[rnrs/records/syntactic.md](rnrs/records/syntactic.md)]
* @[[rnrs/records/procedural.md](rnrs/records/procedural.md)]
* @[[rnrs/records/inspection.md](rnrs/records/inspection.md)]
* @[[rnrs/exceptions.md](rnrs/exceptions.md)]
* @[[rnrs/conditions.md](rnrs/conditions.md)]
* @[[rnrs/io.md](rnrs/io.md)]
* @[[rnrs/files.md](rnrs/files.md)]
* @[[rnrs/programs.md](rnrs/programs.md)]
* @[[rnrs/arithmetic.md](rnrs/arithmetic.md)]
* @[[rnrs/syntax-case.md](rnrs/syntax-case.md)]
* @[[rnrs/hashtables.md](rnrs/hashtables.md)]
* @[[rnrs/enums.md](rnrs/enums.md)]
* @[[rnrs/composite.md](rnrs/composite.md)]
