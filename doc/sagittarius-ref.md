Sagittarius Users' Reference
=============

This document is a manual for Sagittarius, an R6RS/R7RS Scheme implementation.
This is for version @@(sagittarius-version)@@.

{{table-of-contents}}

[§1] Introduction
=============

This is a users' guide and reference manual of Sagittarius Scheme system. Here
I tried to describe the points which are not conformed to R6RS.

The target readers are those who already know Scheme and want to write useful
programs in Sagittarius.


[§2] Overview of Sagittarius
-------------

Sagittarius is a Scheme script engine; it reads Scheme programs, compiles it
on-the-fly and executes it on a virtual machine. Sagittarius conforms the
language standard "Revised^6 Report on the Algorithmic Language Scheme" (R6RS),
"Revised^7 Report on the Algorithmic Language Scheme" (R7RS), and supports
various common libraries defined in "Scheme Requests for Implementation"
(SRFI)s.

There are a lot of Scheme implementations and they have different strong and
weak points. Sagittarius focuses on "Flexibility" and "Easy to Use". R6RS
specifies strict requirements, but sometimes you may think this is too
much. For that purpose, Sagittarius has less strictness. There are invocation
options that make Sagittarius run on strict RnRS mode.

To avoid to lose portability or write miss-working code, you may want to know
what are the non conformed points.

Reader
: Basically reader has 3 modes. One is R6RS mode, one is R7RS mode and the 
  last one is compatible mode. Although, user can modify reader with reader
  macro. These modes can be switched via `#!r6rs`, `#!r7rs` or
  `#!compatible`. For more details, see
  [Predefined reader macros](#lib.sagittarius.reader.predefined).

Miscellaneous
: Redefinition of exported values are allowed on script file. This can be
  restricted by `-r6` option to run the script strict R6RS mode.
  Multiple import of the same identifier is allowed. The value which 
  imported at the last will be used.

[§2] Notations
-------------

In this manual, each entry is represented like this.

###### [!Category] `foo`  _arg1_ _arg2_

[spec] Description foo ...

_Category_ denotes category of the entry **foo**. The following category
will appear in this manual.

Program
: A command line program

Function
: A Scheme function

Syntax
: A syntax

Auxiliary Syntax
: A auxiliary syntax

Macro
: A macro

Auxiliary Macro
: A auxiliary macro

Library
: A library

Condition Type
: A condition type

Reader Macro
: A reader macro

Class
: A CLOS class

Generic
: A generic function

Method
: A method

For functions, syntaxes, or macros, the the entry may be followed by one or more
arguments. In the arguments list, following notations may appear.

_arg ..._
: Indicates zero or more arguments

_:optional x y z_
_:optional (x x-default) (y y-default) (z z-default)_
: Indicates is may take up to three optional arguments. The second form specifies
  default values for x, y and z.

The description of the entry follows the entry line. If the specification of the
entry comes from some standard or implementation, its origin is noted in the
bracket at the beginning of the description. The following origins are noted:

[R6RS]
[R6RS+]
: The entry works as specified in "Revised^6 Report on the Algorithmic Language
  Scheme.". If it is marked as "[R6RS+]", the entry has additional functionality.

[R7RS]
: The entry works as specified in "Revised^7 Report on the Algorithmic Language
  Scheme.".

[SRFI-n]
[SRFI-n+]
: The entry works as specified in SRFI-n. If
  it is marked as "[SRFI-n+]", the entry has additional functionality.

[§1] Programming in Sagittarius
=============

[§2] Invoking Sagittarius
-------------

Sagittarius can be used as an independent Scheme interpreter. The interpreter
which comes with Sagittarius distribution is a program named `sagittarius`on Unix like environment and `sash` on Windows environment.

###### [!Program] `sagittarius`  _[options]_ _scheme-file_ _arg_ _..._

Invoking sagittarius. If _scheme-file_ is not given, it runs with
interactive mode.

Specifying `-r` option with Scheme standard number, currently `6`and
`7` are supported, forces to run Sagittarius on strict standard
mode. For example, entire script is read then evaluated on R6RS (`-r6`
option) mode. Thus macros can be located below the main script.

Detail options are given with option `"-h"`.

For backward compatibility, symbolic link `sash` is also provided
on Unix like environment. However this may not exist if Sagittarius is built
with disabling symbolic link option.

[§2] Writing Scheme scripts
-------------

When a Scheme file is given to `sagittarius`, it bounds an internal
variable to list of the remaining command-line arguments which you can get with
the `command-line` procedure, then loads the Scheme program. If the first
line of scheme-file begins with `"#!"`, then Sagittarius ignores the
entire line. This is useful to write a Scheme program that works as an
executable script in unix-like systems.

Typical Sagittarius script has the first line like this:

``#!/usr/local/bin/sagittarius``

or

``#!/bin/env sagittarius``

The second form uses "shell trampoline" technique so that the script works as
far as `sagittarius` is in the PATH.

After the script file is successfully loaded, then Sagittarius will process all
toplevel expression the same as Perl.

Now I show a simple example below. This script works like `cat(1)`, without
any command-line option processing and error handling.

```scheme
#!/usr/local/bin/sagittarius
(import (rnrs))
(let ((args (command-line)))
  (unless (null? (cdr args))
    (for-each (lambda (file)
		(call-with-input-file file
		  (lambda (in)
		    (display (get-string-all in)))))
	      (cdr args)))
  0)
```

If you need to add extra load path in the executing script file, you can
also write like this:

```scheme
#!/bin/bash
#|
# Adding lib directory located in the same directory as this script
# is located
DIR=$(dirname "$0")
exec sagittarius -L${DIR}/lib $0 "$@"
|#
;; Scheme script
(import (rnrs))
```

If the script file contains `main` procedure, then Sagittarius execute
it as well with one argument which contains all command line arguments. This
feature is defined in 
[SRFI-22](http://srfi.schemers.org/srfi-22/). So the
above example can also be written like the following:

```scheme
#!/usr/local/bin/sagittarius
(import (rnrs))
(define (main args)
  (unless (null? (cdr args))
    (for-each (lambda (file)
		(call-with-input-file file
		  (lambda (in)
		    (display (get-string-all in)))))
	      (cdr args)))
  0)
```

NOTE: the `main` procedure is called after all toplevel expressions
are executed.

[§2] Working on REPL
-------------

If `sagittarius` does not get any script file to process, then it will
go in to REPL (read-eval-print-loop). For developers' convenience, REPL
imports some libraries by default such as `(rnrs)`.

If `.sashrc` file is located in the directory indicated `HOME` or
`USERPROFILE` environment variable, then REPL reads it before evaluating
user input. So developer can pre-load some more libraries, instead of typing
each time.

NOTE: `.sashrc` is only for REPL, it is developers duty to load all
libraries on script file.

[§2] Writing a library
-------------

Sagittarius provides 2 styles to write a library, one is R6RS style and other
one is R7RS style. Both styles are processed the same and users can use it
without losing code portability.

Following example is written in R6RS style, for the detail of
`library` syntax please see the R6RS document described in bellow
sections.

```scheme
(library (foo)
  (export bar)
  (import (rnrs))

 (define bar 'bar) )
```

The library named `(foo)` must be saved the file
named `foo.scm`, `foo.ss`, `foo.sls` or `foo.sld` (I use
`.scm` for all examples) and located on the loading path, the value is
returned by calling `add-load-path` with 0 length string.

### [§3] Library file extensions

The file extension also controls the reader mode.

`.ss` and `.sls`
: When a file with one of the extensions is loaded during library importing,
  then `#!r6rs` directive is applied to the target file by default.

`.sld`
: When a file has this extensions, then `#!r7rs` directive is applied to the
  target file by default.

`.scm`
: This file extension uses `#!compatible` directive. 

If the loading file contains other directive, then the default directive
is overwritten.

If you want to write portable code yet want to use Sagittarius specific
functionality, then you can write implementation specific code separately using
`.sagittarius.scm`, `.sagittarius.ss`, `.sagittarius.sls` or
`.sagittarius.sld` extensions. This functionality is implemented almost
all R6RS implementation. If you use R7RS style library syntax, then you can
also use `cond-expand` to separate implementation specific
functionalities.

### [§3] Hidden library

If you don't want to share a library but only used in specific one, you can
write both in one file and name the file you want to show. For example;

```scheme
(library (not showing)
  ;; exports all internal use procedures
  (export ...)
  (import (rnrs))
;; write procedures
...
)

(library (shared)
  (export shared-procedure ...)
  (import (rnrs) (not showing))
;; write shared procedures here
)
```

Above script must be saved the file named `shared.scm`. The order of
libraries are important. Top most dependency must be the first and next is
second most, so on.

Note: This style can hide some private procedures however if you want to write
portable code, some implementations do not allow you to write this style.

[§2] Compiled cache
-------------

For better starting time, Sagittarius caches compiled libraries. The cache files
are stored in one of the following environment variables;

For Unix like (POSIX) environment:

- `SAGITTARIUS_CACHE_DIR`
- `HOME`

For Windows environment:

- `SAGITTARIUS_CACHE_DIR`
- `TEMP`
- `TMP`

Sagittarius will use the variables respectively, so if the
`SAGITTARIUS_CACHE_DIR` is found then it will be used.

The caching compiled file is carefully designed however the cache file might be
stored in broken state. In that case use `-c` option with
`sagittarius`, then it will wipe all cache files. If you don't want to use
it, pass `-d` option then Sagittarius won't use it.

Adding `#!nocache` directive to a library file would disable caching of
the file. This is sometimes useful especially if you need importing time
information, e.g. loading path, which is not available if the library is
loaded from a cache.


[§2] Deviation
--------------

Some of the behaviours are not compliant or deviate from other implementations.
Here, we list some of them.

### [§3] Macro expansion

The following code will print `#t` instead of `#f` with `-r6`command line option or doing it on a library:

```scheme
(import (rnrs) (for (rnrs eval) expand))

(define-syntax foo
  (lambda(ctx)
    (syntax-case ctx ()
      ((_ id)
       (free-identifier=?
	#'id
	(eval '(datum->syntax #'k 'bar) (environment '(rnrs))))))))

(define bar)

(display (foo bar))
```

This is because, Sagittarius doesn't have separated phases of macro expansion
and compilation. When `foo` is expanded, then the `bar` is not defined
yet or at least it's not visible during the macro expansion. So, both _bar_s
are not bound, then `free-identifier=?` will consider them the same
identifiers.

### [§3] Directives

On R6RS, symbol starts with `#!` is ignored, however this is not the case
on R7RS. Sagittarius ignores these symbols on both mode.

* @[[r6rs.md](r6rs.md)]
* @[[r7rs.md](r7rs.md)]
* @[[clos.md](clos.md)]
* @[[sagittarius.md](sagittarius.md)]
* @[[utils.md](utils.md)]
* @[[ported.md](ported.md)]
* @[[srfi.md](srfi.md)]

[§1] Index {#index}
=============

{{index-table}}

{{author}}

