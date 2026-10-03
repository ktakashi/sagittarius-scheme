---
name: scheme
description: 'Instructions for writing Scheme code in the project.'
applyTo: '**/*.scm'
---

# Scheme Code Instructions

These instructions provide guidelines for writing Scheme code in the project. Follow these conventions to ensure consistency and maintainability across the codebase.

## File Naming

- Library `(text json)` → file `sitelib/text/json.scm`
- SRFI `(srfi :64 testing)` → file `sitelib/srfi/%3a64/testing.scm`
  - Note: `:` is encoded as `%3a` in filenames

## File Header

Scheme files should include a standard header at the top of each file. The header should contain the file mode, a brief description, copyright information, and licensing terms.

Example:

```scheme
;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; library/name.scm - Brief description
;;;
;;;   Copyright (c) YYYY  Author Name  <email@example.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
```

Optionally, if the library is implementing an SRFI, RFC, or other standard, 
include a reference to the relevant document in the header.

Example:
```scheme
;; references:
;; - SRFI 0: https://srfi.schemers.org/srfi-0/srfi-0.html
;; - RFC 1234: https://www.rfc-editor.org/info/rfc1234
```

## Library form

Scheme library should be defined using the `(library ...)` form, specifying the library name, exported identifiers, and internal definitions. This ensures that the library can be properly imported and used by other Scheme code.

Example:
```scheme
#!nounbound
(library (my-library)
    (export my-function)
    (import (rnrs))

(define (my-function x)
  (* x x))
)
```

### Annotations

Scheme library may have annotations, such as `#!nounbound`. Use the annotations at the beginning of the library form, before the `(library ...)` declaration.
The list below provides some common annotations used in Scheme libraries.

- `#!nounbound`: Ensures that references to unbound variables are treated as errors.
- `#!deprecated`: Marks the library or certain definitions as deprecated, indicating that they should not be used in new code.
- `#!read-macro=sagittarius/regex`: Enables the use of the Sagittarius Scheme regular expression read macro. Make sure to `(sagittarius regex)` library is imported in the library if you use this annotation.
- `#!read-macro=sagittarius/bv-string`: Enables the use of the Sagittarius Scheme byte string read macro.

#### `#!read-macro` annotation

The `#!read-macro` annotation internally loads the corresponding library, specified after the `=`.
The format of the annotation is `#!read-macro=<normalized-library>`, where `<normalized-library>` is the library to be loaded. For example, `#!read-macro=sagittarius/regex` will load the `(sagittarius regex)` library.

##### `#!read-macro=sagittarius/regex`

This enables the read macro of `#/regex/` in the Scheme code. The read expression will
be interpreted as a regular expression pattern object.

##### `#!read-macro=sagittarius/bv-string`

This enables the read macro of `#*"abcde"` in the Scheme code. The read expression will
be interpreted as a `(string->utf8 "abcde")`, but the conversion is done at read time.
So the read macro returns a bytevector.

## Coding conventions

### Type checking

When a argument type of the defining procedure is known, use Sagittarius builtin
type checking functionality.

Example 1: simple type checking

```scheme
(define (square (x integer?))
  (* x x))
```

Example 2: polymorphic type checking
```scheme
;; #f or environment
(define (run expr (env (or #f environment?))
  (eval expr (or env (interaction-environment)))))

```
```scheme
;; string or symbol
(define (f (name (or string? symbol?)))
  (display name))
```

Example 3: complex type checking
```scheme
(define (foo (port (and input-port? binary-port?)))
  (get-bytevector port 8))
```

### Record type export

R6RS record generates accessors automatically, however record type name
from the other libraries should look like `<record-type>`. To do this,
we use `rename` clause on the `export`.

Example:

```scheme
(library (my-library)
  (export (rename (record-type <record-type>))
          record-type?
          record-type-field)
  (import ...)
(define-record-type record-type
  (fields field))
)
```

## Testing

### Test library

Use SRFI-64 testing library, `(srfi :64 testing)`, for writing tests.

### Test File Location

Test files mirror library structure under `test/tests/`:
Test paths mirror the library path with the source root (`lib/` or `sitelib/`) 
stripped: e.g. `sitelib/text/json.scm` → `test/tests/text/json.scm`.
- `sitelib/json.scm` → `test/tests/json.scm`
- `sitelib/text/json.scm` → `test/tests/text/json.scm`
- `sitelib/srfi/%3a64/testing.scm` → `test/tests/srfi/%3a64/testing.scm`

If the library located under `ext/<name>`, then the test must be in the
`ext/<name>/test.scm`. Use `(include "file")` to separate the test into
multiple files if needed. See `ext/crypto/test.scm` for an example.

### Test Template

```scheme
;; -*- mode:scheme; coding:utf-8; -*-
(import (rnrs)
        (library-to-test)
        (srfi :64 testing))

(test-begin "Library name tests")

;; Group related tests
(test-equal "description" expected actual)
(test-assert "description" expression)
(test-error "description" condition-type expression)

(test-end)
```

### Running Tests

```shell
# Run specific test
./build/sagittarius -Llib -Lsitelib -L'ext/*' -Dbuild \
  test/runner.scm test/tests/your-test.scm

# Run via ctest
ctest --output-on-failure -R pattern
```

If a new test file is added, register it in `test/CMakeLists.txt` (or the relevant CMake test list) so `ctest` discovers it.

## SRFI Implementation

### R7RS-style SRFI Libraries

Write only the R6RS-named library file under `sitelib/srfi/%3aNN/`; the R7RS-named `(srfi NN)` wrapper is generated automatically by `./dist.sh srfi` and should not be hand-written.

```scheme
;; sitelib/srfi/%3a99/records.scm
(library (srfi :99 records)
    (export ...)
    (import ...)
  ...)
```

After adding, run the SRFI generator:
```shell
./dist.sh srfi
```

This generates R7RS-style wrappers in `sitelib/srfi/`.

## Build & Test Failure Policy

1. If `./build/sagittarius` is missing, build the project before running tests.
2. The existing tests are expected to pass; if any test fails, investigate the cause without modifying the test itself.
3. If the failing test is due to the implementation change of the library itself, and the stakeholder accepts the backward incompatibility, update the test accordingly.
