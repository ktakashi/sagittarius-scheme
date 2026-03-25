---
name: scheme-library
description: 'Guide for developing Scheme libraries for Sagittarius. Use this when creating or modifying Scheme libraries in sitelib/, implementing SRFI, or adding new utility libraries.'
---

# Scheme Library Development Guide

This guide provides best practices for developing Scheme libraries for Sagittarius.

## When to Use

- Creating new utility libraries in `sitelib/`
- Implementing SRFI libraries
- Refactoring existing libraries
- Adding features to existing libraries

## Directory Selection

| Directory | Purpose | Dependencies |
|-----------|---------|--------------|
| `lib/` | Core/builtin libraries (rnrs, core, clos) | No sitelib or ext dependencies |
| `sitelib/` | Utility libraries (srfi, rfc, text, util) | Can depend on lib and other sitelib |
| `ext/*/sagittarius/` | Extension libraries with C bindings | Can depend on lib only (with exceptions) |

## Library Structure

### File Header

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

### Library Form

```scheme
#!nounbound
(library (library-path name)
    (export procedure-1
            procedure-2
            <class-name>
            +constant+)
    (import (rnrs)
            (other dependencies))

;; Library implementation
  
)
```

### File Naming

- Library `(text json)` → file `sitelib/text/json.scm`
- SRFI `(srfi :64 testing)` → file `sitelib/srfi/%3a64/testing.scm`
  - Note: `:` is encoded as `%3a` in filenames

## Testing

### Test File Location

Test files mirror library structure under `test/tests/`:
- `sitelib/json.scm` → `test/tests/json.scm`
- `sitelib/text/json.scm` → `test/tests/text/json.scm`

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

## SRFI Implementation

### R7RS-style SRFI Libraries

SRFI libraries should support both R6RS and R7RS naming:

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

This generates R7RS-style wrappers in `lib/scheme/`.

## Documentation

Add documentation in `doc/utils/` following the project's scribble-like format:

```markdown
[§2] (library name) - Brief description
---------------------------------------

Brief overview of the library purpose.

###### [!Library] `(library name)`

Library description.

###### [!Function] `(procedure arg1 arg2)` 

Describe function behavior and return value.

- _arg1_: Parameter description
- _arg2_: Parameter description
```

## Checklist

Before submitting:

- [ ] Library follows naming conventions
- [ ] Copyright header present
- [ ] Export list is complete and sorted
- [ ] Test file created with adequate coverage
- [ ] Documentation added (if user-facing)
- [ ] `ctest --output-on-failure` passes
- [ ] Dependencies respect directory constraints
