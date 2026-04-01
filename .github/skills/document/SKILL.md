---
name: document
description: Guide for writing Sagittarius documents. Use this when asked to write a user reference manual or other Sagittarius documents.
---

Structure of the doc directory
==============================

doc/
 + CMakeLists.txt - the build file
 + gendoc - script to generate document
 + lib/   - static library for after conversion, such as Javascript or CSS
 + sagittarius-ref.md - The main document
 + clos.md - CLOS library documents
 + srfi.md - table of supporting SRFI
 + r6rs.md - top document for R6RS libraries
 + rnrs/ - R6RS standard library document
 + r7rs.md - R7RS support document
 + sagittarius.md - top document of Sagittarius extensions. This file includes documents located in `sagittarius/` directory.
 + sagittarius/ - Sagittarius extensions document
 + utils.md - surface of utils/ directory. To expose, add inclusion in this file.
 + utils/ - Utility library document, mainly located in `sitelib/` directory
 + ported/ - Ported library document.

Structure of the document
=========================

Sagittarius document can be written GFM with extra extensions.

Section extension
-----------------
Section starts with `[§{n}]` together with Markdown header notation
where `{n}` specifies the section level.

Header 1 section example:
```markdown
[§1] Header 1 section
=====================
```
The header 1 section must be used for the top level section such as 
`Sagittarius extensions` or `R6RS Libraries`.

Header 2 section example:
```markdown
[§2] Header 2 section
---------------------
```
The header 2 section must be used for library level description.
Such as `(sagittarius)`.

Header 3 section example:
```markdown
### [§3] Header 3 section
```
The header 3 section must be used to split the library document
logically. Such as `High level API` or `Low level API`.

Header 4 section example:
```markdown
#### [§4] Header 4 section
```
The header 4 section must be used to split the header 3 section into
the finer section.

Inclusion extension
-------------------

Document can include document or example code.

Document inclusion
```markdown
* @[[path/to/markdown.md](path/to/markdown.md)]
```

The document inclusion must use the path as its title.

Example code inclusion
```markdown
* @[-[Title of the example](path/to/the/example/code.scm)]
```

The code inclusion must specify the title and path.

Build the document
==================

Building document uses `doc` or `online-doc` target.

For POSIX like environment
```shell
make doc
make online-doc
```

For Windows
```bat
ninja doc
ninja online-doc
```

The result document will be `doc` directory of the building directory.
One file document is `sagittarius-ref.html`, multiple file document, aka
online document, is `sagittarius-online-ref.html` and its sections are
located in `sections/` directory.

Checklist
---------
Before submitting:

- [ ] Review the document for clarity and accuracy
- [ ] Check the document can be built by the commands above
- [ ] Check the generated HTML files are correctly sectioned
