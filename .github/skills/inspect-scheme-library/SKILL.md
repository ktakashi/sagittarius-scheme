---
name: inspect-scheme-library
description: Read a Sagittarius R6RS or R7RS library's name, exports, imports, and unexpanded declarations without evaluating its body. Use for API inventories, dependency checks, or focused library exploration.
---

# Inspect a Scheme library

```sh
bash .github/skills/scripts/sagittarius.sh inspect boot/compiler.scm
```

The [reader-based helper](../scripts/scheme-tools.scm) prints the library form,
name, and each export/import, preserving rename and import modifiers. It does
not evaluate the library body. Reader directives can still load their reader
libraries. Malformed files or files without a library form fail explicitly.

## Use the output to narrow the next read

- Locate only the relevant exported definition and representative callers.
  Do not dump the whole source/dependency tree for an interface question.
- Library names map to directory paths; SRFI `:` components use `%3a`.
- Check the matching test path under `test/tests/` and the relevant reference
  under `doc/`; extension tests instead live in `ext/<name>/test.scm`.
- `lib/` must not acquire `sitelib/` or extension dependencies.
  `ext/*/sagittarius/` normally uses only builtin libraries; check existing
  exceptions before changing that boundary.
- `cond-expand`, includes, and included library declarations are reported as
  **unexpanded**, not treated as a complete API inventory. Read the relevant
  included/conditional declarations before drawing conclusions.

This is an inspection tool, not an import smoke test or a full dependency
resolver. For loading/runtime behavior, run an existing example or focused
test with [run-test](../run-test/SKILL.md).
