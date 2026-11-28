---
name: develop-scheme
description: Implement or modify Sagittarius Scheme libraries, macros, SRFIs, or utility APIs with focused tests and reference documentation.
argument-hint: "Describe the feature or enhancement..."
---
# Develop a Sagittarius Scheme library

Use `$ARGUMENTS` when provided. Read `AGENTS.md` and
[scheme.instructions.md](../../instructions/scheme.instructions.md) before
editing; this skill supplements, not replaces, those rules.

## Bounded workflow

1. Identify the library, a representative caller, existing tests, and the
   relevant documentation. Use
   [inspect-scheme-library](../inspect-scheme-library/SKILL.md) to get its
   exports/imports before reading implementation details.
2. Clarify only material API or behavior ambiguity. For a small fix, do not
   create a user story, implementation plan, or multi-agent workflow.
3. Reuse existing conditions, records/classes, generic methods, bytevector
   helpers, resource management, and concurrency primitives.
4. Make the smallest change preserving exported names and intended behavior.
   For resources, verify ownership, closure, cancellation, and error propagation.
5. Add a few SRFI-64 tests that fail without the change, in the existing test
   file where possible. Include relevant boundary/failure cases, not trivial
   accessor or language-behavior tests.
6. Run the focused files/selectors with [run-test](../run-test/SKILL.md).
   Reconfigure/rebuild for new CTest files and verify their discovery.
7. Update the directly related reference and execute nontrivial examples with
   [document-sagittarius](../document-sagittarius/SKILL.md).
8. Summarize the API change, exact validation, compatibility limits, and
   request the user's review through the available structured question tool.

## Placement and dependencies

- `(text json)` maps to `sitelib/text/json.scm`; test paths mirror the library
  under `test/tests/`. Native extension tests live in `ext/<name>/test.scm`.
- `lib/` is builtin-only: do not introduce `sitelib/` or extension dependencies.
- `ext/*/sagittarius/` normally depends only on `lib/`; verify established
  exceptions instead of inventing one.
- Write R6RS SRFI sources under `%3aNN/`. Generate the R7RS wrappers with
  `generate srfi`; do not maintain both manually.
- For `.stub`/C changes use [native-bindings](../native-bindings/SKILL.md).
  For bootstrap compiler changes use
  [compiler-development](../compiler-development/SKILL.md).


