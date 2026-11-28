---
name: native-bindings
description: Add or fix Sagittarius C11 runtime code, extension bindings, .stub declarations, or OS-specific implementations. Use for Scheme-C bindings and native portability regressions.
---

# Sagittarius native code and bindings

Read [c-coding.instructions.md](../../instructions/c-coding.instructions.md)
before C/header changes and the Scheme instructions before test/library edits.

1. Trace the exported Scheme API to its `.stub`, C implementation, and callers.
   Inspect an existing neighboring binding before adding one.
2. Edit the authoritative `.stub` or C source, never its generated C output.
   Keep stub signatures, header declarations, and implementations consistent.
3. Preserve GC rooting, object/type checks, error/condition propagation,
   locking, and resource ownership using existing Sagittarius mechanisms.
4. For OS-dependent code, inspect the corresponding `src/os/` implementations
   and conditional build paths. Do not claim BSD/Windows validation from a
   macOS run or replace an unavailable primitive with a silent default.
5. Regenerate only the needed phase during iteration, then rebuild:

   ```sh
   bash .github/skills/scripts/sagittarius.sh generate stub
   bash .github/skills/scripts/sagittarius.sh build
   ```

   Omit generation for ordinary C-only edits. Complete full regeneration when
   required by `AGENTS.md`; see [build-sagittarius](../build-sagittarius/SKILL.md).
6. Add a Scheme-level regression to the affected extension's `test.scm` or
   existing runtime test file. List the actual CTest selector and execute it:

   ```sh
   bash .github/skills/scripts/sagittarius.sh list-tests 'socket|threads'
   bash .github/skills/scripts/sagittarius.sh test '^threads$'
   ```

7. For concurrency/timeout/lifetime changes, repeat the focused selector with
   `--repeat N` and stop on the first failure. Use the same backend/build
   configuration that exposed the issue.
8. Update directly affected reference docs. Report generation/build commands,
   assertions exercised, backend/platform coverage, and unverified variants.

Use [run-test](../run-test/SKILL.md) for failure triage. Do not loosen a
regression, disable a backend, or retry a native crash to obtain a green result.
