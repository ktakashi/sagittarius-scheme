---
name: run-test
description: Run, select, repeat, and triage Sagittarius CTest or SRFI-64 tests. Use for test execution, targeted regressions, and concurrency stress checks.
---

# Running Sagittarius tests

Use the [workflow helper](../scripts/sagittarius.sh) from the workspace root.
It selects the configured CMake tree, sets local load/dynamic paths, isolates
temporary files and caches, and preserves failures. Do not reconstruct these
commands or launch another agent just to execute them.

## Select the smallest useful test set

```sh
bash .github/skills/scripts/sagittarius.sh list-tests 'continuations|threads'
bash .github/skills/scripts/sagittarius.sh test '^sagittarius/continuations\.scm$'
bash .github/skills/scripts/sagittarius.sh test-file test/tests/sagittarius/continuations.scm
bash .github/skills/scripts/sagittarius.sh --repeat 10 test '^threads$'
```

- Combine related CTest selectors into one regex. Zero matches is an error.
- Multiple `test-file` arguments run independently: an empty test file cannot
  hide behind another file's passing summary.
- Use `--build-dir DIR` **before** the command for a different configured tree.
  Do not configure `build/` blindly: it can contain an in-source build's binary.
- If the interpreter is missing, follow [build-sagittarius](../build-sagittarius/SKILL.md).
- New `test/tests/*.scm` files are discovered by the root CMake
  `GLOB_RECURSE ... CONFIGURE_DEPENDS`; rebuild/reconfigure and check `list-tests`.
  Extension tests belong in `ext/<name>/test.scm`.

## Full suite and time limits

```sh
bash .github/skills/scripts/sagittarius.sh --jobs 4 test-all
bash .github/skills/scripts/sagittarius.sh --timeout 300 test-file ext/crypto/test.scm
```

`test-all` is explicit, not the default. It can take ten minutes. Use the full
suite when required by the change or repository instructions, not for every
intermediate edit. Keep parallelism at or below available CPU cores.

Default operation budgets are 900s for CTest and 120s per direct test file.
`--timeout` changes that budget; CTest's registered per-test limits remain in
effect. Repeated runs stop on the first failure.

## Failure triage

1. Read the failed command's output, not the entire suite log.
2. A `FAIL`/`XPASS` test record, a nonzero exit, or a missing SRFI-64 summary
   is not a pass.
3. Re-run only the failing selector after investigating it. A timeout normally
   exits 124 (137 on a GNU timeout forced kill); do not retry repeatedly without
   a new hypothesis.
4. On a native crash, retain/report the stack trace; do not blindly retry.
5. The helper already uses a fresh workspace cache. Never clear the user's
   global Sagittarius cache to diagnose one test.

Report the exact selector/files, pass counts, failed assertions, and any
platforms not exercised. Do not infer a full-suite pass from a targeted run.

