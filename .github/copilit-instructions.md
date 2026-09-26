# Copilot Instructions

Use these rules for all suggestions in this repository.
Apply shared project context from `AGENTS.md`, then follow the constraints below.

## Language and Runtime

- C code must stay C11-compatible.
- Scheme behavior must remain compatible with R6RS and R7RS expectations used in this repo.
- Prefer existing local patterns over introducing a new style.

## Hard Constraints

- Keep changes minimal and task-focused. Do not refactor unrelated code.
- Do not change public behavior unless explicitly requested.
- Every bug fix must include a regression test when practical.
- Do not hand-edit generated files unless the task explicitly requires it.
- Keep platform-specific C changes in `src/os/<platform>/`.
- For `lib/` Scheme libraries, do not add new dependencies on `sitelib/` or `ext/` unless requested.

## C Code Rules

- Validate pointers and critical inputs before use.
- Avoid undefined behavior: out-of-bounds access, invalid aliasing, signed overflow assumptions, and use-after-free patterns.
- Keep error paths explicit and consistent with surrounding code.
- Preserve existing threading and global-state safety assumptions.
- Keep header changes minimal: avoid include cycles and unnecessary includes.

## Scheme Code Rules

- Follow nearby naming and library structure conventions.
- Keep exports minimal and APIs explicit.
- Prefer clear, composable procedures over deeply nested forms.
- Preserve existing error semantics and data shapes unless the task says otherwise.

## Stub and Generation Workflow

- If `.stub` files or generator inputs are modified, run `./dist.sh gen` before build/test.
- Treat generated artifacts as derived output.

## Testing Requirements

- For C changes, rebuild and run relevant tests.
- For Scheme changes, run focused tests in `test/tests/` first, then broader suites when feasible.
- Use `ctest --output-on-failure` for suite runs.
- Final summaries should state what was tested and what passed/failed.

## Response Quality

- Explain what changed and why.
- Call out behavior changes and risks explicitly.
- Reference exact files touched.

## Avoid

- Unrequested dependency upgrades.
- Silent behavioral changes without tests.
- Editing unrelated files for style-only reasons.
- Non-portable script assumptions when existing scripts are portable.
