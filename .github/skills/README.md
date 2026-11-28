# Sagittarius agent skills

These skills keep recurring work small: load the relevant workflow, execute a
tested script, and reason about its result instead of reconstructing build
paths, load paths, timeouts, test summaries, or benchmark arithmetic.

## Choose one starting skill

| Work | Skill |
| --- | --- |
| Test selection, execution, repetition, failure triage | [run-test](run-test/SKILL.md) |
| Configure, locate binaries, incrementally build, regenerate | [build-sagittarius](build-sagittarius/SKILL.md) |
| Implement/fix Scheme libraries or SRFIs | [develop-scheme](develop-scheme/SKILL.md) |
| Inspect library names, imports, exports | [inspect-scheme-library](inspect-scheme-library/SKILL.md) |
| C/runtime/extension/stub or OS-specific changes | [native-bindings](native-bindings/SKILL.md) |
| Compiler changes and pass diagnostics | [compiler-development](compiler-development/SKILL.md) |
| Performance sampling and regression comparison | [benchmark-sagittarius](benchmark-sagittarius/SKILL.md) |
| Reference/internal docs and executable examples | [document-sagittarius](document-sagittarius/SKILL.md) |
| Explicitly requested feature requirements | [create-user-story](create-user-story/SKILL.md) |
| Explicitly requested plan for an approved story | [plan-user-story](plan-user-story/SKILL.md) |

Do not load every skill, create a plan for a routine fix, or delegate a single
script invocation. The story/plan skills remain explicit-only; development
and routine skills are discoverable from their focused descriptions.

## Shared command contract

Run from the workspace root:

```sh
bash .github/skills/scripts/sagittarius.sh --help
bash .github/skills/scripts/sagittarius.sh status
bash .github/skills/scripts/sagittarius.sh test-file test/tests/sagittarius/continuations.scm
```

[sagittarius.sh](scripts/sagittarius.sh) uses Bash (including macOS Bash 3.2),
CMake/CTest, and the existing Sagittarius interpreter/generators. It adds no
package dependencies. [scheme-tools.scm](scripts/scheme-tools.scm) reads
libraries and calls existing compiler diagnostics;
[compare-benchmarks.awk](scripts/compare-benchmarks.awk) validates and compares
the existing benchmark CSV format.

- Options precede the command: `--build-dir`, `--jobs`, `--timeout`, `--repeat`.
  Integer controls accept at most nine digits to avoid shell arithmetic overflow.
- CMake detection prefers a root cache, then `build/`, otherwise `out/build`.
  If multiple configurations exist, explicitly choose the one being validated.
- Builds require a configured cache and never install or clean automatically.
- Success output is compact; errors include the failing command and its output.
  Original command failures retain their status. Helper validation/input
  failures exit 2; interpreter errors keep the interpreter's status. Timeouts
  normally exit 124 (137 if GNU timeout must force a kill).
- Timeouts prefer `gtimeout`, then `timeout`, then a Bash watchdog that stops
  its own process groups. No name-based process killing is used.
- Temporary files and Sagittarius caches live in unique directories under
  ignored `.tmp/` and are cleaned on exit. The user's global cache is untouched.
- Workspace paths are checked, including path components. Symlinks and `..`
  traversal are rejected, even when they would point within the workspace.
  Commands run from the workspace root, not an unrelated caller directory.
- Benchmark outputs are explicit, persistent workspace files; no baseline is
  silently overwritten. Rendering/checking docs leaves no generated HTML.
- POSIX helpers do not replace native Windows `dist.bat`/CMake workflows.

## Model and credit use

Skills do **not** select or switch models. Select a lower-cost model through
the client's model picker or its configured subagent preferences when desired.
These scripts make routine execution practical on that model, without an
extra agent or large successful build/test logs.

Reserve deeper reasoning for API design, compiler transformations, native
resource/concurrency bugs, or failures the scripts expose. This is a workflow
improvement, not a measured claim about a particular credit reduction.

## Validate changes to the skill tools

```sh
bash -n .github/skills/scripts/sagittarius.sh .github/skills/scripts/test-workflows.sh
bash .github/skills/scripts/test-workflows.sh
bash .github/skills/scripts/test-workflows.sh --real
```

The focused shell checks use an isolated workspace fixture and fake tools:
they cover build-layout selection, quoted paths, nonzero/zero-exit failures,
incomplete summaries, empty selections, generation errors, process cleanup,
benchmark publication, CSV validation, and regression threshold boundaries.
They do not build Sagittarius. `--real` additionally requires a built
interpreter and checks reader modifiers, unexpanded R7RS declarations,
non-evaluation of library bodies, compiler-pass routing, and invalid inputs.
On macOS, repeat with `/bin/bash` to verify Bash 3.2 compatibility. For workflow
changes, also smoke-test the affected real command.
