---
name: build-sagittarius
description: Locate, configure, incrementally build, or regenerate Sagittarius using CMake and dist.sh. Use for missing interpreters, native rebuilds, and bootstrap or stub generation.
---

# Build and regenerate Sagittarius

Use the [shared helper](../scripts/sagittarius.sh); do not infer the CMake
directory from the executable's `build/` location.

```sh
bash .github/skills/scripts/sagittarius.sh status
bash .github/skills/scripts/sagittarius.sh build sash
```

The helper prefers a configured root, then configured `build/`, otherwise
`out/build`. Override with `--build-dir DIR` before the command. It validates
the cache's source tree. An out-of-tree build normally places the executable
at `<configured-dir>/build/sagittarius`.

## Configure only when necessary

```sh
bash .github/skills/scripts/sagittarius.sh configure
bash .github/skills/scripts/sagittarius.sh --build-dir out/debug configure -DBUILD_TYPE=Debug
bash .github/skills/scripts/sagittarius.sh --build-dir out/debug build
```

Existing cache options are preserved. `configure` accepts `-DNAME=VALUE`
arguments, not arbitrary source/build-directory overrides. Keep jobs within
available CPU cores. Dependencies are CMake, a C11 compiler, a host Sagittarius
for generation, Boehm GC, zlib, libffi, and OpenSSL. Do not install anything
unless a chosen command reports a missing dependency.

## Select generation deliberately

| Changed source | Iteration command |
| --- | --- |
| `.stub` bindings | `generate stub` |
| `boot/` compiler/precompiled libraries | `generate precomp` |
| R6RS SRFI source needing R7RS wrappers | `generate srfi` |
| Instruction definitions | `generate insn` |
| Unicode, HTML entities, timezones | Corresponding `unicode`, `html`, `tz` phase |
| Complete regeneration required/requested | `generate gen` |

```sh
bash .github/skills/scripts/sagittarius.sh generate stub
bash .github/skills/scripts/sagittarius.sh build
```

`SASH` may name a host command on PATH or an executable inside the workspace.
The default prefers the installed `sagittarius`, otherwise the selected
workspace binary. Generation runs `dist.sh` with shell error checking so a
failed early generator cannot be hidden by a later successful command.

Never hand-edit generated C or wrappers. Finish the full `generate gen`
required by `AGENTS.md` after stub/precompiled changes, then rebuild and run
the relevant [tests](../run-test/SKILL.md). Do not generate unrelated data for
a Scheme-only utility change. Do not install or clean the build automatically.

The helper uses 300s operation timeouts and private workspace temporary/cache
directories. Increase `--timeout` explicitly for a justified long build.
For native Windows use `dist.bat` and CMake/Ninja in the configured directory.
