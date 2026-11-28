---
name: compiler-development
description: Develop or optimize the Sagittarius Scheme compiler, inspect intermediate forms and VM instructions, and regenerate its bootstrap C sources.
---
# Sagittarius compiler development

Read the Scheme instructions before editing. Start with
[boot/compiler.scm](../../../boot/compiler.scm), then the affected pass, its
IR constructors, and focused tests. Do not read every pass by default.

## Source map

| Source | Responsibility |
| --- | --- |
| `boot/compiler.scm` | Entry points, pass sequencing, compiler-state restoration |
| `boot/lib/pass1*.scm` | Expansion/translation to intermediate forms |
| `boot/lib/pass2.scm` | IR optimization |
| `boot/lib/pass3.scm` | Further IR rewriting/inlining |
| `boot/lib/pass4.scm` | Free-variable analysis and lambda lifting |
| `boot/lib/pass5.scm` | VM instruction emission |
| `boot/lib/iform.scm` | IR nodes and accessors |
| `boot/lib/compiler-util.scm`, `smatch.scm` | Shared helpers and matching |

Normal compilation emits **VM instructions**, not C. `dist.sh precomp`
separately translates bootstrap libraries/compiler sources into generated C.
Never patch that generated C as the source of an optimization.

## Inspect a pass without a scratch script

```sh
bash .github/skills/scripts/sagittarius.sh compiler-pass 2 '((lambda (x) (+ x 1)) 5)'
bash .github/skills/scripts/sagittarius.sh compiler-pass 5 '((lambda (x) (+ x 1)) 5)'
```

The [Scheme helper](../scripts/scheme-tools.scm) calls the existing
`compile-p1` through `compile-p5` diagnostics. Passes 1..4 print IR; pass 5
dumps VM instructions. It accepts exactly one expression and exits, rather
than entering the REPL.

## Change, rebuild, validate

1. Capture the smallest expression exposing the problem and its current pass
   output. Preserve evaluation order, binding identity, tail position,
   multiple values, and exception behavior.
2. Change the source pass; reuse its IR helpers and mutation conventions.
3. During iteration, regenerate and rebuild:

   ```sh
   bash .github/skills/scripts/sagittarius.sh generate precomp
   bash .github/skills/scripts/sagittarius.sh build sash
   ```

4. Run the pass diagnostic again and the focused semantic regression using
   [run-test](../run-test/SKILL.md). IR appearance alone is not correctness.
5. Complete the generation/build checks required by `AGENTS.md`, including
   full `generate gen` after precompiled-source changes.
6. For an optimization claim, use
   [benchmark-sagittarius](../benchmark-sagittarius/SKILL.md) with comparable
   before/after samples. Never infer speedup from fewer instructions alone.

For native Windows use `dist.bat precomp` and
`cmake --build <configured-dir> --target sash`; the shell helpers target POSIX.
