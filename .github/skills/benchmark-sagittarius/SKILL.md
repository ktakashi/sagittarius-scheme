---
name: benchmark-sagittarius
description: Measure Sagittarius performance with existing Gambit benchmarks and compare validated before/after CSV samples, optionally enforcing a regression threshold.
---

# Measure and compare Sagittarius performance

Run correctness tests first. Timing is not proof of semantic correctness.
Use the existing [benchmark runner](../../../bench/benchmark-runner.scm)
through the [workflow helper](../scripts/sagittarius.sh), rather than
reimplementing its benchmark configuration.

## Capture comparable samples

```sh
bash .github/skills/scripts/sagittarius.sh benchmark bench/results/before.csv fib tak
# Apply and rebuild the intended change.
bash .github/skills/scripts/sagittarius.sh benchmark bench/results/after.csv fib tak
bash .github/skills/scripts/sagittarius.sh compare bench/results/before.csv bench/results/after.csv
```

- Defaults are one warmup and three measured runs per benchmark, sequentially.
  `benchmark --runs N --warmup N OUTPUT NAME...` overrides them.
- The runner invokes the benchmark itself. The helper uses `-n` to prevent a
  second automatic invocation of the loaded benchmark's entry point.
- Choose a small relevant benchmark set. The existing runner's no-argument
  mode lists available names; use the helper's `run bench/benchmark-runner.scm`.
- Use identical machine/backend/build flags, benchmark names, warmups, and
  run counts for before/after. Rebuild the selected CMake tree each time.
- The runner measures wall time including benchmark loading. State that
  measurement scope; do not describe it as isolated procedure CPU time.
- Output is `benchmark,time_ms,run` CSV. Existing output is never overwritten.
  Every run, including warmups, must exit successfully and emit exactly one
  positive timing record; failed runs never publish a partial CSV.

## Enforce only a requested threshold

```sh
bash .github/skills/scripts/sagittarius.sh compare bench/results/before.csv bench/results/after.csv 5
```

The optional percentage is the maximum permitted mean-time regression.
Equality passes, allowing only floating-point roundoff; a larger regression
exits 1. Malformed data, duplicate or
missing run IDs, empty data, or unequal benchmark sets exit 2.

The result includes mean before/after milliseconds, `before/after` speedup,
signed percentage change, and both sample counts. Positive change is slower.
No threshold is enforced unless supplied. Report sample counts and noise;
do not present a small noisy change as a proven speedup.
