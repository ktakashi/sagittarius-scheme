# Implementation Plan: VM Performance Improvement

## Overview

This plan outlines a systematic approach to improving VM execution
performance through profiling, analysis, and targeted optimizations.
The focus is on identifying actual bottlenecks before making changes.

## Detailed Plan

### Phase 1: Profiling Infrastructure

**Objective**: Use existing profiling tools to measure VM performance

**Existing Infrastructure**:
- `PROF_INSN` macro enables `COUNT_INSN(c)` in `vm.c`
- `show_inst_count()` outputs instruction frequencies at exit
- `called_instructions[]` array tracks counts per instruction

**Steps**:
1. Build with profiling enabled
   - Add `-DPROF_INSN` to CMake configuration
   - Verify instruction counting works

2. Create benchmark profiling scripts
   - Script to run benchmarks with profiling enabled
   - Parse stderr output for instruction counts
   - Generate comparison reports

**Files to modify/create**:
- `CMakeLists.txt` - Add profiling option
- `bench/profile-benchmarks.sh` - New script
- `bench/analyze-profile.scm` - Analysis tool (optional)

### Phase 2: Profile Analysis

**Objective**: Identify actual performance bottlenecks

**Steps**:
1. Profile each Gabriel benchmark
   - fib, tak, boyer, browse, etc.
   - Record instruction distribution
   - Identify hot paths

2. Analyze GREF behavior
   - What % hit cached gloc?
   - How often is `Sg_FindBinding` called?
   - Sandbox check overhead measurement

3. Analyze call patterns
   - CALL vs LOCAL_CALL distribution
   - Average argument count
   - Tail call frequency

4. Document findings
   - Create report in `bench/doc/profile-analysis.md`
   - Prioritize optimization targets

**Files to create**:
- `bench/doc/profile-analysis.md`

### Phase 3: Instruction Fusion

**Objective**: Add fused instructions for common patterns

**Steps**:
1. Identify fusion candidates from profiling
   - Common two-instruction sequences
   - LREF followed by arithmetic
   - GREF followed by CALL (already exists)

2. Implement new fused instructions
   - Update `src/instructions.scm`
   - Regenerate `vminsn.c`

3. Update compiler to emit fused instructions
   - Modify pass4 in `boot/compiler.scm`
   - Pattern matching for fusable sequences

**Potential fusions** (to be confirmed by profiling):
- `LREF_ADD` - local ref + add
- `CONST_EQ` - constant comparison

**Files to modify**:
- `src/instructions.scm`
- `boot/compiler.scm` (pass4)
- Run `./dist.sh gen` to regenerate

### Phase 4: Call Optimization

**Objective**: Reduce function call overhead

**Steps**:
1. Analyze `vmcall.c` for optimization opportunities
   - Fast path for known arities (0, 1, 2, 3)
   - Reduce argument frame adjustment overhead

2. Implement arity-specialized paths
   - Add CALL0, CALL1, CALL2 instructions
   - Skip argument count validation for known arities

3. Update compiler for new call instructions
   - Emit specialized calls when arity is known

**Files to modify**:
- `src/vmcall.c`
- `src/instructions.scm`
- `boot/compiler.scm`

### Phase 5: Validation & Documentation

**Objective**: Verify improvements and update documentation

**Steps**:
1. Run full test suite
   - All 237 tests must pass
   - No regressions

2. Run benchmark comparison
   ```bash
   ./bench/run-benchmarks.sh --compare baseline.csv
   ```

3. Document results
   - Performance delta for each benchmark
   - Update baseline if >5% improvement

4. Update user documentation
   - Performance notes in HACKING.md if relevant

## Estimation

| Phase | Effort | Notes |
|-------|--------|-------|
| Phase 1: Infrastructure | 2-3 hours | Enable PROF_INSN, scripts |
| Phase 2: Analysis | 4-6 hours | Run profiles, analyze |
| Phase 3: Fusion | 8-12 hours | Depends on findings |
| Phase 4: Call Opt | 8-12 hours | Higher risk |
| Phase 5: Validation | 2-4 hours | Testing, docs |
| **Total** | **24-37 hours** | Spread across sessions |

## Testing Strategy

### Scheme Tests

All existing tests in `test/tests/` must pass:
```bash
ctest --output-on-failure
```

### Benchmark Regression

```bash
# Compare against baseline
./bench/run-benchmarks.sh --compare bench/baseline/baseline.csv

# Quick validation
./bench/run-benchmarks.sh --quick
```

### Manual Verification

1. Run interactive REPL and test basic operations
2. Verify compiler output for fused instructions
3. Test edge cases: empty calls, varargs, apply

## Code Quality Guidelines

### Maintainability

- Follow existing `instructions.scm` patterns
- Use cise macros consistently
- Keep generated code readable

### Comments

- Document new instruction semantics
- Explain optimization rationale
- Note any compiler changes

### Performance Guidelines

- Always profile before optimizing
- Measure impact of each change
- Document performance tradeoffs

## Decision Points

After Phase 2, review findings to determine:
- Which optimizations are worth pursuing?
- Are there unexpected bottlenecks?
- Should we adjust Phase 3-4 priorities?

## Risks

1. **Profiling overhead** - Counting may affect measurement
2. **Compiler changes** - Risk of introducing bugs
3. **Diminishing returns** - Some optimizations may have minimal impact
4. **Platform variance** - Gains may differ across platforms
