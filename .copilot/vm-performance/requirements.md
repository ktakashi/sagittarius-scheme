# VM Performance Improvement

## User Story

As a Sagittarius user, I want the VM execution to be faster so that
my Scheme programs run with better performance.

## Pre-requisites / Dependencies

- Working build environment
- Benchmark suite (`bench/run-benchmarks.sh`)
- Baseline measurements (`bench/baseline/baseline.csv`)
- Profiling tools (optional: `perf`, `Instruments`)

## Analysis Summary

### Current Architecture

The Sagittarius VM is a stack-based bytecode interpreter with:
- Computed goto dispatch (GCC extension) - efficient
- ~100 instruction types in `vminsn.c`
- Call sequences via `vmcall.c`
- Boehm GC for memory management

### Identified Optimization Areas

#### 1. GREF (Global Reference) Optimization
**Current**: Every global reference:
- Checks if already a gloc (`SG_GLOCP(id)`)
- If not, performs `Sg_FindBinding` lookup
- Caches gloc in instruction stream after first lookup

**Analysis**: Sandbox check (`!SG_FALSEP(s1354)`) is cheap when false
(branch prediction favorable). The real overhead is:
- First-access binding lookup via `Sg_FindBinding`
- Instruction patching on first access

**Opportunity**: 
- Profile to confirm binding lookup is the bottleneck
- Pre-resolve more references at compile/load time
- Consider compile-time constant propagation

#### 2. Numeric Operations (Already Optimized)
**Current**: ADD, SUB, MUL already have fixnum/flonum fast paths in
`instructions.scm`. The `$result:n` macro handles overflow to bignum.

```scheme
(cond ((and (SG_INTP (AC vm)) (SG_INTP obj))
       ($result:n (+ (SG_INT_VALUE obj) (SG_INT_VALUE (AC vm)))))
      ...)
```

**Status**: Already optimized - not a target for improvement.

#### 3. Function Call Optimization
**Current**: CALL instruction has significant overhead
- Argument frame adjustment
- Closure stack check
- Value count reset
**Opportunity**: 
- Fused instructions (LREF_CALL)
- Known-arity call paths
- LOCAL_CALL already optimized, extend pattern

#### 4. Box/Unbox Overhead
**Current**: Mutable variables use boxing for all accesses
**Opportunity**: 
- Escape analysis to eliminate unnecessary boxes
- Direct slot access when safe

#### 5. Instruction Fusion
**Current**: Some fused instructions exist (LREF_PUSH, GREF_CAR_PUSH)
**Opportunity**: Add more common patterns
- LREF_ADD, LREF_SUB
- CONST_ADD, CONSTI_MUL
- Pattern analysis of hot paths

#### 6. Memory/GC Optimization
**Opportunity**:
- Arena allocation for short-lived objects
- Inline small object allocation
- Reduce allocation in hot loops

### Benchmark Baseline

Current performance (quick run):
| Benchmark | Time (ms) |
|-----------|-----------|
| fib       | 1030      |
| tak       | 441       |
| sum       | 251       |
| boyer     | 426       |
| browse    | 1266      |

Target: 10-20% improvement on key benchmarks

## Detailed Tasks

### Phase 1: Analysis & Measurement (Low Risk)

1. [ ] Set up profiling infrastructure
   - Add instruction counting option
   - Identify hot instructions per benchmark
2. [ ] Analyze benchmark profiles
   - Which instructions dominate execution time?
   - What are the hot call patterns?
3. [ ] Document findings and prioritize optimizations

### Phase 2: Low-Hanging Fruit (Medium Risk)

4. [ ] Add more fused instructions
   - Analyze instruction sequences
   - Implement common patterns
5. [ ] Profile GREF to understand actual overhead
   - Is binding lookup the bottleneck?
   - What % of GREFs hit the gloc cache?

### Phase 3: Structural Improvements (Higher Risk)

6. [ ] Optimize argument handling
   - Fast path for common arities
   - Reduce frame adjustment overhead
7. [ ] Box elimination via escape analysis
   - Identify non-escaping mutable variables
   - Inline slot access when safe

### Phase 4: Validation

8. [ ] Run full benchmark suite
9. [ ] Compare against baseline
10. [ ] Document performance gains
11. [ ] Update baseline if significant improvement

## Clarifications

- Focus on interpreter optimizations (no JIT)
- Maintain full R6RS/R7RS compatibility
- Preserve sandbox security model
- Changes should be incremental and testable
- Performance gains must not regress correctness (full test suite must pass)

## Risk Assessment

| Optimization | Risk | Potential Gain |
|--------------|------|----------------|
| Instruction fusion | Low | 5-10% |
| GREF analysis | Low | TBD (needs profiling) |
| Call optimization | High | 10-20% |
| Box elimination | High | 5-15% |

## Next Steps

After user approval:
1. Update `.copilot/README.md` to register this feature
2. Create detailed implementation plan
