# VM Performance Optimization - Implementation Log

## Date: 2025-05-18

## Status: Phase 3 Completed (Partial)

## Summary

Implemented Phase 1-3 of the VM performance plan with focus on profiling
infrastructure and one key optimization.

## Completed Tasks

### Phase 1: Profiling Infrastructure ✅

1. **Added PROF_INSN CMake option**
   - Modified `CMakeLists.txt` to add profiling build option
   - Modified `cmake/config-cmake.h.in` to define PROF_INSN macro

2. **Created profiling script**
   - `bench/profile-benchmarks.sh` - script to run benchmarks with profiling
   - `bench/profile-results/` - directory for profiling output

3. **Verified profiling works**
   - Built with `-DPROF_INSN=ON`
   - Confirmed instruction counts are output at exit

### Phase 2: Profile Analysis ✅

1. **Profiled fib(15) and tak(12,6,0)**

2. **Key findings** (documented in `bench/profile-results/analysis_20250518.md`):
   - Local refs (LREF, LREF_PUSH) dominate: 35-50% of hot path
   - Call overhead (FRAME, CALL, RET): 25-30% of hot path
   - Compiler already has extensive instruction combining

3. **Identified optimization opportunity**:
   - `skip_prompt_frame()` called twice in every RET instruction

### Phase 3: Instruction Optimization ✅ (Partial)

1. **Implemented RET optimization**
   - Created `POP_CONT_DIRECT` macro that takes pre-computed cont
   - Modified `RET_INSN` macro to compute cont once and pass it
   - Eliminates redundant `skip_prompt_frame()` call per return

2. **Files modified**:
   - `src/vm.c` - Added POP_CONT_DIRECT, updated RET_INSN

### Phase 5: Validation ✅

1. **All 237 tests passed**

2. **Benchmark results** (3-run average):

   | Benchmark | Before (ms) | After (ms) | Improvement |
   |-----------|-------------|------------|-------------|
   | fib       | ~1031       | ~1004      | ~2.6%       |
   | tak       | ~442        | ~434       | ~1.8%       |
   | sum       | ~251        | ~245       | ~2.4%       |
   | boyer     | ~426        | ~426       | ~0%         |
   | browse    | ~1266       | ~1258      | ~0.6%       |

## Not Implemented

### Phase 3: Additional Fusions (Deferred)
- LREF_ADD fusion - profiling showed arithmetic already has fast paths
- FRAME+CALL fusion - complex due to different argument patterns

### Phase 4: Call Optimization (Deferred)
- Arity-specialized calls (CALL0, CALL1, CALL2)
- These would require compiler changes

## Technical Details

### RET Optimization

Before:
```c
#define RET_INSN()
  do {
    CONT(vm) = skip_prompt_frame(vm);  // First call
    if (CONT(vm) == NULL || BOUNDARY_FRAME_MARK_P(CONT(vm))) {
      return AC(vm);
    }
    POP_CONT();  // Calls skip_prompt_frame again!
  } while (0)
```

After:
```c
#define RET_INSN()
  do {
    SgContFrame *cont__ = skip_prompt_frame(vm);  // Single call
    CONT(vm) = cont__;
    if (cont__ == NULL || BOUNDARY_FRAME_MARK_P(cont__)) {
      return AC(vm);
    }
    POP_CONT_DIRECT(cont__);  // Uses pre-computed cont
  } while (0)
```

## Recommendations for Future Work

1. **Inline caching for GREF_CALL**
   - Cache resolved gloc at call site
   - Skip Sg_FindBinding for repeated calls

2. **Arity-specialized call paths**
   - Most calls are 0-3 arguments
   - Specialize ADJUST_ARGUMENT_FRAME

3. **Continuation stack pooling**
   - Pre-allocate continuation frames
   - Reduce allocation overhead
