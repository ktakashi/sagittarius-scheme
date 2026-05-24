# Sagittarius Scheme Performance Optimization Summary

## Baseline Results
All benchmarks measured on macOS arm64 in Release mode (-O3).

| Benchmark | Avg Time (ms) |
|-----------|---------------|
| fib       | 1091          |
| fibfp     | 1272          |
| browse    | 1075          |
| ctak      | 1015          |
| deriv     | 689           |
| compiler  | 665           |

## Profiling Findings

### CPU Time Distribution (fib benchmark)
1. **GC operations** (~16%) - Boehm GC allocation and marking
2. **VM dispatch loop** (~70%) - run_loop and instruction execution
3. **Library loading/caching** (~14%) - Sg_ReadCache and loading

### Key Hotspots
- `GC_malloc_kind` - allocator invocation
- `run_loop` - VM execution (computed goto already implemented)
- `Sg_VMMakeClosure` - closure creation  
- Arithmetic instructions (ADD, SUB, NUM_LT, etc.)

## Current Optimizations in Place

### Already Optimized
1. **Direct-threaded dispatch** - Computed goto for GCC/Clang
2. **Fixnum fast paths** - Arithmetic operations check for fixnums first
3. **Immediate flonums** - USE_IMMEDIATE_FLONUM stores small flonums in pointers
4. **Fused instructions** - Combined ops like LREF_PUSH, GREF_CALL

### Attempted Optimizations

#### Link-Time Optimization (LTO)
- Added `-DSAGITTARIUS_ENABLE_LTO=ON` option
- **Result**: Causes SIGSEGV crash in `get_possible_paths`
- **Status**: Available but not recommended - potential undefined behavior in codebase

## Recommendations for Further Optimization

### Low-Risk Optimizations
1. **Profile-Guided Optimization (PGO)**
   - Build with profiling, collect data, rebuild with optimization
   ```bash
   cmake -DCMAKE_C_FLAGS="-fprofile-generate" .
   make
   # Run benchmarks
   cmake -DCMAKE_C_FLAGS="-fprofile-use" .
   make
   ```

2. **Increase GC efficiency**
   - Tune GC parameters via environment variables:
     - `GC_INITIAL_HEAP_SIZE`
     - `GC_MAXIMUM_HEAP_SIZE`

3. **Parallel GC**
   - Boehm GC supports parallel marking
   - Set `GC_MARKERS` environment variable

### Medium-Risk Optimizations
1. **Pair freelist** - Custom allocator for pairs
   - Potentially significant speedup
   - Risk of memory management bugs

2. **Closure template caching**
   - Cache common closure patterns
   - Only applies to specific code patterns

### Long-Term Improvements
1. **JIT compilation** for hot code paths
2. **Escape analysis** for stack allocation
3. **Inline caching** for method dispatch

## Files Modified

### Build System
- `CMakeLists.txt` - Added `SAGITTARIUS_ENABLE_LTO` option
- `cmake/FixupCompilerFlags.cmake` - LTO flag handling

### Documentation  
- `bench/doc/profiling-report.md` - Detailed profiling analysis
- `bench/run-benchmarks.sh` - Automated benchmark runner
- `bench/benchmark-runner.scm` - Scheme benchmark driver

## Running Benchmarks

```bash
# Quick test (5 benchmarks, 1 run each)
./bench/run-benchmarks.sh --quick

# Full benchmark suite (30 benchmarks, 3 runs each)
./bench/run-benchmarks.sh --runs 3

# Compare against baseline
./bench/run-benchmarks.sh --compare bench/baseline/baseline.csv

# Save new baseline
./bench/run-benchmarks.sh --baseline
```

## Conclusions

The Sagittarius VM is already well-optimized for an interpreter:
- Uses computed goto for dispatch (best practice)
- Has fixnum fast paths for arithmetic
- Uses immediate representation for small values

Major speedups would require:
1. JIT compilation (significant engineering effort)
2. Reducing GC pressure through careful allocation management
3. Profile-guided optimization (safe, moderate impact)

The current performance is reasonable for a portable C implementation.
