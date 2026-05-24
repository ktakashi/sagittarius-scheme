# Sagittarius Scheme Profiling Report
Date: 2026-03-19

## Baseline Performance Summary

| Benchmark | Time (ms) |
|-----------|-----------|
| fib       | 1091 |
| browse    | 1075 |
| fibfp     | 1272 |
| ctak      | 1015 |
| deriv     | 688 |
| compiler  | 665 |
| divrec    | 577 |
| diviter   | 553 |
| dderiv    | 530 |
| destruc   | 518 |

## CPU Profiling Analysis (fib benchmark)

### Top CPU Consumers

1. **GC_malloc_kind** (Boehm GC allocation) - ~16% of total time
   - High allocation rate is causing frequent GC pauses
   - GC marking and sweeping operations are significant

2. **run_loop** (VM dispatch loop) - ~70% of total time
   - instructions.scm:534 (CLOSURE instruction) - most frequent
   - instructions.scm:192 (ADD instruction)
   - instructions.scm:153 (LREF instruction)
   - instructions.scm:321 (NUM_LT/NUM_GT comparisons)
   - vmcall.c:150,175 (procedure calls)

3. **Cache reading** (Sg_ReadCache) - startup/loading time
   - Library loading incurs cache reading overhead

### Allocation Hotspots

Based on code analysis:
- `Sg_Cons` - Pair allocation
- `Sg_MakeClosure` - Closure creation
- `Sg_MakeFlonum` - Flonum boxing
- Stack frame allocation

### Optimization Opportunities

#### Priority 1: Reduce Allocation Pressure
- [ ] Implement freelist for pairs (avoid GC for common pair operations)
- [ ] Consider stack allocation for temporary closures
- [ ] Cache common flonum values

#### Priority 2: VM Instruction Optimization  
- [x] Already uses computed goto for GCC/Clang
- [ ] Inline more operations in hot paths
- [ ] Reduce checking overhead in arithmetic operations

#### Priority 3: Procedure Call Optimization
- [ ] Fast paths for apply0/apply1/apply2/apply3
- [ ] Optimize tail call handling
- [ ] Reduce argument list construction

#### Priority 4: Arithmetic Fast Paths
- [x] Fixnum fast paths already exist in ADD/SUB/MUL
- [ ] Consider overflow checking optimization
- [ ] Inline comparison operations

## Computed Goto Status

The VM already implements computed goto (direct-threaded dispatch):
- Enabled when compiling with GCC/Clang
- Falls back to switch-case for MSVC

```c
#ifdef __GNUC__
# define SWITCH(val)        goto *dispatch_table[val];
# define NEXT              goto *dispatch_table[INSN(c)];
#else
# define SWITCH(val)        switch (val)
# define NEXT               goto dispatch;
#endif
```

## Next Steps

1. Implement pair freelist to reduce GC pressure
2. Add LTO support to enable cross-module inlining
3. Optimize hot VM instructions based on profiling
4. Add specialized apply functions for common arities
