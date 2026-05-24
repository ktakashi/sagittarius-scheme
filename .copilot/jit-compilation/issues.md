# JIT Implementation Issues and Investigation

This document describes two critical issues discovered during JIT implementation that affect call/cc compatibility and exception handling.

## Issue 1: Exception Handling Crash

### Problem

When JIT-compiled code calls C helper functions (like `Sg_Cdr`, `Sg_Apply`, `Sg__JitApply`) that throw Scheme exceptions, the program crashes.

### Root Cause

1. **JIT prologue** saves callee-saved registers (X19-X23) to the C stack:
   - X19 = VM pointer
   - X20 = Scheme stack pointer (vm->sp)
   - X21 = Scheme frame pointer (vm->fp)
   - X22 = Current closure (vm->cl)
   - X23 = Depth counter

2. **Exception handling** uses `longjmp`:
   - `Sg_Error()`, `Sg_AssertionViolation()`, etc. call `Sg_Raise()`
   - Exception propagation does `longjmp(vm->cstack->jbuf, 1)`
   - This jumps to the nearest `setjmp` in the C stack chain

3. **Register corruption**: When `longjmp` unwinds the C stack, it bypasses the JIT epilogue. The callee-saved registers that held VM state are never restored. After exception handling continues, the VM operates with corrupted register state.

### Affected Code Paths

Any JIT helper that can throw:
- `Sg_Cdr()` - when given non-pair
- `Sg_Car()` - when given non-pair
- `Sg__JitApply()` - when apply fails
- `Sg__JitCall()` - when proc is not a procedure
- Any assertion violations

### Potential Fixes

#### Option A: Save JIT Context in VM Structure

Add fields to `SgVM` to save JIT register state:

```c
struct SgVMRec {
  // ... existing fields ...
  
  // JIT context for exception recovery
  struct {
    void *returnAddr;   // Where to resume in JIT code
    SgObject *sp;       // Saved Scheme SP
    SgObject *fp;       // Saved Scheme FP  
    SgObject cl;        // Saved closure
    int depth;          // Saved depth
    int active;         // Is JIT code currently running?
  } jitContext;
};
```

In JIT prologue, save context:
```c
// Before any helper call that might throw
vm->jitContext.active = 1;
vm->jitContext.sp = X20;
vm->jitContext.fp = X21;
// etc.
```

In exception handler (`throw_continuation_body` in vm.c), check for JIT context and restore.

**Pros**: Clean solution, minimal overhead on normal path
**Cons**: Requires VM structure changes, needs careful integration

#### Option B: Use setjmp/longjmp Wrapper

Wrap JIT entry with `setjmp`:

```c
SgObject jit_entry_wrapper(SgVM *vm, SgClosure *cl) {
  SG_UNWIND_PROTECT {
    return actual_jit_code(vm, cl);
  }
  SG_WHEN_ERROR {
    // Exception occurred - JIT registers already lost
    // Re-raise to let normal handler deal with it
    SG_NEXT_HANDLER;
  }
  SG_END_PROTECT;
}
```

**Pros**: Integrates with existing exception system
**Cons**: Adds overhead to every JIT call, doesn't actually fix register corruption

#### Option C: Mark Exception-Throwing Functions

Don't JIT-compile closures that might throw exceptions:

```c
// During compilation analysis
if (closure_may_throw(cb)) {
  cb->jitFlags = SG_JIT_FLAG_FAILED;  // Don't compile
}
```

**Pros**: Simple, avoids the problem entirely
**Cons**: Severely limits what can be JIT-compiled, hard to determine statically

### Recommended Fix: Option A

Save JIT context in VM structure. This is the cleanest solution because:
1. Minimal overhead (only a few stores before helper calls)
2. Works with existing exception mechanism
3. Allows full JIT compilation coverage

---

## Issue 2: `Sg_Apply` Creates C Continuation Boundary

### Problem

When JIT code calls non-JIT procedures using `Sg__JitCall()`, `Sg__JitTailCall()`, or `Sg__JitApply()`, they use `Sg_Apply()` which creates a C continuation boundary. This causes different `call/cc` behavior between VM and JIT execution.

### Root Cause

1. **Interpreter CALL**: Sets `PC = closure->code` and continues in `run_loop`
   - No C stack frame created
   - Scheme continuation captured correctly

2. **JIT CALL**: Calls `Sg__JitCall()` → `Sg_Apply()` → `apply_rec()` → `evaluate_safe()`
   - `evaluate_safe()` creates new `SgCStack` with `setjmp`
   - This is a **C continuation boundary**

3. **Impact on call/cc**:
   - If `call/cc` captures continuation inside the called procedure
   - The continuation includes the `SgCStack` state
   - Invoking continuation expects to `longjmp` back to `evaluate_safe`
   - But JIT code's stack frame is different from what `evaluate_safe` expects
   - Results in undefined behavior or crash

### Code Analysis

In `src/jit/jit_compile.c`:

```c
SgObject Sg__JitCall(SgVM *vm, int argc, SgObject proc) {
  // For JIT closures - OK, calls JIT code directly
  if (SG_CLOSUREP(proc) && cb->jitCode != NULL) {
    return jitCode(vm, proc);  // No C boundary
  }
  
  // For non-JIT closures - PROBLEM!
  return Sg_Apply(proc, args);  // Creates C boundary
}
```

Compare to interpreter (vmcall.c):
```c
case SG_PROC_CLOSURE: {
  CL(vm) = AC(vm);
  PC(vm) = cb->code;  // Just set PC, no C boundary
  NEXT;
}
```

### Potential Fixes

#### Option A: Yield to Interpreter

Instead of calling `Sg_Apply`, set up VM state and return from JIT:

```c
SgObject Sg__JitCall(SgVM *vm, int argc, SgObject proc) {
  if (SG_CLOSUREP(proc) && cb->jitCode) {
    return jitCode(vm, proc);  // Direct JIT call
  }
  
  // Set up VM for interpreter execution
  vm->cl = proc;
  vm->pc = cb->code;
  vm->fp = vm->sp - argc;
  
  // Return special marker to tell JIT code to yield
  return SG_JIT_YIELD_TO_INTERPRETER;
}
```

JIT code checks return value and yields:
```asm
; After call to Sg__JitCall
cmp x0, #JIT_YIELD_MARKER
beq .yield_to_interpreter
; Normal return path
...

.yield_to_interpreter:
; Restore VM state
; Return to run_loop
```

**Pros**: Correct semantics, no C boundary
**Cons**: Complex implementation, requires JIT code changes

#### Option B: Re-entry Points

Create JIT code that can be entered at multiple points:

```c
struct JitEntryPoints {
  void *main;           // Normal entry
  void *resume_call_0;  // Resume after CALL at position 0
  void *resume_call_1;  // Resume after CALL at position 1
  // ...
};
```

The continuation frame stores which entry point to use when returning.

**Pros**: Full JIT execution without boundaries
**Cons**: Significantly increases code complexity and size

#### Option C: Mixed Execution Model

Only JIT-compile "hot" inner loops that don't make external calls:

1. Identify code regions that only call:
   - Other JIT-compiled closures
   - Pure primitive operations
2. JIT-compile only these regions
3. Let interpreter handle everything else

**Pros**: Simple, avoids the problem
**Cons**: Limited optimization scope

### Recommended Fix: Option A (Yield to Interpreter)

This provides correct semantics while keeping JIT benefits for hot paths:

1. JIT code for inner loops runs at full speed
2. External calls yield to interpreter (no C boundary)
3. `call/cc` works correctly
4. Can be implemented incrementally

### Implementation Sketch for Option A

1. **Add yield marker**:
   ```c
   #define SG_JIT_YIELD ((SgObject)((intptr_t)-1))
   ```

2. **Modify Sg__JitCall**:
   ```c
   SgObject Sg__JitCall(SgVM *vm, int argc, SgObject proc) {
     if (SG_CLOSUREP(proc)) {
       SgCodeBuilder *cb = SG_CODE_BUILDER(SG_CLOSURE(proc)->code);
       if (cb->jitCode) {
         return cb->jitCode(vm, proc);
       }
       // Set up for interpreter execution
       vm->cl = proc;
       vm->pc = cb->code;
       vm->fp = vm->sp - argc;
       return SG_JIT_YIELD;
     }
     // Handle other procedure types...
   }
   ```

3. **Modify JIT CALL emission**:
   ```c
   int Sg__JitEmit_CALL(SgJitContext *ctx, int argc) {
     // ... existing setup ...
     
     // Call helper
     arm64_bl(a, Sg__JitCall);
     
     // Check for yield
     arm64_cmp_r64_imm(a, ARM64_X0, SG_JIT_YIELD);
     arm64_beq(a, gen->labels[ctx->yieldLabel]);
     
     // Normal return path continues...
   }
   ```

4. **Add yield handler in JIT code**:
   - Save current JIT PC equivalent to VM
   - Exit JIT code back to interpreter
   - Interpreter continues from `vm->pc`

---

## Current Status

Both issues are **known but not fixed**. Current workarounds:

1. **Exception handling**: Disabled JIT during build scripts (`genstub`, `gendoc`) where exceptions are likely
2. **C boundary**: Set high threshold (1M) to avoid auto-JIT; explicit `jit-compile!` works for pure recursive functions like `tak`

---

## Issue 2 Investigation: Direct Procedure Calls

Following investigation of `vmcall.c`, here are the findings on how different procedure types can be called directly from JIT without C boundaries.

### SUBR (C-Implemented Procedures) - CAN CALL DIRECTLY ✓

From vmcall.c line 164-178:
```c
case SG_PROC_SUBR: {
  CL(vm) = AC(vm);
  PC(vm) = PC_TO_RETURN;
  ADJUST_ARGUMENT_FRAME(AC(vm), argc);
  SG_PROF_COUNT_CALL(vm, AC(vm));
  AC(vm) = SG_SUBR_FUNC(AC(vm))(FP(vm), argc, SG_SUBR_DATA(AC(vm)));
  if (TAIL_POS(vm)) RET_INSN();
  CHECK_ATTENTION;
  NEXT;
}
```

**Key insight**: SUBRs are called directly with no C continuation boundary!

**Required steps for JIT**:
1. `ADJUST_ARGUMENT_FRAME(proc, argc)` - validates and adjusts arguments:
   - Check `SG_PROCEDURE_REQUIRED(proc)` vs argc
   - If `SG_PROCEDURE_OPTIONAL(proc)`, fold rest args into a list
   - Throws `Sg_WrongNumberOfArgumentsViolation` if mismatch
2. Direct call: `SG_SUBR_FUNC(proc)(FP(vm), argc, SG_SUBR_DATA(proc))`

**JIT Implementation Approach**:
```c
SgObject Sg__JitCallSubr(SgVM *vm, int argc, SgObject proc) {
  int required = SG_PROCEDURE_REQUIRED(proc);
  int optional = SG_PROCEDURE_OPTIONAL(proc);
  
  // Argument count check
  if (optional) {
    if (argc < required) {
      Sg_WrongNumberOfArgumentsViolation(...);
    }
    // Fold rest args into list
    SgObject rest = SG_NIL;
    while (argc > required + optional - 1) {
      rest = Sg_Cons(*(--vm->sp), rest);
      argc--;
    }
    *(vm->sp++) = rest;
    argc++;
  } else {
    if (argc != required) {
      Sg_WrongNumberOfArgumentsViolation(...);
    }
  }
  
  vm->fp = vm->sp - argc;
  
  // Direct call - no C boundary!
  return SG_SUBR_FUNC(proc)(vm->fp, argc, SG_SUBR_DATA(proc));
}
```

### Generic Functions - PARTIALLY DIRECT

From vmcall.c, generic function dispatch involves:

1. **Compute applicable methods** (line 271):
   ```c
   mm = Sg_ComputeMethods(AC(vm), SP(vm)-argc, argc, APP);
   ```

2. **Create next-method** (lines 280-290):
   ```c
   if (SG_METHOD_LEAF_P(SG_CAR(mm))) {
     nm = SG_TRUE;
   } else {
     nm = Sg_MakeNextMethod(generic, SG_CDR(mm), SP(vm)-argc, argc, TRUE);
   }
   AC(vm) = SG_CAR(mm);  // First applicable method
   ```

3. **Method dispatch** (lines 385-413):
   - If method is SUBR: direct call (no boundary)
   - If method is Closure: set PC and continue in interpreter

**JIT Implementation Approach**:
```c
SgObject Sg__JitCallGeneric(SgVM *vm, int argc, SgObject generic) {
  // 1. Compute applicable methods
  SgObject methods = Sg_ComputeMethods(generic, vm->sp - argc, argc, FALSE);
  
  if (SG_NULLP(methods)) {
    // No applicable methods - call fallback
    vm->fp = vm->sp - argc;
    return SG_GENERIC(generic)->fallback(vm->fp, argc, SG_GENERIC(generic));
  }
  
  SgObject method = SG_CAR(methods);
  SgObject nm;
  
  // 2. Create next-method (unless leaf method)
  if (SG_METHOD_LEAF_P(method)) {
    nm = SG_TRUE;
  } else {
    nm = Sg_MakeNextMethod(generic, SG_CDR(methods), vm->sp - argc, argc, TRUE);
  }
  
  SgObject proc = SG_METHOD_PROCEDURE(method);
  
  // 3. Dispatch based on method type
  if (SG_SUBRP(proc)) {
    // C-defined method - call directly
    ADJUST_ARGUMENT_FRAME(method, argc);
    return SG_SUBR_FUNC(proc)(vm->fp, argc, SG_SUBR_DATA(proc));
  } else {
    // Closure method
    SgClosure *cls = SG_CLOSURE(proc);
    SgCodeBuilder *cb = SG_CODE_BUILDER(cls->code);
    
    // Add next-method as extra arg
    shift_args_and_add_nm(vm, argc, nm);
    argc++;
    
    if (cb->jitCode) {
      // JIT-compiled method - call directly
      vm->fp = vm->sp - argc;
      vm->cl = cls;
      return cb->jitCode(vm, cls);
    } else {
      // Non-JIT closure - yield to interpreter
      vm->fp = vm->sp - argc;
      vm->cl = cls;
      vm->pc = cb->code;
      return SG_JIT_YIELD;
    }
  }
}
```

### Summary Table

| Procedure Type | Direct Call? | Implementation |
|---------------|--------------|----------------|
| SUBR | ✓ Yes | Check args, call `SG_SUBR_FUNC(proc)(fp, argc, data)` |
| Closure (JIT) | ✓ Yes | Already implemented |
| Closure (non-JIT) | ✗ Yield | Set vm->pc, return `SG_JIT_YIELD` |
| Generic (SUBR method) | ✓ Yes | `Sg_ComputeMethods` then direct call |
| Generic (JIT method) | ✓ Yes | `Sg_ComputeMethods` then call JIT |
| Generic (non-JIT method) | ✗ Yield | `Sg_ComputeMethods` then yield |

### Benefits of Direct Calls

1. **No C continuation boundary** - `call/cc` works correctly
2. **Better performance** - avoids `Sg_Apply` → `evaluate_safe` → `setjmp` overhead
3. **Consistent semantics** - same behavior as interpreter

---

## Test Case

The `tak` function works correctly with JIT because:
- It's self-recursive (calls JIT code directly)
- No exceptions thrown
- No external procedure calls

```scheme
(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(jit-compile! tak)
(tak 30 20 10)  ; => 11, ~2.2x faster than interpreted
```

---

## Implementation Status (Updated 2025-05-22)

### Completed

1. **JIT Context Fields Added to VM** (vm.h):
   ```c
   struct {
     int       active;           /* Is JIT code currently executing? */
     SgObject *savedSp;          /* Saved Scheme stack pointer */
     SgObject *savedFp;          /* Saved Scheme frame pointer */
     SgObject  savedCl;          /* Saved current closure */
     int       savedDepth;       /* Saved recursion depth */
   } jitContext;
   ```

2. **JIT Context Helper Macros** (jit.h):
   - `SG_JIT_SAVE_CONTEXT(vm, sp, fp, cl, depth)` - save before helper calls
   - `SG_JIT_CLEAR_CONTEXT(vm)` - clear after normal return
   - `SG_JIT_CONTEXT_ACTIVE(vm)` - check if JIT is active
   - `Sg_InitJitContext(vm)` - initialize in new VMs

3. **VM Initialization** (vm.c):
   - `Sg_InitJitContext()` called in `Sg_NewThreadVM()`

4. **Exception Handling Test Passed**:
   ```scheme
   ;; Test verified:
   ;; 1. JIT-compiled (cdr '(a . b)) => b
   ;; 2. Exception caught by guard when JIT throws
   ;; 3. Execution continues correctly after exception
   ```

### R6RS Test Suite

R6RS tests are already configured in ctest:
```
ctest -R r6rs  # Runs r6rs and r6rs+.scm tests
```

Current timing: ~25 seconds total (12-13s each test)

### SUBR Direct Calls - COMPLETED ✓

Implemented `Sg__JitCallSubr()` and `Sg__JitTailCallSubr()` in jit_compile.c:

```c
SgObject Sg__JitCallSubr(SgVM *vm, int argc, SgObject proc) {
  // 1. Validate argument count (required vs optional)
  // 2. Fold rest args into list if optional
  // 3. Pop continuation frame
  // 4. Direct call: SG_SUBR_FUNC(proc)(vm->fp, argc, SG_SUBR_DATA(proc))
  // 5. Restore VM state
}
```

Updated `Sg__JitCall` and `Sg__JitTailCall` to check for `SG_SUBRP(proc)` first:
- SUBRs: call `Sg__JitCallSubr()` directly (no C boundary)
- JIT closures: call JIT code directly (already implemented)
- Non-JIT closures: fall back to `Sg_Apply()` (TODO: yield to interpreter)

**Test results** (test-jit-subr.scm):
- `(+ x y)` - basic SUBR call ✓
- Multiple SUBRs in sequence ✓
- `list` with optional arguments ✓
- `car`/`cdr` calls ✓
- Nested SUBR calls ✓

### Next Steps

1. **Generic Function Support** - Implement `Sg__JitCallGeneric()` with method dispatch
2. **Yield to Interpreter** - For non-JIT closures, yield instead of `Sg_Apply`

### Generic Function Support - COMPLETED ✓

Implemented `Sg__JitCallGeneric()` and `Sg__JitTailCallGeneric()` in jit_compile.c:

```c
SgObject Sg__JitCallGeneric(SgVM *vm, int argc, SgObject generic) {
  // 1. Compute applicable methods
  SgObject methods = Sg_ComputeMethods(generic, vm->sp - argc, argc, FALSE);
  
  // 2. Create next-method (unless leaf)
  if (!SG_METHOD_LEAF_P(method)) {
    nm = Sg_MakeNextMethod(...);
  }
  
  // 3. Dispatch to first method
  if (SG_SUBRP(proc)) {
    // Direct C call - no boundary
    return SG_SUBR_FUNC(proc)(vm->fp, argc, SG_SUBR_DATA(proc));
  } else {
    // Closure method - add next-method arg
    shift_and_add_next_method(vm, argc, nm);
    if (cb->jitCode) {
      return jitCode(vm, cls);  // JIT call
    } else {
      return Sg_Apply(cls, args);  // Non-JIT fallback
    }
  }
}
```

Updated `Sg__JitCall` and `Sg__JitTailCall` to check for `SG_GENERICP(proc)`.

**Test results** (test-jit-generic.scm):
- Generic numeric dispatch: `(test-add 10 20)` → `30` ✓
- Generic string dispatch: `(test-add "hello" " world")` → `"hello world"` ✓
- Direct generic calls work ✓

### Remaining Work

1. **Yield to Interpreter** - For non-JIT closures, yield instead of `Sg_Apply` to avoid C continuation boundary for proper `call/cc` semantics
