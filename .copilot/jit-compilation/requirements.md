# JIT Compilation User Story

## Epic: Just-In-Time Compilation for Sagittarius Scheme

### Overview

Implement a Method JIT compiler for Sagittarius Scheme's bytecode VM to improve execution performance of hot code paths. The JIT compilation happens after bytecode is loaded from cache, enabling repeated executions of library code to benefit from native code generation.

### Target Platforms

| Platform | Architecture | ABI | Priority |
|----------|-------------|-----|----------|
| Linux | x86_64 | System V AMD64 | High |
| Linux | ARM64 (AArch64) | AAPCS64 | High |
| macOS | x86_64 | System V AMD64 | High |
| macOS | ARM64 (Apple Silicon) | AAPCS64 | High |
| Linux | x86 (32-bit) | cdecl | Medium |
| Linux | ARM (32-bit) | AAPCS | Medium |
| Windows | x86_64 | Microsoft x64 | Low |

### Current VM Architecture

The Sagittarius bytecode VM has the following characteristics:

#### Bytecode Format
- **Word size**: `SgWord` (typedef `intptr_t`) - pointer-sized
- **Instruction encoding**: 32-bit format (for cache compatibility)
  - Bits 0-7: Opcode (8 bits, max 256 instructions)
  - Bits 8-19: value1 (12 bits)
  - Bits 20-31: value2 (12 bits)
- **Operands**: Some instructions have object operands in subsequent words

#### Key Data Structures
```c
typedef struct SgCodeBuilderRec {
    SgWord *code;      // Bytecode array
    int size;          // Number of words
    int argc;          // Argument count
    int optional;      // Optional argument flag
    int freec;         // Free variable count
    int maxStack;      // Maximum stack depth
    SgObject name;     // Procedure name
    SgObject src;      // Source info
} SgCodeBuilder;

typedef struct SgClosureRec {
    SgCodeBuilder *code;
    SgObject frees[];  // Captured free variables
} SgClosure;
```

#### VM Registers
- `AC`: Accumulator (result register)
- `SP`: Stack pointer
- `FP`: Frame pointer
- `PC`: Program counter
- `CL`: Current closure
- `CONT`: Continuation chain

#### Dispatch Mechanism
- **GCC/Clang**: Computed goto (`goto *dispatch_table[opcode]`)
- **MSVC**: Switch statement fallback

#### VM Stack Architecture (JIT Design)

The Scheme stack is **heap-allocated**, separate from the native C/JIT stack. JIT code accesses Scheme local variables through pointer arithmetic on VM registers, not through native stack operations.

```
┌─────────────────────────────────────────┐
│  SgVM structure (in heap)               │
│  ├── sp  ──────────────────────┐        │
│  ├── fp  ──────────┐           │        │
│  └── ...           │           │        │
└────────────────────│───────────│────────┘
                     │           │
                     ▼           ▼
┌─────────────────────────────────────────┐
│  Scheme Stack (heap-allocated array)    │
│  ┌─────┬─────┬─────┬─────┬─────┬─────┐  │
│  │local│local│local│ ... │ top │     │  │
│  │  0  │  1  │  2  │     │     │     │  │
│  └─────┴─────┴─────┴─────┴─────┴─────┘  │
│    ▲ FP                    ▲ SP         │
└─────────────────────────────────────────┘
```

**How JIT Accesses Local Variables:**

1. **Prologue**: JIT code receives `SgVM *vm` as C argument, loads `vm->fp` and `vm->sp` into CPU registers (e.g., X21, X20 on ARM64)

2. **LREF instruction**: Access `FP[index]` via memory load from heap
   ```c
   // Interpreter: *(FP + index)
   // JIT ARM64:   ldr x0, [x21, #index*8]
   ```

3. **PUSH instruction**: Store to `*SP++` via memory store to heap
   ```c
   // JIT ARM64:   str x0, [x20], #8
   ```

4. **Epilogue**: Store modified SP/FP back to VM structure before returning

**Key Insight**: JIT uses CPU registers as **pointers to heap memory**, not as a native call stack. The Scheme stack remains in the GC-managed heap, ensuring compatibility with continuations and garbage collection.

### User Stories

---

## US-1: JIT Infrastructure Foundation

**As a** Sagittarius developer  
**I want** a platform-independent JIT framework  
**So that** platform-specific code generators can be added incrementally

### Acceptance Criteria
1. JIT code buffer allocation with execute permissions
2. Platform detection at compile time
3. Abstract interface for code generation
4. JIT code invalidation and cleanup

### Technical Tasks

#### T-1.1: JIT Memory Management
```c
// New file: src/jit/jit_memory.h
typedef struct JitCodeBufferRec {
    uint8_t *code;       // Executable memory
    size_t size;         // Allocated size
    size_t used;         // Used bytes
} JitCodeBuffer;

JitCodeBuffer* Sg_AllocJitBuffer(size_t size);
void Sg_FreeJitBuffer(JitCodeBuffer *buf);
void Sg_MakeExecutable(JitCodeBuffer *buf);
```

Platform-specific allocation:
- **POSIX**: `mmap(PROT_READ | PROT_WRITE | PROT_EXEC, MAP_ANONYMOUS)`
- **Windows**: `VirtualAlloc(PAGE_EXECUTE_READWRITE)`

#### T-1.2: JIT Entry Point Structure
```c
// Native function pointer type
typedef SgObject (*JitCompiledCode)(SgVM *vm, SgClosure *closure);

// Extended CodeBuilder with JIT info
struct SgCodeBuilderRec {
    // ... existing fields ...
    JitCompiledCode jitCode;    // NULL if not JIT-compiled
    uint32_t callCount;         // For hot code detection
    uint32_t jitFlags;          // JIT state flags
};
```

#### T-1.3: Platform Abstraction
```c
// src/jit/jit_codegen.h
typedef struct JitContextRec JitContext;

// Platform-independent interface
JitContext* Sg_CreateJitContext(JitCodeBuffer *buf);
void Sg_JitEmitPrologue(JitContext *ctx, int argc, int freec);
void Sg_JitEmitEpilogue(JitContext *ctx);
void Sg_JitEmitInstruction(JitContext *ctx, SgWord insn, SgWord *operand);
JitCompiledCode Sg_JitFinalize(JitContext *ctx);
```

---

## US-2: Hot Code Detection and Tiered Execution

**As a** Sagittarius user running scripts  
**I want** the VM to automatically JIT-compile frequently executed code  
**So that** I get fast startup for one-shot code and optimized execution for hot paths

### Acceptance Criteria
1. Closures start in interpreter mode
2. Call counter tracks execution frequency
3. Hot threshold triggers JIT compilation (configurable)
4. Seamless transition from interpreted to JIT code

### Technical Design

#### Tiered Execution Model
```
+-------------+     hot threshold     +--------------+
| Interpreter | ------------------->  | JIT Compiled |
+-------------+     (N calls)         +--------------+
```

#### T-2.1: Call Counter Integration
```c
// In CALL/LOCAL_CALL handlers
if (closure->code->jitCode != NULL) {
    // Execute JIT code
    AC(vm) = closure->code->jitCode(vm, closure);
} else if (++closure->code->callCount >= JIT_HOT_THRESHOLD) {
    // Compile and execute
    closure->code->jitCode = Sg_JitCompile(closure->code);
    if (closure->code->jitCode) {
        AC(vm) = closure->code->jitCode(vm, closure);
    } else {
        // Fallback to interpreter
        interpret(vm, closure);
    }
} else {
    // Interpret
    interpret(vm, closure);
}
```

#### T-2.2: Configuration
```c
// Environment variable or parameter
#define JIT_HOT_THRESHOLD 100  // Default

// Runtime configuration
void Sg_SetJitThreshold(int threshold);
int Sg_GetJitThreshold(void);
void Sg_DisableJit(void);      // For debugging
```

---

## US-3: x86_64 Code Generator

**As a** user on Linux/macOS x86_64  
**I want** native code generation  
**So that** hot Scheme code runs at near-native speed

### Acceptance Criteria
1. Generate valid x86_64 System V ABI code
2. Handle VM register mapping
3. Support all arithmetic and control flow instructions
4. Pass existing test suite

### Technical Design

#### T-3.1: Register Allocation Strategy
```
x86_64 Register Mapping (System V ABI):
-----------------------------------------
VM Register     x86_64 Register    Notes
-----------     ---------------    -----
AC (result)     RAX                Return value register
vm              RBX                Callee-saved, VM pointer
SP              R12                Callee-saved, Scheme stack
FP              R13                Callee-saved, Frame pointer
PC              R14                Callee-saved, Program counter
CL (closure)    R15                Callee-saved, Current closure
temp1           RCX                Caller-saved scratch
temp2           RDX                Caller-saved scratch
temp3           RSI                Caller-saved scratch
temp4           RDI                Caller-saved scratch
```

#### T-3.2: Custom x86_64 Assembler
```c
// src/jit/x86_64/asm_x86_64.h
typedef struct X64AsmRec {
    uint8_t *buf;
    size_t pos;
    size_t size;
    // Label management
    int labelCount;
    int *labelOffsets;
    // Patch list for forward refs
    struct PatchEntry *patches;
} X64Asm;

// Example encoding functions
void x64_mov_r64_r64(X64Asm *a, int dst, int src);
void x64_mov_r64_imm64(X64Asm *a, int dst, int64_t imm);
void x64_mov_r64_mem(X64Asm *a, int dst, int base, int32_t offset);
void x64_add_r64_r64(X64Asm *a, int dst, int src);
void x64_sub_r64_r64(X64Asm *a, int dst, int src);
void x64_cmp_r64_r64(X64Asm *a, int left, int right);
void x64_jcc(X64Asm *a, int cond, int label);
void x64_jmp(X64Asm *a, int label);
void x64_call(X64Asm *a, void *target);
void x64_ret(X64Asm *a);
```

#### T-3.3: Instruction Translation
```c
// Example: LREF (local reference)
case LREF:
    INSN_VAL1(val1, c);
    // mov RAX, [R13 + val1*8]  ; FP[val1]
    x64_mov_r64_mem(asm, RAX, R13, val1 * sizeof(SgObject));
    break;

// Example: ADD
case ADD:
    // pop temp from stack
    x64_mov_r64_mem(asm, RCX, R12, -8);  // temp = SP[-1]
    x64_sub_r64_imm(asm, R12, 8);         // SP--
    // Check if both fixnums
    x64_mov_r64_r64(asm, RDX, RAX);
    x64_or_r64_r64(asm, RDX, RCX);
    x64_test_r64_imm(asm, RDX, TAG_MASK);
    x64_jcc(asm, JNZ, slow_path);
    // Fast path: add fixnums
    x64_add_r64_r64(asm, RAX, RCX);
    x64_jo(asm, overflow_handler);  // Handle overflow
    // ... slow path calls Sg_Add ...
    break;
```

---

## US-4: ARM64 Code Generator

**As a** user on ARM64 (Linux/macOS Apple Silicon)  
**I want** native code generation  
**So that** Sagittarius runs fast on ARM devices

### Acceptance Criteria
1. Generate valid ARM64 AAPCS64 code
2. Handle VM register mapping
3. Support all priority instructions
4. Handle Apple Silicon specifics (W^X)

### Technical Design

#### T-4.1: Register Allocation
```
ARM64 Register Mapping (AAPCS64):
---------------------------------
VM Register     ARM64 Register    Notes
-----------     --------------    -----
AC (result)     X0                Return value
vm              X19               Callee-saved, VM pointer
SP (scheme)     X20               Callee-saved
FP              X21               Callee-saved
PC              X22               Callee-saved
CL              X23               Callee-saved
temp1           X1                Caller-saved
temp2           X2                Caller-saved
temp3           X3                Caller-saved
```

#### T-4.2: Apple Silicon W^X Handling
```c
// On Apple Silicon, memory cannot be W+X simultaneously
void arm64_make_writable(JitCodeBuffer *buf) {
#ifdef __APPLE__
    pthread_jit_write_protect_np(false);
#endif
}

void arm64_make_executable(JitCodeBuffer *buf) {
#ifdef __APPLE__
    pthread_jit_write_protect_np(true);
    sys_icache_invalidate(buf->code, buf->used);
#else
    __builtin___clear_cache(buf->code, buf->code + buf->used);
#endif
}
```

---

## US-5: x86 (32-bit) Code Generator

**As a** user on 32-bit x86 systems  
**I want** JIT support  
**So that** older systems also benefit from optimization

### Acceptance Criteria
1. Generate valid x86 cdecl code
2. Handle limited register count (8 GPRs)
3. Support stack-based argument passing

### Technical Notes
- More register spilling required due to limited registers
- Lower priority than 64-bit platforms

---

## US-6: ARM (32-bit) Code Generator

**As a** user on ARM32 devices (Raspberry Pi, etc.)  
**I want** JIT support  
**So that** embedded devices can run Scheme efficiently

### Acceptance Criteria
1. Generate valid ARM AAPCS code
2. Handle Thumb2 interworking if needed
3. Support VFP for floating-point (optional)

---

## US-7: GC Integration

**As a** Sagittarius developer  
**I want** JIT code to work correctly with Boehm GC  
**So that** there are no memory safety issues

### Technical Design

#### Conservative Scanning Approach
Boehm GC will conservatively scan JIT code regions:
1. JIT code buffers registered with GC as roots
2. Object pointers embedded in JIT code are valid GC references
3. No explicit safepoints needed (conservative scanning finds all pointers)

```c
// Register JIT buffer with GC
JitCodeBuffer* Sg_AllocJitBuffer(size_t size) {
    JitCodeBuffer *buf = GC_MALLOC_UNCOLLECTABLE(sizeof(JitCodeBuffer));
    buf->code = mmap(...);
    // Add code region to GC roots
    GC_add_roots(buf->code, buf->code + size);
    return buf;
}

void Sg_FreeJitBuffer(JitCodeBuffer *buf) {
    GC_remove_roots(buf->code, buf->code + buf->size);
    munmap(buf->code, buf->size);
    GC_FREE(buf);
}
```

---

## US-8: Debugging and Profiling Support

**As a** developer debugging Scheme code  
**I want** to disable JIT and get stack traces  
**So that** I can troubleshoot issues

### Acceptance Criteria
1. Command-line flag to disable JIT (`--no-jit`)
2. JIT code includes debug info for stack traces
3. Profile information shows JIT vs interpreted time

---

## Architecture: Modular Instruction Emission

### Overview

The JIT compiler uses a modular architecture that separates the instruction dispatch loop from platform-specific code generation. This enables:
1. Single instruction loop implementation in platform-agnostic `jit_compile.c`
2. Per-instruction emit handlers implemented per platform
3. Easy addition of new platforms without duplicating dispatch logic
4. Composable combined instructions using existing handlers

### Design

#### Platform-Agnostic Instruction Loop

The main compilation function `Sg_JitCompile` contains the instruction dispatch loop:

```c
// src/jit/jit_compile.c
SgJitCompiledCode Sg_JitCompile(SgCodeBuilder *cb)
{
    JitContext *ctx = Sg_JitCreateContext(cb);
    if (!ctx) return NULL;
    
    if (!Sg__JitEmit_Prologue(ctx)) goto fail;
    
    for (int pc = 0; pc < cb->codeSize; /* pc updated per instruction */) {
        SgWord insn = cb->code[pc];
        int opcode = INSN_OP(insn);
        
        switch (opcode) {
        case LREF:
            if (!Sg__JitEmit_LREF(ctx, INSN_VAL1(insn))) goto fail;
            pc++;
            break;
            
        case PUSH:
            if (!Sg__JitEmit_PUSH(ctx)) goto fail;
            pc++;
            break;
            
        case LREF_PUSH:
            // Combined: call both or use optimized version
            if (!Sg__JitEmit_LREF_PUSH(ctx, INSN_VAL1(insn))) goto fail;
            pc++;
            break;
            
        // ... more instructions
        
        default:
            goto fail;  // Unsupported instruction
        }
    }
    
    if (!Sg__JitEmit_Epilogue(ctx)) goto fail;
    
    return Sg_JitFinalize(ctx);
    
fail:
    Sg_JitDestroyContext(ctx);
    return NULL;
}
```

#### Platform-Specific Emit Handlers

Each platform implements the `Sg__JitEmit_*` functions:

```c
// src/jit/jit_emit.h - Platform-independent declarations
typedef struct JitContextRec JitContext;

// Lifecycle
JitContext* Sg_JitCreateContext(SgCodeBuilder *cb);
void Sg_JitDestroyContext(JitContext *ctx);
SgJitCompiledCode Sg_JitFinalize(JitContext *ctx);

// Prologue/Epilogue
int Sg__JitEmit_Prologue(JitContext *ctx);
int Sg__JitEmit_Epilogue(JitContext *ctx);

// Basic Instructions
int Sg__JitEmit_NOP(JitContext *ctx);
int Sg__JitEmit_CONST(JitContext *ctx, SgObject val);
int Sg__JitEmit_CONSTI(JitContext *ctx, intptr_t val);
int Sg__JitEmit_LREF(JitContext *ctx, int index);
int Sg__JitEmit_LSET(JitContext *ctx, int index);
int Sg__JitEmit_PUSH(JitContext *ctx);

// Arithmetic
int Sg__JitEmit_ADD(JitContext *ctx);
int Sg__JitEmit_ADDI(JitContext *ctx, intptr_t val);
int Sg__JitEmit_SUB(JitContext *ctx);
int Sg__JitEmit_SUBI(JitContext *ctx, intptr_t val);

// Control Flow
int Sg__JitEmit_TEST(JitContext *ctx, int labelIndex);
int Sg__JitEmit_JUMP(JitContext *ctx, int labelIndex);
int Sg__JitEmit_RET(JitContext *ctx);

// Combined Instructions (can use default or optimized)
int Sg__JitEmit_LREF_PUSH(JitContext *ctx, int index);
int Sg__JitEmit_CONST_PUSH(JitContext *ctx, SgObject val);
```

#### Default Combined Instruction Implementation

Combined instructions can have default implementations that call primitives:

```c
// src/jit/jit_emit_default.c
#ifndef Sg__JitEmit_LREF_PUSH_CUSTOM
int Sg__JitEmit_LREF_PUSH(JitContext *ctx, int index)
{
    if (!Sg__JitEmit_LREF(ctx, index)) return 0;
    if (!Sg__JitEmit_PUSH(ctx)) return 0;
    return 1;
}
#endif
```

Platforms can override with optimized versions:

```c
// src/jit/arm64/emit_arm64.c
#define Sg__JitEmit_LREF_PUSH_CUSTOM
int Sg__JitEmit_LREF_PUSH(JitContext *ctx, int index)
{
    Arm64CodeGen *gen = (Arm64CodeGen *)ctx->platform;
    Arm64Asm *a = gen->a;
    // Optimized: load and push in one sequence without
    // storing/loading intermediate result
    arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_SCHFP, index * 8);
    arm64_str_r64_mem_post(a, JIT_REG_TEMP1, JIT_REG_SCHSP, 8);
    return 1;
}
```

#### JitContext Structure

```c
// src/jit/jit_internal.h
struct JitContextRec {
    SgCodeBuilder *cb;      // Code builder being compiled
    SgJitCodeBuffer *buf;   // Target code buffer
    void *platform;         // Platform-specific context (Arm64CodeGen*, etc.)
    
    // Label management
    int labelCount;
    int *pcToLabel;         // Maps bytecode PC to label index
    
    // Compilation state
    int failed;             // Set to 1 on failure
};
```

### Benefits

1. **Single Instruction Loop**: Eliminates code duplication across platforms
2. **Easy Platform Addition**: New platforms only implement `Sg__JitEmit_*` functions
3. **Composable**: Combined instructions can reuse primitive implementations
4. **Performance**: Platforms can provide optimized combined instruction handlers
5. **Maintainability**: Instruction set changes are localized to one place

---

## Implementation Phases

### Phase 1: Foundation (4-6 weeks)
- [ ] JIT memory management (T-1.1)
- [ ] JIT entry point structure (T-1.2)
- [ ] Platform abstraction layer (T-1.3)
- [ ] Hot code detection (T-2.1, T-2.2)
- [ ] x86_64 assembler basics (T-3.2)

### Phase 2: x86_64 MVP (4-6 weeks)
- [ ] VM register mapping (T-3.1)
- [ ] Basic instruction set:
  - Local variables: LREF, LSET, LREF_PUSH
  - Arithmetic: ADD, ADDI, SUB, SUBI, MUL, DIV
  - Stack: PUSH, POP
  - Control: TEST, JUMP, RET
  - Comparisons: NUM_EQ, NUM_LT, NUM_LE, NUM_GT, NUM_GE
- [ ] Function calls: CALL, TAIL_CALL, RET
- [ ] Test suite passes with JIT enabled

### Phase 3: ARM64 (3-4 weeks)
- [ ] ARM64 assembler (T-4.2)
- [ ] Register mapping (T-4.1)
- [ ] Same instruction set as x86_64
- [ ] Apple Silicon W^X handling

### Phase 4: 32-bit Platforms (2-3 weeks each)
- [ ] x86 assembler and code generator
- [ ] ARM32 assembler and code generator

### Phase 5: Optimization and Polish (ongoing)
- [ ] Inline caching for global references
- [ ] Type specialization for common patterns
- [ ] Better overflow handling
- [ ] Performance benchmarking and tuning

---

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Complex ABI handling | High | Start with simple calling convention; add full support incrementally |
| Debugging difficulty | Medium | Comprehensive logging; ability to dump generated code |
| Platform-specific bugs | Medium | Extensive testing on each platform; CI matrix |
| GC interaction issues | High | Conservative approach; thorough stress testing |
| Performance regression | Low | Always keep interpreter path; JIT is optional |

---

## Success Metrics

1. **Performance**: 2-5x speedup on arithmetic-heavy benchmarks
2. **Stability**: All existing tests pass with JIT enabled
3. **Coverage**: 80%+ of commonly-used instructions JIT-compiled
4. **Memory**: JIT overhead < 10% memory increase for typical workloads

---

## Files to Create/Modify

### New Files
```
src/jit/
├── jit.h              # Public JIT interface
├── jit_memory.c       # Memory allocation
├── jit_compile.c      # Main compilation logic
├── jit_common.h       # Shared definitions
├── x86_64/
│   ├── asm_x86_64.h   # x86_64 assembler
│   ├── asm_x86_64.c
│   └── codegen_x86_64.c
├── arm64/
│   ├── asm_arm64.h    # ARM64 assembler
│   ├── asm_arm64.c
│   └── codegen_arm64.c
├── x86/
│   └── ...
└── arm/
    └── ...
```

### Modified Files
- `src/sagittarius/private/code.h` - Add JIT fields to CodeBuilder
- `src/vm.c` - Add JIT entry points in CALL handlers
- `src/cache.c` - Reset JIT state when loading from cache
- `CMakeLists.txt` - Add JIT source files and platform detection

---

## References

- [System V AMD64 ABI](https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf)
- [ARM64 Procedure Call Standard](https://developer.arm.com/documentation/ihi0055/latest)
- [Intel x86 Instruction Reference](https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html)
- [ARM Architecture Reference Manual](https://developer.arm.com/documentation/ddi0487/latest)
- [Boehm GC Documentation](https://www.hboehm.info/gc/)
