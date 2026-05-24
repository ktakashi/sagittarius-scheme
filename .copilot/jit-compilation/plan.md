# Implementation Plan: JIT Compilation

## Overview

Implement a Method JIT compiler for Sagittarius Scheme's bytecode VM. The first MVP targets **ARM64 macOS** (Apple Silicon), providing two-tier execution: interpreter for cold code, JIT for hot code. Subsequent phases will add x86_64, then 32-bit platforms.

## Detailed Plan

### Phase 1: JIT Infrastructure Foundation

**Objective**: Build the platform-independent JIT framework that all code generators will use.

**Steps**:

1. Create JIT memory management
   - Implementation: Allocate executable memory using `mmap` with `MAP_JIT` on macOS
   - Handle Apple Silicon W^X (Write XOR Execute) requirements
   - Files to create:
     - `src/jit/jit.h` - Public JIT interface
     - `src/jit/jit_memory.c` - Memory allocation functions

   ```c
   // src/jit/jit.h
   #ifndef SAGITTARIUS_JIT_H_
   #define SAGITTARIUS_JIT_H_
   
   #include "sagittariusdefs.h"
   
   // JIT code buffer
   typedef struct JitCodeBufferRec {
       uint8_t *code;
       size_t size;
       size_t used;
   } JitCodeBuffer;
   
   // Native function signature
   typedef SgObject (*JitCompiledCode)(SgVM *vm, SgClosure *closure);
   
   // Memory management
   JitCodeBuffer* Sg_AllocJitBuffer(size_t size);
   void Sg_FreeJitBuffer(JitCodeBuffer *buf);
   
   // Platform-specific
   void Sg_JitMakeWritable(JitCodeBuffer *buf);
   void Sg_JitMakeExecutable(JitCodeBuffer *buf);
   
   // Compilation
   JitCompiledCode Sg_JitCompile(SgCodeBuilder *cb);
   
   // Configuration
   void Sg_SetJitEnabled(int enabled);
   int Sg_JitEnabled(void);
   void Sg_SetJitThreshold(int threshold);
   
   #endif
   ```

2. Extend CodeBuilder with JIT fields
   - Modify `src/sagittarius/private/code.h`
   - Add `jitCode`, `callCount`, `jitFlags` fields

   ```c
   // Add to SgCodeBuilder structure
   JitCompiledCode jitCode;  // Compiled native code, NULL if not compiled
   uint32_t callCount;       // Execution counter for hot detection
   uint32_t jitFlags;        // JIT_FLAG_COMPILED, JIT_FLAG_FAILED, etc.
   ```

3. Integrate hot code detection in VM
   - Modify `src/vm.c` CALL/LOCAL_CALL/TAIL_CALL handlers
   - Add call counter increment and JIT trigger logic

   ```c
   // In CALL handler
   if (SG_CLOSUREP(proc)) {
       SgCodeBuilder *cb = SG_CLOSURE(proc)->code;
       if (cb->jitCode != NULL) {
           // Fast path: execute JIT code
           AC(vm) = cb->jitCode(vm, SG_CLOSURE(proc));
       } else if (Sg_JitEnabled() && ++cb->callCount >= jit_threshold) {
           // Compile and execute
           cb->jitCode = Sg_JitCompile(cb);
           if (cb->jitCode) {
               AC(vm) = cb->jitCode(vm, SG_CLOSURE(proc));
           } else {
               cb->jitFlags |= JIT_FLAG_FAILED;
               // Fall through to interpreter
           }
       } else {
           // Interpreter path
       }
   }
   ```

4. Update CMakeLists.txt
   - Add platform detection for JIT support using existing `SAGITTARIUS_PROCESSOR`
   - Add conditional compilation for `src/jit/`

   ```cmake
   # JIT Platform detection (using existing SAGITTARIUS_PROCESSOR)
   IF(${SAGITTARIUS_PROCESSOR} STREQUAL "arm64")
       SET(JIT_ARCH "arm64")
       SET(HAS_JIT ON)
   ELSEIF(${SAGITTARIUS_PROCESSOR} STREQUAL "x86_64")
       SET(JIT_ARCH "x86_64")
       SET(HAS_JIT ON)
   ELSEIF(${SAGITTARIUS_PROCESSOR} MATCHES "^i.86" OR
          ${SAGITTARIUS_PROCESSOR} STREQUAL "x86")
       SET(JIT_ARCH "x86")
       SET(HAS_JIT ON)
   ELSEIF(${SAGITTARIUS_PROCESSOR} MATCHES "arm")
       SET(JIT_ARCH "arm")
       SET(HAS_JIT ON)
   ELSE()
       SET(HAS_JIT OFF)
   ENDIF()
   
   IF(HAS_JIT)
       ADD_DEFINITIONS(-DHAVE_JIT)
       ADD_DEFINITIONS(-DJIT_ARCH_${JIT_ARCH})
       SET(JIT_SOURCES
           src/jit/jit_memory.c
           src/jit/jit_compile.c
       )
       IF(JIT_ARCH STREQUAL "arm64")
           LIST(APPEND JIT_SOURCES
               src/jit/arm64/asm_arm64.c
               src/jit/arm64/codegen_arm64.c
           )
       ENDIF()
   ENDIF()
   ```

---

### Phase 2: ARM64 Assembler

**Objective**: Implement a minimal ARM64 assembler for native code generation on Apple Silicon.

**Steps**:

1. Create ARM64 assembler header
   - File: `src/jit/arm64/asm_arm64.h`
   - Define register constants, encoding helpers

   ```c
   // ARM64 general-purpose registers
   typedef enum {
       X0 = 0, X1, X2, X3, X4, X5, X6, X7,
       X8, X9, X10, X11, X12, X13, X14, X15,
       X16, X17, X18, X19, X20, X21, X22, X23,
       X24, X25, X26, X27, X28, X29, X30,
       XZR = 31,  // Zero register
       SP = 31    // Stack pointer (context-dependent)
   } Arm64Reg;
   
   // VM register mapping
   #define JIT_VM      X19  // VM pointer (callee-saved)
   #define JIT_SP      X20  // Scheme stack pointer
   #define JIT_FP      X21  // Scheme frame pointer
   #define JIT_CL      X22  // Current closure
   #define JIT_TEMP1   X0   // Also return value
   #define JIT_TEMP2   X1
   #define JIT_TEMP3   X2
   
   // Assembler context
   typedef struct Arm64AsmRec {
       uint8_t *buf;
       size_t pos;
       size_t size;
       // Label management
       int labelCount;
       int *labelOffsets;
       // Forward reference patches
       struct Arm64Patch *patches;
       int patchCount;
   } Arm64Asm;
   
   // Core functions
   Arm64Asm* arm64_asm_new(uint8_t *buf, size_t size);
   void arm64_asm_free(Arm64Asm *a);
   size_t arm64_asm_size(Arm64Asm *a);
   
   // Labels
   int arm64_new_label(Arm64Asm *a);
   void arm64_bind_label(Arm64Asm *a, int label);
   
   // Instructions
   void arm64_mov_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg src);
   void arm64_mov_r64_imm(Arm64Asm *a, Arm64Reg dst, int64_t imm);
   void arm64_ldr_r64_mem(Arm64Asm *a, Arm64Reg dst, Arm64Reg base, int32_t off);
   void arm64_str_r64_mem(Arm64Asm *a, Arm64Reg src, Arm64Reg base, int32_t off);
   void arm64_add_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m);
   void arm64_add_r64_r64_imm(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, int32_t imm);
   void arm64_sub_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m);
   void arm64_sub_r64_r64_imm(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, int32_t imm);
   void arm64_cmp_r64_r64(Arm64Asm *a, Arm64Reg n, Arm64Reg m);
   void arm64_cmp_r64_imm(Arm64Asm *a, Arm64Reg n, int32_t imm);
   void arm64_b(Arm64Asm *a, int label);
   void arm64_b_cond(Arm64Asm *a, int cond, int label);
   void arm64_bl(Arm64Asm *a, void *target);
   void arm64_blr(Arm64Asm *a, Arm64Reg reg);
   void arm64_ret(Arm64Asm *a);
   void arm64_stp(Arm64Asm *a, Arm64Reg r1, Arm64Reg r2, Arm64Reg base, int32_t off);
   void arm64_ldp(Arm64Asm *a, Arm64Reg r1, Arm64Reg r2, Arm64Reg base, int32_t off);
   ```

2. Implement ARM64 instruction encoding
   - File: `src/jit/arm64/asm_arm64.c`
   - Implement all functions from header

   Key ARM64 encoding patterns:
   - Data processing: `sf | opc | 01011 | shift | 0 | Rm | imm6 | Rn | Rd`
   - Load/Store: `size | 111 | V | 00 | opc | imm12 | Rn | Rt`
   - Branch: `cond | 0101010 | imm19 | 0`

3. Add comprehensive tests for assembler
   - File: `test/tests/jit-arm64-asm.scm`
   - Test each instruction encoding

---

### Phase 3: ARM64 Code Generator (MVP)

**Objective**: Generate native ARM64 code for a minimal set of bytecode instructions.

**Steps**:

1. Create code generator structure
   - File: `src/jit/arm64/codegen_arm64.c`
   - Implement instruction translation

   ```c
   typedef struct Arm64CodeGenRec {
       Arm64Asm *asm;
       SgCodeBuilder *cb;
       // Register state tracking
       int vmLoaded;     // Is VM pointer in JIT_VM?
       // Label mapping for bytecode branches
       int *bcLabels;    // bcLabels[i] = label for bytecode index i
   } Arm64CodeGen;
   
   JitCompiledCode arm64_compile(SgCodeBuilder *cb);
   ```

2. Implement function prologue/epilogue
   - Save callee-saved registers (X19-X28, LR)
   - Load VM registers into ARM64 registers
   - Restore on return

   ```c
   void arm64_emit_prologue(Arm64CodeGen *gen) {
       Arm64Asm *a = gen->asm;
       // Save frame pointer and link register
       arm64_stp(a, X29, X30, SP, -16);  // stp x29, x30, [sp, #-16]!
       arm64_mov_r64_r64(a, X29, SP);     // mov x29, sp
       // Save callee-saved registers we'll use
       arm64_stp(a, X19, X20, SP, -16);
       arm64_stp(a, X21, X22, SP, -16);
       // Load VM pointer (passed in X0)
       arm64_mov_r64_r64(a, JIT_VM, X0);
       // Load closure (passed in X1)
       arm64_mov_r64_r64(a, JIT_CL, X1);
       // Load VM registers from VM struct
       arm64_ldr_r64_mem(a, JIT_SP, JIT_VM, offsetof(SgVM, sp));
       arm64_ldr_r64_mem(a, JIT_FP, JIT_VM, offsetof(SgVM, fp));
   }
   
   void arm64_emit_epilogue(Arm64CodeGen *gen) {
       Arm64Asm *a = gen->asm;
       // Store VM registers back
       arm64_str_r64_mem(a, JIT_SP, JIT_VM, offsetof(SgVM, sp));
       arm64_str_r64_mem(a, JIT_FP, JIT_VM, offsetof(SgVM, fp));
       // Result is in X0 (JIT_TEMP1)
       // Restore callee-saved registers
       arm64_ldp(a, X21, X22, SP, 16);
       arm64_ldp(a, X19, X20, SP, 16);
       arm64_ldp(a, X29, X30, SP, 16);
       arm64_ret(a);
   }
   ```

3. Implement MVP instruction set

   **Priority 1 - Local Variables and Constants**:
   - `LREF` - Load local variable
   - `LSET` - Store local variable  
   - `LREF_PUSH` - Load and push
   - `CONST` - Load constant
   - `CONSTI` - Load small integer
   - `CONST_PUSH` - Load and push constant
   - `PUSH` - Push accumulator

   **Priority 2 - Arithmetic (fast path for fixnums)**:
   - `ADD`, `ADDI` - Addition
   - `SUB`, `SUBI` - Subtraction
   - `MUL`, `MULI` - Multiplication (optional for MVP)
   - `NEG` - Negation

   **Priority 3 - Comparisons and Branches**:
   - `NUM_EQ`, `NUM_LT`, `NUM_LE`, `NUM_GT`, `NUM_GE`
   - `TEST`, `JUMP`
   - `BNNUME`, `BNLT`, `BNLE`, `BNGT`, `BNGE`

   **Priority 4 - Function Calls**:
   - `RET` - Return from function
   - `CALL`, `TAIL_CALL` - Call procedures (deferred to C helper)

   Example implementation:
   ```c
   void arm64_emit_lref(Arm64CodeGen *gen, int index) {
       Arm64Asm *a = gen->asm;
       // AC = FP[index]
       arm64_ldr_r64_mem(a, JIT_TEMP1, JIT_FP, index * sizeof(SgObject));
   }
   
   void arm64_emit_add(Arm64CodeGen *gen) {
       Arm64Asm *a = gen->asm;
       int slow_path = arm64_new_label(a);
       int done = arm64_new_label(a);
       
       // Pop operand from stack
       arm64_ldr_r64_mem(a, JIT_TEMP2, JIT_SP, -8);  // temp2 = SP[-1]
       arm64_sub_r64_r64_imm(a, JIT_SP, JIT_SP, 8);  // SP--
       
       // Check if both are fixnums (tag = 0)
       arm64_orr_r64_r64_r64(a, JIT_TEMP3, JIT_TEMP1, JIT_TEMP2);
       arm64_tst_r64_imm(a, JIT_TEMP3, SG_TAG_MASK);
       arm64_b_cond(a, ARM64_NE, slow_path);
       
       // Fast path: add fixnums
       arm64_adds_r64_r64_r64(a, JIT_TEMP1, JIT_TEMP1, JIT_TEMP2);
       arm64_b_cond(a, ARM64_VS, slow_path);  // Overflow
       arm64_b(a, done);
       
       // Slow path: call Sg_Add
       arm64_bind_label(a, slow_path);
       arm64_mov_r64_r64(a, X0, JIT_TEMP2);  // arg1
       arm64_mov_r64_r64(a, X1, JIT_TEMP1);  // arg2
       arm64_bl(a, Sg_Add);
       arm64_mov_r64_r64(a, JIT_TEMP1, X0);  // result
       
       arm64_bind_label(a, done);
   }
   ```

4. Implement fallback to interpreter
   - When JIT compilation fails
   - For unsupported instructions
   - Mixed mode: partial JIT with interpreter fallback

---

### Phase 4: Architecture Refactoring - Modular Instruction Emission

**Objective**: Refactor the JIT compiler to use a modular architecture that separates the instruction loop from platform-specific code generation. This prepares the codebase for adding new platform backends (x86_64, x86, ARM32) without duplicating the instruction dispatch logic.

**Current State**:
- `codegen_arm64.c` contains both the instruction loop and ARM64-specific emit code
- Adding new platforms would require duplicating the entire instruction switch

**Target State**:
- `jit_compile.c` contains the instruction loop (platform-agnostic)
- Per-instruction emit handlers (`Sg__JitEmit_*`) are implemented per platform
- Combined instructions can call primitive handlers or use optimized implementations

**Steps**:

1. Create platform-independent emit header
   - File: `src/jit/jit_emit.h`
   - Declare all `Sg__JitEmit_*` function prototypes
   ```c
   // Lifecycle
   SG_EXTERN int Sg__JitEmit_Prologue(JitContext *ctx);
   SG_EXTERN int Sg__JitEmit_Epilogue(JitContext *ctx);
   
   // Basic instructions
   SG_EXTERN int Sg__JitEmit_NOP(JitContext *ctx);
   SG_EXTERN int Sg__JitEmit_CONST(JitContext *ctx, SgObject val);
   SG_EXTERN int Sg__JitEmit_CONSTI(JitContext *ctx, intptr_t val);
   SG_EXTERN int Sg__JitEmit_LREF(JitContext *ctx, int index);
   SG_EXTERN int Sg__JitEmit_LSET(JitContext *ctx, int index);
   SG_EXTERN int Sg__JitEmit_PUSH(JitContext *ctx);
   
   // Arithmetic
   SG_EXTERN int Sg__JitEmit_ADD(JitContext *ctx);
   SG_EXTERN int Sg__JitEmit_ADDI(JitContext *ctx, intptr_t val);
   SG_EXTERN int Sg__JitEmit_SUB(JitContext *ctx);
   SG_EXTERN int Sg__JitEmit_SUBI(JitContext *ctx, intptr_t val);
   
   // Comparisons
   SG_EXTERN int Sg__JitEmit_NUM_EQ(JitContext *ctx);
   SG_EXTERN int Sg__JitEmit_NUM_LT(JitContext *ctx);
   SG_EXTERN int Sg__JitEmit_NUM_LE(JitContext *ctx);
   SG_EXTERN int Sg__JitEmit_NUM_GT(JitContext *ctx);
   SG_EXTERN int Sg__JitEmit_NUM_GE(JitContext *ctx);
   
   // Control flow
   SG_EXTERN int Sg__JitEmit_TEST(JitContext *ctx, int targetPc);
   SG_EXTERN int Sg__JitEmit_JUMP(JitContext *ctx, int targetPc);
   SG_EXTERN int Sg__JitEmit_RET(JitContext *ctx);
   
   // Combined instructions - default implementations available
   SG_EXTERN int Sg__JitEmit_LREF_PUSH(JitContext *ctx, int index);
   SG_EXTERN int Sg__JitEmit_CONST_PUSH(JitContext *ctx, SgObject val);
   SG_EXTERN int Sg__JitEmit_CONSTI_PUSH(JitContext *ctx, intptr_t val);
   ```

2. Define JitContext structure
   - File: `src/jit/jit_internal.h`
   - Platform-agnostic context with platform-specific pointer
   ```c
   typedef struct JitContextRec {
       SgCodeBuilder *cb;
       SgJitCodeBuffer *buf;
       void *platform;       // Platform-specific (Arm64CodeGen*, X64CodeGen*, etc.)
       
       // Label management for branches
       int labelCount;
       int *pcToLabel;       // Maps bytecode PC to label index
       int *labelBound;      // Whether label is already bound
   } JitContext;
   
   // Platform must implement
   SG_EXTERN void* Sg__JitPlatformInit(JitContext *ctx);
   SG_EXTERN void Sg__JitPlatformCleanup(void *platform);
   SG_EXTERN SgJitCompiledCode Sg__JitPlatformFinalize(JitContext *ctx);
   ```

3. Move instruction loop to `jit_compile.c`
   - Create label pre-scan pass for branch targets
   - Implement main compilation loop calling `Sg__JitEmit_*`
   ```c
   SgJitCompiledCode Sg_JitCompile(SgCodeBuilder *cb)
   {
       JitContext ctx;
       ctx.cb = cb;
       ctx.buf = Sg_AllocJitBuffer(JIT_BUFFER_SIZE);
       if (!ctx.buf) return NULL;
       
       ctx.platform = Sg__JitPlatformInit(&ctx);
       if (!ctx.platform) { Sg_FreeJitBuffer(ctx.buf); return NULL; }
       
       // Pre-scan for branch targets to create labels
       if (!jit_prescan_labels(&ctx)) goto fail;
       
       // Emit prologue
       Sg_JitMakeWritable(ctx.buf);
       if (!Sg__JitEmit_Prologue(&ctx)) goto fail;
       
       // Main instruction loop
       int pc = 0;
       while (pc < cb->codeSize) {
           SgWord insn = cb->code[pc];
           int opcode = INSN_OP(insn);
           
           // Bind label if this PC is a branch target
           jit_maybe_bind_label(&ctx, pc);
           
           switch (opcode) {
           case NOP:
               if (!Sg__JitEmit_NOP(&ctx)) goto fail;
               pc++;
               break;
           case LREF:
               if (!Sg__JitEmit_LREF(&ctx, INSN_VAL1(insn))) goto fail;
               pc++;
               break;
           case PUSH:
               if (!Sg__JitEmit_PUSH(&ctx)) goto fail;
               pc++;
               break;
           case LREF_PUSH:
               if (!Sg__JitEmit_LREF_PUSH(&ctx, INSN_VAL1(insn))) goto fail;
               pc++;
               break;
           // ... more cases
           default:
               goto fail; // Unsupported
           }
       }
       
       if (!Sg__JitEmit_Epilogue(&ctx)) goto fail;
       
       return Sg__JitPlatformFinalize(&ctx);
       
   fail:
       Sg__JitPlatformCleanup(ctx.platform);
       Sg_FreeJitBuffer(ctx.buf);
       return NULL;
   }
   ```

4. Refactor ARM64 code generator
   - File: `src/jit/arm64/emit_arm64.c` (rename from codegen_arm64.c)
   - Remove instruction loop, keep only emit functions
   - Implement all `Sg__JitEmit_*` functions
   ```c
   typedef struct {
       Arm64Asm *a;
       JitContext *ctx;
   } Arm64CodeGen;
   
   void* Sg__JitPlatformInit(JitContext *ctx)
   {
       Arm64CodeGen *gen = malloc(sizeof(Arm64CodeGen));
       gen->ctx = ctx;
       gen->a = arm64_asm_new(ctx->buf->code, ctx->buf->size);
       return gen;
   }
   
   int Sg__JitEmit_LREF(JitContext *ctx, int index)
   {
       Arm64CodeGen *gen = ctx->platform;
       arm64_ldr_r64_mem(gen->a, JIT_REG_TEMP1, JIT_REG_SCHFP, 
                         index * sizeof(SgObject));
       return 1;
   }
   
   int Sg__JitEmit_PUSH(JitContext *ctx)
   {
       Arm64CodeGen *gen = ctx->platform;
       arm64_str_r64_mem_post(gen->a, JIT_REG_TEMP1, JIT_REG_SCHSP,
                              sizeof(SgObject));
       return 1;
   }
   
   // Optimized combined version
   int Sg__JitEmit_LREF_PUSH(JitContext *ctx, int index)
   {
       // Can call primitives or emit optimized sequence
       Arm64CodeGen *gen = ctx->platform;
       arm64_ldr_r64_mem(gen->a, JIT_REG_TEMP1, JIT_REG_SCHFP,
                         index * sizeof(SgObject));
       arm64_str_r64_mem_post(gen->a, JIT_REG_TEMP1, JIT_REG_SCHSP,
                              sizeof(SgObject));
       return 1;
   }
   ```

5. Create default implementations for combined instructions
   - File: `src/jit/jit_emit_default.c`
   - Weak implementations that call primitives
   ```c
   // Default: call both primitives
   // Platforms can override with optimized versions
   __attribute__((weak))
   int Sg__JitEmit_LREF_PUSH(JitContext *ctx, int index)
   {
       if (!Sg__JitEmit_LREF(ctx, index)) return 0;
       if (!Sg__JitEmit_PUSH(ctx)) return 0;
       return 1;
   }
   ```

6. Update CMakeLists.txt
   ```cmake
   IF(HAVE_JIT)
     SET(JIT_SOURCES
       jit/jit_memory.c
       jit/jit_compile.c
       jit/jit_emit_default.c)
     IF(JIT_ARCH STREQUAL "arm64")
       LIST(APPEND JIT_SOURCES
         jit/arm64/asm_arm64.c
         jit/arm64/emit_arm64.c
         jit/arm64/disasm_arm64.c)
     ELSEIF(JIT_ARCH STREQUAL "x86_64")
       LIST(APPEND JIT_SOURCES
         jit/x86_64/asm_x86_64.c
         jit/x86_64/emit_x86_64.c)
     ENDIF()
   ENDIF()
   ```

**Verification**:
- All existing tests pass
- JIT compilation produces same results
- Disassembler still works

---

### Phase 5: Integration and Testing

**Objective**: Integrate JIT with VM and ensure all tests pass.

**Steps**:

1. Add command-line options
   - `--no-jit` - Disable JIT compilation
   - `--jit-threshold=N` - Set hot code threshold
   - `--jit-verbose` - Print JIT compilation info

   Modify `src/main.c`:
   ```c
   {"no-jit", 0, 0, 'J'},
   {"jit-threshold", 1, 0, 'T'},
   {"jit-verbose", 0, 0, 'V'},
   ```

2. Reset JIT state on cache load
   - Modify `src/cache.c` `Sg_MakeCodeBuilderFromCache`
   - Initialize `jitCode = NULL`, `callCount = 0`

3. Run test suite with JIT enabled
   - Execute `ctest --output-on-failure`
   - Identify and fix any failures

4. Add JIT-specific tests
   - File: `test/tests/jit.scm`
   - Test hot code compilation triggers
   - Test arithmetic operations
   - Test function calls and returns

---

### Phase 6: x86_64 Code Generator

**Objective**: Add x86_64 support for Linux and macOS Intel.

**Steps**:

1. Create x86_64 assembler
   - Files: `src/jit/x86_64/asm_x86_64.h`, `src/jit/x86_64/asm_x86_64.c`
   - REX prefixes, ModR/M encoding

2. Implement emit functions (following modular architecture)
   - File: `src/jit/x86_64/emit_x86_64.c`
   - Same `Sg__JitEmit_*` functions as ARM64

3. Register mapping for System V AMD64 ABI:
   ```c
   #define JIT_VM      RBX   // Callee-saved
   #define JIT_SP      R12   // Callee-saved
   #define JIT_FP      R13   // Callee-saved
   #define JIT_CL      R14   // Callee-saved
   #define JIT_TEMP1   RAX   // Return value
   #define JIT_TEMP2   RCX
   #define JIT_TEMP3   RDX
   ```

---

### Phase 7: 32-bit Platforms (Future)

**Objective**: Add x86 and ARM32 support.

**Steps**:
1. x86 assembler and emit functions
2. ARM32 assembler and emit functions
3. Handle limited register count

---

## Estimation

| Phase | Effort | Notes |
|-------|--------|-------|
| Phase 1: Infrastructure | 2-3 days | Memory, hooks, CMake |
| Phase 2: ARM64 Assembler | 3-4 days | Instruction encoding |
| Phase 3: ARM64 CodeGen | 4-5 days | MVP instructions |
| Phase 4: Architecture Refactoring | 2-3 days | Modular emit system |
| Phase 5: Integration | 2-3 days | Testing, debugging |
| Phase 6: x86_64 | 3-4 days | Similar to ARM64, uses modular arch |
| Phase 7: 32-bit | 4-6 days | Lower priority |
| **Total MVP (Phases 1-5)** | **13-18 days** | ARM64 macOS |
| **Total with x86_64** | **16-22 days** | + Linux/macOS Intel |

## Testing Strategy

> **Note**: This project implements tests at the Scheme level, not C-level unit tests.
> All test files are located in `test/tests/` directory.

### Scheme Tests

1. **JIT Triggering Test** (`test/tests/jit.scm`):
   ```scheme
   ;; Test that hot functions get JIT-compiled
   (define (hot-loop n)
     (let loop ((i 0) (sum 0))
       (if (< i n)
           (loop (+ i 1) (+ sum i))
           sum)))
   
   ;; Run enough times to trigger JIT
   (test-equal "hot-loop-sum" 4950 (hot-loop 100))
   (test-equal "hot-loop-repeated" 4950 (hot-loop 100))
   ```

2. **Arithmetic Operations**:
   ```scheme
   ;; Test fixnum arithmetic in JIT
   (define (add-test) (+ 1 2))
   (define (sub-test) (- 10 3))
   (define (mul-test) (* 4 5))
   
   ;; Force JIT compilation
   (do ((i 0 (+ i 1))) ((>= i 200))
     (add-test) (sub-test) (mul-test))
   
   (test-equal "jit-add" 3 (add-test))
   (test-equal "jit-sub" 7 (sub-test))
   (test-equal "jit-mul" 20 (mul-test))
   ```

3. **Overflow Handling**:
   ```scheme
   ;; Test overflow from fixnum to bignum
   (define (overflow-test n)
     (let loop ((i 0) (acc 1))
       (if (< i n)
           (loop (+ i 1) (* acc 2))
           acc)))
   
   (test-equal "overflow" 1267650600228229401496703205376 (overflow-test 100))
   ```

### Cross-component Tests

1. Run full test suite with JIT enabled:
   ```shell
   ctest --output-on-failure
   ```

2. Run with different thresholds:
   ```shell
   ./build/sagittarius --jit-threshold=10 -Llib test/runner.scm
   ```

### Manual Verification

1. Verify JIT compilation occurs:
   ```shell
   ./build/sagittarius --jit-verbose -e '(define (f x) (+ x 1)) (do ((i 0 (+ i 1))) ((>= i 200)) (f i))'
   ```

2. Compare performance:
   ```shell
   time ./build/sagittarius bench/fib.scm
   time ./build/sagittarius --no-jit bench/fib.scm
   ```

## Code Quality Guidelines

### Maintainability
- Follow existing `src/` code style (K&R braces, 2-space indent in some files)
- Use macros sparingly - prefer inline functions for type safety
- Keep assembler code well-documented (ARM64 encoding is complex)

### Comments
- Document each ARM64/x86_64 instruction encoding
- Explain register allocation decisions
- Note ABI requirements

### Example Style
```c
/*
 * arm64_emit_lref - Generate code for LREF instruction
 * 
 * LREF loads a local variable from the frame.
 * val1: index into frame (FP[val1])
 * 
 * Generated code:
 *   ldr x0, [x21, #index*8]   ; AC = FP[index]
 */
static void arm64_emit_lref(Arm64CodeGen *gen, int index) {
    Arm64Asm *a = gen->asm;
    arm64_ldr_r64_mem(a, JIT_TEMP1, JIT_FP, index * sizeof(SgObject));
}
```

## Implementation Checklist

### Phase 1: Infrastructure
- [ ] Create `src/jit/` directory structure
- [ ] Implement `jit.h` header
- [ ] Implement `jit_memory.c` with macOS MAP_JIT support
- [ ] Add JIT fields to `SgCodeBuilder`
- [ ] Integrate hot code detection in `vm.c`
- [ ] Update `CMakeLists.txt`

### Phase 2: ARM64 Assembler
- [ ] Create `src/jit/arm64/asm_arm64.h`
- [ ] Implement basic instructions in `asm_arm64.c`
- [ ] Add label management
- [ ] Test instruction encoding

### Phase 3: ARM64 Code Generator
- [ ] Implement prologue/epilogue
- [ ] Implement LREF, LSET, PUSH instructions
- [ ] Implement CONST, CONSTI instructions
- [ ] Implement ADD, SUB with fixnum fast path
- [ ] Implement comparisons and branches
- [ ] Implement RET instruction
- [ ] Add fallback for unsupported instructions

### Phase 4: Integration
- [ ] Add --no-jit flag
- [ ] Reset JIT state on cache load
- [ ] Run test suite
- [ ] Fix any failures
- [ ] Add JIT-specific tests

### Phase 5: x86_64
- [ ] Create x86_64 assembler
- [ ] Implement x86_64 code generator
- [ ] Test on Linux/macOS Intel

## User Feedback

After implementation is complete:
1. Present the implementation to the user for review
2. Address any feedback or requested changes
3. Once approved, update `.copilot/README.md` to mark this feature as completed

## File Structure

```
src/jit/
├── jit.h                  # Public interface
├── jit_memory.c           # Memory allocation
├── jit_compile.c          # Main compilation driver
├── jit_common.h           # Shared definitions
├── arm64/
│   ├── asm_arm64.h        # ARM64 assembler
│   ├── asm_arm64.c
│   └── codegen_arm64.c    # ARM64 code generator
└── x86_64/
    ├── asm_x86_64.h       # x86_64 assembler
    ├── asm_x86_64.c
    └── codegen_x86_64.c   # x86_64 code generator
```
