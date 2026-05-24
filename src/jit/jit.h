/* jit.h                                           -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2026  Takashi Kato <ktakashi@ymail.com>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef SAGITTARIUS_JIT_H_
#define SAGITTARIUS_JIT_H_

#include "../sagittarius/private/sagittariusdefs.h"

#ifdef HAVE_JIT

#include <stddef.h>
#include <stdint.h>

SG_CDECL_BEGIN

/*
 * JIT Code Buffer
 *
 * Manages executable memory for JIT-compiled code.
 */
typedef struct SgJitCodeBufferRec {
  uint8_t *code;         /* Executable memory region */
  size_t   size;         /* Total allocated size */
  size_t   used;         /* Bytes used */
} SgJitCodeBuffer;

/*
 * JIT Compiled Function Signature
 *
 * JIT-compiled code is called with:
 *   - vm: The current VM (provides access to registers via pointer)
 *   - closure: The closure being executed (for free variables)
 *
 * Returns: The result value (stored in AC register)
 *
 * The JIT code accesses Scheme stack through vm->fp and vm->sp pointers,
 * which point to heap-allocated memory, not the native stack.
 */
struct SgVMRec;
struct SgClosureRec;
typedef SgObject (*SgJitCompiledCode)(struct SgVMRec *vm,
				      struct SgClosureRec *closure);

/*
 * JIT Memory Management
 */

/* Allocate a JIT code buffer with the given size */
SG_EXTERN SgJitCodeBuffer* Sg_AllocJitBuffer(size_t size);

/* Free a JIT code buffer */
SG_EXTERN void Sg_FreeJitBuffer(SgJitCodeBuffer *buf);

/* Resize a JIT code buffer. Returns 0 on success, -1 on failure.
   After resizing, buf->code and buf->size are updated to reflect
   the new allocation. The old content is preserved. */
SG_EXTERN int Sg_ResizeJitBuffer(SgJitCodeBuffer *buf, size_t newSize);

/* Make buffer writable (for code generation) */
SG_EXTERN void Sg_JitMakeWritable(SgJitCodeBuffer *buf);

/* Make buffer executable (after code generation) */
SG_EXTERN void Sg_JitMakeExecutable(SgJitCodeBuffer *buf);


/*
 * JIT Compilation
 */

struct SgCodeBuilderRec;

/* Compile a code builder to native code. Returns NULL on failure. */
SG_EXTERN SgJitCompiledCode Sg_JitCompile(struct SgCodeBuilderRec *cb);

/* Check if JIT compilation is available for the current platform */
SG_EXTERN int Sg_JitAvailable(void);


/*
 * JIT Configuration
 */

/* Default threshold for hot code detection */
/* Default threshold for auto-JIT (call count before compilation)
 * Set very high (1M) to effectively disable auto-JIT by default.
 * Auto-JIT can cause crashes when exceptions occur because the JIT
 * code's callee-saved registers are not restored on longjmp.
 * Users should use explicit (jit-compile! proc) for now.
 */
#define SG_JIT_DEFAULT_THRESHOLD 10000

/* Enable/disable JIT compilation */
SG_EXTERN void Sg_SetJitEnabled(int enabled);
SG_EXTERN int  Sg_JitEnabled(void);

/* Set/get hot code threshold */
SG_EXTERN void Sg_SetJitThreshold(int threshold);
SG_EXTERN int  Sg_GetJitThreshold(void);

/* Increment call count for hot code detection */
SG_EXTERN void Sg_JitIncrementCallCount(SgCodeBuilder *cb);

/* Verbose mode for debugging */
SG_EXTERN void Sg_SetJitVerbose(int verbose);
SG_EXTERN int  Sg_JitVerbose(void);

/* Profiling */
SG_EXTERN void Sg_JitProfileReset(void);
SG_EXTERN void Sg_JitProfileEnable(int enable);
SG_EXTERN void Sg_JitProfilePrint(struct SgPortRec *port);


/*
 * JIT Disassembly
 */
struct SgPortRec;

/* Disassemble JIT code for a closure to a port */
SG_EXTERN void Sg_JitDisassemble(struct SgCodeBuilderRec *cb, struct SgPortRec *port);


/*
 * JIT Flags for CodeBuilder
 */
#define SG_JIT_FLAG_COMPILED   (1 << 0)  /* Successfully JIT compiled */
#define SG_JIT_FLAG_FAILED     (1 << 1)  /* JIT compilation failed */
#define SG_JIT_FLAG_NEVER      (1 << 2)  /* Never JIT (contains unsupported ops) */
#define SG_JIT_FLAG_COMPILING  (1 << 3)  /* Currently being compiled (prevents double-compilation) */


/*
 * JIT Helper Functions
 * Called from JIT-compiled code for operations that need C support.
 */

/* Push a continuation frame before a non-tail call */
SG_EXTERN void Sg__JitPushFrame(struct SgVMRec *vm, SgWord *returnPc);

/* Look up a global variable (GREF) */
SG_EXTERN SgObject Sg__JitGref(SgObject id);

/* Create a box for mutable variable (BOX) */
SG_EXTERN SgObject Sg__JitMakeBox(SgObject value);

/* Call a global procedure (GREF_CALL) */
SG_EXTERN SgObject Sg__JitGrefCall(struct SgVMRec *vm, int argc, SgObject id);

/* Tail-call a global procedure (GREF_TAIL_CALL) */
SG_EXTERN SgObject Sg__JitGrefTailCall(struct SgVMRec *vm, int argc, SgObject id);

/* Call a procedure (CALL) - proc already in AC */
SG_EXTERN SgObject Sg__JitCall(struct SgVMRec *vm, int argc, SgObject proc);

/* Tail-call a procedure (TAIL_CALL) - proc already in AC */
SG_EXTERN SgObject Sg__JitTailCall(struct SgVMRec *vm, int argc, SgObject proc);

/* Call a SUBR directly without C continuation boundary */
SG_EXTERN SgObject Sg__JitCallSubr(struct SgVMRec *vm, int argc, SgObject proc);

/* Tail-call a SUBR directly */
SG_EXTERN SgObject Sg__JitTailCallSubr(struct SgVMRec *vm, int argc, SgObject proc);

/* Call a generic function directly */
SG_EXTERN SgObject Sg__JitCallGeneric(struct SgVMRec *vm, int argc, SgObject generic);

/* Tail-call a generic function directly */
SG_EXTERN SgObject Sg__JitTailCallGeneric(struct SgVMRec *vm, int argc, SgObject generic);

/* Apply a procedure to a list of arguments (APPLY) */
SG_EXTERN SgObject Sg__JitApply(struct SgVMRec *vm, int nargc, SgObject listArg, int isTail);

/* Return multiple values (VALUES) */
SG_EXTERN SgObject Sg__JitValues(struct SgVMRec *vm, int nvalues, SgObject lastVal);

/* Receive multiple values (RECEIVE) */
SG_EXTERN SgObject Sg__JitReceive(struct SgVMRec *vm, int reqCount, int optCount);

/*
 * JIT Context Helpers for Exception Recovery
 *
 * When JIT code calls C helpers that might throw exceptions,
 * we save the JIT register state before the call so it can be
 * restored after longjmp bypasses the JIT epilogue.
 */

/* Save JIT context before calling a potentially-throwing helper */
#define SG_JIT_SAVE_CONTEXT(vm, sp, fp, cl, depth)      \
  do {                                                   \
    (vm)->jitContext.active = 1;                         \
    (vm)->jitContext.savedSp = (sp);                     \
    (vm)->jitContext.savedFp = (fp);                     \
    (vm)->jitContext.savedCl = (cl);                     \
    (vm)->jitContext.savedDepth = (depth);               \
  } while (0)

/* Clear JIT context after helper returns normally */
#define SG_JIT_CLEAR_CONTEXT(vm)                         \
  do {                                                   \
    (vm)->jitContext.active = 0;                         \
  } while (0)

/* Check if JIT context is active (for exception handler) */
#define SG_JIT_CONTEXT_ACTIVE(vm) ((vm)->jitContext.active)

/* Initialize JIT context fields in a new VM */
SG_EXTERN void Sg_InitJitContext(struct SgVMRec *vm);

/*
 * JIT Yield Marker
 *
 * When JIT code needs to call a non-JIT closure, it yields to the interpreter
 * by returning this special marker. The VM loop checks for this marker and
 * continues with interpreter execution instead of doing RET_INSN.
 *
 * The marker is a unique object that cannot be confused with normal return values.
 * We use a tagged pointer that points to an invalid address.
 */
#define SG_JIT_YIELD_MARKER ((SgObject)(uintptr_t)0xDEADBEEF00000007UL)
#define SG_JIT_YIELD_P(obj) ((obj) == SG_JIT_YIELD_MARKER)


SG_CDECL_END

#else /* !HAVE_JIT */

/* Stubs when JIT is not available */
#define Sg_JitAvailable()       (0)
#define Sg_JitEnabled()         (0)
#define Sg_SetJitEnabled(x)     ((void)0)
#define Sg_SetJitThreshold(x)   ((void)0)
#define Sg_GetJitThreshold()    (0)
#define Sg_JitIncrementCallCount(cb) ((void)0)
#define Sg_SetJitVerbose(x)     ((void)0)
#define Sg_JitVerbose()         (0)
#define Sg_JitProfileReset()    ((void)0)
#define Sg_JitProfileEnable(x)  ((void)0)
#define Sg_JitProfilePrint(p)   ((void)0)
#define Sg_JitDisassemble(cb, port) ((void)0)

#endif /* HAVE_JIT */

#endif /* SAGITTARIUS_JIT_H_ */
