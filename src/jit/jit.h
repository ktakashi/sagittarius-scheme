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
#define SG_JIT_DEFAULT_THRESHOLD 100

/* Enable/disable JIT compilation */
SG_EXTERN void Sg_SetJitEnabled(int enabled);
SG_EXTERN int  Sg_JitEnabled(void);

/* Set/get hot code threshold */
SG_EXTERN void Sg_SetJitThreshold(int threshold);
SG_EXTERN int  Sg_GetJitThreshold(void);

/* Verbose mode for debugging */
SG_EXTERN void Sg_SetJitVerbose(int verbose);
SG_EXTERN int  Sg_JitVerbose(void);


/*
 * JIT Flags for CodeBuilder
 */
#define SG_JIT_FLAG_COMPILED   (1 << 0)  /* Successfully JIT compiled */
#define SG_JIT_FLAG_FAILED     (1 << 1)  /* JIT compilation failed */
#define SG_JIT_FLAG_NEVER      (1 << 2)  /* Never JIT (contains unsupported ops) */


/*
 * Internal: Platform-specific compilation
 */
#if defined(JIT_ARCH_arm64)
SG_EXTERN SgJitCompiledCode Sg_JitCompileArm64(struct SgCodeBuilderRec *cb,
					       SgJitCodeBuffer *buf);
#elif defined(JIT_ARCH_x86_64)
SG_EXTERN SgJitCompiledCode Sg_JitCompileX86_64(struct SgCodeBuilderRec *cb,
						SgJitCodeBuffer *buf);
#elif defined(JIT_ARCH_x86)
SG_EXTERN SgJitCompiledCode Sg_JitCompileX86(struct SgCodeBuilderRec *cb,
					     SgJitCodeBuffer *buf);
#elif defined(JIT_ARCH_arm)
SG_EXTERN SgJitCompiledCode Sg_JitCompileArm(struct SgCodeBuilderRec *cb,
					     SgJitCodeBuffer *buf);
#endif

SG_CDECL_END

#else /* !HAVE_JIT */

/* Stubs when JIT is not available */
#define Sg_JitAvailable()       (0)
#define Sg_JitEnabled()         (0)
#define Sg_SetJitEnabled(x)     ((void)0)
#define Sg_SetJitThreshold(x)   ((void)0)
#define Sg_GetJitThreshold()    (0)
#define Sg_SetJitVerbose(x)     ((void)0)
#define Sg_JitVerbose()         (0)

#endif /* HAVE_JIT */

#endif /* SAGITTARIUS_JIT_H_ */
