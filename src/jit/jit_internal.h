/* jit_internal.h                                  -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_JIT_INTERNAL_H_
#define SAGITTARIUS_JIT_INTERNAL_H_

#include "jit.h"

#ifdef HAVE_JIT

#include "../sagittarius/private/code.h"

/*
 * JitContext - Platform-agnostic compilation context
 *
 * This structure is passed to all emit functions and contains:
 * - The code builder being compiled
 * - The target code buffer
 * - Platform-specific context pointer
 * - Label management for branch targets
 */
typedef struct SgJitContextRec {
  SgCodeBuilder *cb;      /* Code builder being compiled */
  SgJitCodeBuffer *buf;   /* Target executable memory */
  void *platform;         /* Platform-specific context (Arm64CodeGen*, etc.) */
  
  /* Label management for branches */
  int *pcToLabel;         /* Maps bytecode PC to label index (-1 if no label) */
  int labelCount;         /* Total number of labels allocated */
  int epilogueLabel;      /* Label for function epilogue (stores vm->cl) */
  int yieldEpilogueLabel; /* Label for yield epilogue (does NOT store vm->cl) */
} SgJitContext;

/*
 * Platform Lifecycle Functions
 *
 * Each platform must implement these to initialize and clean up
 * platform-specific state.
 */

/* Initialize platform-specific context. Returns platform data or NULL on failure. */
SG_EXTERN void* Sg__JitPlatformInit(SgJitContext *ctx);

/* Clean up platform-specific context */
SG_EXTERN void Sg__JitPlatformCleanup(void *platform);

/* Finalize compilation and return executable code pointer */
SG_EXTERN SgJitCompiledCode Sg__JitPlatformFinalize(SgJitContext *ctx);

/* Resolve forward references (called before finalize) */
SG_EXTERN int Sg__JitPlatformResolve(SgJitContext *ctx);

/* Bind a label at current code position */
SG_EXTERN void Sg__JitBindLabel(SgJitContext *ctx, int label);

/* Get current code size (for buffer used calculation) */
SG_EXTERN size_t Sg__JitGetCodeSize(SgJitContext *ctx);

#endif /* HAVE_JIT */
#endif /* SAGITTARIUS_JIT_INTERNAL_H_ */
