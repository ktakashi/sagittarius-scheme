/* jit_compile.c                                   -*- mode:c; coding:utf-8; -*-
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

#include "jit.h"

#ifdef HAVE_JIT

#include "../sagittarius.h"
#include "../sagittarius/private/code.h"
#include "../sagittarius/private/instruction.h"

/* Global JIT configuration */
static int jit_enabled = 1;
static int jit_threshold = SG_JIT_DEFAULT_THRESHOLD;
static int jit_verbose = 0;  /* Disabled by default */

/* Initial buffer size for JIT code (4KB) */
#define JIT_INITIAL_BUFFER_SIZE 4096

int Sg_JitAvailable(void)
{
#if defined(JIT_ARCH_arm64) || defined(JIT_ARCH_x86_64) || \
    defined(JIT_ARCH_x86) || defined(JIT_ARCH_arm)
  return 1;
#else
  return 0;
#endif
}

void Sg_SetJitEnabled(int enabled)
{
  jit_enabled = enabled;
}

int Sg_JitEnabled(void)
{
  return jit_enabled && Sg_JitAvailable();
}

void Sg_SetJitThreshold(int threshold)
{
  if (threshold > 0) {
    jit_threshold = threshold;
  }
}

int Sg_GetJitThreshold(void)
{
  return jit_threshold;
}

void Sg_SetJitVerbose(int verbose)
{
  jit_verbose = verbose;
}

int Sg_JitVerbose(void)
{
  return jit_verbose;
}

SgJitCompiledCode Sg_JitCompile(SgCodeBuilder *cb)
{
  SgJitCodeBuffer *buf;
  SgJitCompiledCode compiled;

  if (!Sg_JitEnabled()) {
    return NULL;
  }

  /* Allocate code buffer */
  buf = Sg_AllocJitBuffer(JIT_INITIAL_BUFFER_SIZE);
  if (buf == NULL) {
    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
		UC("JIT: Failed to allocate code buffer for %A\n"),
		SG_CODE_BUILDER_NAME(cb));
    }
    return NULL;
  }

  /* Make buffer writable for code generation */
  Sg_JitMakeWritable(buf);

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: Compiling %A (%d instructions)\n"),
	      SG_CODE_BUILDER_NAME(cb), cb->size);
    /* Print bytecode */
    for (int i = 0; i < cb->size; i++) {
      int opcode = INSN(cb->code[i]);
      Sg_Printf(Sg_StandardErrorPort(),
		UC("  [%d] opcode=%d\n"), i, opcode);
    }
  }

  /* Dispatch to platform-specific compiler */
#if defined(JIT_ARCH_arm64)
  compiled = Sg_JitCompileArm64(cb, buf);
#elif defined(JIT_ARCH_x86_64)
  compiled = Sg_JitCompileX86_64(cb, buf);
#elif defined(JIT_ARCH_x86)
  compiled = Sg_JitCompileX86(cb, buf);
#elif defined(JIT_ARCH_arm)
  compiled = Sg_JitCompileArm(cb, buf);
#else
  compiled = NULL;
#endif

  if (compiled == NULL) {
    /* Compilation failed - restore executable protection before freeing */
    Sg_JitMakeExecutable(buf);
    Sg_FreeJitBuffer(buf);
    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
		UC("JIT: Compilation failed for %A\n"),
		SG_CODE_BUILDER_NAME(cb));
    }
    return NULL;
  }

  /* Make code executable */
  Sg_JitMakeExecutable(buf);

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: Successfully compiled %A (%zu bytes), code at %p\n"),
	      SG_CODE_BUILDER_NAME(cb), buf->used, buf->code);
  }

  return compiled;
}

#endif /* HAVE_JIT */
