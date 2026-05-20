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
#include "jit_internal.h"
#include "jit_emit.h"

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
  return 1;  /* This file is only compiled when JIT is available */
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
  SgJitContext ctx;
  SgJitCompiledCode compiled;
  int pc;

  if (!Sg_JitEnabled()) {
    return NULL;
  }

  /* Initialize context */
  ctx.cb = cb;
  ctx.buf = Sg_AllocJitBuffer(JIT_INITIAL_BUFFER_SIZE);
  if (ctx.buf == NULL) {
    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
		UC("JIT: Failed to allocate code buffer for %A\n"),
		SG_CODE_BUILDER_NAME(cb));
    }
    return NULL;
  }

  /* Allocate label array: one label per bytecode position */
  ctx.pcToLabel = SG_NEW_ARRAY(int, cb->size);
  ctx.labelCount = 0;
  for (int i = 0; i < cb->size; i++) {
    ctx.pcToLabel[i] = ctx.labelCount++;
  }
  /* One more label for epilogue */
  ctx.epilogueLabel = ctx.labelCount++;

  /* Initialize platform-specific context */
  ctx.platform = Sg__JitPlatformInit(&ctx);
  if (ctx.platform == NULL) {
    Sg_FreeJitBuffer(ctx.buf);
    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
		UC("JIT: Failed to initialize platform for %A\n"),
		SG_CODE_BUILDER_NAME(cb));
    }
    return NULL;
  }

  /* Make buffer writable for code generation */
  Sg_JitMakeWritable(ctx.buf);

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: Compiling %A (%d instructions)\n"),
	      SG_CODE_BUILDER_NAME(cb), cb->size);
  }

  /* Emit prologue */
  if (!Sg__JitEmit_Prologue(&ctx)) goto fail;

  /* Main instruction loop */
  pc = 0;
  while (pc < cb->size) {
    SgWord insn = cb->code[pc];
    int opcode = INSN(insn);
    int val1 = INSN_VALUE1(insn);

    /* Bind label for this bytecode position */
    Sg__JitBindLabel(&ctx, ctx.pcToLabel[pc]);

    switch (opcode) {

    case NOP:
      if (!Sg__JitEmit_NOP(&ctx)) goto fail;
      pc++;
      break;

    case UNDEF:
      if (!Sg__JitEmit_UNDEF(&ctx)) goto fail;
      pc++;
      break;

    case CONST:
      if (pc + 1 >= cb->size) goto fail;
      if (!Sg__JitEmit_CONST(&ctx, SG_OBJ(cb->code[pc + 1]))) goto fail;
      pc += 2;
      break;

    case CONSTI:
      if (!Sg__JitEmit_CONSTI(&ctx, val1)) goto fail;
      pc++;
      break;

    case LREF:
      if (!Sg__JitEmit_LREF(&ctx, val1)) goto fail;
      pc++;
      break;

    case LSET:
      if (!Sg__JitEmit_LSET(&ctx, val1)) goto fail;
      pc++;
      break;

    case FREF:
      if (!Sg__JitEmit_FREF(&ctx, val1)) goto fail;
      pc++;
      break;

    case PUSH:
      if (!Sg__JitEmit_PUSH(&ctx)) goto fail;
      pc++;
      break;

    case LREF_PUSH:
      if (!Sg__JitEmit_LREF_PUSH(&ctx, val1)) goto fail;
      pc++;
      break;

    case CONST_PUSH:
      if (pc + 1 >= cb->size) goto fail;
      if (!Sg__JitEmit_CONST_PUSH(&ctx, SG_OBJ(cb->code[pc + 1]))) goto fail;
      pc += 2;
      break;

    case CONSTI_PUSH:
      if (!Sg__JitEmit_CONSTI_PUSH(&ctx, val1)) goto fail;
      pc++;
      break;

    case ADD:
      if (!Sg__JitEmit_ADD(&ctx)) goto fail;
      pc++;
      break;

    case ADDI:
      if (!Sg__JitEmit_ADDI(&ctx, val1)) goto fail;
      pc++;
      break;

    case SUB:
      if (!Sg__JitEmit_SUB(&ctx)) goto fail;
      pc++;
      break;

    case SUBI:
      if (!Sg__JitEmit_SUBI(&ctx, val1)) goto fail;
      pc++;
      break;

    case NUM_EQ:
      if (!Sg__JitEmit_NUM_EQ(&ctx)) goto fail;
      pc++;
      break;

    case NUM_LT:
      if (!Sg__JitEmit_NUM_LT(&ctx)) goto fail;
      pc++;
      break;

    case NUM_LE:
      if (!Sg__JitEmit_NUM_LE(&ctx)) goto fail;
      pc++;
      break;

    case NUM_GT:
      if (!Sg__JitEmit_NUM_GT(&ctx)) goto fail;
      pc++;
      break;

    case NUM_GE:
      if (!Sg__JitEmit_NUM_GE(&ctx)) goto fail;
      pc++;
      break;

    case TEST:
      {
	int targetPc = pc + val1;
	if (!Sg__JitEmit_TEST(&ctx, targetPc)) goto fail;
	pc++;
      }
      break;

    case JUMP:
      {
	int targetPc = pc + val1;
	if (!Sg__JitEmit_JUMP(&ctx, targetPc)) goto fail;
	pc++;
      }
      break;

    case BNNUME:
      {
	int targetPc = pc + val1;
	if (!Sg__JitEmit_BNNUME(&ctx, targetPc)) goto fail;
	pc++;
      }
      break;

    case BNLT:
      {
	int targetPc = pc + val1;
	if (!Sg__JitEmit_BNLT(&ctx, targetPc)) goto fail;
	pc++;
      }
      break;

    case BNLE:
      {
	int targetPc = pc + val1;
	if (!Sg__JitEmit_BNLE(&ctx, targetPc)) goto fail;
	pc++;
      }
      break;

    case BNGT:
      {
	int targetPc = pc + val1;
	if (!Sg__JitEmit_BNGT(&ctx, targetPc)) goto fail;
	pc++;
      }
      break;

    case BNGE:
      {
	int targetPc = pc + val1;
	if (!Sg__JitEmit_BNGE(&ctx, targetPc)) goto fail;
	pc++;
      }
      break;

    case RET:
      if (!Sg__JitEmit_RET(&ctx)) goto fail;
      pc++;
      break;

    default:
      /* Unsupported instruction */
      if (jit_verbose) {
	Sg_Printf(Sg_StandardErrorPort(),
		  UC("JIT: Unsupported opcode %d at pc=%d\n"),
		  opcode, pc);
      }
      goto fail;
    }
  }

  /* Bind epilogue label and emit epilogue */
  Sg__JitBindLabel(&ctx, ctx.epilogueLabel);
  if (!Sg__JitEmit_Epilogue(&ctx)) goto fail;

  /* Resolve forward references */
  if (!Sg__JitPlatformResolve(&ctx)) goto fail;

  /* Update buffer used size */
  ctx.buf->used = Sg__JitGetCodeSize(&ctx);

  /* Finalize and get compiled code */
  compiled = Sg__JitPlatformFinalize(&ctx);
  if (compiled == NULL) goto fail;

  /* Make code executable */
  Sg_JitMakeExecutable(ctx.buf);

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: Successfully compiled %A (%zu bytes), code at %p\n"),
	      SG_CODE_BUILDER_NAME(cb), ctx.buf->used, ctx.buf->code);
  }

  return compiled;

fail:
  Sg__JitPlatformCleanup(ctx.platform);
  /* Restore executable protection before freeing */
  Sg_JitMakeExecutable(ctx.buf);
  Sg_FreeJitBuffer(ctx.buf);
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: Compilation failed for %A\n"),
	      SG_CODE_BUILDER_NAME(cb));
  }
  return NULL;
}

/*
 * Disassemble JIT code
 */

void Sg_JitDisassemble(SgCodeBuilder *cb, SgPort *port)
{
  if (cb == NULL || port == NULL) {
    return;
  }

#ifdef HAVE_JIT
  if (cb->jitCode == NULL) {
    Sg_Printf(port, UC("Not JIT compiled\n"));
    return;
  }

  Sg_Printf(port, UC("JIT code for %A:\n"), SG_CODE_BUILDER_NAME(cb));

  /* For now, we'll disassemble a fixed amount or until we see a RET */
  uint8_t *code = (uint8_t *)cb->jitCode;
  size_t max_size = 1024;  /* 256 instructions * 4 bytes */
  Sg__JitDisasmBuffer(code, max_size, port);

#else
  Sg_Printf(port, UC("JIT not available\n"));
#endif
}

#endif /* HAVE_JIT */
