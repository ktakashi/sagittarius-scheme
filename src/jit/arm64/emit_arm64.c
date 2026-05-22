/* emit_arm64.c                                    -*- mode:c; coding:utf-8; -*-
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

#include "asm_arm64.h"
#include "disasm_arm64.h"

#include "../jit_internal.h"
#include "../jit_emit.h"
#include "../../sagittarius.h"
#include "../../sagittarius/private/code.h"
#include "../../sagittarius/private/vm.h"
#include "../../sagittarius/private/closure.h"

#include <stddef.h>

/*
 * ARM64 Code Generator Context
 */
typedef struct {
  Arm64Asm *a;          /* Assembler context */
  SgJitContext *ctx;    /* Parent JIT context */
  int *labels;          /* Label array (allocated by assembler) */
  int bodyEntryLabel;   /* Label for self-call entry point (after prologue) */
} Arm64CodeGen;


/*
 * VM Register Mapping (callee-saved for persistence across C calls)
 */
#define JIT_REG_VM      ARM64_X19  /* SgVM* pointer */
#define JIT_REG_SCHSP   ARM64_X20  /* Scheme stack pointer (vm->sp) */
#define JIT_REG_SCHFP   ARM64_X21  /* Scheme frame pointer (vm->fp) */
#define JIT_REG_CL      ARM64_X22  /* Current closure */
#define JIT_REG_DEPTH   ARM64_X23  /* Self-call recursion depth counter */
#define JIT_REG_TEMP1   ARM64_X0   /* Temp/Accumulator (also return value) */
#define JIT_REG_TEMP2   ARM64_X1   /* Temp register */
#define JIT_REG_TEMP3   ARM64_X2   /* Temp register */
#define JIT_REG_TEMP4   ARM64_X16  /* Temp register (intra-procedure call) */


/*
 * Tagged Value Constants
 */
#define FIXNUM_TAG   1
#define FIXNUM_MASK  3
#define FIXNUM_SHIFT 2


/*
 * VM Structure Offsets
 */
#define VM_OFFSET_SP         offsetof(SgVM, sp)
#define VM_OFFSET_FP         offsetof(SgVM, fp)
#define VM_OFFSET_AC         offsetof(SgVM, ac)
#define VM_OFFSET_CL         offsetof(SgVM, cl)
#define VM_OFFSET_CONT       offsetof(SgVM, cont)
#define VM_OFFSET_VALUESCOUNT offsetof(SgVM, valuesCount)


/*
 * Continuation Frame Offsets
 */
#define CONT_OFFSET_PREV    offsetof(SgContFrame, prev)
#define CONT_OFFSET_SIZE    offsetof(SgContFrame, size)
#define CONT_OFFSET_TYPE    offsetof(SgContFrame, type)
#define CONT_OFFSET_PC      offsetof(SgContFrame, pc)
#define CONT_OFFSET_CL      offsetof(SgContFrame, cl)
#define CONT_OFFSET_FP      offsetof(SgContFrame, fp)

/*
 * Continuation frame size in SgObjects (typically 6 on 64-bit)
 */
#define CONT_FRAME_SIZE_BYTES (sizeof(SgContFrame))


/*
 * Closure Structure Offsets
 */
#define CLOSURE_OFFSET_FREES offsetof(SgClosure, frees)


/*
 * Box Structure Offset
 */
#define BOX_OFFSET_VALUE offsetof(SgBox, value)


/*
 * Helper: Get Arm64Asm from context
 */
#define GET_GEN(ctx) ((Arm64CodeGen*)((ctx)->platform))
#define GET_ASM(ctx) (GET_GEN(ctx)->a)


/*
 * Platform Lifecycle Functions
 */

void* Sg__JitPlatformInit(SgJitContext *ctx)
{
  Arm64CodeGen *gen = SG_NEW(Arm64CodeGen);
  gen->ctx = ctx;
  gen->a = arm64_asm_new(ctx->buf->code, ctx->buf->size);
  if (gen->a == NULL) {
    return NULL;
  }

  /* Pre-allocate labels for all bytecode positions plus epilogue */
  gen->labels = SG_NEW_ARRAY(int, ctx->labelCount);
  for (int i = 0; i < ctx->labelCount; i++) {
    gen->labels[i] = arm64_new_label(gen->a);
  }

  return gen;
}

void Sg__JitPlatformCleanup(void *platform)
{
  if (platform != NULL) {
    Arm64CodeGen *gen = (Arm64CodeGen *)platform;
    if (gen->a != NULL) {
      arm64_asm_free(gen->a);
    }
  }
}

SgJitCompiledCode Sg__JitPlatformFinalize(SgJitContext *ctx)
{
  Arm64CodeGen *gen = GET_GEN(ctx);
  arm64_asm_free(gen->a);
  gen->a = NULL;
  return (SgJitCompiledCode)ctx->buf->code;
}

int Sg__JitPlatformResolve(SgJitContext *ctx)
{
  Arm64CodeGen *gen = GET_GEN(ctx);
  return arm64_asm_finalize(gen->a) == 0;
}

void Sg__JitBindLabel(SgJitContext *ctx, int label)
{
  Arm64CodeGen *gen = GET_GEN(ctx);
  arm64_bind_label(gen->a, gen->labels[label]);
}

size_t Sg__JitGetCodeSize(SgJitContext *ctx)
{
  Arm64CodeGen *gen = GET_GEN(ctx);
  return arm64_asm_size(gen->a);
}


/*
 * Prologue/Epilogue
 */

int Sg__JitEmit_Prologue(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  Arm64CodeGen *gen = GET_GEN(ctx);

  /* Save frame pointer and link register (required by ABI) */
  /* STP X29, X30, [SP, #-16]! */
  arm64_stp_pre(a, ARM64_FP, ARM64_LR, ARM64_SP, -16);

  /* Set frame pointer to zero (not using it) */
  arm64_mov_r64_r64(a, ARM64_FP, ARM64_XZR);

  /* Save callee-saved registers we'll use */
  /* STP X19, X20, [SP, #-16]! */
  arm64_stp_pre(a, ARM64_X19, ARM64_X20, ARM64_SP, -16);
  /* STP X21, X22, [SP, #-16]! */
  arm64_stp_pre(a, ARM64_X21, ARM64_X22, ARM64_SP, -16);
  /* Save X23 for self-call depth tracking (paired with XZR for alignment) */
  arm64_stp_pre(a, ARM64_X23, ARM64_XZR, ARM64_SP, -16);

  /* Initialize recursion depth counter to 0 */
  arm64_mov_r64_imm(a, JIT_REG_DEPTH, 0);

  /* Load VM pointer from first argument (X0) */
  arm64_mov_r64_r64(a, JIT_REG_VM, ARM64_X0);

  /* Load closure from second argument (X1) */
  arm64_mov_r64_r64(a, JIT_REG_CL, ARM64_X1);

  /* Load VM registers from VM structure */
  /* SP = vm->sp */
  arm64_ldr_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  /* FP = vm->fp */
  arm64_ldr_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  /* AC (X0) = vm->ac */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_VM, VM_OFFSET_AC);

  /* Create bodyEntry label for potential future self-recursive calls */
  gen->bodyEntryLabel = arm64_new_label(a);
  arm64_bind_label(a, gen->bodyEntryLabel);

  return 1;
}

int Sg__JitEmit_Epilogue(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Store VM registers back to VM structure */
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_VM, VM_OFFSET_AC);
  arm64_str_r64_mem(a, JIT_REG_CL, JIT_REG_VM, VM_OFFSET_CL);

  /* Set valuesCount = 1 (single return value) */
  arm64_mov_r64_imm(a, JIT_REG_TEMP2, 1);
  arm64_str_r64_mem(a, JIT_REG_TEMP2, JIT_REG_VM, VM_OFFSET_VALUESCOUNT);

  /* Restore callee-saved registers (reverse order of prologue) */
  /* Restore X23 */
  arm64_ldp(a, ARM64_X23, ARM64_XZR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);
  arm64_ldp(a, ARM64_X21, ARM64_X22, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);
  arm64_ldp(a, ARM64_X19, ARM64_X20, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);
  arm64_ldp(a, ARM64_FP, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  arm64_ret(a);
  return 1;
}


/*
 * Basic Instructions
 */

int Sg__JitEmit_NOP(SgJitContext *ctx)
{
  arm64_nop(GET_ASM(ctx));
  return 1;
}

int Sg__JitEmit_UNDEF(SgJitContext *ctx)
{
  arm64_mov_r64_ptr(GET_ASM(ctx), JIT_REG_TEMP1, SG_UNDEF);
  return 1;
}

int Sg__JitEmit_CONST(SgJitContext *ctx, SgObject val)
{
  arm64_mov_r64_ptr(GET_ASM(ctx), JIT_REG_TEMP1, val);
  return 1;
}

int Sg__JitEmit_CONSTI(SgJitContext *ctx, long val)
{
  /* Convert to tagged fixnum */
  intptr_t tagged = (val << FIXNUM_SHIFT) | FIXNUM_TAG;
  arm64_mov_r64_imm(GET_ASM(ctx), JIT_REG_TEMP1, tagged);
  return 1;
}


/*
 * Local Variable Access
 */

int Sg__JitEmit_LREF(SgJitContext *ctx, int index)
{
  int32_t offset = index * sizeof(SgObject);
  arm64_ldr_r64_mem(GET_ASM(ctx), JIT_REG_TEMP1, JIT_REG_SCHFP, offset);
  return 1;
}

int Sg__JitEmit_LSET(SgJitContext *ctx, int index)
{
  Arm64Asm *a = GET_ASM(ctx);
  int32_t offset = index * sizeof(SgObject);

  /* Load box pointer from frame */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP2, JIT_REG_SCHFP, offset);
  /* Store AC to box->value */
  arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP2, BOX_OFFSET_VALUE);
  /* AC = SG_UNDEF */
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_UNDEF);
  return 1;
}

int Sg__JitEmit_FREF(SgJitContext *ctx, int index)
{
  int32_t offset = CLOSURE_OFFSET_FREES + index * sizeof(SgObject);
  arm64_ldr_r64_mem(GET_ASM(ctx), JIT_REG_TEMP1, JIT_REG_CL, offset);
  return 1;
}


/*
 * Stack Operations
 */

int Sg__JitEmit_PUSH(SgJitContext *ctx)
{
  /* STR X0, [X20], #8 (post-increment) */
  arm64_str_r64_mem_post(GET_ASM(ctx), JIT_REG_TEMP1, JIT_REG_SCHSP, 
			 sizeof(SgObject));
  return 1;
}


/*
 * Combined Instructions
 */

int Sg__JitEmit_LREF_PUSH(SgJitContext *ctx, int index)
{
  Arm64Asm *a = GET_ASM(ctx);
  int32_t offset = index * sizeof(SgObject);
  arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_SCHFP, offset);
  arm64_str_r64_mem_post(a, JIT_REG_TEMP1, JIT_REG_SCHSP, sizeof(SgObject));
  return 1;
}

int Sg__JitEmit_CONST_PUSH(SgJitContext *ctx, SgObject val)
{
  Arm64Asm *a = GET_ASM(ctx);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, val);
  arm64_str_r64_mem_post(a, JIT_REG_TEMP1, JIT_REG_SCHSP, sizeof(SgObject));
  return 1;
}

int Sg__JitEmit_CONSTI_PUSH(SgJitContext *ctx, long val)
{
  Arm64Asm *a = GET_ASM(ctx);
  intptr_t tagged = (val << FIXNUM_SHIFT) | FIXNUM_TAG;
  arm64_mov_r64_imm(a, JIT_REG_TEMP1, tagged);
  arm64_str_r64_mem_post(a, JIT_REG_TEMP1, JIT_REG_SCHSP, sizeof(SgObject));
  return 1;
}


/*
 * Arithmetic Operations
 */

int Sg__JitEmit_ADD(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  int slowPath = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Pop operand: X1 = *--SP */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP, 
			-(int32_t)sizeof(SgObject));

  /* Check if both are fixnums */
  /* Both must have bit 0 set */
  arm64_and_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_tst_r64_imm(a, JIT_REG_TEMP3, 1);
  arm64_b_cond(a, ARM64_EQ, slowPath);

  /* Neither should have bit 1 set */
  arm64_orr_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_tst_r64_imm(a, JIT_REG_TEMP3, 2);
  arm64_b_cond(a, ARM64_NE, slowPath);

  /* Fast path: add fixnums (result has tag 2, subtract 1) */
  arm64_add_r64_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_sub_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 1);
  arm64_b(a, done);

  /* Slow path: call Sg_Add */
  arm64_bind_label(a, slowPath);
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);
  arm64_mov_r64_r64(a, ARM64_X1, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Add);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  arm64_bind_label(a, done);
  return 1;
}

int Sg__JitEmit_ADDI(SgJitContext *ctx, long val)
{
  Arm64Asm *a = GET_ASM(ctx);
  int slowPath = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Check if AC is fixnum */
  arm64_tst_r64_imm(a, JIT_REG_TEMP1, 1);
  arm64_b_cond(a, ARM64_EQ, slowPath);
  arm64_tst_r64_imm(a, JIT_REG_TEMP1, 2);
  arm64_b_cond(a, ARM64_NE, slowPath);

  /* Fast path: add immediate (scaled by fixnum shift) */
  intptr_t scaledVal = val << FIXNUM_SHIFT;
  if (scaledVal >= 0 && scaledVal <= 4095) {
    arm64_add_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, (int32_t)scaledVal);
  } else if (scaledVal < 0 && scaledVal >= -4095) {
    /* Use SUB for negative values */
    arm64_sub_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, (int32_t)(-scaledVal));
  } else {
    /* Value too large, use register form */
    arm64_mov_r64_imm(a, JIT_REG_TEMP3, scaledVal);
    arm64_add_r64_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP1, JIT_REG_TEMP3);
  }
  arm64_b(a, done);

  /* Slow path: call Sg_Add */
  arm64_bind_label(a, slowPath);
  intptr_t taggedVal = (val << FIXNUM_SHIFT) | FIXNUM_TAG;
  arm64_mov_r64_imm(a, ARM64_X1, taggedVal);
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Add);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  arm64_bind_label(a, done);
  return 1;
}

int Sg__JitEmit_SUB(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  int slowPath = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Pop operand: X1 = *--SP */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
			-(int32_t)sizeof(SgObject));

  /* Check if both are fixnums */
  arm64_and_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_tst_r64_imm(a, JIT_REG_TEMP3, 1);
  arm64_b_cond(a, ARM64_EQ, slowPath);

  arm64_orr_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_tst_r64_imm(a, JIT_REG_TEMP3, 2);
  arm64_b_cond(a, ARM64_NE, slowPath);

  /* Fast path: X1 - X0 (result has tag 0, add 1) */
  arm64_sub_r64_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP2, JIT_REG_TEMP1);
  arm64_add_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 1);
  arm64_b(a, done);

  /* Slow path: call Sg_Sub */
  arm64_bind_label(a, slowPath);
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);
  arm64_mov_r64_r64(a, ARM64_X1, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Sub);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  arm64_bind_label(a, done);
  return 1;
}

int Sg__JitEmit_SUBI(SgJitContext *ctx, long val)
{
  Arm64Asm *a = GET_ASM(ctx);
  int slowPath = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Check if AC is fixnum */
  arm64_tst_r64_imm(a, JIT_REG_TEMP1, 1);
  arm64_b_cond(a, ARM64_EQ, slowPath);
  arm64_tst_r64_imm(a, JIT_REG_TEMP1, 2);
  arm64_b_cond(a, ARM64_NE, slowPath);

  /* Fast path: subtract immediate */
  intptr_t scaledVal = val << FIXNUM_SHIFT;
  if (scaledVal >= 0 && scaledVal <= 4095) {
    arm64_sub_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, (int32_t)scaledVal);
  } else if (scaledVal < 0 && scaledVal >= -4095) {
    /* Use ADD for negative values (subtracting negative = adding positive) */
    arm64_add_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, (int32_t)(-scaledVal));
  } else {
    /* Value too large, use register form */
    arm64_mov_r64_imm(a, JIT_REG_TEMP3, scaledVal);
    arm64_sub_r64_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP1, JIT_REG_TEMP3);
  }
  arm64_b(a, done);

  /* Slow path: call Sg_Sub */
  arm64_bind_label(a, slowPath);
  intptr_t taggedVal = (val << FIXNUM_SHIFT) | FIXNUM_TAG;
  arm64_mov_r64_imm(a, ARM64_X1, taggedVal);
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Sub);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  arm64_bind_label(a, done);
  return 1;
}


/*
 * Comparison Operations
 */

int Sg__JitEmit_NUM_EQ(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  int slowPath = arm64_new_label(a);
  int done = arm64_new_label(a);
  int isEqual = arm64_new_label(a);

  /* Pop operand */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
			-(int32_t)sizeof(SgObject));

  /* Check if both are fixnums */
  arm64_and_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_tst_r64_imm(a, JIT_REG_TEMP3, 1);
  arm64_b_cond(a, ARM64_EQ, slowPath);
  arm64_orr_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_tst_r64_imm(a, JIT_REG_TEMP3, 2);
  arm64_b_cond(a, ARM64_NE, slowPath);

  /* Fast path: compare */
  arm64_cmp_r64_r64(a, JIT_REG_TEMP2, JIT_REG_TEMP1);
  arm64_b_cond(a, ARM64_EQ, isEqual);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_FALSE);
  arm64_b(a, done);

  arm64_bind_label(a, isEqual);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_TRUE);
  arm64_b(a, done);

  /* Slow path */
  arm64_bind_label(a, slowPath);
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);
  arm64_mov_r64_r64(a, ARM64_X1, JIT_REG_TEMP1);
  arm64_bl(a, Sg_NumEq);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  arm64_bind_label(a, done);
  return 1;
}

/* Helper macro for comparison operations */
#define EMIT_NUM_CMP(ctx, cond, slowFn)				\
  do {								\
    Arm64Asm *a = GET_ASM(ctx);					\
    int slowPath = arm64_new_label(a);				\
    int done = arm64_new_label(a);				\
    int isTrue = arm64_new_label(a);				\
    arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,	\
			  -(int32_t)sizeof(SgObject));		\
    arm64_and_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1,	\
			  JIT_REG_TEMP2);			\
    arm64_tst_r64_imm(a, JIT_REG_TEMP3, 1);			\
    arm64_b_cond(a, ARM64_EQ, slowPath);			\
    arm64_orr_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1,	\
			  JIT_REG_TEMP2);			\
    arm64_tst_r64_imm(a, JIT_REG_TEMP3, 2);			\
    arm64_b_cond(a, ARM64_NE, slowPath);			\
    arm64_cmp_r64_r64(a, JIT_REG_TEMP2, JIT_REG_TEMP1);		\
    arm64_b_cond(a, cond, isTrue);				\
    arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_FALSE);		\
    arm64_b(a, done);						\
    arm64_bind_label(a, isTrue);				\
    arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_TRUE);		\
    arm64_b(a, done);						\
    arm64_bind_label(a, slowPath);				\
    arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);		\
    arm64_mov_r64_r64(a, ARM64_X1, JIT_REG_TEMP1);		\
    arm64_bl(a, slowFn);					\
    arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);		\
    arm64_bind_label(a, done);					\
    return 1;							\
  } while (0)

int Sg__JitEmit_NUM_LT(SgJitContext *ctx)
{
  EMIT_NUM_CMP(ctx, ARM64_LT, Sg_NumLt);
}

int Sg__JitEmit_NUM_LE(SgJitContext *ctx)
{
  EMIT_NUM_CMP(ctx, ARM64_LE, Sg_NumLe);
}

int Sg__JitEmit_NUM_GT(SgJitContext *ctx)
{
  EMIT_NUM_CMP(ctx, ARM64_GT, Sg_NumGt);
}

int Sg__JitEmit_NUM_GE(SgJitContext *ctx)
{
  EMIT_NUM_CMP(ctx, ARM64_GE, Sg_NumGe);
}


/*
 * Control Flow
 */

int Sg__JitEmit_TEST(SgJitContext *ctx, int targetPc)
{
  Arm64Asm *a = GET_ASM(ctx);
  Arm64CodeGen *gen = GET_GEN(ctx);

  /* If AC is #f, jump to target */
  arm64_mov_r64_ptr(a, JIT_REG_TEMP2, SG_FALSE);
  arm64_cmp_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_b_cond(a, ARM64_EQ, gen->labels[ctx->pcToLabel[targetPc]]);
  return 1;
}

int Sg__JitEmit_JUMP(SgJitContext *ctx, int targetPc)
{
  Arm64Asm *a = GET_ASM(ctx);
  Arm64CodeGen *gen = GET_GEN(ctx);

  arm64_b(a, gen->labels[ctx->pcToLabel[targetPc]]);
  return 1;
}

int Sg__JitEmit_RET(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  Arm64CodeGen *gen = GET_GEN(ctx);

  /*
   * If depth > 0, we're returning from a self-recursive call,
   * so use ARM RET to return to the SELF_CALL's return point.
   * Otherwise, go to epilogue for top-level return.
   */
  int topLevelReturn = arm64_new_label(a);
  
  /* Check if depth == 0 */
  arm64_cmp_r64_imm(a, JIT_REG_DEPTH, 0);
  arm64_b_cond(a, ARM64_EQ, topLevelReturn);
  
  /* Depth > 0: return to SELF_CALL via ARM RET */
  arm64_ret(a);
  
  /* Depth == 0: go to epilogue */
  arm64_bind_label(a, topLevelReturn);
  arm64_b(a, gen->labels[ctx->epilogueLabel]);

  return 1;
}


/*
 * Branch Instructions
 */

#define EMIT_BRANCH_CMP(ctx, cond, targetPc)				\
  do {									\
    Arm64Asm *a = GET_ASM(ctx);						\
    Arm64CodeGen *gen = GET_GEN(ctx);					\
    int slowPath = arm64_new_label(a);					\
    int done = arm64_new_label(a);					\
    arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,		\
			  -(int32_t)sizeof(SgObject));			\
    arm64_and_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1,		\
			  JIT_REG_TEMP2);				\
    arm64_tst_r64_imm(a, JIT_REG_TEMP3, 1);				\
    arm64_b_cond(a, ARM64_EQ, slowPath);				\
    arm64_orr_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1,		\
			  JIT_REG_TEMP2);				\
    arm64_tst_r64_imm(a, JIT_REG_TEMP3, 2);				\
    arm64_b_cond(a, ARM64_NE, slowPath);				\
    arm64_cmp_r64_r64(a, JIT_REG_TEMP2, JIT_REG_TEMP1);			\
    arm64_b_cond(a, cond, gen->labels[ctx->pcToLabel[targetPc]]);	\
    arm64_b(a, done);							\
    arm64_bind_label(a, slowPath);					\
    /* Slow path: For now, just fall through (assume comparison fails) */ \
    /* TODO: Call helper for bignum/flonum comparison */		\
    arm64_bind_label(a, done);						\
  } while (0)

int Sg__JitEmit_BNNUME(SgJitContext *ctx, int targetPc)
{
  /* Branch if not numeric equal - jump if X1 != X0 */
  EMIT_BRANCH_CMP(ctx, ARM64_NE, targetPc);
  return 1;
}

int Sg__JitEmit_BNLT(SgJitContext *ctx, int targetPc)
{
  /* Branch if not less than - jump if X1 >= X0 */
  EMIT_BRANCH_CMP(ctx, ARM64_GE, targetPc);
  return 1;
}

int Sg__JitEmit_BNLE(SgJitContext *ctx, int targetPc)
{
  /* Branch if not less or equal - jump if X1 > X0 */
  EMIT_BRANCH_CMP(ctx, ARM64_GT, targetPc);
  return 1;
}

int Sg__JitEmit_BNGT(SgJitContext *ctx, int targetPc)
{
  /* Branch if not greater than - jump if X1 <= X0 */
  EMIT_BRANCH_CMP(ctx, ARM64_LE, targetPc);
  return 1;
}

int Sg__JitEmit_BNGE(SgJitContext *ctx, int targetPc)
{
  /* Branch if not greater or equal - jump if X1 < X0 */
  EMIT_BRANCH_CMP(ctx, ARM64_LT, targetPc);
  return 1;
}

/*
 * FRAME instruction - push a continuation frame (inlined)
 *
 * FRAME saves the return address and VM state so we can return
 * after a non-tail call completes.
 *
 * This is a performance-critical instruction, so we inline the frame
 * push directly in ARM64 assembly instead of calling a C helper.
 *
 * Frame layout at current SP:
 *   cont->prev = vm->cont
 *   cont->size = SP - FP (in SgObjects)
 *   cont->type = 0 (NORMAL_FRAME)
 *   cont->pc = returnAddr
 *   cont->cl = CL
 *   cont->fp = FP
 * Then: vm->cont = SP, SP += CONT_FRAME_SIZE
 */
int Sg__JitEmit_FRAME(SgJitContext *ctx, int returnPc)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* TEMP2 = current SP (will become the new cont frame) */
  arm64_mov_r64_r64(a, JIT_REG_TEMP2, JIT_REG_SCHSP);

  /* cont->prev = vm->cont */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP3, JIT_REG_VM, VM_OFFSET_CONT);
  arm64_str_r64_mem(a, JIT_REG_TEMP3, JIT_REG_TEMP2, CONT_OFFSET_PREV);

  /* cont->size = (SP - FP) / sizeof(SgObject) */
  arm64_sub_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_SCHSP, JIT_REG_SCHFP);
  /* Shift right by 3 (divide by 8 = sizeof(SgObject)) */
  arm64_lsr_r64_r64_imm(a, JIT_REG_TEMP3, JIT_REG_TEMP3, 3);
  /* Store as 32-bit value at size offset */
  arm64_str_r32_mem(a, JIT_REG_TEMP3, JIT_REG_TEMP2, CONT_OFFSET_SIZE);

  /* cont->type = 0 (NORMAL_FRAME) - stored as 32-bit */
  arm64_mov_r64_imm(a, JIT_REG_TEMP3, 0);
  arm64_str_r32_mem(a, JIT_REG_TEMP3, JIT_REG_TEMP2, CONT_OFFSET_TYPE);

  /* cont->pc = returnAddr (bytecode address) */
  SgCodeBuilder *cb = ctx->cb;
  SgWord *returnAddr = &cb->code[returnPc];
  arm64_mov_r64_imm(a, JIT_REG_TEMP3, (intptr_t)returnAddr);
  arm64_str_r64_mem(a, JIT_REG_TEMP3, JIT_REG_TEMP2, CONT_OFFSET_PC);

  /* cont->cl = CL (current closure) */
  arm64_str_r64_mem(a, JIT_REG_CL, JIT_REG_TEMP2, CONT_OFFSET_CL);

  /* cont->fp = FP */
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_TEMP2, CONT_OFFSET_FP);

  /* vm->cont = TEMP2 (the new frame) */
  arm64_str_r64_mem(a, JIT_REG_TEMP2, JIT_REG_VM, VM_OFFSET_CONT);

  /* SP += CONT_FRAME_SIZE_BYTES */
  arm64_add_r64_r64_imm(a, JIT_REG_SCHSP, JIT_REG_SCHSP, (int32_t)CONT_FRAME_SIZE_BYTES);

  /* Sync new SP to VM (needed for C helper calls that may follow) */
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);

  return 1;
}

/*
 * GREF_CALL instruction - call a global procedure
 *
 * This looks up the global identifier, pushes arguments from the stack,
 * and calls the procedure. The continuation frame was already pushed by FRAME.
 */
int Sg__JitEmit_GREF_CALL(SgJitContext *ctx, int argc, SgObject id)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Sync VM state before call */
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  arm64_str_r64_mem(a, JIT_REG_CL, JIT_REG_VM, VM_OFFSET_CL);

  /* Call Sg__JitGrefCall(vm, argc, id)
   * X0 = vm
   * X1 = argc
   * X2 = id (the global identifier)
   */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_VM);
  arm64_mov_r64_imm(a, ARM64_X1, argc);
  arm64_mov_r64_imm(a, ARM64_X2, (intptr_t)id);

  arm64_bl(a, Sg__JitGrefCall);

  /* Result is in X0, move to AC (TEMP1) */
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Reload VM state after call (continuation was popped by helper) */
  arm64_ldr_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_ldr_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  arm64_ldr_r64_mem(a, JIT_REG_CL, JIT_REG_VM, VM_OFFSET_CL);

  return 1;
}

/*
 * GREF_TAIL_CALL instruction - tail-call a global procedure
 *
 * This is like GREF_CALL but doesn't push a continuation frame,
 * and returns the result directly (proper tail call).
 *
 * When depth > 0 (inside SELF_CALL), the tail call consumes the
 * continuation frame that SELF_CALL expects to pop. We need to
 * properly unwind the SELF_CALL's ARM stack frame and return
 * directly to the original caller.
 */
int Sg__JitEmit_GREF_TAIL_CALL(SgJitContext *ctx, int argc, SgObject id)
{
  Arm64Asm *a = GET_ASM(ctx);
  Arm64CodeGen *gen = GET_GEN(ctx);

  /* Sync VM state before call */
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  arm64_str_r64_mem(a, JIT_REG_CL, JIT_REG_VM, VM_OFFSET_CL);

  /* Call Sg__JitGrefTailCall(vm, argc, id)
   * X0 = vm
   * X1 = argc
   * X2 = id (the global identifier)
   */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_VM);
  arm64_mov_r64_imm(a, ARM64_X1, argc);
  arm64_mov_r64_imm(a, ARM64_X2, (intptr_t)id);

  arm64_bl(a, Sg__JitGrefTailCall);

  /* Result is in X0, move to AC (TEMP1) */
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /*
   * For tail call, we need to return this result.
   * If depth > 0, we're inside SELF_CALL(s). The tail call consumed
   * the Scheme continuation frame(s), so we need to unwind the ARM
   * stack frames for all SELF_CALLs and decrement depth accordingly.
   */
  int topLevelReturn = arm64_new_label(a);
  int unwindLoop = arm64_new_label(a);
  
  arm64_cmp_r64_imm(a, JIT_REG_DEPTH, 0);
  arm64_b_cond(a, ARM64_EQ, topLevelReturn);
  
  /* Depth > 0: unwind all SELF_CALL ARM stack frames */
  arm64_bind_label(a, unwindLoop);
  
  /* Pop saved_LR (we don't need it, just skip) */
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);
  /* Pop saved_cont (we don't need it, tail call consumed the frame) */
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);
  /* Decrement depth */
  arm64_sub_r64_r64_imm(a, JIT_REG_DEPTH, JIT_REG_DEPTH, 1);
  
  /* Check if more frames to unwind */
  arm64_cmp_r64_imm(a, JIT_REG_DEPTH, 0);
  arm64_b_cond(a, ARM64_NE, unwindLoop);
  
  /* Fall through to epilogue */
  
  /* Depth == 0: go to epilogue */
  arm64_bind_label(a, topLevelReturn);
  arm64_b(a, gen->labels[ctx->epilogueLabel]);

  return 1;
}

/*
 * SELF_CALL instruction - optimized self-recursive call
 *
 * This is called when we detect a recursive call to the same function.
 * Instead of going through the C helper, we directly branch to the
 * function's body entry point after setting up the new frame.
 *
 * Prerequisites: FRAME instruction already pushed continuation frame.
 * Arguments are on stack at SP - argc to SP.
 */
int Sg__JitEmit_SELF_CALL(SgJitContext *ctx, int argc)
{
  Arm64Asm *a = GET_ASM(ctx);
  Arm64CodeGen *gen = GET_GEN(ctx);

  /*
   * For self-recursive non-tail call:
   * 1. Save vm->cont and LR to ARM stack
   * 2. Increment depth counter
   * 3. Set FP = SP - argc
   * 4. Sync SP/FP/CL to VM
   * 5. BL to bodyEntry
   * 6. Restore LR and cont
   * 7. Restore SP, FP from cont frame
   * 8. Update vm->cont
   * 9. Decrement depth
   *
   * Optimizations applied:
   * - Skip CL restore (same closure for self-calls)
   * - Skip AC sync (register passing)
   * - Skip post-call VM syncs (will be done by FRAME)
   */

  int32_t argBytes = argc * (int32_t)sizeof(SgObject);

  /* Save LR and vm->cont to ARM stack */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP2, JIT_REG_VM, VM_OFFSET_CONT);
  arm64_str_r64_mem_pre(a, JIT_REG_TEMP2, ARM64_SP, -16);  /* Push saved_cont */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);       /* Push saved_LR */

  /* Increment recursion depth */
  arm64_add_r64_r64_imm(a, JIT_REG_DEPTH, JIT_REG_DEPTH, 1);

  /* FP = SP - argc (point to arguments for the recursive call) */
  arm64_sub_r64_r64_imm(a, JIT_REG_SCHFP, JIT_REG_SCHSP, argBytes);

  /* Sync SP/FP/CL to VM */
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  arm64_str_r64_mem(a, JIT_REG_CL, JIT_REG_VM, VM_OFFSET_CL);

  /* BL to bodyEntry */
  arm64_bl_label(a, gen->bodyEntryLabel);

  /* Restore LR */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  /* Load saved cont */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP2, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  /* SP = cont (pop the frame) */
  arm64_mov_r64_r64(a, JIT_REG_SCHSP, JIT_REG_TEMP2);

  /* FP = cont->fp (skip CL restore - same closure!) */
  arm64_ldr_r64_mem(a, JIT_REG_SCHFP, JIT_REG_TEMP2, CONT_OFFSET_FP);

  /* vm->cont = cont->prev */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP3, JIT_REG_TEMP2, CONT_OFFSET_PREV);
  arm64_str_r64_mem(a, JIT_REG_TEMP3, JIT_REG_VM, VM_OFFSET_CONT);

  /* Decrement recursion depth */
  arm64_sub_r64_r64_imm(a, JIT_REG_DEPTH, JIT_REG_DEPTH, 1);

  return 1;
}

/*
 * SELF_TAIL_CALL instruction - optimized self-recursive tail call
 *
 * This is a proper tail call that reuses the current frame.
 * Arguments need to be shifted from their current position to FP.
 *
 * IMPORTANT: The last argument is in AC, not on the stack!
 * Stack has argc-1 arguments, AC has the last one.
 *
 * We branch to bodyEntryLabel (after ENTRY) to avoid re-executing
 * the ENTRY instruction which might reinitialize state incorrectly.
 *
 * IMPORTANT: This optimization only works at depth=0 (top-level call).
 * When inside a SELF_CALL (depth > 0), vm->cont points to continuation
 * frames that we would corrupt. In that case, we fall back to the
 * C helper (same as GREF_TAIL_CALL).
 */
int Sg__JitEmit_SELF_TAIL_CALL(SgJitContext *ctx, int argc, SgObject id)
{
  Arm64Asm *a = GET_ASM(ctx);
  Arm64CodeGen *gen = GET_GEN(ctx);

  /*
   * Runtime check: if depth > 0, use helper (can't optimize safely).
   * If depth == 0, use direct branch optimization.
   */
  int useHelper = arm64_new_label(a);
  int done = arm64_new_label(a);
  
  arm64_cmp_r64_imm(a, JIT_REG_DEPTH, 0);
  arm64_b_cond(a, ARM64_NE, useHelper);

  /* ===== Depth == 0: Use optimized direct branch ===== */

  if (argc == 0) {
    /* No arguments to copy, just reset SP and branch */
    arm64_mov_r64_r64(a, JIT_REG_SCHSP, JIT_REG_SCHFP);
    /* Sync state to VM before branching */
    arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
    arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
    arm64_b(a, gen->bodyEntryLabel);
  } else {
    int32_t argBytes = argc * (int32_t)sizeof(SgObject);

    /* TEMP2 = source base (SP - argc * sizeof(SgObject)) */
    arm64_sub_r64_r64_imm(a, JIT_REG_TEMP2, JIT_REG_SCHSP, argBytes);

    /* Copy all arguments from stack to FP: FP[i] = source[i] */
    for (int i = 0; i < argc; i++) {
      int32_t off = i * (int32_t)sizeof(SgObject);
      /* Load from source */
      arm64_ldr_r64_mem(a, JIT_REG_TEMP3, JIT_REG_TEMP2, off);
      /* Store to FP */
      arm64_str_r64_mem(a, JIT_REG_TEMP3, JIT_REG_SCHFP, off);
    }

    /* Load last argument into AC for VM state consistency */
    int32_t lastArgOff = (argc - 1) * (int32_t)sizeof(SgObject);
    arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_SCHFP, lastArgOff);

    /* SP = FP + argc * sizeof(SgObject) */
    arm64_add_r64_r64_imm(a, JIT_REG_SCHSP, JIT_REG_SCHFP, argBytes);

    /* Sync state to VM before branching */
    arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
    arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
    arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_VM, VM_OFFSET_AC);

    /* Branch to body entry (after ENTRY instruction) */
    arm64_b(a, gen->bodyEntryLabel);
  }

  /* ===== Depth > 0: Fall back to C helper ===== */
  arm64_bind_label(a, useHelper);

  /* Sync VM state before call */
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  arm64_str_r64_mem(a, JIT_REG_CL, JIT_REG_VM, VM_OFFSET_CL);

  /* Call Sg__JitGrefTailCall(vm, argc, id) */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_VM);
  arm64_mov_r64_imm(a, ARM64_X1, argc);
  arm64_mov_r64_imm(a, ARM64_X2, (intptr_t)id);

  arm64_bl(a, Sg__JitGrefTailCall);

  /* Result is in X0, move to AC */
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /*
   * Unwind all SELF_CALL ARM stack frames (same logic as GREF_TAIL_CALL).
   * The tail call consumed the Scheme continuation frames.
   */
  int unwindLoop = arm64_new_label(a);
  arm64_bind_label(a, unwindLoop);
  
  /* Pop saved_LR (skip) */
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);
  /* Pop saved_cont (skip) */
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);
  /* Decrement depth */
  arm64_sub_r64_r64_imm(a, JIT_REG_DEPTH, JIT_REG_DEPTH, 1);
  
  /* Check if more frames to unwind */
  arm64_cmp_r64_imm(a, JIT_REG_DEPTH, 0);
  arm64_b_cond(a, ARM64_NE, unwindLoop);
  
  /* Go to epilogue */
  arm64_b(a, gen->labels[ctx->epilogueLabel]);

  return 1;
}

/*
 * Disassembly interface
 */
void Sg__JitDisasmBuffer(uint8_t *code, size_t size, SgPort *port)
{
  arm64_disasm_buffer(code, size, port);
}
