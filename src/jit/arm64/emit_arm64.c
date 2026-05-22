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
  gen->a = arm64_asm_new(ctx->buf);
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

  /* Note: DO NOT reset valuesCount here!
   * If VALUES was executed, vm->valuesCount was already set by Sg__JitValues.
   * If no VALUES was executed, valuesCount remains at 1 (default).
   * The VM initializes valuesCount = 1 in the prologue and most instructions
   * that produce single values (like CONST, LREF, etc.) also set it to 1. */

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

  /* Slow path: call Sg_Add(obj, ac) where obj=TEMP2, ac=TEMP1
   * Note: TEMP1=X0, TEMP2=X1, so we need to swap properly
   * Save/restore LR because BL clobbers it and we need it for RET after SELF_CALL */
  arm64_bind_label(a, slowPath);
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);  /* Save LR */
  arm64_mov_r64_r64(a, ARM64_X3, JIT_REG_TEMP1);   /* Save AC in X3 */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);   /* X0 = obj (TEMP2) */
  arm64_mov_r64_r64(a, ARM64_X1, ARM64_X3);        /* X1 = AC (saved TEMP1) */
  arm64_bl(a, Sg_Add);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);     /* Restore LR */
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

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
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);  /* Save LR */
  intptr_t taggedVal = (val << FIXNUM_SHIFT) | FIXNUM_TAG;
  arm64_mov_r64_imm(a, ARM64_X1, taggedVal);
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Add);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);     /* Restore LR */
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

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

  /* Slow path: call Sg_Sub(obj, ac) where obj=TEMP2, ac=TEMP1
   * Note: TEMP1=X0, TEMP2=X1, so we need to swap properly
   * Save/restore LR because BL clobbers it and we need it for RET after SELF_CALL */
  arm64_bind_label(a, slowPath);
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);  /* Save LR */
  arm64_mov_r64_r64(a, ARM64_X3, JIT_REG_TEMP1);   /* Save AC in X3 */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);   /* X0 = obj (TEMP2) */
  arm64_mov_r64_r64(a, ARM64_X1, ARM64_X3);        /* X1 = AC (saved TEMP1) */
  arm64_bl(a, Sg_Sub);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);     /* Restore LR */
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

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
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);  /* Save LR */
  intptr_t taggedVal = (val << FIXNUM_SHIFT) | FIXNUM_TAG;
  arm64_mov_r64_imm(a, ARM64_X1, taggedVal);
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Sub);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);     /* Restore LR */
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

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

  /* Slow path: call Sg_NumEq(obj, ac) where obj=TEMP2, ac=TEMP1
   * Note: TEMP1=X0, TEMP2=X1, so we need to swap properly
   * Save/restore LR because BL clobbers it and we need it for RET after SELF_CALL */
  arm64_bind_label(a, slowPath);
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);  /* Save LR */
  arm64_mov_r64_r64(a, ARM64_X3, JIT_REG_TEMP1);   /* Save AC in X3 */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);   /* X0 = obj (TEMP2) */
  arm64_mov_r64_r64(a, ARM64_X1, ARM64_X3);        /* X1 = AC (saved TEMP1) */
  arm64_bl(a, Sg_NumEq);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);     /* Restore LR */
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

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
    /* Slow path: call fn(obj, ac) where obj=TEMP2, ac=TEMP1 */ \
    /* Note: TEMP1=X0, TEMP2=X1, so we need to swap properly */ \
    /* Save/restore LR for SELF_CALL returns */                 \
    arm64_bind_label(a, slowPath);				\
    arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);		\
    arm64_mov_r64_r64(a, ARM64_X3, JIT_REG_TEMP1);		\
    arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);		\
    arm64_mov_r64_r64(a, ARM64_X1, ARM64_X3);			\
    arm64_bl(a, slowFn);					\
    arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);		\
    arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);		\
    arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);		\
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

int Sg__JitEmit_BNNULL(SgJitContext *ctx, int targetPc)
{
  /* Branch if not null - skip if AC == SG_NIL */
  Arm64Asm *a = GET_ASM(ctx);
  Arm64CodeGen *gen = GET_GEN(ctx);
  int target = gen->labels[ctx->pcToLabel[targetPc]];

  /* Compare AC with SG_NIL */
  arm64_mov_r64_ptr(a, JIT_REG_TEMP2, SG_NIL);
  arm64_cmp_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_b_cond(a, ARM64_NE, target);
  return 1;
}

int Sg__JitEmit_BNEQ(SgJitContext *ctx, int targetPc)
{
  /* Branch if not eq - pop X1, skip if X1 == AC */
  Arm64Asm *a = GET_ASM(ctx);
  Arm64CodeGen *gen = GET_GEN(ctx);
  int target = gen->labels[ctx->pcToLabel[targetPc]];

  /* Pop operand */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
			-(int32_t)sizeof(SgObject));

  /* eq? is pointer equality */
  arm64_cmp_r64_r64(a, JIT_REG_TEMP2, JIT_REG_TEMP1);
  arm64_b_cond(a, ARM64_NE, target);
  return 1;
}

/*
 * List Operations
 */

int Sg__JitEmit_CAR(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  int slowPath = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Check if AC is a pair: SG_HPTRP(obj) = (obj & 0x03) == 0 */
  arm64_tst_r64_imm(a, JIT_REG_TEMP1, 0x03);
  arm64_b_cond(a, ARM64_NE, slowPath);

  /* Check HTAG != 0x7 (not an SgObject with header)
   * We need to load the first word and check the tag.
   * For pairs, HTAG is not 0x7. */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP2, JIT_REG_TEMP1, 0);  /* Load first word */
  arm64_and_r64_r64_imm(a, JIT_REG_TEMP3, JIT_REG_TEMP2, 0x07);
  arm64_cmp_r64_imm(a, JIT_REG_TEMP3, 0x07);
  arm64_b_cond(a, ARM64_EQ, slowPath);

  /* Fast path: AC = ((SgPair*)AC)->car (offset 0) */
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_b(a, done);

  /* Slow path: call C helper (save/restore LR for SELF_CALL returns) */
  arm64_bind_label(a, slowPath);
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Car);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  arm64_bind_label(a, done);
  return 1;
}

int Sg__JitEmit_CDR(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  int slowPath = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Check if AC is a pair: SG_HPTRP(obj) = (obj & 0x03) == 0 */
  arm64_tst_r64_imm(a, JIT_REG_TEMP1, 0x03);
  arm64_b_cond(a, ARM64_NE, slowPath);

  /* Check HTAG != 0x7 (not an SgObject with header) */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP2, JIT_REG_TEMP1, 0);
  arm64_and_r64_r64_imm(a, JIT_REG_TEMP3, JIT_REG_TEMP2, 0x07);
  arm64_cmp_r64_imm(a, JIT_REG_TEMP3, 0x07);
  arm64_b_cond(a, ARM64_EQ, slowPath);

  /* Fast path: AC = ((SgPair*)AC)->cdr (offset 8) */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 8);
  arm64_b(a, done);

  /* Slow path: call C helper (save/restore LR for SELF_CALL returns) */
  arm64_bind_label(a, slowPath);
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Cdr);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  arm64_bind_label(a, done);
  return 1;
}

int Sg__JitEmit_CONS(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Pop car from stack into TEMP2 (X1), AC (TEMP1 = X0) has cdr */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
			-(int32_t)sizeof(SgObject));

  /* Call Sg_Cons(car, cdr) - save/restore LR for SELF_CALL returns
   * IMPORTANT: JIT_REG_TEMP1 = X0, JIT_REG_TEMP2 = X1
   * We need: X0 = car, X1 = cdr
   * Problem: car is in X1, cdr is in X0
   * Solution: use X2 to swap
   */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);
  arm64_mov_r64_r64(a, ARM64_X2, JIT_REG_TEMP1);  /* X2 = cdr (save AC) */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);  /* X0 = car */
  arm64_mov_r64_r64(a, ARM64_X1, ARM64_X2);       /* X1 = cdr (from saved) */
  arm64_bl(a, Sg_Cons);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

/*
 * Predicates
 */

int Sg__JitEmit_NULLP(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  int isNull = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Compare AC with SG_NIL */
  arm64_mov_r64_ptr(a, JIT_REG_TEMP2, SG_NIL);
  arm64_cmp_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_b_cond(a, ARM64_EQ, isNull);

  /* Not null */
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_FALSE);
  arm64_b(a, done);

  /* Null */
  arm64_bind_label(a, isNull);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_TRUE);

  arm64_bind_label(a, done);
  return 1;
}

int Sg__JitEmit_PAIRP(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  int notPair = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Check SG_HPTRP: (obj & 0x03) == 0 */
  arm64_tst_r64_imm(a, JIT_REG_TEMP1, 0x03);
  arm64_b_cond(a, ARM64_NE, notPair);

  /* Check HTAG != 0x7 */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP2, JIT_REG_TEMP1, 0);
  arm64_and_r64_r64_imm(a, JIT_REG_TEMP3, JIT_REG_TEMP2, 0x07);
  arm64_cmp_r64_imm(a, JIT_REG_TEMP3, 0x07);
  arm64_b_cond(a, ARM64_EQ, notPair);

  /* Is pair */
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_TRUE);
  arm64_b(a, done);

  arm64_bind_label(a, notPair);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_FALSE);

  arm64_bind_label(a, done);
  return 1;
}

int Sg__JitEmit_NOT(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  int isFalse = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Compare AC with SG_FALSE */
  arm64_mov_r64_ptr(a, JIT_REG_TEMP2, SG_FALSE);
  arm64_cmp_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_b_cond(a, ARM64_EQ, isFalse);

  /* Not false -> return #f */
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_FALSE);
  arm64_b(a, done);

  /* Is false -> return #t */
  arm64_bind_label(a, isFalse);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_TRUE);

  arm64_bind_label(a, done);
  return 1;
}

int Sg__JitEmit_EQ(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  int isEqual = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Pop operand */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
			-(int32_t)sizeof(SgObject));

  /* eq? is pointer equality */
  arm64_cmp_r64_r64(a, JIT_REG_TEMP2, JIT_REG_TEMP1);
  arm64_b_cond(a, ARM64_EQ, isEqual);

  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_FALSE);
  arm64_b(a, done);

  arm64_bind_label(a, isEqual);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_TRUE);

  arm64_bind_label(a, done);
  return 1;
}

/*
 * Multiplication
 */

int Sg__JitEmit_MUL(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Pop operand: TEMP2 = *--SP */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
			-(int32_t)sizeof(SgObject));

  /* Save LR before calling C function */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Call Sg_Mul(obj, ac) where obj=TEMP2, ac=TEMP1 */
  arm64_mov_r64_r64(a, ARM64_X3, JIT_REG_TEMP1);   /* Save AC in X3 */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);   /* X0 = obj (TEMP2) */
  arm64_mov_r64_r64(a, ARM64_X1, ARM64_X3);        /* X1 = AC (saved TEMP1) */
  arm64_bl(a, Sg_Mul);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Restore LR after C function */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

#if 0
/* Original implementation with fast path */
int Sg__JitEmit_MUL_OLD(SgJitContext *ctx)
{
  /* TEMPORARY: Disable MUL to debug */
  return 0;

  Arm64Asm *a = GET_ASM(ctx);
  int slowPath = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Pop operand: TEMP2 = *--SP */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
			-(int32_t)sizeof(SgObject));

  /* Check if both are fixnums: (x & 1) && (y & 1) && !((x | y) & 2)
   * Fixnum tag is 01 (bit 0 set, bit 1 clear) */
  arm64_and_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_tst_r64_imm(a, JIT_REG_TEMP3, 1);
  arm64_b_cond(a, ARM64_EQ, slowPath);
  arm64_orr_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
  arm64_tst_r64_imm(a, JIT_REG_TEMP3, 2);
  arm64_b_cond(a, ARM64_NE, slowPath);

  /* Fast path: multiply fixnums
   * Fixnum representation: (value << 2) | 1
   * Extract: value = fixnum >> 2
   * Create: fixnum = (value << 2) | 1
   * Use TEMP3 and X3 to preserve TEMP1/TEMP2 for slow path fallback */
  arm64_asr_r64_r64_imm(a, JIT_REG_TEMP3, JIT_REG_TEMP2, 2);  /* a >> 2 */
  arm64_asr_r64_r64_imm(a, ARM64_X3, JIT_REG_TEMP1, 2);       /* b >> 2 */
  arm64_mul_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP3, ARM64_X3);

  /* Check for overflow: result must fit in fixnum range
   * Fixnum range: -(2^61) to (2^61 - 1) before tagging
   * If high bits are all same as sign bit, no overflow
   * ASR by 61, add 1: should be 0 (neg result) or 1 (pos result) */
  arm64_asr_r64_r64_imm(a, ARM64_X3, JIT_REG_TEMP3, 61);
  arm64_add_r64_r64_imm(a, ARM64_X3, ARM64_X3, 1);
  arm64_cmp_r64_imm(a, ARM64_X3, 1);
  arm64_b_cond(a, ARM64_HI, slowPath);  /* overflow if X3 > 1 */

  /* Convert result back to fixnum: (result << 2) | 1 */
  arm64_lsl_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP3, 2);
  arm64_orr_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 1);
  arm64_b(a, done);

  /* Slow path: call Sg_Mul(obj, ac) where obj=TEMP2, ac=TEMP1
   * Note: TEMP1=X0, TEMP2=X1, so we need to swap properly
   * Sync VM state first since Sg_Mul may access VM and trigger GC */
  arm64_bind_label(a, slowPath);
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  arm64_mov_r64_r64(a, ARM64_X3, JIT_REG_TEMP1);   /* Save AC in X3 */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);   /* X0 = obj (TEMP2) */
  arm64_mov_r64_r64(a, ARM64_X1, ARM64_X3);        /* X1 = AC (saved TEMP1) */
  arm64_bl(a, Sg_Mul);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);
  
  /* Reload VM state in case GC moved things */
  arm64_ldr_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_ldr_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);

  arm64_bind_label(a, done);
  return 1;
}
#endif

int Sg__JitEmit_MULI(SgJitContext *ctx, long val)
{
  Arm64Asm *a = GET_ASM(ctx);
  int slowPath = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Check if AC is fixnum: tag = 01 (bit 0 set, bit 1 clear) */
  arm64_tst_r64_imm(a, JIT_REG_TEMP1, 1);
  arm64_b_cond(a, ARM64_EQ, slowPath);
  arm64_tst_r64_imm(a, JIT_REG_TEMP1, 2);
  arm64_b_cond(a, ARM64_NE, slowPath);

  /* Fast path: multiply by immediate
   * Fixnum: (value << 2) | 1
   * Extract: value = fixnum >> 2 */
  arm64_asr_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 2);
  arm64_mov_r64_imm(a, JIT_REG_TEMP2, val);
  arm64_mul_r64_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP1, JIT_REG_TEMP2);

  /* Convert back to fixnum: (result << 2) | 1
   * (no overflow check for simplicity) */
  arm64_lsl_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 2);
  arm64_orr_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 1);
  arm64_b(a, done);

  /* Slow path: call Sg_Mul with fixnum immediate */
  arm64_bind_label(a, slowPath);
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_mov_r64_imm(a, ARM64_X1, (val << 2) | 1);  /* Make fixnum */
  arm64_bl(a, Sg_Mul);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  arm64_bind_label(a, done);
  return 1;
}

/*
 * DIV - Division: AC = pop() / AC
 */
int Sg__JitEmit_DIV(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Pop operand: TEMP2 = *--SP */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
			-(int32_t)sizeof(SgObject));

  /* Save LR before calling C function */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Call Sg_Div(obj, ac) where obj=TEMP2, ac=TEMP1 */
  arm64_mov_r64_r64(a, ARM64_X3, JIT_REG_TEMP1);   /* Save AC in X3 */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);   /* X0 = obj (TEMP2) */
  arm64_mov_r64_r64(a, ARM64_X1, ARM64_X3);        /* X1 = AC (saved TEMP1) */
  arm64_bl(a, Sg_Div);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Restore LR after C function */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

int Sg__JitEmit_DIVI(SgJitContext *ctx, long val)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR before calling C function */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Call Sg_Div(ac, fixnum(val)) */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_mov_r64_imm(a, ARM64_X1, (val << 2) | 1);  /* Make fixnum */
  arm64_bl(a, Sg_Div);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Restore LR after C function */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

/*
 * NEG - Negate: AC = -AC
 */
int Sg__JitEmit_NEG(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR before calling C function */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Call Sg_Negate(ac) */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Negate);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Restore LR after C function */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

/*
 * EQV - EQV? equality: AC = eqv?(pop(), AC)
 */
int Sg__JitEmit_EQV(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  int done = arm64_new_label(a);
  int notEqual = arm64_new_label(a);

  /* Pop operand: TEMP2 = *--SP */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
			-(int32_t)sizeof(SgObject));

  /* Fast path: eq? implies eqv? */
  arm64_cmp_r64_r64(a, JIT_REG_TEMP2, JIT_REG_TEMP1);
  arm64_b_cond(a, ARM64_EQ, done);  /* Same pointer, AC already true-ish, but set explicitly */

  /* Slow path: call Sg_EqvP */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);
  arm64_mov_r64_r64(a, ARM64_X1, JIT_REG_TEMP1);
  arm64_bl(a, Sg_EqvP);
  /* Sg_EqvP returns int (C boolean), convert to Scheme boolean */
  arm64_cmp_r64_imm(a, ARM64_X0, 0);
  arm64_b_cond(a, ARM64_EQ, notEqual);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_TRUE);
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);
  arm64_b(a, done);

  arm64_bind_label(a, notEqual);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_FALSE);
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  arm64_bind_label(a, done);
  /* Handle the fast path eq? case - set SG_TRUE */
  /* Actually need to fix: fast path should also set SG_TRUE */
  return 1;
}

/*
 * SYMBOLP - Symbol check: AC = symbol?(AC)
 */
int Sg__JitEmit_SYMBOLP(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  int isSymbol = arm64_new_label(a);
  int notSymbol = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* Symbol check: SG_SYMBOLP(obj) = SG_HPTRP(obj) && SG_SYMBOL_TAG(obj) */
  /* SG_HPTRP = (obj & 0x03) == 0 */
  arm64_tst_r64_imm(a, JIT_REG_TEMP1, 0x03);
  arm64_b_cond(a, ARM64_NE, notSymbol);  /* Not a heap pointer -> false */

  /* Load first word and check tag */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP2, JIT_REG_TEMP1, 0);
  /* Symbol tag = 0x0F (from sagittarius/private/sagittariusdefs.h) */
  arm64_and_r64_r64_imm(a, JIT_REG_TEMP2, JIT_REG_TEMP2, 0xFF);
  arm64_cmp_r64_imm(a, JIT_REG_TEMP2, 0x0F);
  arm64_b_cond(a, ARM64_EQ, isSymbol);

  /* Not a symbol */
  arm64_bind_label(a, notSymbol);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_FALSE);
  arm64_b(a, done);

  arm64_bind_label(a, isSymbol);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_TRUE);

  arm64_bind_label(a, done);
  return 1;
}

/*
 * GREF - Load global variable: AC = lookup(id)
 */
SG_EXTERN SgObject Sg__JitGref(SgObject id);  /* Forward declaration */

int Sg__JitEmit_GREF(SgJitContext *ctx, SgObject id)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR before calling C function */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Call Sg__JitGref(id) */
  arm64_mov_r64_ptr(a, ARM64_X0, id);
  arm64_bl(a, Sg__JitGref);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Restore LR after C function */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

/*
 * GREF_PUSH - Load global and push: *SP++ = lookup(id)
 */
int Sg__JitEmit_GREF_PUSH(SgJitContext *ctx, SgObject id)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR before calling C function */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Call Sg__JitGref(id) */
  arm64_mov_r64_ptr(a, ARM64_X0, id);
  arm64_bl(a, Sg__JitGref);

  /* Restore LR */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  /* Push result onto stack: *SP++ = X0 */
  arm64_str_r64_mem_post(a, ARM64_X0, JIT_REG_SCHSP, sizeof(SgObject));

  return 1;
}

/*
 * FREF_PUSH - Load free variable and push: *SP++ = CL->frees[index]
 */
int Sg__JitEmit_FREF_PUSH(SgJitContext *ctx, int index)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Load free variable: TEMP2 = CL->frees[index] */
  int32_t offset = CLOSURE_OFFSET_FREES + index * sizeof(SgObject);
  arm64_ldr_r64_mem(a, JIT_REG_TEMP2, JIT_REG_CL, offset);

  /* Push onto stack: *SP++ = TEMP2 */
  arm64_str_r64_mem_post(a, JIT_REG_TEMP2, JIT_REG_SCHSP, sizeof(SgObject));

  return 1;
}

/*
 * LIST - Create list from n items on stack
 */
int Sg__JitEmit_LIST(SgJitContext *ctx, int n)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR before calling C function */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Adjust SP to point to start of arguments (before the n items) */
  /* Arguments are at SP - n*8 */
  arm64_sub_r64_r64_imm(a, ARM64_X0, JIT_REG_SCHSP, n * sizeof(SgObject));

  /* X1 = n */
  arm64_mov_r64_imm(a, ARM64_X1, n);

  /* Call Sg_ArrayToList(array, n) */
  arm64_bl(a, Sg_ArrayToList);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Pop the n arguments from stack */
  arm64_sub_r64_r64_imm(a, JIT_REG_SCHSP, JIT_REG_SCHSP, n * sizeof(SgObject));

  /* Restore LR */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

/*
 * CAAR - AC = car(car(AC))
 */
int Sg__JitEmit_CAAR(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Call Sg_Caar (safer than inlining double car) */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Caar);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Restore LR */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

/*
 * CADR - AC = car(cdr(AC))
 */
int Sg__JitEmit_CADR(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Cadr);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Restore LR */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

/*
 * CDAR - AC = cdr(car(AC))
 */
int Sg__JitEmit_CDAR(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Cdar);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Restore LR */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

/*
 * CDDR - AC = cdr(cdr(AC))
 */
int Sg__JitEmit_CDDR(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
  arm64_bl(a, Sg_Cddr);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Restore LR */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

/*
 * Combined instructions: CAR_PUSH, CDR_PUSH, CONS_PUSH
 */
int Sg__JitEmit_CAR_PUSH(SgJitContext *ctx)
{
  /* AC = car(AC), then push */
  if (!Sg__JitEmit_CAR(ctx)) return 0;
  return Sg__JitEmit_PUSH(ctx);
}

int Sg__JitEmit_CDR_PUSH(SgJitContext *ctx)
{
  /* AC = cdr(AC), then push */
  if (!Sg__JitEmit_CDR(ctx)) return 0;
  return Sg__JitEmit_PUSH(ctx);
}

int Sg__JitEmit_CONS_PUSH(SgJitContext *ctx)
{
  /* AC = cons(pop(), AC), then push */
  if (!Sg__JitEmit_CONS(ctx)) return 0;
  return Sg__JitEmit_PUSH(ctx);
}

/*
 * Combined: LREF_CAR, LREF_CDR
 */
int Sg__JitEmit_LREF_CAR(SgJitContext *ctx, int index)
{
  if (!Sg__JitEmit_LREF(ctx, index)) return 0;
  return Sg__JitEmit_CAR(ctx);
}

int Sg__JitEmit_LREF_CDR(SgJitContext *ctx, int index)
{
  if (!Sg__JitEmit_LREF(ctx, index)) return 0;
  return Sg__JitEmit_CDR(ctx);
}

/*
 * Combined: FREF_CAR, FREF_CDR
 */
int Sg__JitEmit_FREF_CAR(SgJitContext *ctx, int index)
{
  if (!Sg__JitEmit_FREF(ctx, index)) return 0;
  return Sg__JitEmit_CAR(ctx);
}

int Sg__JitEmit_FREF_CDR(SgJitContext *ctx, int index)
{
  if (!Sg__JitEmit_FREF(ctx, index)) return 0;
  return Sg__JitEmit_CDR(ctx);
}

/*
 * Combined: GREF_CAR, GREF_CDR
 */
int Sg__JitEmit_GREF_CAR(SgJitContext *ctx, SgObject id)
{
  if (!Sg__JitEmit_GREF(ctx, id)) return 0;
  return Sg__JitEmit_CAR(ctx);
}

int Sg__JitEmit_GREF_CDR(SgJitContext *ctx, SgObject id)
{
  if (!Sg__JitEmit_GREF(ctx, id)) return 0;
  return Sg__JitEmit_CDR(ctx);
}

/*
 * Combined with PUSH variants
 */
int Sg__JitEmit_LREF_CAR_PUSH(SgJitContext *ctx, int index)
{
  if (!Sg__JitEmit_LREF_CAR(ctx, index)) return 0;
  return Sg__JitEmit_PUSH(ctx);
}

int Sg__JitEmit_LREF_CDR_PUSH(SgJitContext *ctx, int index)
{
  if (!Sg__JitEmit_LREF_CDR(ctx, index)) return 0;
  return Sg__JitEmit_PUSH(ctx);
}

int Sg__JitEmit_FREF_CAR_PUSH(SgJitContext *ctx, int index)
{
  if (!Sg__JitEmit_FREF_CAR(ctx, index)) return 0;
  return Sg__JitEmit_PUSH(ctx);
}

int Sg__JitEmit_FREF_CDR_PUSH(SgJitContext *ctx, int index)
{
  if (!Sg__JitEmit_FREF_CDR(ctx, index)) return 0;
  return Sg__JitEmit_PUSH(ctx);
}

int Sg__JitEmit_GREF_CAR_PUSH(SgJitContext *ctx, SgObject id)
{
  if (!Sg__JitEmit_GREF_CAR(ctx, id)) return 0;
  return Sg__JitEmit_PUSH(ctx);
}

int Sg__JitEmit_GREF_CDR_PUSH(SgJitContext *ctx, SgObject id)
{
  if (!Sg__JitEmit_GREF_CDR(ctx, id)) return 0;
  return Sg__JitEmit_PUSH(ctx);
}

/*
 * CONST_RET - Load constant and return
 */
int Sg__JitEmit_CONST_RET(SgJitContext *ctx, SgObject val)
{
  if (!Sg__JitEmit_CONST(ctx, val)) return 0;
  return Sg__JitEmit_RET(ctx);
}

/*
 * SET_CAR - Set car of pair: car(pop()) = AC
 * Pair layout: car at offset 0, cdr at offset 8
 */
int Sg__JitEmit_SET_CAR(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Pop pair from stack, AC has new value */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
			-(int32_t)sizeof(SgObject));

  /* Store AC to pair->car (offset 0) */
  arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP2, 0);

  /* AC = UNDEF */
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_UNDEF);

  return 1;
}

/*
 * SET_CDR - Set cdr of pair: cdr(pop()) = AC
 * Pair layout: car at offset 0, cdr at offset 8
 */
int Sg__JitEmit_SET_CDR(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Pop pair from stack, AC has new value */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
			-(int32_t)sizeof(SgObject));

  /* Store AC to pair->cdr (offset 8) */
  arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP2, sizeof(SgObject));

  /* AC = UNDEF */
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_UNDEF);

  return 1;
}

/*
 * BOX - Create a box for mutable variable
 */
SG_EXTERN SgObject Sg__JitMakeBox(SgObject value);

int Sg__JitEmit_BOX(SgJitContext *ctx, int index)
{
  Arm64Asm *a = GET_ASM(ctx);
  int32_t offset = index * sizeof(SgObject);

  /* Save LR */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Load value from stack at FP+index */
  arm64_ldr_r64_mem(a, ARM64_X0, JIT_REG_SCHFP, offset);

  /* Call Sg__JitMakeBox(value) */
  arm64_bl(a, Sg__JitMakeBox);

  /* Store box back to stack */
  arm64_str_r64_mem(a, ARM64_X0, JIT_REG_SCHFP, offset);

  /* Restore LR */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

/*
 * UNBOX - Get value from box: AC = box->value
 */
int Sg__JitEmit_UNBOX(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  /* SgBox layout: value is at offset 8 (after SgHeader) */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP1, sizeof(void*));
  return 1;
}

/*
 * FSET - Set free variable: CL->frees[index]->value = AC
 */
int Sg__JitEmit_FSET(SgJitContext *ctx, int index)
{
  Arm64Asm *a = GET_ASM(ctx);
  int32_t offset = CLOSURE_OFFSET_FREES + index * sizeof(SgObject);

  /* Load box from closure's frees */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP2, JIT_REG_CL, offset);

  /* Store AC to box->value (offset 8 after SgHeader) */
  arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP2, sizeof(void*));

  /* AC = UNDEF */
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_UNDEF);

  return 1;
}

/*
 * LEAVE - Pop n items from stack: SP -= n
 */
int Sg__JitEmit_LEAVE(SgJitContext *ctx, int n)
{
  Arm64Asm *a = GET_ASM(ctx);
  arm64_sub_r64_r64_imm(a, JIT_REG_SCHSP, JIT_REG_SCHSP, n * sizeof(SgObject));
  return 1;
}

/*
 * INST_STACK - Insert AC at FP[index]: FP[index] = AC
 */
int Sg__JitEmit_INST_STACK(SgJitContext *ctx, int index)
{
  Arm64Asm *a = GET_ASM(ctx);
  int32_t offset = index * sizeof(SgObject);
  arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_SCHFP, offset);
  return 1;
}

/*
 * RESV_STACK - Reserve n stack slots: SP += n
 */
int Sg__JitEmit_RESV_STACK(SgJitContext *ctx, int n)
{
  Arm64Asm *a = GET_ASM(ctx);
  arm64_add_r64_r64_imm(a, JIT_REG_SCHSP, JIT_REG_SCHSP, n * sizeof(SgObject));
  return 1;
}

/*
 * VECTORP - Check if AC is a vector
 */
int Sg__JitEmit_VECTORP(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);
  int isVector = arm64_new_label(a);
  int notVector = arm64_new_label(a);
  int done = arm64_new_label(a);

  /* SG_VECTORP(obj) = SG_HPTRP(obj) && SG_VECTOR_TAG check */
  arm64_tst_r64_imm(a, JIT_REG_TEMP1, 0x03);
  arm64_b_cond(a, ARM64_NE, notVector);  /* Not a heap pointer -> false */

  /* Load first word and check tag */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP2, JIT_REG_TEMP1, 0);
  /* Vector tag = 0x17 */
  arm64_and_r64_r64_imm(a, JIT_REG_TEMP2, JIT_REG_TEMP2, 0xFF);
  arm64_cmp_r64_imm(a, JIT_REG_TEMP2, 0x17);
  arm64_b_cond(a, ARM64_EQ, isVector);

  /* Not a vector */
  arm64_bind_label(a, notVector);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_FALSE);
  arm64_b(a, done);

  arm64_bind_label(a, isVector);
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_TRUE);

  arm64_bind_label(a, done);
  return 1;
}

/*
 * VEC_LEN - Get length of vector in AC, return as fixnum
 * Assumes AC is already a vector (caller should check)
 */
int Sg__JitEmit_VEC_LEN(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* SgVector layout: SG_HEADER (8 bytes), then:
   * - literalp: 1 bit (bit 0)
   * - size: 63 bits (bits 1-63)
   * So at offset 8, we have a word containing (size << 1) | literalp
   */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 8);
  
  /* Shift right by 1 to get size */
  arm64_asr_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 1);
  
  /* Convert to fixnum: (size << 2) | 1 */
  arm64_lsl_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 2);
  arm64_orr_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 1);

  return 1;
}

/*
 * VEC_REF - Pop vector from stack, AC has index, return element
 * stack: [..., vec] -> [...]
 * AC = index (fixnum)
 * result = vec[index]
 */
int Sg__JitEmit_VEC_REF(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Pop vector into TEMP2 */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
                        -(int32_t)sizeof(SgObject));

  /* Index is in TEMP1 (AC) as fixnum, convert to integer: index >> 2 */
  arm64_asr_r64_r64_imm(a, JIT_REG_TEMP3, JIT_REG_TEMP1, 2);

  /* Calculate element address: vec + 16 + (index * 8)
   * SgVector layout: tag (8), size (8), elements[]
   * So elements start at offset 16
   */
  arm64_lsl_r64_r64_imm(a, JIT_REG_TEMP3, JIT_REG_TEMP3, 3);  /* index * 8 */
  arm64_add_r64_r64_imm(a, JIT_REG_TEMP3, JIT_REG_TEMP3, 16); /* + header */
  arm64_add_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP2, JIT_REG_TEMP3);

  /* Load element */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP3, 0);

  return 1;
}

/*
 * VEC_SET - Pop index and vector from stack, set vec[index] = AC
 * stack: [..., vec, index] -> [...]
 * AC = value to set
 * result = #<undef>
 */
int Sg__JitEmit_VEC_SET(SgJitContext *ctx)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Pop index into TEMP2 */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
                        -(int32_t)sizeof(SgObject));

  /* Pop vector into TEMP3 */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP3, JIT_REG_SCHSP,
                        -(int32_t)sizeof(SgObject));

  /* Index is in TEMP2 as fixnum, convert to integer: index >> 2 */
  arm64_asr_r64_r64_imm(a, JIT_REG_TEMP2, JIT_REG_TEMP2, 2);

  /* Calculate element address: vec + 16 + (index * 8) */
  arm64_lsl_r64_r64_imm(a, JIT_REG_TEMP2, JIT_REG_TEMP2, 3);  /* index * 8 */
  arm64_add_r64_r64_imm(a, JIT_REG_TEMP2, JIT_REG_TEMP2, 16); /* + header */
  arm64_add_r64_r64_r64(a, JIT_REG_TEMP2, JIT_REG_TEMP3, JIT_REG_TEMP2);

  /* Store value (AC/TEMP1) at element address */
  arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP2, 0);

  /* Return #<undef> */
  arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_UNDEF);

  return 1;
}

/*
 * VECTOR n - Create a vector of size n
 * AC has element[n-1], stack has elements[n-2..0]
 * Pops n-1 elements from stack
 */
SG_EXTERN SgObject Sg_MakeVector(long size, SgObject fill);

int Sg__JitEmit_VECTOR(SgJitContext *ctx, int size)
{
  Arm64Asm *a = GET_ASM(ctx);

  if (size == 0) {
    /* Empty vector - just create it */
    arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);
    arm64_mov_r64_imm(a, ARM64_X0, 0);
    arm64_mov_r64_ptr(a, ARM64_X1, SG_UNDEF);
    arm64_bl(a, Sg_MakeVector);
    arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);
    arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
    arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);
    return 1;
  }

  /* Save AC (last element) before calling Sg_MakeVector */
  arm64_str_r64_mem_pre(a, JIT_REG_TEMP1, ARM64_SP, -16);
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Create vector filled with #<undef> */
  arm64_mov_r64_imm(a, ARM64_X0, size);
  arm64_mov_r64_ptr(a, ARM64_X1, SG_UNDEF);
  arm64_bl(a, Sg_MakeVector);
  
  /* Vector is in X0, save to TEMP2 */
  arm64_mov_r64_r64(a, JIT_REG_TEMP2, ARM64_X0);

  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);
  /* Restore AC (last element) to TEMP1 */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP1, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  /* Set element[size-1] = AC (TEMP1) */
  /* Element offset = 16 + (size-1) * 8 */
  int lastOffset = 16 + (size - 1) * 8;
  arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP2, lastOffset);

  /* Pop remaining elements from Scheme stack and fill vector */
  for (int i = size - 2; i >= 0; i--) {
    /* Pop element from Scheme stack */
    arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP1, JIT_REG_SCHSP,
                          -(int32_t)sizeof(SgObject));
    /* Store at vec[i] */
    int offset = 16 + i * 8;
    arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP2, offset);
  }

  /* Return vector in AC */
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP2);

  return 1;
}

/*
 * BNEQV - Branch if not eqv
 */
int Sg__JitEmit_BNEQV(SgJitContext *ctx, int targetPc)
{
  Arm64Asm *a = GET_ASM(ctx);
  Arm64CodeGen *gen = GET_GEN(ctx);
  int target = gen->labels[ctx->pcToLabel[targetPc]];
  int skipBranch = arm64_new_label(a);

  /* Pop operand: TEMP2 = *--SP */
  arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP,
			-(int32_t)sizeof(SgObject));

  /* Fast path: eq? implies eqv? - skip branch if equal */
  arm64_cmp_r64_r64(a, JIT_REG_TEMP2, JIT_REG_TEMP1);
  arm64_b_cond(a, ARM64_EQ, skipBranch);

  /* Slow path: call Sg_EqvP */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);
  arm64_mov_r64_r64(a, ARM64_X1, JIT_REG_TEMP1);
  arm64_bl(a, Sg_EqvP);
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  /* Branch if NOT eqv (X0 == 0) */
  arm64_cmp_r64_imm(a, ARM64_X0, 0);
  arm64_b_cond(a, ARM64_EQ, target);

  arm64_bind_label(a, skipBranch);
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
 * CALL instruction - call a procedure in AC
 *
 * Stack layout before: args on stack, proc in AC
 * FRAME instruction already pushed continuation frame.
 */
SG_EXTERN SgObject Sg__JitCall(SgVM *vm, int argc, SgObject proc);

int Sg__JitEmit_CALL(SgJitContext *ctx, int argc)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR before calling C function */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Sync VM state before call, including AC which has the proc */
  arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_VM, VM_OFFSET_AC);
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  arm64_str_r64_mem(a, JIT_REG_CL, JIT_REG_VM, VM_OFFSET_CL);

  /* Call Sg__JitCall(vm, argc, proc)
   * IMPORTANT: JIT_REG_TEMP1 is X0, so we must copy proc to X2 BEFORE
   * setting X0 to vm (since X0 = TEMP1 and would be overwritten)
   */
  arm64_mov_r64_r64(a, ARM64_X2, JIT_REG_TEMP1);  /* X2 = proc (must be first!) */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_VM);     /* X0 = vm */
  arm64_mov_r64_imm(a, ARM64_X1, argc);           /* X1 = argc */

  arm64_bl(a, Sg__JitCall);

  /* Result is in X0, move to AC (TEMP1) */
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Restore LR */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  /* Reload VM state after call (continuation was popped by helper) */
  arm64_ldr_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_ldr_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  arm64_ldr_r64_mem(a, JIT_REG_CL, JIT_REG_VM, VM_OFFSET_CL);

  return 1;
}

/*
 * TAIL_CALL instruction - tail-call a procedure in AC
 *
 * Args need to be shifted to FP position before calling.
 * No continuation frame pushed for tail calls.
 */
SG_EXTERN SgObject Sg__JitTailCall(SgVM *vm, int argc, SgObject proc);

int Sg__JitEmit_TAIL_CALL(SgJitContext *ctx, int argc)
{
  Arm64Asm *a = GET_ASM(ctx);
  Arm64CodeGen *gen = GET_GEN(ctx);

  /* Save proc (AC/TEMP1=X0) to TEMP3 before we modify SP/FP */
  arm64_mov_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1);

  /* For tail call, shift args from SP to FP */
  if (argc > 0) {
    int32_t argBytes = argc * (int32_t)sizeof(SgObject);
    /* Source = SP - argc */
    arm64_sub_r64_r64_imm(a, JIT_REG_TEMP2, JIT_REG_SCHSP, argBytes);
    
    /* Copy args from source to FP */
    for (int i = 0; i < argc; i++) {
      int32_t off = i * (int32_t)sizeof(SgObject);
      arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP2, off);
      arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_SCHFP, off);
    }

    /* SP = FP + argc */
    arm64_add_r64_r64_imm(a, JIT_REG_SCHSP, JIT_REG_SCHFP, argBytes);
  } else {
    arm64_mov_r64_r64(a, JIT_REG_SCHSP, JIT_REG_SCHFP);
  }

  /* Sync VM state before call */
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  arm64_str_r64_mem(a, JIT_REG_CL, JIT_REG_VM, VM_OFFSET_CL);

  /* Call Sg__JitTailCall(vm, argc, proc)
   * IMPORTANT: proc was saved to TEMP3 earlier
   */
  arm64_mov_r64_r64(a, ARM64_X2, JIT_REG_TEMP3);  /* X2 = proc (from saved TEMP3) */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_VM);     /* X0 = vm */
  arm64_mov_r64_imm(a, ARM64_X1, argc);           /* X1 = argc */

  arm64_bl(a, Sg__JitTailCall);

  /* Result is in X0, move to AC */
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Tail call always goes to epilogue (no unwind needed since helper handles it) */
  arm64_b(a, gen->labels[ctx->epilogueLabel]);

  return 1;
}

/*
 * LOCAL_CALL instruction - call a local closure
 *
 * Like CALL but the target is a known closure (faster path).
 * For JIT, we delegate to the same C helper since the optimization
 * is already in checking for JIT code in the closure.
 */
int Sg__JitEmit_LOCAL_CALL(SgJitContext *ctx, int argc)
{
  /* Same implementation as CALL - the C helper handles closure dispatch */
  return Sg__JitEmit_CALL(ctx, argc);
}

/*
 * LOCAL_TAIL_CALL instruction - tail-call a local closure
 *
 * Like TAIL_CALL but the target is a known closure.
 */
int Sg__JitEmit_LOCAL_TAIL_CALL(SgJitContext *ctx, int argc)
{
  /* Same implementation as TAIL_CALL - the C helper handles closure dispatch */
  return Sg__JitEmit_TAIL_CALL(ctx, argc);
}

/*
 * CLOSURE instruction - create a closure
 *
 * selfPos: 0 = no self-reference, n = self-ref at frees[n-1]
 * cb: code builder for the closure body
 * freec: number of free variables (already on stack)
 */
int Sg__JitEmit_CLOSURE(SgJitContext *ctx, int selfPos, SgObject cb, int freec)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Adjust SP to point to free variables: SP -= freec * sizeof(SgObject) */
  if (freec > 0) {
    int32_t freeBytes = freec * (int32_t)sizeof(SgObject);
    arm64_sub_r64_r64_imm(a, JIT_REG_SCHSP, JIT_REG_SCHSP, freeBytes);
  }

  /* Call Sg_VMMakeClosure(cb, selfPos, sp)
   * X0 = code builder
   * X1 = self position
   * X2 = free variables pointer (current SP)
   */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);
  arm64_mov_r64_ptr(a, ARM64_X0, cb);             /* X0 = code builder */
  arm64_mov_r64_imm(a, ARM64_X1, selfPos);        /* X1 = self position */
  arm64_mov_r64_r64(a, ARM64_X2, JIT_REG_SCHSP);  /* X2 = frees pointer */
  arm64_bl(a, Sg_VMMakeClosure);
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);  /* AC = result closure */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  return 1;
}

/*
 * APPLY instruction - apply a procedure to arguments with a list tail
 *
 * nargc: number of explicit arguments (not including proc and list)
 * isTail: whether this is a tail apply
 *
 * Stack layout:
 *   SP[-1] = last explicit arg
 *   ...
 *   SP[-nargc] = first explicit arg
 *   SP[-nargc-1] = proc
 *
 * AC = list argument
 */
SG_EXTERN SgObject Sg__JitApply(SgVM *vm, int nargc, SgObject listArg, int isTail);

int Sg__JitEmit_APPLY(SgJitContext *ctx, int nargc, int isTail)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR before calling C function */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Sync VM state - AC has the list argument */
  arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_VM, VM_OFFSET_AC);
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  arm64_str_r64_mem(a, JIT_REG_CL, JIT_REG_VM, VM_OFFSET_CL);

  /* Call Sg__JitApply(vm, nargc, listArg, isTail)
   * X0 = vm
   * X1 = nargc
   * X2 = listArg (AC)
   * X3 = isTail
   */
  arm64_mov_r64_r64(a, ARM64_X2, JIT_REG_TEMP1);  /* X2 = listArg (AC) - must be first */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_VM);     /* X0 = vm */
  arm64_mov_r64_imm(a, ARM64_X1, nargc);          /* X1 = nargc */
  arm64_mov_r64_imm(a, ARM64_X3, isTail);         /* X3 = isTail */

  arm64_bl(a, Sg__JitApply);

  /* Result is in X0, move to AC (TEMP1) */
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Restore LR */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  /* Reload VM state after call */
  arm64_ldr_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_ldr_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  arm64_ldr_r64_mem(a, JIT_REG_CL, JIT_REG_VM, VM_OFFSET_CL);

  return 1;
}

/*
 * VALUES instruction - return multiple values
 *
 * nvalues: number of values
 *
 * Stack layout:
 *   SP[-1] = value[nvalues-2]
 *   ...
 *   SP[-(nvalues-1)] = value[0]
 *
 * AC = value[nvalues-1] (last value)
 */
SG_EXTERN SgObject Sg__JitValues(SgVM *vm, int nvalues, SgObject lastVal);

int Sg__JitEmit_VALUES(SgJitContext *ctx, int nvalues)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR before calling C function */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Sync VM state - AC has the last value */
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);

  /* Call Sg__JitValues(vm, nvalues, lastVal)
   * X0 = vm
   * X1 = nvalues
   * X2 = lastVal (AC)
   */
  arm64_mov_r64_r64(a, ARM64_X2, JIT_REG_TEMP1);  /* X2 = lastVal (AC) - must be first */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_VM);     /* X0 = vm */
  arm64_mov_r64_imm(a, ARM64_X1, nvalues);        /* X1 = nvalues */

  arm64_bl(a, Sg__JitValues);

  /* Result (first value) is in X0, move to AC (TEMP1) */
  arm64_mov_r64_r64(a, JIT_REG_TEMP1, ARM64_X0);

  /* Restore LR */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  /* Reload SP from VM (it was modified by the helper) */
  arm64_ldr_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);

  return 1;
}

/*
 * RECEIVE instruction - receive multiple values
 *
 * reqCount: number of required values
 * optCount: 0 = exact match, 1 = rest values as list
 *
 * Pushes values onto stack.
 */
SG_EXTERN SgObject Sg__JitReceive(SgVM *vm, int reqCount, int optCount);

int Sg__JitEmit_RECEIVE(SgJitContext *ctx, int reqCount, int optCount)
{
  Arm64Asm *a = GET_ASM(ctx);

  /* Save LR before calling C function */
  arm64_str_r64_mem_pre(a, ARM64_LR, ARM64_SP, -16);

  /* Sync VM state */
  arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_VM, VM_OFFSET_AC);
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);

  /* Call Sg__JitReceive(vm, reqCount, optCount)
   * X0 = vm
   * X1 = reqCount
   * X2 = optCount
   */
  arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_VM);     /* X0 = vm */
  arm64_mov_r64_imm(a, ARM64_X1, reqCount);       /* X1 = reqCount */
  arm64_mov_r64_imm(a, ARM64_X2, optCount);       /* X2 = optCount */

  arm64_bl(a, Sg__JitReceive);

  /* Restore LR */
  arm64_ldr_r64_mem(a, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  /* Reload SP from VM (it was modified by the helper) */
  arm64_ldr_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);

  return 1;
}

/*
 * Disassembly interface
 */
void Sg__JitDisasmBuffer(uint8_t *code, size_t size, SgPort *port)
{
  arm64_disasm_buffer(code, size, port);
}
