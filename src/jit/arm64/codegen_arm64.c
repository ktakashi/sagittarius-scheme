/* codegen_arm64.c                                 -*- mode:c; coding:utf-8; -*-
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

#ifdef JIT_ARCH_arm64

#include "../../sagittarius.h"
#include "../../sagittarius/private/code.h"
#include "../../sagittarius/private/instruction.h"
#include "../../sagittarius/private/vm.h"
#include "../../sagittarius/private/closure.h"

#include <stddef.h>

/*
 * ARM64 JIT Code Generator for Sagittarius Scheme
 *
 * Memory Model:
 * - The Scheme stack is heap-allocated, accessed via vm->sp and vm->fp
 * - JIT code uses CPU registers as pointers to this heap memory
 * - No native stack is used for Scheme values
 *
 * Register Allocation (defined in asm_arm64.h):
 * - X19 (JIT_REG_VM):    SgVM* pointer
 * - X20 (JIT_REG_SCHSP): Scheme stack pointer (vm->sp)
 * - X21 (JIT_REG_SCHFP): Scheme frame pointer (vm->fp)
 * - X22 (JIT_REG_CL):    Current closure
 * - X0  (JIT_REG_TEMP1): Accumulator (AC) / return value
 * - X1-X3: Scratch registers
 */

/*
 * Fixnum tagging:
 * - Tag bits: lower 2 bits
 * - Fixnum tag: 0b01 (value 1)
 * - Value is stored shifted left by 2
 */
#define FIXNUM_TAG     1
#define FIXNUM_MASK    3
#define FIXNUM_SHIFT   2

/*
 * Code generator context
 */
typedef struct Arm64CodeGenRec {
  Arm64Asm *a;               /* Assembler context */
  SgCodeBuilder *cb;          /* Source bytecode */
  SgWord *code;               /* Bytecode array */
  int codeSize;               /* Number of bytecode words */

  /* Label for each bytecode position (for branches) */
  int *bcLabels;

  /* Labels for special code paths */
  int epilogueLabel;
} Arm64CodeGen;

/* Forward declarations */
static int emit_prologue(Arm64CodeGen *gen);
static int emit_epilogue(Arm64CodeGen *gen);
static int emit_instruction(Arm64CodeGen *gen, int pc);
static void emit_fallback_to_interpreter(Arm64CodeGen *gen);

/*
 * VM Structure Offsets
 *
 * These must match the actual struct layout in vm.h
 */
#define VM_OFFSET_SP    offsetof(SgVM, sp)
#define VM_OFFSET_FP    offsetof(SgVM, fp)
#define VM_OFFSET_AC    offsetof(SgVM, ac)
#define VM_OFFSET_CL    offsetof(SgVM, cl)
#define VM_OFFSET_CONT  offsetof(SgVM, cont)
#define VM_OFFSET_VALUESCOUNT offsetof(SgVM, valuesCount)

/*
 * Closure Structure Offsets
 */
#define CLOSURE_OFFSET_FREES offsetof(SgClosure, frees)

/*
 * Box Structure Offset
 */
#define BOX_OFFSET_VALUE offsetof(SgBox, value)


/*
 * Main compilation entry point
 */
SgJitCompiledCode Sg_JitCompileArm64(SgCodeBuilder *cb, SgJitCodeBuffer *buf)
{
  Arm64CodeGen gen;
  Arm64Asm *a;
  int pc;

  /* Initialize assembler */
  a = arm64_asm_new(buf->code, buf->size);
  if (a == NULL) {
    return NULL;
  }

  /* Initialize code generator */
  gen.a = a;
  gen.cb = cb;
  gen.code = cb->code;
  gen.codeSize = cb->size;

  /* Allocate labels for each bytecode position */
  gen.bcLabels = SG_NEW_ARRAY(int, cb->size);
  for (int i = 0; i < cb->size; i++) {
    gen.bcLabels[i] = arm64_new_label(a);
  }

  /* Create epilogue label */
  gen.epilogueLabel = arm64_new_label(a);

  /* Emit function prologue */
  if (!emit_prologue(&gen)) {
    arm64_asm_free(a);
    return NULL;
  }

  /* Emit code for each bytecode instruction */
  pc = 0;
  while (pc < gen.codeSize) {
    /* Bind label for this bytecode position */
    arm64_bind_label(a, gen.bcLabels[pc]);

    int nextPc = emit_instruction(&gen, pc);
    if (nextPc < 0) {
      /* Unsupported instruction - abort compilation */
      arm64_asm_free(a);
      return NULL;
    }
    pc = nextPc;
  }

  /* Emit epilogue */
  arm64_bind_label(a, gen.epilogueLabel);
  if (!emit_epilogue(&gen)) {
    arm64_asm_free(a);
    return NULL;
  }

  /* Resolve forward references */
  if (arm64_asm_finalize(a) != 0) {
    arm64_asm_free(a);
    return NULL;
  }

  /* Update buffer used size */
  buf->used = arm64_asm_size(a);

  /* Get the compiled function pointer */
  SgJitCompiledCode result = (SgJitCompiledCode)buf->code;

  arm64_asm_free(a);
  return result;
}


/*
 * Function Prologue
 *
 * Called with: X0 = SgVM*, X1 = SgClosure*
 * Sets up callee-saved registers with VM state.
 */
static int emit_prologue(Arm64CodeGen *gen)
{
  Arm64Asm *a = gen->a;

  /* Save frame pointer and link register (required by ABI) */
  /* STP X29, X30, [SP, #-16]! */
  arm64_stp_pre(a, ARM64_FP, ARM64_LR, ARM64_SP, -16);

  /* Set frame pointer */
  arm64_mov_r64_r64(a, ARM64_FP, ARM64_SP);

  /* Save callee-saved registers we'll use */
  /* STP X19, X20, [SP, #-16]! */
  arm64_stp_pre(a, ARM64_X19, ARM64_X20, ARM64_SP, -16);
  /* STP X21, X22, [SP, #-16]! */
  arm64_stp_pre(a, ARM64_X21, ARM64_X22, ARM64_SP, -16);

  /* Load VM pointer into X19 */
  arm64_mov_r64_r64(a, JIT_REG_VM, ARM64_X0);

  /* Load closure into X22 */
  arm64_mov_r64_r64(a, JIT_REG_CL, ARM64_X1);

  /* Load VM registers from VM structure */
  /* SP = vm->sp */
  arm64_ldr_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  /* FP = vm->fp */
  arm64_ldr_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  /* AC (X0) = vm->ac */
  arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_VM, VM_OFFSET_AC);

  return !arm64_asm_error(a);
}


/*
 * Function Epilogue
 *
 * Stores VM state back and returns.
 * Result value should be in X0 (JIT_REG_TEMP1).
 */
static int emit_epilogue(Arm64CodeGen *gen)
{
  Arm64Asm *a = gen->a;

  /* Store VM registers back to VM structure */
  /* vm->sp = SP */
  arm64_str_r64_mem(a, JIT_REG_SCHSP, JIT_REG_VM, VM_OFFSET_SP);
  /* vm->fp = FP */
  arm64_str_r64_mem(a, JIT_REG_SCHFP, JIT_REG_VM, VM_OFFSET_FP);
  /* vm->ac = X0 (result) */
  arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_VM, VM_OFFSET_AC);
  /* vm->cl = closure */
  arm64_str_r64_mem(a, JIT_REG_CL, JIT_REG_VM, VM_OFFSET_CL);

  /* Set valuesCount = 1 */
  arm64_mov_r64_imm(a, JIT_REG_TEMP2, 1);
  arm64_str_r64_mem(a, JIT_REG_TEMP2, JIT_REG_VM, VM_OFFSET_VALUESCOUNT);

  /* Restore callee-saved registers */
  /* LDP X21, X22, [SP], #16 */
  arm64_ldp(a, ARM64_X21, ARM64_X22, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);
  /* LDP X19, X20, [SP], #16 */
  arm64_ldp(a, ARM64_X19, ARM64_X20, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);
  /* LDP X29, X30, [SP], #16 */
  arm64_ldp(a, ARM64_FP, ARM64_LR, ARM64_SP, 0);
  arm64_add_r64_r64_imm(a, ARM64_SP, ARM64_SP, 16);

  /* Return */
  arm64_ret(a);

  return !arm64_asm_error(a);
}


/*
 * Emit code for a single bytecode instruction.
 * Returns the next PC, or -1 on error (unsupported instruction).
 */
static int emit_instruction(Arm64CodeGen *gen, int pc)
{
  Arm64Asm *a = gen->a;
  SgWord insn = gen->code[pc];
  int opcode = INSN(insn);
  int val1 = INSN_VALUE1(insn);

  switch (opcode) {

  /*
   * NOP - No operation
   */
  case NOP:
    arm64_nop(a);
    return pc + 1;

  /*
   * UNDEF - Load undefined value
   */
  case UNDEF:
    arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_UNDEF);
    return pc + 1;

  /*
   * CONST - Load constant from operand
   * Operand follows the instruction word.
   */
  case CONST:
    if (pc + 1 >= gen->codeSize) return -1;
    {
      SgObject constVal = SG_OBJ(gen->code[pc + 1]);
      arm64_mov_r64_ptr(a, JIT_REG_TEMP1, constVal);
    }
    return pc + 2;

  /*
   * CONSTI - Load small integer immediate
   * Value encoded in instruction.
   */
  case CONSTI:
    {
      /* val1 is a signed value */
      intptr_t intVal = val1;
      /* Convert to tagged fixnum */
      intptr_t tagged = (intVal << FIXNUM_SHIFT) | FIXNUM_TAG;
      arm64_mov_r64_imm(a, JIT_REG_TEMP1, tagged);
    }
    return pc + 1;

  /*
   * LREF - Load local variable
   * val1 = index into frame
   * AC = FP[val1]
   */
  case LREF:
    {
      int32_t offset = val1 * sizeof(SgObject);
      arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_SCHFP, offset);
    }
    return pc + 1;

  /*
   * LSET - Store to local variable (through box)
   * val1 = index into frame
   * FP[val1]->value = AC; AC = UNDEF
   */
  case LSET:
    {
      int32_t offset = val1 * sizeof(SgObject);
      /* Load box pointer from frame */
      arm64_ldr_r64_mem(a, JIT_REG_TEMP2, JIT_REG_SCHFP, offset);
      /* Store AC to box->value */
      arm64_str_r64_mem(a, JIT_REG_TEMP1, JIT_REG_TEMP2, BOX_OFFSET_VALUE);
      /* AC = SG_UNDEF */
      arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_UNDEF);
    }
    return pc + 1;

  /*
   * FREF - Load free variable from closure
   * val1 = index into closure's frees array
   * AC = CL->frees[val1]
   */
  case FREF:
    {
      int32_t offset = CLOSURE_OFFSET_FREES + val1 * sizeof(SgObject);
      arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_CL, offset);
    }
    return pc + 1;

  /*
   * PUSH - Push AC onto stack
   * *SP++ = AC
   */
  case PUSH:
    /* STR X0, [X20], #8  (post-increment) */
    arm64_str_r64_mem_post(a, JIT_REG_TEMP1, JIT_REG_SCHSP, sizeof(SgObject));
    return pc + 1;

  /*
   * LREF_PUSH - Combined LREF + PUSH
   */
  case LREF_PUSH:
    {
      int32_t offset = val1 * sizeof(SgObject);
      arm64_ldr_r64_mem(a, JIT_REG_TEMP1, JIT_REG_SCHFP, offset);
      arm64_str_r64_mem_post(a, JIT_REG_TEMP1, JIT_REG_SCHSP, sizeof(SgObject));
    }
    return pc + 1;

  /*
   * CONST_PUSH - Load constant and push
   */
  case CONST_PUSH:
    if (pc + 1 >= gen->codeSize) return -1;
    {
      SgObject constVal = SG_OBJ(gen->code[pc + 1]);
      arm64_mov_r64_ptr(a, JIT_REG_TEMP1, constVal);
      arm64_str_r64_mem_post(a, JIT_REG_TEMP1, JIT_REG_SCHSP, sizeof(SgObject));
    }
    return pc + 2;

  /*
   * CONSTI_PUSH - Load small integer and push
   */
  case CONSTI_PUSH:
    {
      intptr_t intVal = val1;
      intptr_t tagged = (intVal << FIXNUM_SHIFT) | FIXNUM_TAG;
      arm64_mov_r64_imm(a, JIT_REG_TEMP1, tagged);
      arm64_str_r64_mem_post(a, JIT_REG_TEMP1, JIT_REG_SCHSP, sizeof(SgObject));
    }
    return pc + 1;

  /*
   * ADD - Add two values
   * AC = pop() + AC
   * Fast path for fixnums, slow path calls Sg_Add.
   */
  case ADD:
    {
      int slowPath = arm64_new_label(a);
      int done = arm64_new_label(a);

      /* Pop operand: X1 = *--SP */
      arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP, -sizeof(SgObject));

      /* Check if both are fixnums: (X0 | X1) & 3 == 1 */
      /* If either has tag != 1, go to slow path */
      arm64_orr_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
      arm64_tst_r64_imm(a, JIT_REG_TEMP3, FIXNUM_MASK);
      /* If tag bits are not exactly 1, we need slow path */
      /* But TST with immediate 3 and check == 1 is complex */
      /* Simpler: check low bit of both separately */

      /* Actually, for fixnum fast path:
       * Both must have low 2 bits == 01
       * So (x & 3) == 1 AND (y & 3) == 1
       * Trick: (x & y) must have bit 0 set, and (x | y) must not have bit 1 set
       * Or just: if ((x ^ 1) & 3) || ((y ^ 1) & 3) then slow */

      /* Simpler approach: check tag of each */
      arm64_and_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
      /* Both must have bit 0 set */
      arm64_tst_r64_imm(a, JIT_REG_TEMP3, 1);
      arm64_b_cond(a, ARM64_EQ, slowPath);

      /* Neither should have bit 1 set (that would mean different tag) */
      arm64_orr_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
      arm64_tst_r64_imm(a, JIT_REG_TEMP3, 2);
      arm64_b_cond(a, ARM64_NE, slowPath);

      /* Fast path: add fixnums */
      /* Since both have tag 1, we can add directly and result has tag 2
       * We need to subtract 1 to get correct tag */
      arm64_add_r64_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP1, JIT_REG_TEMP2);
      arm64_sub_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 1);
      /* TODO: Check for overflow and call Sg_Add if so */
      arm64_b(a, done);

      /* Slow path: call Sg_Add(X1, X0) */
      arm64_bind_label(a, slowPath);
      /* Arguments: X0 = first arg, X1 = second arg */
      arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);  /* first operand */
      arm64_mov_r64_r64(a, ARM64_X1, JIT_REG_TEMP1);  /* second operand */
      arm64_bl(a, Sg_Add);
      /* Result in X0 */

      arm64_bind_label(a, done);
    }
    return pc + 1;

  /*
   * SUB - Subtract two values
   * AC = pop() - AC
   */
  case SUB:
    {
      int slowPath = arm64_new_label(a);
      int done = arm64_new_label(a);

      /* Pop operand */
      arm64_ldr_r64_mem_pre(a, JIT_REG_TEMP2, JIT_REG_SCHSP, -sizeof(SgObject));

      /* Check fixnum tags */
      arm64_and_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
      arm64_tst_r64_imm(a, JIT_REG_TEMP3, 1);
      arm64_b_cond(a, ARM64_EQ, slowPath);
      arm64_orr_r64_r64_r64(a, JIT_REG_TEMP3, JIT_REG_TEMP1, JIT_REG_TEMP2);
      arm64_tst_r64_imm(a, JIT_REG_TEMP3, 2);
      arm64_b_cond(a, ARM64_NE, slowPath);

      /* Fast path: subtract fixnums */
      /* x - y with tag 1: (x - y) + 1 to restore tag */
      arm64_sub_r64_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP2, JIT_REG_TEMP1);
      arm64_add_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, 1);
      arm64_b(a, done);

      /* Slow path */
      arm64_bind_label(a, slowPath);
      arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);
      arm64_mov_r64_r64(a, ARM64_X1, JIT_REG_TEMP1);
      arm64_bl(a, Sg_Sub);

      arm64_bind_label(a, done);
    }
    return pc + 1;

  /*
   * ADDI - Add immediate
   * AC = AC + val1
   */
  case ADDI:
    {
      int slowPath = arm64_new_label(a);
      int done = arm64_new_label(a);

      /* Check if AC is fixnum */
      arm64_tst_r64_imm(a, JIT_REG_TEMP1, 1);
      arm64_b_cond(a, ARM64_EQ, slowPath);
      arm64_tst_r64_imm(a, JIT_REG_TEMP1, 2);
      arm64_b_cond(a, ARM64_NE, slowPath);

      /* Fast path: add immediate */
      /* val1 as fixnum = (val1 << 2) | 1 */
      /* But we can just add (val1 << 2) since tag is preserved */
      int32_t addVal = val1 << FIXNUM_SHIFT;
      if (addVal >= 0 && addVal <= 4095) {
	arm64_add_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, addVal);
      } else if (addVal >= -4095 && addVal < 0) {
	arm64_sub_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, -addVal);
      } else {
	/* Load immediate into register */
	arm64_mov_r64_imm(a, JIT_REG_TEMP2, addVal);
	arm64_add_r64_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP1, JIT_REG_TEMP2);
      }
      arm64_b(a, done);

      /* Slow path */
      arm64_bind_label(a, slowPath);
      {
	intptr_t tagged = (val1 << FIXNUM_SHIFT) | FIXNUM_TAG;
	arm64_mov_r64_imm(a, JIT_REG_TEMP2, tagged);
      }
      arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP2);
      arm64_mov_r64_r64(a, ARM64_X1, JIT_REG_TEMP1);
      arm64_bl(a, Sg_Add);

      arm64_bind_label(a, done);
    }
    return pc + 1;

  /*
   * SUBI - Subtract immediate
   */
  case SUBI:
    {
      int slowPath = arm64_new_label(a);
      int done = arm64_new_label(a);

      arm64_tst_r64_imm(a, JIT_REG_TEMP1, 1);
      arm64_b_cond(a, ARM64_EQ, slowPath);
      arm64_tst_r64_imm(a, JIT_REG_TEMP1, 2);
      arm64_b_cond(a, ARM64_NE, slowPath);

      int32_t subVal = val1 << FIXNUM_SHIFT;
      if (subVal >= 0 && subVal <= 4095) {
	arm64_sub_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, subVal);
      } else if (subVal >= -4095 && subVal < 0) {
	arm64_add_r64_r64_imm(a, JIT_REG_TEMP1, JIT_REG_TEMP1, -subVal);
      } else {
	arm64_mov_r64_imm(a, JIT_REG_TEMP2, subVal);
	arm64_sub_r64_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP1, JIT_REG_TEMP2);
      }
      arm64_b(a, done);

      arm64_bind_label(a, slowPath);
      {
	intptr_t tagged = (val1 << FIXNUM_SHIFT) | FIXNUM_TAG;
	arm64_mov_r64_imm(a, JIT_REG_TEMP2, tagged);
      }
      arm64_mov_r64_r64(a, ARM64_X0, JIT_REG_TEMP1);
      arm64_mov_r64_r64(a, ARM64_X1, JIT_REG_TEMP2);
      arm64_bl(a, Sg_Sub);

      arm64_bind_label(a, done);
    }
    return pc + 1;

  /*
   * TEST - Conditional branch if AC is false
   * if AC == #f then PC += offset
   */
  case TEST:
    if (pc + 1 >= gen->codeSize) return -1;
    {
      /* Get jump offset from operand */
      intptr_t offset = (intptr_t)gen->code[pc + 1];
      int targetPc = pc + 1 + offset;

      if (targetPc < 0 || targetPc >= gen->codeSize) {
	return -1;  /* Invalid jump target */
      }

      /* Compare AC with #f */
      arm64_mov_r64_ptr(a, JIT_REG_TEMP2, SG_FALSE);
      arm64_cmp_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP2);
      arm64_b_cond(a, ARM64_EQ, gen->bcLabels[targetPc]);
    }
    return pc + 2;

  /*
   * JUMP - Unconditional branch
   */
  case JUMP:
    if (pc + 1 >= gen->codeSize) return -1;
    {
      intptr_t offset = (intptr_t)gen->code[pc + 1];
      int targetPc = pc + 1 + offset;

      if (targetPc < 0 || targetPc >= gen->codeSize) {
	return -1;
      }

      arm64_b(a, gen->bcLabels[targetPc]);
    }
    return pc + 2;

  /*
   * NOT - Boolean negation
   * AC = (AC == #f) ? #t : #f
   */
  case NOT:
    {
      int isFalse = arm64_new_label(a);
      int done = arm64_new_label(a);

      arm64_mov_r64_ptr(a, JIT_REG_TEMP2, SG_FALSE);
      arm64_cmp_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP2);
      arm64_b_cond(a, ARM64_EQ, isFalse);

      /* AC was not #f, result is #f */
      arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_FALSE);
      arm64_b(a, done);

      /* AC was #f, result is #t */
      arm64_bind_label(a, isFalse);
      arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_TRUE);

      arm64_bind_label(a, done);
    }
    return pc + 1;

  /*
   * RET - Return from procedure
   * For JIT, we jump to epilogue which stores state and returns.
   */
  case RET:
    arm64_b(a, gen->epilogueLabel);
    return pc + 1;

  /*
   * CONST_RET - Load constant and return
   */
  case CONST_RET:
    if (pc + 1 >= gen->codeSize) return -1;
    {
      SgObject constVal = SG_OBJ(gen->code[pc + 1]);
      arm64_mov_r64_ptr(a, JIT_REG_TEMP1, constVal);
      arm64_b(a, gen->epilogueLabel);
    }
    return pc + 2;

  /*
   * NULLP - Check if AC is null
   */
  case NULLP:
    {
      int isNull = arm64_new_label(a);
      int done = arm64_new_label(a);

      arm64_mov_r64_ptr(a, JIT_REG_TEMP2, SG_NIL);
      arm64_cmp_r64_r64(a, JIT_REG_TEMP1, JIT_REG_TEMP2);
      arm64_b_cond(a, ARM64_EQ, isNull);

      arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_FALSE);
      arm64_b(a, done);

      arm64_bind_label(a, isNull);
      arm64_mov_r64_ptr(a, JIT_REG_TEMP1, SG_TRUE);

      arm64_bind_label(a, done);
    }
    return pc + 1;

  /*
   * Unsupported instruction - abort JIT compilation
   */
  default:
    return -1;
  }

  return -1;  /* Should not reach here */
}


/*
 * Fallback: When we encounter an unsupported instruction,
 * we abort JIT compilation and return NULL.
 * The caller will fall back to the interpreter.
 */
static void emit_fallback_to_interpreter(Arm64CodeGen *gen)
{
  /* Not used currently - we abort compilation on unsupported instructions */
  (void)gen;
}

#endif /* JIT_ARCH_arm64 */
