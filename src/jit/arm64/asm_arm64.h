/* asm_arm64.h                                     -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_JIT_ASM_ARM64_H_
#define SAGITTARIUS_JIT_ASM_ARM64_H_

#include "../jit.h"

#ifdef JIT_ARCH_arm64

#include <stdint.h>
#include <stddef.h>

/*
 * ARM64 General Purpose Registers
 *
 * X0-X7:   Arguments / Return value / Scratch
 * X8:      Indirect result location (struct returns)
 * X9-X15:  Scratch registers (caller-saved)
 * X16-X17: Intra-procedure-call scratch (IP0, IP1)
 * X18:     Platform register (reserved on some platforms)
 * X19-X28: Callee-saved registers
 * X29:     Frame pointer (FP)
 * X30:     Link register (LR)
 * SP:      Stack pointer (XZR when used as register)
 * XZR:     Zero register
 */
typedef enum {
  ARM64_X0 = 0,  ARM64_X1,  ARM64_X2,  ARM64_X3,
  ARM64_X4,      ARM64_X5,  ARM64_X6,  ARM64_X7,
  ARM64_X8,      ARM64_X9,  ARM64_X10, ARM64_X11,
  ARM64_X12,     ARM64_X13, ARM64_X14, ARM64_X15,
  ARM64_X16,     ARM64_X17, ARM64_X18, ARM64_X19,
  ARM64_X20,     ARM64_X21, ARM64_X22, ARM64_X23,
  ARM64_X24,     ARM64_X25, ARM64_X26, ARM64_X27,
  ARM64_X28,     ARM64_X29, ARM64_X30, ARM64_XZR = 31,
  ARM64_SP = 31  /* Context-dependent: SP or XZR */
} Arm64Reg;

/* Aliases for clarity */
#define ARM64_FP ARM64_X29
#define ARM64_LR ARM64_X30

/*
 * VM Register Mapping
 *
 * We use callee-saved registers to hold VM state across C calls.
 * The Scheme stack is in heap memory; these registers hold pointers to it.
 */
#define JIT_REG_VM     ARM64_X19  /* SgVM* pointer */
#define JIT_REG_SCHSP  ARM64_X20  /* Scheme stack pointer (vm->sp) */
#define JIT_REG_SCHFP  ARM64_X21  /* Scheme frame pointer (vm->fp) */
#define JIT_REG_CL     ARM64_X22  /* Current closure */
#define JIT_REG_TEMP1  ARM64_X0   /* Temp / Return value / AC */
#define JIT_REG_TEMP2  ARM64_X1   /* Temp / Arg2 */
#define JIT_REG_TEMP3  ARM64_X2   /* Temp / Arg3 */
#define JIT_REG_TEMP4  ARM64_X3   /* Temp / Arg4 */

/*
 * Condition Codes
 */
typedef enum {
  ARM64_EQ = 0x0,   /* Equal (Z=1) */
  ARM64_NE = 0x1,   /* Not equal (Z=0) */
  ARM64_CS = 0x2,   /* Carry set / unsigned >= (C=1) */
  ARM64_HS = 0x2,   /* Unsigned >= (alias for CS) */
  ARM64_CC = 0x3,   /* Carry clear / unsigned < (C=0) */
  ARM64_LO = 0x3,   /* Unsigned < (alias for CC) */
  ARM64_MI = 0x4,   /* Minus / negative (N=1) */
  ARM64_PL = 0x5,   /* Plus / positive or zero (N=0) */
  ARM64_VS = 0x6,   /* Overflow (V=1) */
  ARM64_VC = 0x7,   /* No overflow (V=0) */
  ARM64_HI = 0x8,   /* Unsigned > (C=1 && Z=0) */
  ARM64_LS = 0x9,   /* Unsigned <= (C=0 || Z=1) */
  ARM64_GE = 0xA,   /* Signed >= (N==V) */
  ARM64_LT = 0xB,   /* Signed < (N!=V) */
  ARM64_GT = 0xC,   /* Signed > (Z=0 && N==V) */
  ARM64_LE = 0xD,   /* Signed <= (Z=1 || N!=V) */
  ARM64_AL = 0xE,   /* Always */
  ARM64_NV = 0xF    /* Never (reserved) */
} Arm64Cond;

/*
 * Forward reference patch entry
 */
typedef struct Arm64PatchRec {
  size_t offset;              /* Offset in code buffer */
  int    label;               /* Label number */
  int    type;                /* Patch type (branch, etc.) */
  struct Arm64PatchRec *next;
} Arm64Patch;

/* Patch types */
#define ARM64_PATCH_B      0  /* Unconditional branch */
#define ARM64_PATCH_BCOND  1  /* Conditional branch */
#define ARM64_PATCH_ADR    2  /* PC-relative address */

/*
 * Assembler Context
 */
typedef struct Arm64AsmRec {
  uint8_t *buf;         /* Code buffer */
  size_t   pos;         /* Current write position */
  size_t   size;        /* Buffer size */

  /* Label management */
  int      labelCount;       /* Number of labels allocated */
  int      labelCapacity;    /* Capacity of labels array */
  int     *labelOffsets;     /* Label offset in code (-1 if unbound) */

  /* Forward reference patches */
  Arm64Patch *patches;       /* Linked list of patches */

  /* Error flag */
  int      error;
} Arm64Asm;

/*
 * Assembler Lifecycle
 */

/* Create assembler context */
Arm64Asm* arm64_asm_new(uint8_t *buf, size_t size);

/* Free assembler context */
void arm64_asm_free(Arm64Asm *a);

/* Get current code size */
size_t arm64_asm_size(Arm64Asm *a);

/* Check for errors */
int arm64_asm_error(Arm64Asm *a);

/* Resolve all forward references. Returns 0 on success, -1 on error. */
int arm64_asm_finalize(Arm64Asm *a);


/*
 * Label Management
 */

/* Allocate a new label. Returns label number. */
int arm64_new_label(Arm64Asm *a);

/* Bind label to current position */
void arm64_bind_label(Arm64Asm *a, int label);


/*
 * Data Movement Instructions
 */

/* MOV Xd, Xm (register to register) */
void arm64_mov_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg src);

/* MOV Xd, #imm16 (move immediate, shifted) */
void arm64_movz(Arm64Asm *a, Arm64Reg dst, uint16_t imm, int shift);
void arm64_movk(Arm64Asm *a, Arm64Reg dst, uint16_t imm, int shift);
void arm64_movn(Arm64Asm *a, Arm64Reg dst, uint16_t imm, int shift);

/* Load 64-bit immediate (uses multiple instructions) */
void arm64_mov_r64_imm(Arm64Asm *a, Arm64Reg dst, int64_t imm);

/* Load address of object (for embedding object pointers) */
void arm64_mov_r64_ptr(Arm64Asm *a, Arm64Reg dst, void *ptr);


/*
 * Load/Store Instructions
 */

/* LDR Xt, [Xn, #offset] */
void arm64_ldr_r64_mem(Arm64Asm *a, Arm64Reg dst, Arm64Reg base, int32_t offset);

/* STR Xt, [Xn, #offset] */
void arm64_str_r64_mem(Arm64Asm *a, Arm64Reg src, Arm64Reg base, int32_t offset);

/* LDR Xt, [Xn, #offset]! (pre-indexed) */
void arm64_ldr_r64_mem_pre(Arm64Asm *a, Arm64Reg dst, Arm64Reg base, int32_t offset);

/* STR Xt, [Xn, #offset]! (pre-indexed) */
void arm64_str_r64_mem_pre(Arm64Asm *a, Arm64Reg src, Arm64Reg base, int32_t offset);

/* LDR Xt, [Xn], #offset (post-indexed) */
void arm64_ldr_r64_mem_post(Arm64Asm *a, Arm64Reg dst, Arm64Reg base, int32_t offset);

/* STR Xt, [Xn], #offset (post-indexed) */
void arm64_str_r64_mem_post(Arm64Asm *a, Arm64Reg src, Arm64Reg base, int32_t offset);

/* LDP Xt1, Xt2, [Xn, #offset] (load pair) */
void arm64_ldp(Arm64Asm *a, Arm64Reg r1, Arm64Reg r2, Arm64Reg base, int32_t offset);

/* STP Xt1, Xt2, [Xn, #offset] (store pair) */
void arm64_stp(Arm64Asm *a, Arm64Reg r1, Arm64Reg r2, Arm64Reg base, int32_t offset);

/* LDP with pre-index [Xn, #offset]! */
void arm64_ldp_pre(Arm64Asm *a, Arm64Reg r1, Arm64Reg r2, Arm64Reg base, int32_t offset);

/* STP with pre-index [Xn, #offset]! */
void arm64_stp_pre(Arm64Asm *a, Arm64Reg r1, Arm64Reg r2, Arm64Reg base, int32_t offset);


/*
 * Arithmetic Instructions
 */

/* ADD Xd, Xn, Xm */
void arm64_add_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m);

/* ADD Xd, Xn, #imm12 */
void arm64_add_r64_r64_imm(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, int32_t imm);

/* ADDS Xd, Xn, Xm (set flags) */
void arm64_adds_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m);

/* SUB Xd, Xn, Xm */
void arm64_sub_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m);

/* SUB Xd, Xn, #imm12 */
void arm64_sub_r64_r64_imm(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, int32_t imm);

/* SUBS Xd, Xn, Xm (set flags) */
void arm64_subs_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m);

/* NEG Xd, Xm (negate) */
void arm64_neg_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg src);

/* MUL Xd, Xn, Xm */
void arm64_mul_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m);


/*
 * Logical Instructions
 */

/* AND Xd, Xn, Xm */
void arm64_and_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m);

/* ORR Xd, Xn, Xm */
void arm64_orr_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m);

/* EOR Xd, Xn, Xm (XOR) */
void arm64_eor_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m);

/* TST Xn, #imm (AND with immediate, set flags, discard result) */
void arm64_tst_r64_imm(Arm64Asm *a, Arm64Reg n, uint64_t imm);

/* TST Xn, Xm (AND with register, set flags, discard result) */
void arm64_tst_r64_r64(Arm64Asm *a, Arm64Reg n, Arm64Reg m);


/*
 * Comparison Instructions
 */

/* CMP Xn, Xm (SUBS Xzr, Xn, Xm) */
void arm64_cmp_r64_r64(Arm64Asm *a, Arm64Reg n, Arm64Reg m);

/* CMP Xn, #imm12 */
void arm64_cmp_r64_imm(Arm64Asm *a, Arm64Reg n, int32_t imm);


/*
 * Branch Instructions
 */

/* B label (unconditional branch) */
void arm64_b(Arm64Asm *a, int label);

/* B.cond label (conditional branch) */
void arm64_b_cond(Arm64Asm *a, Arm64Cond cond, int label);

/* BL addr (branch with link - call) */
void arm64_bl(Arm64Asm *a, void *target);

/* BLR Xn (branch to register with link) */
void arm64_blr(Arm64Asm *a, Arm64Reg reg);

/* BR Xn (branch to register) */
void arm64_br(Arm64Asm *a, Arm64Reg reg);

/* RET (return, branch to LR) */
void arm64_ret(Arm64Asm *a);


/*
 * Miscellaneous
 */

/* NOP */
void arm64_nop(Arm64Asm *a);


#endif /* JIT_ARCH_arm64 */
#endif /* SAGITTARIUS_JIT_ASM_ARM64_H_ */
