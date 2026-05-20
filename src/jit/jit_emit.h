/* jit_emit.h                                      -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_JIT_EMIT_H_
#define SAGITTARIUS_JIT_EMIT_H_

#include "jit_internal.h"

#ifdef HAVE_JIT

/*
 * JIT Emit Functions
 *
 * Each platform implements these functions to emit native code for
 * VM bytecode instructions. The instruction dispatch loop in jit_compile.c
 * calls these functions.
 *
 * Return value:
 *   1 = success
 *   0 = failure (unsupported instruction or error)
 *
 * All functions receive SgJitContext* which contains platform-specific
 * context in ctx->platform.
 */

/*
 * Prologue/Epilogue
 */

/* Emit function prologue (save registers, load VM state) */
SG_EXTERN int Sg__JitEmit_Prologue(SgJitContext *ctx);

/* Emit function epilogue (store VM state, restore registers, return) */
SG_EXTERN int Sg__JitEmit_Epilogue(SgJitContext *ctx);


/*
 * Basic Instructions
 */

/* NOP - No operation */
SG_EXTERN int Sg__JitEmit_NOP(SgJitContext *ctx);

/* UNDEF - Load undefined value into AC */
SG_EXTERN int Sg__JitEmit_UNDEF(SgJitContext *ctx);

/* CONST - Load constant from operand into AC */
SG_EXTERN int Sg__JitEmit_CONST(SgJitContext *ctx, SgObject val);

/* CONSTI - Load small integer immediate into AC */
SG_EXTERN int Sg__JitEmit_CONSTI(SgJitContext *ctx, long val);


/*
 * Local Variable Access
 */

/* LREF - Load local variable: AC = FP[index] */
SG_EXTERN int Sg__JitEmit_LREF(SgJitContext *ctx, int index);

/* LSET - Store to local variable (through box): FP[index]->value = AC */
SG_EXTERN int Sg__JitEmit_LSET(SgJitContext *ctx, int index);

/* FREF - Load free variable: AC = CL->frees[index] */
SG_EXTERN int Sg__JitEmit_FREF(SgJitContext *ctx, int index);


/*
 * Stack Operations
 */

/* PUSH - Push AC onto stack: *SP++ = AC */
SG_EXTERN int Sg__JitEmit_PUSH(SgJitContext *ctx);


/*
 * Arithmetic Operations
 */

/* ADD - Add: AC = pop() + AC */
SG_EXTERN int Sg__JitEmit_ADD(SgJitContext *ctx);

/* ADDI - Add immediate: AC = AC + val */
SG_EXTERN int Sg__JitEmit_ADDI(SgJitContext *ctx, long val);

/* SUB - Subtract: AC = pop() - AC */
SG_EXTERN int Sg__JitEmit_SUB(SgJitContext *ctx);

/* SUBI - Subtract immediate: AC = AC - val */
SG_EXTERN int Sg__JitEmit_SUBI(SgJitContext *ctx, long val);


/*
 * Comparison Operations
 */

/* NUM_EQ - Numeric equal: AC = (pop() == AC) */
SG_EXTERN int Sg__JitEmit_NUM_EQ(SgJitContext *ctx);

/* NUM_LT - Numeric less than: AC = (pop() < AC) */
SG_EXTERN int Sg__JitEmit_NUM_LT(SgJitContext *ctx);

/* NUM_LE - Numeric less or equal: AC = (pop() <= AC) */
SG_EXTERN int Sg__JitEmit_NUM_LE(SgJitContext *ctx);

/* NUM_GT - Numeric greater than: AC = (pop() > AC) */
SG_EXTERN int Sg__JitEmit_NUM_GT(SgJitContext *ctx);

/* NUM_GE - Numeric greater or equal: AC = (pop() >= AC) */
SG_EXTERN int Sg__JitEmit_NUM_GE(SgJitContext *ctx);


/*
 * Control Flow
 */

/* TEST - Conditional jump: if AC is false, jump to targetPc */
SG_EXTERN int Sg__JitEmit_TEST(SgJitContext *ctx, int targetPc);

/* JUMP - Unconditional jump to targetPc */
SG_EXTERN int Sg__JitEmit_JUMP(SgJitContext *ctx, int targetPc);

/* RET - Return from function (jump to epilogue) */
SG_EXTERN int Sg__JitEmit_RET(SgJitContext *ctx);


/*
 * Combined Instructions
 *
 * Platforms can implement optimized versions of these, or use
 * default implementations that call primitive emit functions.
 */

/* LREF_PUSH - Load local and push: *SP++ = FP[index] */
SG_EXTERN int Sg__JitEmit_LREF_PUSH(SgJitContext *ctx, int index);

/* CONST_PUSH - Load constant and push */
SG_EXTERN int Sg__JitEmit_CONST_PUSH(SgJitContext *ctx, SgObject val);

/* CONSTI_PUSH - Load small integer and push */
SG_EXTERN int Sg__JitEmit_CONSTI_PUSH(SgJitContext *ctx, long val);


/*
 * Branch Instructions (conditional jump if NOT condition)
 */

/* BNNUME - Branch if not numeric equal */
SG_EXTERN int Sg__JitEmit_BNNUME(SgJitContext *ctx, int targetPc);

/* BNLT - Branch if not less than */
SG_EXTERN int Sg__JitEmit_BNLT(SgJitContext *ctx, int targetPc);

/* BNLE - Branch if not less or equal */
SG_EXTERN int Sg__JitEmit_BNLE(SgJitContext *ctx, int targetPc);

/* BNGT - Branch if not greater than */
SG_EXTERN int Sg__JitEmit_BNGT(SgJitContext *ctx, int targetPc);

/* BNGE - Branch if not greater or equal */
SG_EXTERN int Sg__JitEmit_BNGE(SgJitContext *ctx, int targetPc);

/* Disassembly interface - platform-specific disassembler */
SG_EXTERN void Sg__JitDisasmBuffer(uint8_t *code, size_t size, SgPort *port);

#endif /* HAVE_JIT */
#endif /* SAGITTARIUS_JIT_EMIT_H_ */
