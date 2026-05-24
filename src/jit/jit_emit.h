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

/* Emit yield epilogue (like epilogue but does NOT store vm->cl)
 * Used when yielding to interpreter for non-JIT closures - the helper
 * already set vm->cl to the callee's closure */
SG_EXTERN int Sg__JitEmit_YieldEpilogue(SgJitContext *ctx);


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

/* BNNULL - Branch if not null */
SG_EXTERN int Sg__JitEmit_BNNULL(SgJitContext *ctx, int targetPc);

/* BNEQ - Branch if not eq */
SG_EXTERN int Sg__JitEmit_BNEQ(SgJitContext *ctx, int targetPc);


/*
 * List Operations
 */

/* CAR - Get car of pair: AC = car(AC) */
SG_EXTERN int Sg__JitEmit_CAR(SgJitContext *ctx);

/* CDR - Get cdr of pair: AC = cdr(AC) */
SG_EXTERN int Sg__JitEmit_CDR(SgJitContext *ctx);

/* CONS - Create pair: AC = cons(pop(), AC) */
SG_EXTERN int Sg__JitEmit_CONS(SgJitContext *ctx);


/*
 * Predicates
 */

/* NULLP - Null check: AC = (AC == '()) */
SG_EXTERN int Sg__JitEmit_NULLP(SgJitContext *ctx);

/* PAIRP - Pair check: AC = pair?(AC) */
SG_EXTERN int Sg__JitEmit_PAIRP(SgJitContext *ctx);

/* NOT - Boolean negation: AC = not(AC) */
SG_EXTERN int Sg__JitEmit_NOT(SgJitContext *ctx);

/* EQ - Pointer equality: AC = eq?(pop(), AC) */
SG_EXTERN int Sg__JitEmit_EQ(SgJitContext *ctx);


/*
 * More Arithmetic
 */

/* MUL - Multiply: AC = pop() * AC */
SG_EXTERN int Sg__JitEmit_MUL(SgJitContext *ctx);

/* MULI - Multiply by immediate: AC = AC * val */
SG_EXTERN int Sg__JitEmit_MULI(SgJitContext *ctx, long val);

/* DIV - Divide: AC = pop() / AC */
SG_EXTERN int Sg__JitEmit_DIV(SgJitContext *ctx);

/* DIVI - Divide by immediate: AC = AC / val */
SG_EXTERN int Sg__JitEmit_DIVI(SgJitContext *ctx, long val);

/* NEG - Negate: AC = -AC */
SG_EXTERN int Sg__JitEmit_NEG(SgJitContext *ctx);

/* EQV - EQV? equality: AC = eqv?(pop(), AC) */
SG_EXTERN int Sg__JitEmit_EQV(SgJitContext *ctx);

/* SYMBOLP - Symbol check: AC = symbol?(AC) */
SG_EXTERN int Sg__JitEmit_SYMBOLP(SgJitContext *ctx);

/* GREF - Load global variable: AC = lookup(id) */
SG_EXTERN int Sg__JitEmit_GREF(SgJitContext *ctx, SgObject id);

/* GREF_PUSH - Load global and push: *SP++ = lookup(id) */
SG_EXTERN int Sg__JitEmit_GREF_PUSH(SgJitContext *ctx, SgObject id);

/* FREF_PUSH - Load free variable and push: *SP++ = CL->frees[index] */
SG_EXTERN int Sg__JitEmit_FREF_PUSH(SgJitContext *ctx, int index);

/* LIST - Create list from n items on stack */
SG_EXTERN int Sg__JitEmit_LIST(SgJitContext *ctx, int n);

/* CAAR - AC = car(car(AC)) */
SG_EXTERN int Sg__JitEmit_CAAR(SgJitContext *ctx);

/* CADR - AC = car(cdr(AC)) */
SG_EXTERN int Sg__JitEmit_CADR(SgJitContext *ctx);

/* CDAR - AC = cdr(car(AC)) */
SG_EXTERN int Sg__JitEmit_CDAR(SgJitContext *ctx);

/* CDDR - AC = cdr(cdr(AC)) */
SG_EXTERN int Sg__JitEmit_CDDR(SgJitContext *ctx);

/* BNEQV - Branch if not eqv */
SG_EXTERN int Sg__JitEmit_BNEQV(SgJitContext *ctx, int targetPc);

/*
 * Combined Instructions
 */
SG_EXTERN int Sg__JitEmit_CAR_PUSH(SgJitContext *ctx);
SG_EXTERN int Sg__JitEmit_CDR_PUSH(SgJitContext *ctx);
SG_EXTERN int Sg__JitEmit_CONS_PUSH(SgJitContext *ctx);
SG_EXTERN int Sg__JitEmit_LREF_CAR(SgJitContext *ctx, int index);
SG_EXTERN int Sg__JitEmit_LREF_CDR(SgJitContext *ctx, int index);
SG_EXTERN int Sg__JitEmit_FREF_CAR(SgJitContext *ctx, int index);
SG_EXTERN int Sg__JitEmit_FREF_CDR(SgJitContext *ctx, int index);
SG_EXTERN int Sg__JitEmit_GREF_CAR(SgJitContext *ctx, SgObject id);
SG_EXTERN int Sg__JitEmit_GREF_CDR(SgJitContext *ctx, SgObject id);
SG_EXTERN int Sg__JitEmit_LREF_CAR_PUSH(SgJitContext *ctx, int index);
SG_EXTERN int Sg__JitEmit_LREF_CDR_PUSH(SgJitContext *ctx, int index);
SG_EXTERN int Sg__JitEmit_FREF_CAR_PUSH(SgJitContext *ctx, int index);
SG_EXTERN int Sg__JitEmit_FREF_CDR_PUSH(SgJitContext *ctx, int index);
SG_EXTERN int Sg__JitEmit_GREF_CAR_PUSH(SgJitContext *ctx, SgObject id);
SG_EXTERN int Sg__JitEmit_GREF_CDR_PUSH(SgJitContext *ctx, SgObject id);
SG_EXTERN int Sg__JitEmit_CONST_RET(SgJitContext *ctx, SgObject val);

/*
 * Mutation Operations
 */
SG_EXTERN int Sg__JitEmit_SET_CAR(SgJitContext *ctx);
SG_EXTERN int Sg__JitEmit_SET_CDR(SgJitContext *ctx);
SG_EXTERN int Sg__JitEmit_BOX(SgJitContext *ctx, int index);
SG_EXTERN int Sg__JitEmit_UNBOX(SgJitContext *ctx);
SG_EXTERN int Sg__JitEmit_FSET(SgJitContext *ctx, int index);

/*
 * Stack Management
 */
SG_EXTERN int Sg__JitEmit_LEAVE(SgJitContext *ctx, int n);
SG_EXTERN int Sg__JitEmit_INST_STACK(SgJitContext *ctx, int index);
SG_EXTERN int Sg__JitEmit_RESV_STACK(SgJitContext *ctx, int n);

/*
 * Vector Operations
 */
SG_EXTERN int Sg__JitEmit_VECTORP(SgJitContext *ctx);
SG_EXTERN int Sg__JitEmit_VEC_LEN(SgJitContext *ctx);
SG_EXTERN int Sg__JitEmit_VEC_REF(SgJitContext *ctx);
SG_EXTERN int Sg__JitEmit_VEC_SET(SgJitContext *ctx);
SG_EXTERN int Sg__JitEmit_VECTOR(SgJitContext *ctx, int size);


/*
 * Call Instructions
 */

/* FRAME - Push a continuation frame for non-tail call */
SG_EXTERN int Sg__JitEmit_FRAME(SgJitContext *ctx, int returnPc);

/* GREF_CALL - Call a global procedure */
SG_EXTERN int Sg__JitEmit_GREF_CALL(SgJitContext *ctx, int argc, SgObject id);

/* GREF_TAIL_CALL - Tail-call a global procedure */
SG_EXTERN int Sg__JitEmit_GREF_TAIL_CALL(SgJitContext *ctx, int argc, SgObject id);

/* CALL - Call a procedure in AC */
SG_EXTERN int Sg__JitEmit_CALL(SgJitContext *ctx, int argc);

/* TAIL_CALL - Tail-call a procedure in AC */
SG_EXTERN int Sg__JitEmit_TAIL_CALL(SgJitContext *ctx, int argc);

/* LOCAL_CALL - Call a local closure */
SG_EXTERN int Sg__JitEmit_LOCAL_CALL(SgJitContext *ctx, int argc);

/* LOCAL_TAIL_CALL - Tail-call a local closure */
SG_EXTERN int Sg__JitEmit_LOCAL_TAIL_CALL(SgJitContext *ctx, int argc);

/* CLOSURE - Create a closure from code builder */
SG_EXTERN int Sg__JitEmit_CLOSURE(SgJitContext *ctx, int selfPos, SgObject cb, int freec);

/* SELF_CALL - Optimized self-recursive call (direct branch) */
SG_EXTERN int Sg__JitEmit_SELF_CALL(SgJitContext *ctx, int argc);

/* SELF_TAIL_CALL - Optimized self-recursive tail call (direct branch) */
SG_EXTERN int Sg__JitEmit_SELF_TAIL_CALL(SgJitContext *ctx, int argc, SgObject id);

/* APPLY - Apply a procedure to a list of arguments */
SG_EXTERN int Sg__JitEmit_APPLY(SgJitContext *ctx, int nargc, int isTail);

/* VALUES - Return multiple values */
SG_EXTERN int Sg__JitEmit_VALUES(SgJitContext *ctx, int nvalues);

/* RECEIVE - Receive multiple values */
SG_EXTERN int Sg__JitEmit_RECEIVE(SgJitContext *ctx, int reqCount, int optCount);


/* Disassembly interface - platform-specific disassembler */
SG_EXTERN void Sg__JitDisasmBuffer(uint8_t *code, size_t size, SgPort *port);

#endif /* HAVE_JIT */
#endif /* SAGITTARIUS_JIT_EMIT_H_ */
