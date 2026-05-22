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
#include <time.h>

/* Global JIT configuration */
static int jit_enabled = 1;
static int jit_threshold = SG_JIT_DEFAULT_THRESHOLD;
static int jit_verbose = 0;  /* Disabled by default */

/* Profiling counters */
static int jit_profile_enabled = 0;
static uint64_t jit_call_count = 0;
static uint64_t jit_tail_call_count = 0;
static uint64_t jit_resolve_time_ns = 0;
static uint64_t jit_frame_setup_time_ns = 0;
static uint64_t jit_call_time_ns = 0;
static uint64_t jit_frame_restore_time_ns = 0;

static inline uint64_t get_nanos(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

void Sg_JitProfileReset(void)
{
  jit_call_count = 0;
  jit_tail_call_count = 0;
  jit_resolve_time_ns = 0;
  jit_frame_setup_time_ns = 0;
  jit_call_time_ns = 0;
  jit_frame_restore_time_ns = 0;
}

void Sg_JitProfileEnable(int enable)
{
  jit_profile_enabled = enable;
}

void Sg_JitProfilePrint(SgPort *port)
{
  Sg_Printf(port, UC("JIT Profile:\n"));
  Sg_Printf(port, UC("  GREF_CALL count:     %ld\n"), jit_call_count);
  Sg_Printf(port, UC("  GREF_TAIL_CALL count:%ld\n"), jit_tail_call_count);
  Sg_Printf(port, UC("  resolve_gref time:   %ld ms\n"), jit_resolve_time_ns / 1000000);
  Sg_Printf(port, UC("  frame_setup time:    %ld ms\n"), jit_frame_setup_time_ns / 1000000);
  Sg_Printf(port, UC("  JIT call time:       %ld ms\n"), jit_call_time_ns / 1000000);
  Sg_Printf(port, UC("  frame_restore time:  %ld ms\n"), jit_frame_restore_time_ns / 1000000);
}

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

/*
 * JIT Helper Functions - called from JIT-compiled code
 */

/* Helper to resolve a GREF identifier to a procedure */
static SgObject resolve_gref(SgVM *vm, SgObject id)
{
  if (SG_GLOCP(id)) {
    return SG_GLOC_GET(SG_GLOC(id));
  } else if (SG_IDENTIFIERP(id)) {
    SgGloc *gloc = Sg_FindBinding(SG_IDENTIFIER_LIBRARY(id),
				  SG_IDENTIFIER_NAME(id),
				  SG_UNBOUND);
    if (SG_UNBOUNDP(gloc)) {
      Sg_Error(UC("unbound variable: %A"), id);
      return SG_UNDEF;
    }
    return SG_GLOC_GET(gloc);
  }
  return SG_UNDEF;
}

/* Push a continuation frame - called from JIT code before CALL */
void Sg__JitPushFrame(SgVM *vm, SgWord *returnPc)
{
  SgContFrame *cont = (SgContFrame *)vm->sp;
  cont->type = 0;  /* NORMAL_FRAME */
  cont->prev = vm->cont;
  cont->size = (int)(vm->sp - vm->fp);
  cont->pc = returnPc;
  cont->cl = vm->cl;
  cont->fp = vm->fp;
  /* Note: push_cont_marks is not called for simplicity - may need to add later */
  vm->cont = cont;
  vm->sp += CONT_FRAME_SIZE;
}

/* Look up a global variable - called from JIT code for GREF */
SgObject Sg__JitGref(SgObject id)
{
  SgVM *vm = Sg_VM();
  return resolve_gref(vm, id);
}

/* Create a box - called from JIT code for BOX */
SgObject Sg__JitMakeBox(SgObject value)
{
  SgBox *b = SG_NEW(SgBox);
  SG_SET_CLASS(b, SG_CLASS_BOX);
  b->value = value;
  return SG_OBJ(b);
}

/* Call a global procedure - called from JIT code for GREF_CALL */
SgObject Sg__JitGrefCall(SgVM *vm, int argc, SgObject id)
{
  uint64_t t_start = 0, t_before_jit = 0, t_after_jit = 0;

  if (jit_profile_enabled) {
    jit_call_count++;
    t_start = get_nanos();
  }

  SgObject proc = resolve_gref(vm, id);

  if (jit_profile_enabled) {
    jit_resolve_time_ns += (get_nanos() - t_start);
  }

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: GREF_CALL proc=%A argc=%d\n"), proc, argc);
  }

  if (!SG_PROCEDUREP(proc)) {
    Sg_Error(UC("procedure required, but got: %A"), proc);
    return SG_UNDEF;
  }

  /* Check if it's a closure with JIT code */
  if (SG_CLOSUREP(proc)) {
    SgClosure *cl = SG_CLOSURE(proc);
    SgCodeBuilder *cb = SG_CODE_BUILDER(cl->code);
    if (cb->jitCode != NULL) {
      /* Call JIT code directly */
      SgJitCompiledCode jitCode = cb->jitCode;

      if (jit_profile_enabled) {
        t_before_jit = get_nanos();
      }

      /* Set up frame pointer for the call */
      vm->fp = vm->sp - argc;
      vm->cl = proc;

      if (jit_profile_enabled) {
        jit_frame_setup_time_ns += (get_nanos() - t_before_jit);
        t_before_jit = get_nanos();
      }

      /* Call the JIT code */
      SgObject result = jitCode(vm, proc);

      if (jit_profile_enabled) {
        t_after_jit = get_nanos();
        /* Don't add to jit_call_time here - it's counted by nested calls */
      }

      /* Restore VM state from continuation frame */
      SgContFrame *cont = vm->cont;
      vm->sp = (SgObject *)cont;  /* Pop the continuation frame */
      vm->cl = cont->cl;
      vm->fp = cont->fp;
      vm->cont = cont->prev;

      if (jit_profile_enabled) {
        jit_frame_restore_time_ns += (get_nanos() - t_after_jit);
      }

      /* Return value is in result */
      vm->valuesCount = 1;
      return result;
    }
  }

  /* Fall back to VM interpretation for non-JIT procedures */
  /* This is complex - for now, just call the procedure using Sg_VMApply */
  vm->fp = vm->sp - argc;

  /* Collect arguments from the stack */
  SgObject args = SG_NIL;
  for (int i = argc - 1; i >= 0; i--) {
    args = Sg_Cons(vm->fp[i], args);
  }

  /* Pop the continuation frame since we're handling the call here */
  SgContFrame *cont = vm->cont;
  vm->sp = (SgObject *)cont;
  vm->cl = cont->cl;
  vm->fp = cont->fp;
  vm->cont = cont->prev;

  /* Apply the procedure */
  return Sg_Apply(proc, args);
}

/* Tail-call a global procedure - called from JIT code for GREF_TAIL_CALL */
SgObject Sg__JitGrefTailCall(SgVM *vm, int argc, SgObject id)
{
  SgObject proc = resolve_gref(vm, id);

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: GREF_TAIL_CALL proc=%A argc=%d\n"), proc, argc);
  }

  if (!SG_PROCEDUREP(proc)) {
    Sg_Error(UC("procedure required, but got: %A"), proc);
    return SG_UNDEF;
  }

  /* Check if it's a closure with JIT code */
  if (SG_CLOSUREP(proc)) {
    SgClosure *cl = SG_CLOSURE(proc);
    SgCodeBuilder *cb = SG_CODE_BUILDER(cl->code);
    if (cb->jitCode != NULL) {
      /* Tail call JIT code directly */
      SgJitCompiledCode jitCode = cb->jitCode;

      /* Set up frame pointer for the call - overwrite current frame */
      vm->fp = vm->sp - argc;
      vm->cl = proc;

      /* Jump to JIT code (tail call - no continuation pushed) */
      return jitCode(vm, proc);
    }
  }

  /* Fall back to VM interpretation */
  vm->fp = vm->sp - argc;

  /* Collect arguments from the stack */
  SgObject args = SG_NIL;
  for (int i = argc - 1; i >= 0; i--) {
    args = Sg_Cons(vm->fp[i], args);
  }

  /* Apply the procedure (tail call) */
  return Sg_Apply(proc, args);
}

/* Call a procedure (already in AC) - called from JIT code for CALL */
SgObject Sg__JitCall(SgVM *vm, int argc, SgObject proc)
{
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: CALL proc=%A argc=%d sp=%p fp=%p cont=%p\n"),
	      proc, argc, vm->sp, vm->fp, vm->cont);
  }

  if (!SG_PROCEDUREP(proc)) {
    Sg_Error(UC("procedure required, but got: %A"), proc);
    return SG_UNDEF;
  }

  /* Check if it's a closure with JIT code */
  if (SG_CLOSUREP(proc)) {
    SgClosure *cl = SG_CLOSURE(proc);
    SgCodeBuilder *cb = SG_CODE_BUILDER(cl->code);
    if (cb->jitCode != NULL) {
      /* Call JIT code directly */
      SgJitCompiledCode jitCode = cb->jitCode;

      /* Set up frame pointer for the call */
      vm->fp = vm->sp - argc;
      vm->cl = proc;

      /* Call the JIT code */
      SgObject result = jitCode(vm, proc);

      /* Restore VM state from continuation frame */
      SgContFrame *cont = vm->cont;
      vm->sp = (SgObject *)cont;  /* Pop the continuation frame */
      vm->cl = cont->cl;
      vm->fp = cont->fp;
      vm->cont = cont->prev;

      /* Return value is in result.
       * Note: DO NOT reset valuesCount here - if the JIT code used VALUES,
       * vm->valuesCount was already set appropriately. */
      return result;
    }
  }

  /* Fall back to VM interpretation */
  vm->fp = vm->sp - argc;

  /* Collect arguments from the stack */
  SgObject args = SG_NIL;
  for (int i = argc - 1; i >= 0; i--) {
    args = Sg_Cons(vm->fp[i], args);
  }

  /* Pop the continuation frame since we're handling the call here */
  SgContFrame *cont = vm->cont;
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: CALL fallback: cont=%p cont->prev=%p, setting sp=%p\n"),
	      cont, cont->prev, cont);
  }
  vm->sp = (SgObject *)cont;
  vm->cl = cont->cl;
  vm->fp = cont->fp;
  vm->cont = cont->prev;

  /* Apply the procedure */
  return Sg_Apply(proc, args);
}

/* Tail-call a procedure (already in AC) - called from JIT code for TAIL_CALL */
SgObject Sg__JitTailCall(SgVM *vm, int argc, SgObject proc)
{
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: TAIL_CALL proc=%A argc=%d\n"), proc, argc);
  }

  if (!SG_PROCEDUREP(proc)) {
    Sg_Error(UC("procedure required, but got: %A"), proc);
    return SG_UNDEF;
  }

  /* Check if it's a closure with JIT code */
  if (SG_CLOSUREP(proc)) {
    SgClosure *cl = SG_CLOSURE(proc);
    SgCodeBuilder *cb = SG_CODE_BUILDER(cl->code);
    if (cb->jitCode != NULL) {
      /* Tail call JIT code directly */
      SgJitCompiledCode jitCode = cb->jitCode;

      /* Set up frame pointer for the call - overwrite current frame */
      vm->fp = vm->sp - argc;
      vm->cl = proc;

      /* Jump to JIT code (tail call - no continuation pushed) */
      return jitCode(vm, proc);
    }
  }

  /* Fall back to VM interpretation */
  vm->fp = vm->sp - argc;

  /* Collect arguments from the stack */
  SgObject args = SG_NIL;
  for (int i = argc - 1; i >= 0; i--) {
    args = Sg_Cons(vm->fp[i], args);
  }

  /* Apply the procedure (tail call) */
  return Sg_Apply(proc, args);
}

/*
 * APPLY helper - called from JIT code for APPLY instruction
 *
 * nargc: number of explicit arguments (not including proc and tail list)
 * listArg: the list argument (last arg to apply)
 * isTail: whether this is a tail apply
 *
 * Stack layout before call:
 *   SP[0] = arg[nargc-1]
 *   SP[1] = arg[nargc-2]
 *   ...
 *   SP[nargc-1] = arg[0]
 *   SP[nargc] = proc
 *
 * Returns: result of apply
 */
SgObject Sg__JitApply(SgVM *vm, int nargc, SgObject listArg, int isTail)
{
  long listLen = Sg_Length(listArg);
  SgObject proc;
  SgObject args = SG_NIL;

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: APPLY nargc=%d listArg=%A isTail=%d\n"),
	      nargc, listArg, isTail);
  }

  if (listLen < 0) {
    Sg_AssertionViolation(SG_INTERN("apply"),
			  SG_MAKE_STRING("improper list not allowed"),
			  listArg);
    return SG_UNDEF;
  }

  /* Get procedure from stack (it's at SP[-nargc-1] position) */
  proc = vm->sp[-(nargc + 1)];

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: APPLY proc=%A\n"), proc);
  }

  /* Build args list: explicit args + list args */
  /* Start with the list argument */
  args = Sg_CopyList(listArg);

  /* Add explicit args in reverse order (they're on stack in reverse) */
  for (int i = 0; i < nargc; i++) {
    SgObject arg = vm->sp[-(i + 1)];
    args = Sg_Cons(arg, args);
  }

  /* Adjust stack pointer - pop proc and explicit args */
  vm->sp -= (nargc + 1);

  if (!isTail) {
    /* Non-tail apply: pop continuation frame after call */
    SgContFrame *cont = vm->cont;
    vm->sp = (SgObject *)cont;
    vm->cl = cont->cl;
    vm->fp = cont->fp;
    vm->cont = cont->prev;
  }

  /* Apply the procedure */
  return Sg_Apply(proc, args);
}

/*
 * VALUES helper - called from JIT code for VALUES instruction
 *
 * nvalues: number of values
 * lastVal: the last value (AC)
 *
 * Stack has nvalues-1 values to pop.
 * Sets vm->valuesCount and stores values in vm->values[] array.
 * Returns the first value (which goes into AC).
 */
SgObject Sg__JitValues(SgVM *vm, int nvalues, SgObject lastVal)
{
  int n = nvalues - 1;  /* Number of values to pop from stack */
  SgObject v = lastVal;

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: VALUES nvalues=%d lastVal=%A\n"),
	      nvalues, lastVal);
  }

  vm->valuesCount = nvalues;

  /* Allocate extra buffer if needed */
  if (n > DEFAULT_VALUES_SIZE) {
    SG_ALLOC_VALUES_BUFFER(vm, n - DEFAULT_VALUES_SIZE);
  }

  /* Store values from stack to values array (in reverse order) */
  for (; n > 0; n--) {
    SG_VALUES_SET(vm, n - 1, v);
    v = *(--vm->sp);  /* POP */
  }

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: VALUES returning first=%A, valuesCount=%d, values[0]=%A\n"),
	      v, vm->valuesCount, (nvalues > 1) ? vm->values[0] : SG_UNDEF);
  }

  /* Return the first value (to be stored in AC) */
  return v;
}

/*
 * RECEIVE helper - called from JIT code for RECEIVE instruction
 *
 * reqCount: number of required values
 * optCount: 0 = exact match required, 1 = rest values collected as list
 *
 * Returns the first value (or SG_UNDEF if error).
 */
SgObject Sg__JitReceive(SgVM *vm, int reqCount, int optCount)
{
  int numValues = vm->valuesCount;

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: RECEIVE reqCount=%d optCount=%d numValues=%d ac=%A\n"),
	      reqCount, optCount, numValues, vm->ac);
  }

  if (numValues < reqCount) {
    Sg_AssertionViolation(SG_INTERN("receive"),
			  SG_MAKE_STRING("received fewer values than expected"),
			  vm->ac);
    return SG_UNDEF;
  }

  if (optCount == 0 && numValues > reqCount) {
    Sg_AssertionViolation(SG_INTERN("receive"),
			  SG_MAKE_STRING("received more values than expected"),
			  vm->ac);
    return SG_UNDEF;
  }

  if (optCount == 0) {
    /* Exact match - push required values to stack */
    if (reqCount > 0) {
      *(vm->sp++) = vm->ac;  /* PUSH */
    }
    for (int i = 0; i < reqCount - 1; i++) {
      *(vm->sp++) = SG_VALUES_REF(vm, i);  /* PUSH */
    }
  } else if (reqCount == 0) {
    /* All values as list */
    SgObject h = SG_NIL, t = SG_NIL;
    if (numValues > 0) {
      SG_APPEND1(h, t, vm->ac);
    }
    if (numValues > 1) {
      for (int i = 0; i < numValues - 1; i++) {
	SG_APPEND1(h, t, SG_VALUES_REF(vm, i));
      }
    }
    *(vm->sp++) = h;  /* PUSH the list */
  } else {
    /* reqCount required values + rest as list */
    SgObject h = SG_NIL, t = SG_NIL;
    int i = 0;
    *(vm->sp++) = vm->ac;  /* PUSH first value */
    for (; i < numValues - 1; i++) {
      if (i < reqCount - 1) {
	*(vm->sp++) = SG_VALUES_REF(vm, i);  /* PUSH */
      } else {
	SG_APPEND1(h, t, SG_VALUES_REF(vm, i));
      }
    }
    *(vm->sp++) = h;  /* PUSH the rest list */
  }

  vm->valuesCount = 1;
  return SG_UNDEF;  /* AC is not used after RECEIVE */
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

    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
		UC("JIT: Processing opcode %d at pc=%d\n"),
		opcode, pc);
    }

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
	/* TEST has operand in next word */
	if (pc + 1 >= cb->size) goto fail;
	intptr_t offset = (intptr_t)cb->code[pc + 1];
	int targetPc = (pc + 1) + offset;  /* operand position + offset */
	if (!Sg__JitEmit_TEST(&ctx, targetPc)) goto fail;
	pc += 2;
      }
      break;

    case JUMP:
      {
	/* JUMP has operand in next word */
	if (pc + 1 >= cb->size) goto fail;
	intptr_t offset = (intptr_t)cb->code[pc + 1];
	int targetPc = (pc + 1) + offset;  /* operand position + offset */
	if (!Sg__JitEmit_JUMP(&ctx, targetPc)) goto fail;
	pc += 2;
      }
      break;

    case BNNUME:
      {
	/* BNxxx has operand in next word */
	if (pc + 1 >= cb->size) goto fail;
	intptr_t offset = (intptr_t)cb->code[pc + 1];
	int targetPc = (pc + 1) + offset;  /* operand position + offset */
	if (!Sg__JitEmit_BNNUME(&ctx, targetPc)) goto fail;
	pc += 2;
      }
      break;

    case BNLT:
      {
	if (pc + 1 >= cb->size) goto fail;
	intptr_t offset = (intptr_t)cb->code[pc + 1];
	int targetPc = (pc + 1) + offset;
	if (!Sg__JitEmit_BNLT(&ctx, targetPc)) goto fail;
	pc += 2;
      }
      break;

    case BNLE:
      {
	if (pc + 1 >= cb->size) goto fail;
	intptr_t offset = (intptr_t)cb->code[pc + 1];
	int targetPc = (pc + 1) + offset;
	if (!Sg__JitEmit_BNLE(&ctx, targetPc)) goto fail;
	pc += 2;
      }
      break;

    case BNGT:
      {
	if (pc + 1 >= cb->size) goto fail;
	intptr_t offset = (intptr_t)cb->code[pc + 1];
	int targetPc = (pc + 1) + offset;
	if (!Sg__JitEmit_BNGT(&ctx, targetPc)) goto fail;
	pc += 2;
      }
      break;

    case BNGE:
      {
	if (pc + 1 >= cb->size) goto fail;
	intptr_t offset = (intptr_t)cb->code[pc + 1];
	int targetPc = (pc + 1) + offset;
	if (!Sg__JitEmit_BNGE(&ctx, targetPc)) goto fail;
	pc += 2;
      }
      break;

    case BNNULL:
      {
	if (pc + 1 >= cb->size) goto fail;
	intptr_t offset = (intptr_t)cb->code[pc + 1];
	int targetPc = (pc + 1) + offset;
	if (!Sg__JitEmit_BNNULL(&ctx, targetPc)) goto fail;
	pc += 2;
      }
      break;

    case BNEQ:
      {
	if (pc + 1 >= cb->size) goto fail;
	intptr_t offset = (intptr_t)cb->code[pc + 1];
	int targetPc = (pc + 1) + offset;
	if (!Sg__JitEmit_BNEQ(&ctx, targetPc)) goto fail;
	pc += 2;
      }
      break;

    case CAR:
      if (!Sg__JitEmit_CAR(&ctx)) goto fail;
      pc++;
      break;

    case CDR:
      if (!Sg__JitEmit_CDR(&ctx)) goto fail;
      pc++;
      break;

    case CONS:
      if (!Sg__JitEmit_CONS(&ctx)) goto fail;
      pc++;
      break;

    case NULLP:
      if (!Sg__JitEmit_NULLP(&ctx)) goto fail;
      pc++;
      break;

    case PAIRP:
      if (!Sg__JitEmit_PAIRP(&ctx)) goto fail;
      pc++;
      break;

    case NOT:
      if (!Sg__JitEmit_NOT(&ctx)) goto fail;
      pc++;
      break;

    case EQ:
      if (!Sg__JitEmit_EQ(&ctx)) goto fail;
      pc++;
      break;

    case MUL:
      if (!Sg__JitEmit_MUL(&ctx)) goto fail;
      pc++;
      break;

    case MULI:
      if (!Sg__JitEmit_MULI(&ctx, val1)) goto fail;
      pc++;
      break;

    case DIV:
      if (!Sg__JitEmit_DIV(&ctx)) goto fail;
      pc++;
      break;

    case DIVI:
      if (!Sg__JitEmit_DIVI(&ctx, val1)) goto fail;
      pc++;
      break;

    case NEG:
      if (!Sg__JitEmit_NEG(&ctx)) goto fail;
      pc++;
      break;

    case EQV:
      if (!Sg__JitEmit_EQV(&ctx)) goto fail;
      pc++;
      break;

    case SYMBOLP:
      if (!Sg__JitEmit_SYMBOLP(&ctx)) goto fail;
      pc++;
      break;

    case GREF:
      {
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);
	if (!Sg__JitEmit_GREF(&ctx, id)) goto fail;
	pc += 2;
      }
      break;

    case GREF_PUSH:
      {
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);
	if (!Sg__JitEmit_GREF_PUSH(&ctx, id)) goto fail;
	pc += 2;
      }
      break;

    case FREF_PUSH:
      if (!Sg__JitEmit_FREF_PUSH(&ctx, val1)) goto fail;
      pc++;
      break;

    case LIST:
      if (!Sg__JitEmit_LIST(&ctx, val1)) goto fail;
      pc++;
      break;

    case CAAR:
      if (!Sg__JitEmit_CAAR(&ctx)) goto fail;
      pc++;
      break;

    case CADR:
      if (!Sg__JitEmit_CADR(&ctx)) goto fail;
      pc++;
      break;

    case CDAR:
      if (!Sg__JitEmit_CDAR(&ctx)) goto fail;
      pc++;
      break;

    case CDDR:
      if (!Sg__JitEmit_CDDR(&ctx)) goto fail;
      pc++;
      break;

    case BNEQV:
      {
	if (pc + 1 >= cb->size) goto fail;
	intptr_t offset = (intptr_t)cb->code[pc + 1];
	int targetPc = (pc + 1) + offset;
	if (!Sg__JitEmit_BNEQV(&ctx, targetPc)) goto fail;
	pc += 2;
      }
      break;

    /* Combined car/cdr instructions */
    case CAR_PUSH:
      if (!Sg__JitEmit_CAR_PUSH(&ctx)) goto fail;
      pc++;
      break;

    case CDR_PUSH:
      if (!Sg__JitEmit_CDR_PUSH(&ctx)) goto fail;
      pc++;
      break;

    case CONS_PUSH:
      if (!Sg__JitEmit_CONS_PUSH(&ctx)) goto fail;
      pc++;
      break;

    case LREF_CAR:
      if (!Sg__JitEmit_LREF_CAR(&ctx, val1)) goto fail;
      pc++;
      break;

    case LREF_CDR:
      if (!Sg__JitEmit_LREF_CDR(&ctx, val1)) goto fail;
      pc++;
      break;

    case FREF_CAR:
      if (!Sg__JitEmit_FREF_CAR(&ctx, val1)) goto fail;
      pc++;
      break;

    case FREF_CDR:
      if (!Sg__JitEmit_FREF_CDR(&ctx, val1)) goto fail;
      pc++;
      break;

    case GREF_CAR:
      {
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);
	if (!Sg__JitEmit_GREF_CAR(&ctx, id)) goto fail;
	pc += 2;
      }
      break;

    case GREF_CDR:
      {
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);
	if (!Sg__JitEmit_GREF_CDR(&ctx, id)) goto fail;
	pc += 2;
      }
      break;

    case LREF_CAR_PUSH:
      if (!Sg__JitEmit_LREF_CAR_PUSH(&ctx, val1)) goto fail;
      pc++;
      break;

    case LREF_CDR_PUSH:
      if (!Sg__JitEmit_LREF_CDR_PUSH(&ctx, val1)) goto fail;
      pc++;
      break;

    case FREF_CAR_PUSH:
      if (!Sg__JitEmit_FREF_CAR_PUSH(&ctx, val1)) goto fail;
      pc++;
      break;

    case FREF_CDR_PUSH:
      if (!Sg__JitEmit_FREF_CDR_PUSH(&ctx, val1)) goto fail;
      pc++;
      break;

    case GREF_CAR_PUSH:
      {
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);
	if (!Sg__JitEmit_GREF_CAR_PUSH(&ctx, id)) goto fail;
	pc += 2;
      }
      break;

    case GREF_CDR_PUSH:
      {
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);
	if (!Sg__JitEmit_GREF_CDR_PUSH(&ctx, id)) goto fail;
	pc += 2;
      }
      break;

    case CONST_RET:
      {
	if (pc + 1 >= cb->size) goto fail;
	if (!Sg__JitEmit_CONST_RET(&ctx, SG_OBJ(cb->code[pc + 1]))) goto fail;
	pc += 2;
      }
      break;

    /* Mutation operations */
    case SET_CAR:
      if (!Sg__JitEmit_SET_CAR(&ctx)) goto fail;
      pc++;
      break;

    case SET_CDR:
      if (!Sg__JitEmit_SET_CDR(&ctx)) goto fail;
      pc++;
      break;

    case BOX:
      if (!Sg__JitEmit_BOX(&ctx, val1)) goto fail;
      pc++;
      break;

    case UNBOX:
      if (!Sg__JitEmit_UNBOX(&ctx)) goto fail;
      pc++;
      break;

    case FSET:
      if (!Sg__JitEmit_FSET(&ctx, val1)) goto fail;
      pc++;
      break;

    /* Stack management */
    case LEAVE:
      if (!Sg__JitEmit_LEAVE(&ctx, val1)) goto fail;
      pc++;
      break;

    case INST_STACK:
      if (!Sg__JitEmit_INST_STACK(&ctx, val1)) goto fail;
      pc++;
      break;

    case RESV_STACK:
      if (!Sg__JitEmit_RESV_STACK(&ctx, val1)) goto fail;
      pc++;
      break;

    /* Vector operations */
    case VECTORP:
      if (!Sg__JitEmit_VECTORP(&ctx)) goto fail;
      pc++;
      break;

    case VEC_LEN:
      if (!Sg__JitEmit_VEC_LEN(&ctx)) goto fail;
      pc++;
      break;

    case VEC_REF:
      if (!Sg__JitEmit_VEC_REF(&ctx)) goto fail;
      pc++;
      break;

    case VEC_SET:
      if (!Sg__JitEmit_VEC_SET(&ctx)) goto fail;
      pc++;
      break;

    case VECTOR:
      if (!Sg__JitEmit_VECTOR(&ctx, val1)) goto fail;
      pc++;
      break;

    case FRAME:
      {
	/* FRAME instruction: push continuation frame
	 * Operand is in next word, gives offset to return PC
	 * After FETCH_OPERAND, VM's PC is at pc+2
	 * Return address is: (pc + 2) + (operand - 1) = (pc + 1) + operand */
	if (pc + 1 >= cb->size) goto fail;
	intptr_t n = (intptr_t)cb->code[pc + 1];
	int returnPc = (pc + 1) + n;
	if (!Sg__JitEmit_FRAME(&ctx, returnPc)) goto fail;
	pc += 2;
      }
      break;

    case CALL:
      /* CALL: val1 = argc, proc is in AC */
      if (!Sg__JitEmit_CALL(&ctx, val1)) goto fail;
      pc++;
      break;

    case TAIL_CALL:
      /* TAIL_CALL: val1 = argc, proc is in AC */
      if (!Sg__JitEmit_TAIL_CALL(&ctx, val1)) goto fail;
      pc++;
      break;

    case LOCAL_CALL:
      /* LOCAL_CALL: val1 = argc, proc (closure) is in AC */
      if (!Sg__JitEmit_LOCAL_CALL(&ctx, val1)) goto fail;
      pc++;
      break;

    case LOCAL_TAIL_CALL:
      /* LOCAL_TAIL_CALL: val1 = argc, proc (closure) is in AC */
      if (!Sg__JitEmit_LOCAL_TAIL_CALL(&ctx, val1)) goto fail;
      pc++;
      break;

    case CLOSURE:
      {
        /* CLOSURE: val1 = self_pos, next word = code builder */
        if (pc + 1 >= cb->size) goto fail;
        SgObject innerCb = SG_OBJ(cb->code[pc + 1]);
        int freec = SG_CODE_BUILDER_FREEC(innerCb);
        if (!Sg__JitEmit_CLOSURE(&ctx, val1, innerCb, freec)) goto fail;
        pc += 2;
      }
      break;

    case GREF_CALL:
      {
	/* GREF_CALL: val1 = argc, next word = identifier */
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);
	/* Check for self-recursion optimization */
	SgObject proc = resolve_gref(Sg_VM(), id);
	if (SG_CLOSUREP(proc) && SG_CODE_BUILDER(SG_CLOSURE(proc)->code) == cb) {
	  /* Self-recursive call - emit direct branch with frame handling */
	  if (jit_verbose) {
	    Sg_Printf(Sg_StandardErrorPort(),
		      UC("JIT: Self-recursive call detected, optimizing\n"));
	  }
	  if (!Sg__JitEmit_SELF_CALL(&ctx, val1)) {
	    /* Fall back to C helper if SELF_CALL fails */
	    if (!Sg__JitEmit_GREF_CALL(&ctx, val1, id)) goto fail;
	  }
	} else {
	  if (!Sg__JitEmit_GREF_CALL(&ctx, val1, id)) goto fail;
	}
	pc += 2;  /* Skip instruction and operand */
      }
      break;

    case GREF_TAIL_CALL:
      {
	/* GREF_TAIL_CALL: val1 = argc, next word = identifier */
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);
	/* Check for self-recursion optimization - tail calls are safe */
	SgObject proc = resolve_gref(Sg_VM(), id);
	if (SG_CLOSUREP(proc) && SG_CODE_BUILDER(SG_CLOSURE(proc)->code) == cb) {
	  /* Self-recursive tail call - emit direct branch (with fallback) */
	  if (jit_verbose) {
	    Sg_Printf(Sg_StandardErrorPort(),
		      UC("JIT: Self-recursive tail call detected, optimizing\n"));
	  }
	  if (!Sg__JitEmit_SELF_TAIL_CALL(&ctx, val1, id)) goto fail;
	} else {
	  if (!Sg__JitEmit_GREF_TAIL_CALL(&ctx, val1, id)) goto fail;
	}
	pc += 2;  /* Skip instruction and operand */
      }
      break;

    case RET:
      if (!Sg__JitEmit_RET(&ctx)) goto fail;
      pc++;
      break;

    case APPLY:
      {
	/* APPLY: val1 = total argc (including proc and list), val2 = tail flag
	 * For 2-value instructions, we need to mask val1 properly */
	int totalArgc = INSN_VALUE1(insn) & INSN_VALUE1_MASK;
	int isTail = INSN_VALUE2(insn);
	int nargc = totalArgc - 2;  /* explicit args (not including proc and list) */
	if (!Sg__JitEmit_APPLY(&ctx, nargc, isTail)) goto fail;
	pc++;
      }
      break;

    case VALUES:
      /* VALUES: val1 = number of values */
      if (!Sg__JitEmit_VALUES(&ctx, val1)) goto fail;
      pc++;
      break;

    case RECEIVE:
      {
	/* RECEIVE: val1 = required count, val2 = optional flag (0 or 1) */
	int reqCount = INSN_VALUE1(insn) & INSN_VALUE1_MASK;
	int optCount = INSN_VALUE2(insn);
	if (!Sg__JitEmit_RECEIVE(&ctx, reqCount, optCount)) goto fail;
	pc++;
      }
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

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: All instructions processed, emitting epilogue\n"));
  }

  /* Bind epilogue label and emit epilogue */
  Sg__JitBindLabel(&ctx, ctx.epilogueLabel);
  if (!Sg__JitEmit_Epilogue(&ctx)) {
    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
		UC("JIT: Epilogue failed\n"));
    }
    goto fail;
  }

  /* Resolve forward references */
  if (!Sg__JitPlatformResolve(&ctx)) {
    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
		UC("JIT: Resolve failed\n"));
    }
    goto fail;
  }

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
