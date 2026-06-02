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
#include <string.h>  /* for strstr */
#include "jit_emit.h"

#ifdef HAVE_JIT

#include "../sagittarius.h"
#include "../sagittarius/private/code.h"
#include "../sagittarius/private/generic.h"
#include "../sagittarius/private/instruction.h"
#include <time.h>
#include <string.h>

/*
 * Static bytecode for dispatching "other" procedure types without C boundary.
 *
 * These bytecode arrays are used when JIT encounters procedure types other than
 * SUBR, CLOSURE, or GENERIC (e.g., NEXT_METHOD, METHOD). Instead of calling
 * Sg_Apply (which creates a C continuation boundary), we yield to interpreter
 * with vm->pc set to this bytecode. The TAIL_CALL instruction will dispatch
 * based on procedure type, and eventually RET will find the JIT continuation
 * frame and re-enter JIT code.
 *
 * This matches the approach used by Sg_VMApply* in vm.c.
 */
static SgWord jit_tail_call_dispatch[][2] = {
  { MERGE_INSN_VALUE1(TAIL_CALL, 0), RET },
  { MERGE_INSN_VALUE1(TAIL_CALL, 1), RET },
  { MERGE_INSN_VALUE1(TAIL_CALL, 2), RET },
  { MERGE_INSN_VALUE1(TAIL_CALL, 3), RET },
  { MERGE_INSN_VALUE1(TAIL_CALL, 4), RET },
  { MERGE_INSN_VALUE1(TAIL_CALL, 5), RET },
  { MERGE_INSN_VALUE1(TAIL_CALL, 6), RET },
  { MERGE_INSN_VALUE1(TAIL_CALL, 7), RET },
};
#define JIT_MAX_DIRECT_ARGC 7

/* Global JIT configuration */
static int jit_enabled = 0;  /* Disabled by default - use -j flag to enable */
static int jit_threshold = SG_JIT_DEFAULT_THRESHOLD;
static int jit_verbose = 0;  /* Disabled by default */

/* Forward declarations for helper functions */
static int adjust_args(SgVM *vm, int argc, SgObject proc);

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

/* Initial buffer size for JIT code (16KB for larger functions) */
#define JIT_INITIAL_BUFFER_SIZE 16384

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

void Sg_JitIncrementCallCount(SgCodeBuilder *cb)
{
  cb->callCount++;
}

void Sg_SetJitVerbose(int verbose)
{
  jit_verbose = verbose;
}

int Sg_JitVerbose(void)
{
  return jit_verbose;
}

/* Debug helper called from JIT code */
void Sg__JitDebugPairp(SgObject obj)
{
  if (jit_verbose) {
    int is_hptr = SG_HPTRP(obj);
    int htag = is_hptr ? SG_HTAG(obj) : -1;
    int is_pair = SG_PAIRP(obj);
    SgWord first_word = is_hptr ? *(SgWord*)obj : 0;
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT PAIRP input: obj=%p hptr=%d htag=%d pair=%d first_word=%lx low3=%d\n"),
              obj, is_hptr, htag, is_pair, (unsigned long)first_word, (int)(first_word & 7));
  }
}

/* Debug helper called after JIT PAIRP */
void Sg__JitDebugPairpResult(SgObject result)
{
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT PAIRP output: result=%A\n"), result);
  }
}

/* Debug helper to trace car and tag in PAIRP */
void Sg__JitDebugPairpCar(SgObject car, int tag)
{
  Sg_Printf(Sg_StandardErrorPort(),
            UC("JIT PAIRP car: car=%p tag=%d (is7=%d)\n"), car, tag, tag == 7);
}

/* Debug helper called at key points - code identifies the point */
void Sg__JitDebugPoint(int code, SgObject ac)
{
  if (jit_verbose) {
    const char *name = "UNKNOWN";
    switch (code) {
    case 1: name = "TEST"; break;
    case 2: name = "BNNULL"; break;
    case 3: name = "SYMBOLP"; break;
    }
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT DEBUG %s: AC=%A\n"), name, ac);
  }
}

/*
 * JIT Context Initialization
 */
void Sg_InitJitContext(SgVM *vm)
{
  vm->jitContext.active = 0;
  vm->jitContext.savedSp = NULL;
  vm->jitContext.savedFp = NULL;
  vm->jitContext.savedCl = SG_FALSE;
  vm->jitContext.savedDepth = 0;
  vm->jitContext.returnAddr = NULL;
}

/*
 * JIT Helper Functions - called from JIT-compiled code
 */

/*
 * Resolve identifier to GLOC at JIT compile time.
 * 
 * The VM interpreter performs "inline caching" - when it executes GREF,
 * it resolves identifiers to GLOCs and overwrites the bytecode. This
 * causes a race condition:
 * 
 * 1. JIT compiles code, reading IDENTIFIER from bytecode
 * 2. VM interpreter runs, patches bytecode with GLOC
 * 3. Original IDENTIFIER may be GC'd (memory reused)
 * 4. JIT code runs with stale IDENTIFIER pointer
 * 
 * We fix this by resolving identifiers to GLOCs during JIT compilation,
 * matching the VM's inline caching behavior. This also updates the bytecode
 * so future JIT compilations see the GLOC directly.
 *
 * Returns: The resolved id (either the original GLOC, or a newly resolved
 *          GLOC if the identifier was successfully resolved, or the original
 *          identifier if resolution failed).
 */
static SgObject resolve_identifier_at_compile_time(SgCodeBuilder *cb, int pc,
                                                    SgObject id)
{
  if (SG_IDENTIFIERP(id)) {
    SgGloc *gloc = Sg_FindBinding(SG_IDENTIFIER_LIBRARY(id),
                                  SG_IDENTIFIER_NAME(id),
                                  SG_UNBOUND);
    if (!SG_UNBOUNDP(gloc) && SG_GLOCP(gloc)) {
      /* Update bytecode with GLOC (same as VM inline caching) */
      cb->code[pc + 1] = (SgWord)gloc;
      if (jit_verbose) {
        Sg_Printf(Sg_StandardErrorPort(),
                  UC("JIT: Resolved identifier to GLOC %A in %A\n"),
                  SG_GLOC(gloc)->name, cb->name);
      }
      return (SgObject)gloc;
    }
  }
  return id;
}

/* Helper to resolve a GREF identifier to a procedure.
 * Returns SG_UNBOUND if the identifier is not yet bound (e.g., during import).
 * This should NOT throw an error - the identifier might be bound by the time
 * the JIT code actually executes.
 */
static SgObject resolve_gref(SgVM *vm, SgObject id)
{
  if (SG_GLOCP(id)) {
    return SG_GLOC_GET(SG_GLOC(id));
  } else if (SG_IDENTIFIERP(id)) {
    SgGloc *gloc = Sg_FindBinding(SG_IDENTIFIER_LIBRARY(id),
				  SG_IDENTIFIER_NAME(id),
				  SG_UNBOUND);
    if (SG_UNBOUNDP(gloc)) {
      /* Don't throw error - return SG_UNBOUND to indicate not yet bound */
      return SG_UNBOUND;
    }
    return SG_GLOC_GET(gloc);
  }
  return SG_UNDEF;
}

/* Push a continuation frame - called from JIT code before CALL */
void Sg__JitPushFrame(SgVM *vm, SgWord *returnPc, void *jitReturnAddr)
{
  SgContFrame *cont = (SgContFrame *)vm->sp;
  /* DEBUG: Check for corruption - vm->cl should never be the VM pointer itself */
  if (vm->cl == (SgObject)vm) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT-PUSH-FRAME ERROR: vm->cl == vm! (corruption detected)\n"));
    Sg_Printf(Sg_StandardErrorPort(),
              UC("  vm=%p, returnPc=%p, jitReturnAddr=%p\n"),
              vm, returnPc, jitReturnAddr);
    Sg_FlushPort(Sg_StandardErrorPort());
    ASSERT(vm->cl != (SgObject)vm);
  }
  cont->type = 0;  /* NORMAL_FRAME */
  cont->prev = vm->cont;
  cont->size = (int)(vm->sp - vm->fp);
  cont->pc = returnPc;
  cont->cl = vm->cl;
  cont->fp = vm->fp;
  vm->cont = cont;
  vm->sp += CONT_FRAME_SIZE;
  
  /* Push continuation marks for this frame and set JIT return address */
  Sg_JitPushContMarks(vm, cont);
  if (jitReturnAddr) {
    Sg_JitSetReturnMark(vm, jitReturnAddr);
  }
}

/* Look up a global variable - called from JIT code for GREF.
 * Unlike resolve_gref (used during compilation), this is called at runtime
 * so it should throw an error if the variable is unbound.
 */
SgObject Sg__JitGref(SgObject id)
{
  SgVM *vm = Sg_VM();
  SgObject result = resolve_gref(vm, id);
  if (SG_UNBOUNDP(result)) {
    Sg_Error(UC("unbound variable: %A"), id);
  }
  return result;
}

/* Create a box - called from JIT code for BOX */
SgObject Sg__JitMakeBox(SgObject value)
{
  SgBox *b = SG_NEW(SgBox);
  SG_SET_CLASS(b, SG_CLASS_BOX);
  b->value = value;
  return SG_OBJ(b);
}

/*
 * Common Helper Functions for Call Dispatch
 *
 * These helpers reduce code duplication between GREF_CALL/CALL and
 * GREF_TAIL_CALL/TAIL_CALL variants.
 */

/* Shift arguments from SP to FP for tail calls */
static void shift_args_to_fp(SgVM *vm, int argc)
{
  if (jit_verbose && argc > 0) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: shift_args_to_fp BEFORE: argc=%d fp=%p sp=%p fp[0]=%p (%A) sp[-1]=%p (%A)\n"),
              argc, vm->fp, vm->sp, vm->fp[0], vm->fp[0], vm->sp[-1], vm->sp[-1]);
    Sg_FlushPort(Sg_StandardErrorPort());
  }
  for (int i = 0; i < argc; i++) {
    vm->fp[i] = vm->sp[-(argc - i)];
  }
  vm->sp = vm->fp + argc;
  if (jit_verbose && argc > 0) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: shift_args_to_fp AFTER: sp=%p fp[0]=%p (%A)\n"),
              vm->sp, vm->fp[0], vm->fp[0]);
    Sg_FlushPort(Sg_StandardErrorPort());
  }
}

/* Adjust optional arguments for non-tail closure calls.
 * Returns the adjusted argc.
 */
static int adjust_optargs_nontail(SgVM *vm, SgObject proc, int argc,
                                   SgObject **args_start_out)
{
  int required = SG_PROCEDURE_REQUIRED(proc);
  int optargs = SG_PROCEDURE_OPTIONAL(proc);
  SgObject *args_start = vm->sp - argc;
  
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: adjust_optargs_nontail proc=%A argc=%d required=%d optargs=%d\n"),
              proc, argc, required, optargs);
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT:   args_start=%p vm->sp=%p\n"),
              args_start, vm->sp);
    for (int i = 0; i < argc && i < 5; i++) {
      Sg_Printf(Sg_StandardErrorPort(),
                UC("JIT:   args_start[%d] = %A\n"), i, args_start[i]);
    }
    Sg_FlushPort(Sg_StandardErrorPort());
  }
  
  if (argc < required) {
    Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(proc),
                                       required, argc,
                                       Sg_ArrayToList(args_start, argc));
    return -1;
  }
  
  /* Fold excess arguments into rest-args list */
  SgObject p = SG_NIL;
  while (argc > required + optargs - 1) {
    argc--;
    p = Sg_Cons(args_start[argc], p);
  }
  
  /* Push the rest-args list at args_start[argc] */
  args_start[argc] = p;
  argc++;
  
  /* Update sp to reflect the new argument count */
  vm->sp = args_start + argc;
  
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT:   AFTER: argc=%d args_start[0]=%A vm->sp=%p\n"),
              argc, args_start[0], vm->sp);
    Sg_FlushPort(Sg_StandardErrorPort());
  }
  
  if (args_start_out) *args_start_out = args_start;
  return argc;
}

/* Adjust optional arguments for tail closure calls and shift to FP.
 * Returns the adjusted argc.
 */
static int adjust_optargs_tail(SgVM *vm, SgObject proc, int argc)
{
  int required = SG_PROCEDURE_REQUIRED(proc);
  int optargs = SG_PROCEDURE_OPTIONAL(proc);
  
  if (argc < required) {
    Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(proc),
                                       required, argc,
                                       Sg_ArrayToList(vm->sp - argc, argc));
    return -1;
  }
  
  /* Fold excess arguments into rest-args list.
   * We need to pop the LAST pushed arguments (at sp[-1]) first,
   * not the first pushed (at sp[-argc]).
   */
  SgObject p = SG_NIL;
  while (argc > required + optargs - 1) {
    SgObject a = vm->sp[-1];  /* Get LAST argument (most recently pushed) */
    p = Sg_Cons(a, p);
    argc--;
    vm->sp--;
  }
  
  /* Shift remaining args to FP and add rest-args list */
  for (int i = 0; i < argc; i++) {
    vm->fp[i] = vm->sp[-(argc - i)];
  }
  vm->fp[argc] = p;
  argc++;
  vm->sp = vm->fp + argc;
  
  return argc;
}

/* Yield to interpreter for a closure call (non-tail).
 * Sets up VM state and returns YIELD_MARKER.
 */
static SgObject yield_for_closure(SgVM *vm, SgObject proc, int argc)
{
  SgClosure *cl = SG_CLOSURE(proc);
  SgCodeBuilder *cb = SG_CODE_BUILDER(cl->code);
  int required = SG_PROCEDURE_REQUIRED(proc);
  int optargs = SG_PROCEDURE_OPTIONAL(proc);
  
  if (optargs) {
    SgObject *args_start;
    int new_argc = adjust_optargs_nontail(vm, proc, argc, &args_start);
    if (new_argc < 0) return SG_UNDEF;
    vm->fp = args_start;
    argc = new_argc;
    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
                UC("JIT: yield_for_closure optargs: argc=%d fp=%p fp[0]=%A\n"),
                argc, vm->fp, vm->fp[0]);
      Sg_FlushPort(Sg_StandardErrorPort());
    }
  } else if (argc != required) {
    Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(proc),
                                       required, argc,
                                       Sg_ArrayToList(vm->sp - argc, argc));
    return SG_UNDEF;
  } else {
    vm->fp = vm->sp - argc;
  }
  
  vm->cl = proc;
  vm->pc = cb->code;
  
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: Yielding for closure call to %A (argc=%d, fp=%p)\n"),
              proc, argc, vm->fp);
  }
  
  return SG_JIT_YIELD_MARKER;
}

/* Yield to interpreter for a closure tail call.
 * Sets up VM state with args shifted to FP and returns YIELD_MARKER.
 */
static SgObject yield_for_closure_tail(SgVM *vm, SgObject proc, int argc)
{
  SgClosure *cl = SG_CLOSURE(proc);
  SgCodeBuilder *cb = SG_CODE_BUILDER(cl->code);
  int required = SG_PROCEDURE_REQUIRED(proc);
  int optargs = SG_PROCEDURE_OPTIONAL(proc);
  
  if (optargs) {
    int new_argc = adjust_optargs_tail(vm, proc, argc);
    if (new_argc < 0) return SG_UNDEF;
    argc = new_argc;
  } else {
    if (argc != required) {
      Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(proc),
                                         required, argc,
                                         Sg_ArrayToList(vm->sp - argc, argc));
      return SG_UNDEF;
    }
    shift_args_to_fp(vm, argc);
  }
  
  vm->cl = proc;
  vm->pc = cb->code;
  
  if (jit_verbose) {
    int i;
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: Yielding for closure tail call to %A (argc=%d, fp=%p, sp=%p)\n"),
              proc, argc, vm->fp, vm->sp);
    for (i = 0; i < argc && i < 5; i++) {
      Sg_Printf(Sg_StandardErrorPort(),
                UC("JIT:   FP[%d] = %p (%A)\n"),
                i, vm->fp[i], vm->fp[i]);
    }
  }
  
  return SG_JIT_YIELD_MARKER;
}

/* Unified procedure dispatch for non-tail calls.
 * Returns result for SUBR/Apply, or YIELD_MARKER for closure/other.
 */
static SgObject dispatch_call(SgVM *vm, int argc, SgObject proc)
{
  /* DEBUG: Check if proc is a yield marker */
  if ((uintptr_t)proc == (uintptr_t)SG_JIT_YIELD_MARKER ||
      (uintptr_t)proc == (uintptr_t)SG_JIT_YIELD_PRESERVE_AC) {
    fprintf(stderr, "DEBUG: dispatch_call called with YIELD MARKER as proc! proc=%p argc=%d\n",
            (void*)proc, argc);
    fflush(stderr);
  }

  /* Handle non-procedure callable objects (e.g., parameters) via object-apply.
   * Same as VM: transform (obj arg...) -> (object-apply obj arg...)
   */
  if (!SG_PROCEDUREP(proc)) {
    int i;
    /* Shift args up by one slot to make room for proc as first arg */
    for (i = 0; i < argc; i++) {
      *(vm->sp - i) = *(vm->sp - i - 1);
    }
    *(vm->sp - argc) = proc;
    vm->sp++;
    argc++;
    proc = SG_OBJ(&Sg_GenericObjectApply);
    /* Fall through to generic dispatch below */
    return Sg__JitCallGeneric(vm, argc, proc);
  }
  
  /* SUBR - call directly */
  if (SG_SUBRP(proc)) {
    return Sg__JitCallSubr(vm, argc, proc);
  }
  
  /* Generic function */
  if (SG_GENERICP(proc)) {
    return Sg__JitCallGeneric(vm, argc, proc);
  }
  
  /* Closure - yield to interpreter */
  if (SG_CLOSUREP(proc)) {
    return yield_for_closure(vm, proc, argc);
  }
  
  /* Other procedure types (NEXT_METHOD, METHOD, etc.):
   * Yield to interpreter with TAIL_CALL bytecode dispatch.
   *
   * This avoids the C continuation boundary that Sg_Apply creates.
   * The JIT's FRAME instruction already pushed a continuation frame,
   * so when the dispatched call completes and executes RET, it will
   * find the JIT continuation frame and re-enter JIT code.
   *
   * We use TAIL_CALL bytecode because:
   * 1. It dispatches based on procedure type (handles NEXT_METHOD, etc.)
   * 2. It doesn't push another continuation frame
   * 3. The callee's RET will pop the JIT's continuation frame
   */
  vm->fp = vm->sp - argc;
  vm->ac = proc;
  
  if (argc <= JIT_MAX_DIRECT_ARGC) {
    vm->pc = jit_tail_call_dispatch[argc];
  } else {
    /* For many arguments, error for now - rare case */
    Sg_Error(UC("JIT: too many arguments (%d) for other procedure type: %A"),
             argc, proc);
    return SG_UNDEF;
  }
  
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: Yielding for other proc type call to %A (type=%d argc=%d)\n"),
              proc, SG_PROCEDURE_TYPE(proc), argc);
  }
  
  return SG_JIT_YIELD_MARKER;
}

/* Unified procedure dispatch for tail calls.
 * Returns result for SUBR, or YIELD_MARKER for closure/other.
 */
static SgObject dispatch_tail_call(SgVM *vm, int argc, SgObject proc)
{
  /* Handle non-procedure callable objects (e.g., parameters) via object-apply.
   * Same as VM: transform (obj arg...) -> (object-apply obj arg...)
   */
  if (!SG_PROCEDUREP(proc)) {
    int i;
    /* Shift args up by one slot to make room for proc as first arg */
    for (i = 0; i < argc; i++) {
      *(vm->sp - i) = *(vm->sp - i - 1);
    }
    *(vm->sp - argc) = proc;
    vm->sp++;
    argc++;
    proc = SG_OBJ(&Sg_GenericObjectApply);
    /* Fall through to generic dispatch below */
    return Sg__JitTailCallGeneric(vm, argc, proc);
  }
  
  /* SUBR - call directly */
  if (SG_SUBRP(proc)) {
    return Sg__JitTailCallSubr(vm, argc, proc);
  }
  
  /* Generic function */
  if (SG_GENERICP(proc)) {
    return Sg__JitTailCallGeneric(vm, argc, proc);
  }
  
  /* Closure - yield to interpreter */
  if (SG_CLOSUREP(proc)) {
    return yield_for_closure_tail(vm, proc, argc);
  }
  
  /* Other procedure types (NEXT_METHOD, METHOD, etc.):
   * Yield to interpreter with TAIL_CALL bytecode dispatch.
   */
  shift_args_to_fp(vm, argc);
  vm->ac = proc;
  
  if (argc <= JIT_MAX_DIRECT_ARGC) {
    vm->pc = jit_tail_call_dispatch[argc];
  } else {
    /* For many arguments, error for now */
    Sg_Error(UC("JIT: too many arguments (%d) for other procedure type tail call: %A"),
             argc, proc);
    return SG_UNDEF;
  }
  
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: Yielding for tail call to %A (type=%d argc=%d)\n"),
              proc, SG_PROCEDURE_TYPE(proc), argc);
  }
  
  return SG_JIT_YIELD_MARKER;
}

/* Call a global procedure - called from JIT code for GREF_CALL */
SgObject Sg__JitGrefCall(SgVM *vm, int argc, SgObject id)
{
  uint64_t t_start = 0;

  if (jit_profile_enabled) {
    jit_call_count++;
    t_start = get_nanos();
  }

  SgObject proc = resolve_gref(vm, id);

  if (jit_profile_enabled) {
    jit_resolve_time_ns += (get_nanos() - t_start);
  }

  /* Check for unbound variable at runtime */
  if (SG_UNBOUNDP(proc)) {
    Sg_Error(UC("unbound variable: %A"), id);
  }

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: GREF_CALL proc=%A argc=%d sp=%p fp=%p cont=%p (in %A)\n"),
	      proc, argc, vm->sp, vm->fp, vm->cont, vm->cl);
  }

  return dispatch_call(vm, argc, proc);
}

/* Tail-call a global procedure - called from JIT code for GREF_TAIL_CALL */
SgObject Sg__JitGrefTailCall(SgVM *vm, int argc, SgObject id)
{
  SgObject proc = resolve_gref(vm, id);

  /* Check for unbound variable at runtime */
  if (SG_UNBOUNDP(proc)) {
    Sg_Error(UC("unbound variable: %A"), id);
  }

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: GREF_TAIL_CALL proc=%A argc=%d\n"), proc, argc);
  }

  return dispatch_tail_call(vm, argc, proc);
}

/* Call a procedure (already in AC) - called from JIT code for CALL */
SgObject Sg__JitCall(SgVM *vm, int argc, SgObject proc)
{
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: CALL proc=%A argc=%d sp=%p fp=%p cont=%p\n"),
	      proc, argc, vm->sp, vm->fp, vm->cont);
  }

  return dispatch_call(vm, argc, proc);
}

/* Tail-call a procedure (already in AC) - called from JIT code for TAIL_CALL */
SgObject Sg__JitTailCall(SgVM *vm, int argc, SgObject proc)
{
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: TAIL_CALL proc=%A argc=%d\n"), proc, argc);
  }

  return dispatch_tail_call(vm, argc, proc);
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
  long listLen;
  SgObject proc;
  int totalArgc;

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("JIT: APPLY nargc=%d listArg=%A isTail=%d sp=%p\n"),
	      nargc, listArg, isTail, vm->sp);
  }

  listLen = Sg_Length(listArg);

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

  /* Total args = explicit args + list args */
  totalArgc = nargc + (int)listLen;

  /* Pop proc from stack but keep explicit args */
  /* Stack currently: [... proc arg0 arg1 ... argN-1] */
  /* We need to remove proc and insert list args */

  /* First, shift explicit args down by one to remove proc slot */
  SgObject *args_start = vm->sp - nargc - 1;  /* Where proc was */
  for (int i = 0; i < nargc; i++) {
    args_start[i] = args_start[i + 1];
  }
  vm->sp--;  /* Adjust SP for removed proc */

  /* Now expand list args onto stack */
  SgObject rest = listArg;
  while (SG_PAIRP(rest)) {
    *(vm->sp++) = SG_CAR(rest);
    rest = SG_CDR(rest);
  }

  /* Now we have all args on stack, ready for dispatch */
  if (!SG_PROCEDUREP(proc)) {
    Sg_Error(UC("apply: procedure required, but got: %A"), proc);
    return SG_UNDEF;
  }

  /* For SUBR, call directly */
  if (SG_SUBRP(proc)) {
    if (isTail) {
      return Sg__JitTailCallSubr(vm, totalArgc, proc);
    } else {
      return Sg__JitCallSubr(vm, totalArgc, proc);
    }
  }

  /* For GENERIC, call directly */
  if (SG_GENERICP(proc)) {
    if (isTail) {
      return Sg__JitTailCallGeneric(vm, totalArgc, proc);
    } else {
      return Sg__JitCallGeneric(vm, totalArgc, proc);
    }
  }

  /* For CLOSURE, yield to interpreter */
  if (SG_CLOSUREP(proc)) {
    if (isTail) {
      return yield_for_closure_tail(vm, proc, totalArgc);
    } else {
      return yield_for_closure(vm, proc, totalArgc);
    }
  }

  /* Other procedure types: yield with TAIL_CALL bytecode */
  vm->fp = vm->sp - totalArgc;
  vm->ac = proc;
  
  if (totalArgc <= JIT_MAX_DIRECT_ARGC) {
    vm->pc = jit_tail_call_dispatch[totalArgc];
  } else {
    /* For many arguments, error for now */
    Sg_Error(UC("JIT APPLY: too many arguments (%d) for other procedure type: %A"),
             totalArgc, proc);
    return SG_UNDEF;
  }
  
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: APPLY yielding for other proc type %A (type=%d argc=%d)\n"),
              proc, SG_PROCEDURE_TYPE(proc), totalArgc);
  }
  
  return SG_JIT_YIELD_MARKER;
}

/*
 * LIST helper - called from JIT code for LIST instruction
 *
 * n: number of elements in the list (from LIST instruction operand)
 * lastVal: the last value (AC)
 *
 * Stack has n-1 elements to pop.
 * Builds list: starts with (lastVal), then prepends stack elements.
 *
 * For LIST(2) with stack=[import] and AC=lib:
 *   ret = Cons(lib, NIL) = (lib)
 *   ret = Cons(import, (lib)) = (import lib)
 *   SP -= 1
 *
 * Returns the constructed list (to be stored in AC).
 */
SgObject Sg__JitList(SgVM *vm, int n, SgObject lastVal)
{
  int numFromStack = n - 1;
  SgObject ret = SG_NIL;
  int i;

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: LIST n=%d lastVal=%p (%A) sp=%p vm->sp=%p\n"),
              n, lastVal, lastVal, vm->sp, vm->sp);
    for (i = 0; i < numFromStack; i++) {
      Sg_Printf(Sg_StandardErrorPort(),
                UC("JIT: LIST stack[%d]=%p (%A)\n"),
                i, *((vm->sp) - i - 1), *((vm->sp) - i - 1));
    }
  }

  if (n > 0) {
    /* Start with AC as the last element */
    ret = Sg_Cons(lastVal, SG_NIL);
    
    /* Prepend stack elements (reading from SP[0] up)
     * INDEX(sp, i) = *((sp) - (i) - 1)
     * SP[0] is at *(sp-1), SP[1] is at *(sp-2), etc.
     */
    for (i = 0; i < numFromStack; i++) {
      ret = Sg_Cons(*((vm->sp) - i - 1), ret);
    }
    
    /* Pop the n-1 elements from stack */
    vm->sp -= numFromStack;
  }

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: LIST result=%p (%A)\n"), ret, ret);
  }

  return ret;
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

/*
 * Helper: Adjust argument frame for optional arguments
 *
 * This implements the same logic as ADJUST_ARGUMENT_FRAME from vmcall.c:
 * - Check required argument count
 * - Fold rest args into a list for optional arguments
 *
 * Returns: new argc after adjustment, or -1 on error
 */
static int adjust_args(SgVM *vm, int argc, SgObject proc)
{
  int required = SG_PROCEDURE_REQUIRED(proc);
  int optargs = SG_PROCEDURE_OPTIONAL(proc);

  if (optargs) {
    if (argc < required) {
      Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(proc),
                                          required, argc, SG_FALSE);
      return -1;
    }
    /* Fold rest args into a list */
    SgObject p = SG_NIL;
    while (argc > required + optargs - 1) {
      SgObject a = *(--vm->sp);
      p = Sg_Cons(a, p);
      argc--;
    }
    *(vm->sp++) = p;
    argc++;
  } else {
    if (argc != required) {
      Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(proc),
                                          required, argc, SG_FALSE);
      return -1;
    }
  }
  return argc;
}

/*
 * Call a SUBR directly from JIT code
 *
 * Most SUBRs don't yield - they execute and return immediately.
 * Since FRAME pushed a continuation frame before CALL, we need to
 * pop it after the SUBR returns.
 *
 * EXCEPTION: Some SUBRs (like `eval`) set up VM continuation state
 * via vm_new_cont/Sg_VMApply. These SUBRs modify vm->pc to point to
 * bytecode that should be executed by the VM loop. For these SUBRs,
 * we must yield to interpreter instead of returning directly.
 *
 * argc: number of arguments on stack (vm->sp[-argc] to vm->sp[-1])
 * proc: the SUBR procedure to call
 */
SgObject Sg__JitCallSubr(SgVM *vm, int argc, SgObject proc)
{
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: SUBR CALL proc=%A argc=%d sp=%p fp=%p cont=%p\n"),
              proc, argc, vm->sp, vm->fp, vm->cont);
    /* Debug: print arguments */
    for (int i = 0; i < argc; i++) {
      Sg_Printf(Sg_StandardErrorPort(),
                UC("JIT:   arg[%d] = %p (raw)\n"),
                i, (void*)vm->sp[-(argc - i)]);
    }
  }

  /* Adjust arguments for optional args */
  argc = adjust_args(vm, argc, proc);
  if (argc < 0) return SG_UNDEF;

  /* Save continuation info BEFORE the call */
  SgContFrame *cont = vm->cont;
  SgObject *saved_fp = cont->fp;
  SgObject saved_cl = cont->cl;
  SgContFrame *prev_cont = cont->prev;
  
  /* Save vm->pc to detect if SUBR set up VM continuation */
  SgWord *saved_pc = vm->pc;

  /* Set up frame pointer - SUBR expects args at FP[0], FP[1], ... */
  vm->fp = vm->sp - argc;
  vm->cl = proc;

  /* Direct call */
  SgObject result = SG_SUBR_FUNC(proc)(vm->fp, argc, SG_SUBR_DATA(proc));

  /* Check if SUBR set up VM continuation (changed vm->pc).
   * SUBRs like `eval` use Sg_VMApply which sets vm->pc to bytecode
   * that should be executed. In this case, we must yield to interpreter
   * so the VM loop can execute the continuation properly.
   */
  if (vm->pc != saved_pc) {
    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
                UC("JIT: SUBR CALL %A set up continuation, yielding to interpreter\n"),
                proc);
      Sg_Printf(Sg_StandardErrorPort(),
                UC("JIT:   saved_pc=%p new_pc=%p result=%A\n"),
                saved_pc, vm->pc, result);
    }
    /* DON'T pop JIT's continuation frame - interpreter will handle it.
     * The SUBR's return value is typically a procedure to be dispatched.
     * Set AC to this procedure and let interpreter run vm->pc.
     * Use YIELD_PRESERVE_AC so vmcall.c doesn't overwrite AC.
     */
    vm->ac = result;
    return SG_JIT_YIELD_PRESERVE_AC;
  }

  /* Pop the continuation frame:
   * - Restore SP to before the FRAME instruction (fp + size from cont)
   * - Restore FP, CL, and cont from the saved frame
   * Also pop the continuation marks associated with this frame.
   */
  Sg_JitPopContMarks(vm, cont);
  vm->sp = (SgObject *)cont;  /* SP to start of cont frame (before FRAME) */
  vm->fp = saved_fp;
  vm->cl = saved_cl;
  vm->cont = prev_cont;

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: SUBR CALL done: sp=%p fp=%p cl=%A result=%A\n"),
              vm->sp, vm->fp, vm->cl, result);
  }

  return result;
}

/*
 * Tail call a SUBR directly
 *
 * For tail calls, we don't have a continuation frame to pop.
 * We just shift args to FP and call directly.
 *
 * EXCEPTION: Some SUBRs (like `eval`) set up VM continuation state.
 * For these, we must yield to interpreter.
 */
SgObject Sg__JitTailCallSubr(SgVM *vm, int argc, SgObject proc)
{
  SgObject *saved_fp = vm->fp; /* Save for JIT register reload */

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: SUBR TAIL_CALL proc=%A argc=%d sp=%p fp=%p\n"),
              proc, argc, vm->sp, vm->fp);
  }

  /* Adjust arguments for optional args */
  argc = adjust_args(vm, argc, proc);
  if (argc < 0) return SG_UNDEF;

  /* Save vm->pc to detect if SUBR set up VM continuation */
  SgWord *saved_pc = vm->pc;

  /* For tail call, set up FP for args */
  vm->fp = vm->sp - argc;
  vm->cl = proc;

  /* Direct call - SUBR tail call returns result directly */
  SgObject result = SG_SUBR_FUNC(proc)(vm->fp, argc, SG_SUBR_DATA(proc));

  /* Check if SUBR set up VM continuation (changed vm->pc).
   * SUBRs like `eval` use Sg_VMApply which sets vm->pc to bytecode
   * that should be executed. In this case, we must yield to interpreter.
   */
  if (vm->pc != saved_pc) {
    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
                UC("JIT: SUBR TAIL_CALL %A set up continuation, yielding to interpreter\n"),
                proc);
      Sg_Printf(Sg_StandardErrorPort(),
                UC("JIT:   saved_pc=%p new_pc=%p result=%A\n"),
                saved_pc, vm->pc, result);
    }
    /* The SUBR's return value is typically a procedure to be dispatched.
     * Set AC to this procedure and let interpreter run vm->pc.
     * DO NOT restore FP - the SUBR has set up a continuation with its own
     * stack layout. Restoring saved_fp would corrupt the continuation.
     * Use YIELD_PRESERVE_AC so vmcall.c doesn't overwrite AC.
     */
    vm->ac = result;
    /* NOTE: Don't restore vm->fp here! The SUBR (e.g., eval via vm_new_cont)
     * has already set up FP/SP for its continuation frame. */
    return SG_JIT_YIELD_PRESERVE_AC;
  }

  /* Normal return (no continuation setup): restore FP for JIT register reload */
  vm->fp = saved_fp;

  return result;
}

/*
 * Helper to shift args and add next-method
 *
 * Before: SP points after args [arg0, arg1, ..., argN-1]
 * After:  SP points after [arg0, arg1, ..., argN-1, next-method]
 */
static void shift_and_add_next_method(SgVM *vm, int argc, SgObject nm)
{
  /* Make room for next-method by shifting args */
  SgObject *src = vm->sp - argc;
  SgObject *dst = src + 1;
  
  /* Shift args forward by one slot */
  for (int i = argc - 1; i >= 0; i--) {
    dst[i] = src[i];
  }
  
  /* Insert next-method at the beginning (first arg position) */
  src[0] = nm;
  vm->sp++;
}

/*
 * Call a generic function
 *
 * This implements the same logic as SG_PROC_GENERIC in vmcall.c:
 * 1. Compute applicable methods
 * 2. Create next-method (unless leaf)
 * 3. Dispatch to first method
 *
 * For SUBR methods and fallback, we pop the continuation frame since
 * there's no RET instruction. For closure methods, we yield to interpreter
 * and let RET pop the frame.
 */
SgObject Sg__JitCallGeneric(SgVM *vm, int argc, SgObject generic)
{
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: GENERIC CALL generic=%A argc=%d\n"),
              generic, argc);
  }

  /* Save continuation info BEFORE the call */
  SgContFrame *cont = vm->cont;
  SgObject *saved_fp = cont->fp;
  SgObject saved_cl = cont->cl;
  SgContFrame *prev_cont = cont->prev;

  /* Compute applicable methods */
  SgObject methods = Sg_ComputeMethods(generic, vm->sp - argc, argc, FALSE);

  if (SG_NULLP(methods)) {
    /* No applicable methods - call fallback directly */
    vm->fp = vm->sp - argc;
    SgObject result = SG_GENERIC(generic)->fallback(vm->fp, argc, SG_GENERIC(generic));
    
    /* Pop the continuation frame and marks */
    Sg_JitPopContMarks(vm, cont);
    vm->sp = (SgObject *)cont;
    vm->fp = saved_fp;
    vm->cl = saved_cl;
    vm->cont = prev_cont;
    return result;
  }

  SgObject method = SG_CAR(methods);
  SgObject nm;

  /* Create next-method (unless leaf method) */
  if (SG_METHOD_LEAF_P(method)) {
    nm = SG_TRUE;  /* dummy - won't be used */
  } else {
    nm = Sg_MakeNextMethod(SG_GENERIC(generic), SG_CDR(methods),
                           vm->sp - argc, argc, TRUE);
  }

  SgObject proc = SG_METHOD_PROCEDURE(method);

  if (SG_SUBRP(proc)) {
    /* C-defined method - call directly */
    argc = adjust_args(vm, argc, method);
    if (argc < 0) return SG_UNDEF;

    vm->fp = vm->sp - argc;
    vm->cl = proc;

    SgObject result = SG_SUBR_FUNC(proc)(vm->fp, argc, SG_SUBR_DATA(proc));

    /* Pop the continuation frame and marks */
    Sg_JitPopContMarks(vm, cont);
    vm->sp = (SgObject *)cont;
    vm->fp = saved_fp;
    vm->cl = saved_cl;
    vm->cont = prev_cont;
    return result;
  } else {
    /* Closure method - yield to interpreter.
     *
     * Whether or not the closure has JIT code, we yield to interpreter.
     * This ensures the callee's RET instruction properly pops the continuation
     * frame that was pushed by the caller's FRAME instruction.
     */
    SgClosure *cls = SG_CLOSURE(proc);
    SgCodeBuilder *cb = SG_CODE_BUILDER(cls->code);

    /* Add next-method as extra argument */
    shift_and_add_next_method(vm, argc, nm);
    argc++;

    /* Argument adjustment for closure */
    argc = adjust_args(vm, argc, SG_OBJ(cls));
    if (argc < 0) return SG_UNDEF;

    /* Set up VM state for the method closure */
    vm->fp = vm->sp - argc;
    vm->cl = SG_OBJ(cls);
    vm->pc = cb->code;

    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
                UC("JIT: Generic yielding to interpreter for call to %A (jit=%A)\n"),
                SG_OBJ(cls), cb->jitCode ? SG_TRUE : SG_FALSE);
    }

    return SG_JIT_YIELD_MARKER;
  }
}

/*
 * Tail-call a generic function
 */
SgObject Sg__JitTailCallGeneric(SgVM *vm, int argc, SgObject generic)
{
  SgObject *saved_fp = vm->fp; /* Save for JIT register reload */

  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: GENERIC TAIL_CALL generic=%A argc=%d\n"),
              generic, argc);
  }

  /* Compute applicable methods */
  SgObject methods = Sg_ComputeMethods(generic, vm->sp - argc, argc, FALSE);

  if (SG_NULLP(methods)) {
    /* No applicable methods - call fallback */
    vm->fp = vm->sp - argc;
    SgObject result = SG_GENERIC(generic)->fallback(vm->fp, argc, SG_GENERIC(generic));
    vm->fp = saved_fp;  /* Restore for JIT register reload */
    return result;
  }

  SgObject method = SG_CAR(methods);
  SgObject nm;

  /* Create next-method (unless leaf method) */
  if (SG_METHOD_LEAF_P(method)) {
    nm = SG_TRUE;
  } else {
    nm = Sg_MakeNextMethod(SG_GENERIC(generic), SG_CDR(methods),
                           vm->sp - argc, argc, TRUE);
  }

  SgObject proc = SG_METHOD_PROCEDURE(method);

  if (SG_SUBRP(proc)) {
    /* C-defined method - call directly */
    argc = adjust_args(vm, argc, method);
    if (argc < 0) return SG_UNDEF;

    vm->fp = vm->sp - argc;
    vm->cl = proc;

    SgObject result = SG_SUBR_FUNC(proc)(vm->fp, argc, SG_SUBR_DATA(proc));
    vm->fp = saved_fp;  /* Restore for JIT register reload */
    return result;
  } else {
    /* Closure method - yield to interpreter for tail call.
     *
     * For tail calls, we yield to interpreter regardless of JIT status.
     * This ensures consistent behavior with non-tail calls.
     * 
     * Note: Tail calls don't push a continuation frame, so there's
     * no continuation to pop. The callee will reuse the caller's frame.
     */
    SgClosure *cls = SG_CLOSURE(proc);
    SgCodeBuilder *cb = SG_CODE_BUILDER(cls->code);

    /* Add next-method */
    shift_and_add_next_method(vm, argc, nm);
    argc++;

    /* Argument adjustment */
    argc = adjust_args(vm, argc, SG_OBJ(cls));
    if (argc < 0) return SG_UNDEF;

    /* Set up VM state for tail call */
    vm->fp = vm->sp - argc;
    vm->cl = SG_OBJ(cls);
    vm->pc = cb->code;

    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
                UC("JIT: Generic yielding to interpreter for tail call to %A (jit=%A)\n"),
                SG_OBJ(cls), cb->jitCode ? SG_TRUE : SG_FALSE);
    }

    return SG_JIT_YIELD_MARKER;
  }
}

/* DEBUG: Scan object for VM pointer contamination */
static int jit_scan_for_vm(SgObject obj, SgVM *vm, int depth)
{
  if (depth > 20) return 0;
  if (!SG_PTRP(obj)) return 0;
  if (obj == (SgObject)vm) return 1;
  if (SG_PAIRP(obj)) {
    if (jit_scan_for_vm(SG_CAR(obj), vm, depth + 1)) return 1;
    if (jit_scan_for_vm(SG_CDR(obj), vm, depth + 1)) return 1;
  } else if (SG_VECTORP(obj)) {
    long i, len = SG_VECTOR_SIZE(obj);
    for (i = 0; i < len && i < 100; i++) {
      if (jit_scan_for_vm(SG_VECTOR_ELEMENT(obj, i), vm, depth + 1)) return 1;
    }
  }
  return 0;
}

static void jit_check_src_integrity_before(SgCodeBuilder *cb)
{
  SgVM *vm = Sg_VM();
  if (!SG_FALSEP(cb->src) && jit_scan_for_vm(cb->src, vm, 0)) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT-DEBUG: VM pointer found in cb->src BEFORE compilation!\n"));
    Sg_Printf(Sg_StandardErrorPort(),
              UC("  cb=%p, cb->name=%A, vm=%p\n"),
              cb, cb->name, vm);
    Sg_FlushPort(Sg_StandardErrorPort());
  }
}

static void jit_check_src_integrity_after(SgCodeBuilder *cb)
{
  SgVM *vm = Sg_VM();
  if (!SG_FALSEP(cb->src) && jit_scan_for_vm(cb->src, vm, 0)) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT-DEBUG: VM pointer found in cb->src AFTER compilation!\n"));
    Sg_Printf(Sg_StandardErrorPort(),
              UC("  cb=%p, cb->name=%A, vm=%p\n"),
              cb, cb->name, vm);
    Sg_FlushPort(Sg_StandardErrorPort());
  }
}

SgJitCompiledCode Sg_JitCompile(SgCodeBuilder *cb)
{
  SgJitContext ctx;
  SgJitCompiledCode compiled;
  int pc;

  if (!Sg_JitEnabled()) {
    return NULL;
  }

  /* DEBUG: Check cb->src integrity BEFORE compilation */
  jit_check_src_integrity_before(cb);

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
  /* One more label for normal epilogue (stores vm->cl) */
  ctx.epilogueLabel = ctx.labelCount++;
  /* One more label for yield epilogue (does NOT store vm->cl) */
  ctx.yieldEpilogueLabel = ctx.labelCount++;
  /* One more label for bare yield epilogue (doesn't store AC either) */
  ctx.bareYieldEpilogueLabel = ctx.labelCount++;

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
	id = resolve_identifier_at_compile_time(cb, pc, id);
	if (!Sg__JitEmit_GREF(&ctx, id)) goto fail;
	pc += 2;
      }
      break;

    case GREF_PUSH:
      {
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);
	id = resolve_identifier_at_compile_time(cb, pc, id);
	if (!Sg__JitEmit_GREF_PUSH(&ctx, id)) goto fail;
	pc += 2;
      }
      break;

    case FREF_PUSH:
      if (jit_verbose) {
        Sg_Printf(Sg_StandardErrorPort(),
                  UC("JIT COMPILE: FREF_PUSH index=%d\n"), val1);
        Sg_FlushPort(Sg_StandardErrorPort());
      }
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
	id = resolve_identifier_at_compile_time(cb, pc, id);
	if (!Sg__JitEmit_GREF_CAR(&ctx, id)) goto fail;
	pc += 2;
      }
      break;

    case GREF_CDR:
      {
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);
	id = resolve_identifier_at_compile_time(cb, pc, id);
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
	id = resolve_identifier_at_compile_time(cb, pc, id);
	if (!Sg__JitEmit_GREF_CAR_PUSH(&ctx, id)) goto fail;
	pc += 2;
      }
      break;

    case GREF_CDR_PUSH:
      {
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);
	id = resolve_identifier_at_compile_time(cb, pc, id);
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
        if (jit_verbose) {
          Sg_Printf(Sg_StandardErrorPort(),
                    UC("JIT COMPILE: CLOSURE instruction found, freec=%d innerCb=%A\n"),
                    freec, innerCb);
          Sg_FlushPort(Sg_StandardErrorPort());
        }
        if (!Sg__JitEmit_CLOSURE(&ctx, val1, innerCb, freec)) goto fail;
        pc += 2;
      }
      break;

    case GREF_CALL:
      {
	/* GREF_CALL: val1 = argc, next word = identifier/gloc */
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);

	/* Resolve identifier to GLOC at compile time */
	id = resolve_identifier_at_compile_time(cb, pc, id);

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
	/* GREF_TAIL_CALL: val1 = argc, next word = identifier/gloc */
	if (pc + 1 >= cb->size) goto fail;
	SgObject id = SG_OBJ(cb->code[pc + 1]);

	/* Resolve identifier to GLOC at compile time */
	id = resolve_identifier_at_compile_time(cb, pc, id);

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
      /* APPLY instruction is currently buggy in JIT - fall back to VM */
      goto fail;

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
	      UC("JIT: All instructions processed, emitting epilogues\n"));
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: epilogueLabel=%d, code offset before bind=%d bytes\n"),
              ctx.epilogueLabel, (int)Sg__JitGetCodeSize(&ctx));
  }

  /* Bind normal epilogue label and emit epilogue (stores vm->cl) */
  Sg__JitBindLabel(&ctx, ctx.epilogueLabel);
  
  if (jit_verbose) {
    Sg_Printf(Sg_StandardErrorPort(),
              UC("JIT: After binding epilogueLabel, code offset=%d bytes\n"),
              (int)Sg__JitGetCodeSize(&ctx));
  }

  if (!Sg__JitEmit_Epilogue(&ctx)) {
    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
		UC("JIT: Epilogue failed\n"));
    }
    goto fail;
  }

  /* Bind yield epilogue label and emit yield epilogue (does NOT store vm->cl) */
  Sg__JitBindLabel(&ctx, ctx.yieldEpilogueLabel);
  if (!Sg__JitEmit_YieldEpilogue(&ctx)) {
    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
		UC("JIT: Yield epilogue failed\n"));
    }
    goto fail;
  }

  /* Bind bare yield epilogue label and emit bare yield epilogue
   * (doesn't store AC - used when SUBR set up continuation with AC value) */
  Sg__JitBindLabel(&ctx, ctx.bareYieldEpilogueLabel);
  if (!Sg__JitEmit_BareYieldEpilogue(&ctx)) {
    if (jit_verbose) {
      Sg_Printf(Sg_StandardErrorPort(),
		UC("JIT: Bare yield epilogue failed\n"));
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

  /* DEBUG: Check cb->src integrity AFTER compilation */
  jit_check_src_integrity_after(cb);

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
