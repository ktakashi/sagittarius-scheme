/* -*- C -*- */
/*
 * vm.c
 *
 *   Copyright (c) 2010  Takashi Kato <ktakashi@ymail.com>
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
 *
 *  $Id: $
 */
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/vm.h"
#include "sagittarius/code.h"
#include "sagittarius/core.h"
#include "sagittarius/closure.h"
#include "sagittarius/error.h"
#include "sagittarius/file.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/identifier.h"
#include "sagittarius/library.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/reader.h"
#include "sagittarius/string.h"
#include "sagittarius/symbol.h"
#include "sagittarius/instruction.h"
#include "sagittarius/writer.h"
#include "sagittarius/number.h"
#include "sagittarius/macro.h"
#include "sagittarius/values.h"
#include "sagittarius/vector.h"
#include "sagittarius/compare.h"
#include "sagittarius/system.h"
#include "sagittarius/exceptions.h"
#include "sagittarius/profiler.h"
#include "sagittarius/gloc.h"

static SgVM *rootVM = NULL;
/* TODO multi thread */
static SgVM *theVM;

static SgSubr default_exception_handler_rec;
#define DEFAULT_EXCEPTION_HANDLER SG_OBJ(&default_exception_handler_rec)

static inline SgObject make_box(SgObject value)
{
  SgBox *b = SG_NEW(SgBox);
  SG_SET_HEADER(b, TC_BOX);
  b->value = value;
  return SG_OBJ(b);
}

static SgObject evaluate_safe(SgObject program, SgWord *compiledCode);
static SgObject run_loop();

/* TODO if prototype was given copy from it. */
SgVM* Sg_NewVM(SgVM *proto, SgObject name)
{
  SgVM *v = SG_NEW(SgVM);
  int i;
  SG_SET_HEADER(v, TC_VM);

  v->flags = 0;
  v->stack = SG_NEW_ARRAY(SgObject, SG_VM_STACK_SIZE);
  v->sp = v->fp = v->stack;
  v->stackEnd = v->stack + SG_VM_STACK_SIZE;
  v->cont = v->sp;
  v->ac = SG_NIL;
  v->cl = v->dc = NULL;

  v->attentionRequest = FALSE;
  v->finalizerPending = FALSE;
  v->escapePoint = NULL;
  v->escapeReason = SG_VM_ESCAPE_NONE;
  v->escapeData[0] = NULL;
  v->escapeData[1] = NULL;

  v->dynamicWinders = SG_NIL;
  v->parentExHandler = SG_FALSE;
  v->exceptionHandler = DEFAULT_EXCEPTION_HANDLER;
  v->toplevelVariables = SG_NIL;
  v->commandLineArgs = SG_NIL;

  v->currentInputPort = Sg_MakeTranscodedInputPort(Sg_StandardInputPort(),
						    Sg_IsUTF16Console(Sg_StandardIn()) ? Sg_MakeNativeConsoleTranscoder()
						                                       : Sg_MakeNativeTranscoder());
  v->currentOutputPort = Sg_MakeTranscodedOutputPort(Sg_StandardOutputPort(),
						     Sg_IsUTF16Console(Sg_StandardOut()) ? Sg_MakeNativeConsoleTranscoder()
						                                         : Sg_MakeNativeTranscoder());
  v->currentErrorPort = Sg_MakeTranscodedOutputPort(Sg_StandardErrorPort(),
						     Sg_IsUTF16Console(Sg_StandardError()) ? Sg_MakeNativeConsoleTranscoder()
						                                           : Sg_MakeNativeTranscoder());
  v->logPort = v->currentErrorPort;
  /* TODO thread, mutex, etc */
  return v;
}

/*
  Current VM.
 */
SgVM* Sg_VM()
{
  return theVM;
}

#define Sg_VM() theVM


#define SAVE_REGS(vm, r)			\
  do {						\
    (r)->ac = (vm)->ac;				\
    (r)->dc = (vm)->dc;				\
    (r)->cl = (vm)->cl;				\
    (r)->pc = (vm)->pc;				\
    (r)->spOffset = (vm)->sp - (vm)->stack;	\
    (r)->fpOffset = (vm)->fp - (vm)->stack;	\
    (r)->cont = (vm)->cont;			\
  } while (0)

#define RESTORE_REGS(vm, r)			\
  do {						\
    (vm)->ac = (r)->ac;				\
    (vm)->dc = (r)->dc;				\
    (vm)->cl = (r)->cl;				\
    (vm)->pc = (r)->pc;				\
    (vm)->sp = (vm)->stack + (r)->spOffset;	\
    (vm)->fp = (vm)->stack + (r)->fpOffset;	\
    (vm)->cont = (r)->cont;			\
  } while (0)

static inline void report_error(SgObject exception)
{
  static const int MAX_STACK_TRACE = 20;
  SgObject error = SG_NIL, stackTrace = SG_NIL;
  SgObject cur;
  if (SG_PAIRP(exception)) {
    error = SG_CAR(exception);
    stackTrace = SG_CDR(exception);
  } else {
    error = exception;
    stackTrace = Sg_GetStackTrace();
  }
  Sg_Printf(SG_PORT(Sg_StandardErrorPort()),
	    UC("*error*\n"
	       "%A\n"
	       "stack trace:\n"), Sg_DescribeCondition(error));
  stackTrace = Sg_ReverseX(stackTrace);
  SG_FOR_EACH(cur, stackTrace) {
    SgObject obj, index, proc,
      tmp, src, file, info, line;
    obj = SG_CAR(cur);
    index = SG_CAR(obj);
    if (SG_INT_VALUE(index) > MAX_STACK_TRACE) {
      Sg_Printf(SG_PORT(Sg_StandardErrorPort()),
		UC("      ... (more stack dump truncated)\n"));
      break;
    }

    proc = SG_CDR(obj);		/* (proc name src) */
    tmp = SG_CAR(SG_CDDR(proc));
    if (!SG_PAIRP(tmp)) {
      src = SG_INTERN("*unknown*");
      info = SG_FALSE;
    } else {
      src = Sg_LastPair(tmp);
      src = SG_CDAR(src);
      info = SG_SOURCE_INFO(src);
    }
    if (SG_FALSEP(info) || !info) {
      line = SG_INTERN("*unknown*");
      file = SG_INTERN("*unknown*");
    } else {
      file = SG_CAR(info);
      line = SG_CDR(info);
    }
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("  [%A] %A: %A (location %S (line %A))\n"
		 "    src: %#50S\n"),
	      index, SG_CAR(proc), SG_CADR(proc),
	      file, line,
	      Sg_UnwrapSyntax(src));
  }
  Sg_FlushAllPort(FALSE);
}

SgGloc* Sg_FindBinding(SgObject library, SgObject name, SgObject callback)
{
  SgLibrary *lib;
  SgObject ret;
  ASSERT(SG_SYMBOLP(name));
  if (SG_LIBRARYP(library)) lib = SG_LIBRARY(library);
  else lib = Sg_FindLibrary(library, FALSE);
  if (SG_FALSEP(lib)) return callback;

  ret = Sg_HashTableRef(SG_LIBRARY_TABLE(lib), name, SG_UNBOUND);
  if (SG_UNBOUNDP(ret)) {
    /* search from toplevel */
    SgObject gloc = Sg_Assq(name, Sg_VM()->toplevelVariables);
    if (SG_FALSEP(gloc)) return callback;
    ASSERT(SG_PAIRP(gloc));
    ASSERT(SG_GLOCP(SG_CDR(gloc)));
    return SG_CDR(gloc);
  }
  return ret;
}
void Sg_InsertBinding(SgLibrary *library, SgObject name, SgObject value_or_gloc)
{
  SgObject value;
  if (SG_GLOCP(value_or_gloc)) {
    value = SG_GLOC_GET(SG_GLOC(value_or_gloc));
  } else {
    value = value_or_gloc;
  }
  if (SG_SYMBOLP(name)) {
    Sg_MakeBinding(library, name, value, 0);
  } else if (SG_IDENTIFIERP(name)) {
    Sg_MakeBinding(library, SG_IDENTIFIER_NAME(name), value, 0);
  } else {
    Sg_Error(UC("symbol or identifier required, but got %S"), name);
  }
}

void Sg_VMSetToplevelVariable(SgSymbol *name, SgObject value)
{
  SgVM *vm = Sg_VM();
  SgGloc *g = Sg_MakeGloc(name, vm->currentLibrary);
  SG_GLOC_SET(g, value);
  vm->toplevelVariables = Sg_Acons(name, g, vm->toplevelVariables);
}

static void vm_dump_code_rec(SgCodeBuilder *cb, int indent)
{
  SgVM *vm = Sg_VM();
  int i, size = cb->size, ind;
  InsnInfo *info;
  SgWord *code = cb->code;

  for (ind = 0; ind < indent; ind++) {
    Sg_Write(SG_MAKE_CHAR(' '), vm->logPort, SG_WRITE_DISPLAY);
  }
  Sg_Printf(vm->logPort, UC("size: %d\n"), size);
  for (i = 0; i < size;) {
    info = Sg_LookupInsnName(INSN(code[i]));
    for (ind = 0; ind < indent; ind++) {
      Sg_Write(SG_MAKE_CHAR(' '), vm->logPort, SG_WRITE_DISPLAY);
    }
    Sg_Printf(vm->logPort, UC("%A"), Sg_MakeStringC(info->name));
    if (info->instValues != 0) {
      int val1, val2;
      Sg_Printf(vm->logPort, UC("("));
      switch (info->instValues) {
      case 1:
	INSN_VAL1(val1, code[i]);
	Sg_Printf(vm->logPort, UC("%d"), val1);
	break;
      case 2:
	INSN_VAL2(val1, val2, code[i]);
	Sg_Printf(vm->logPort, UC("%d %d"), val1, val2);
      }
      Sg_Printf(vm->logPort, UC(")"));
    }
    if (info->argc != 0) {
      /* for now insn argument is only one */
      SgObject arg = SG_OBJ(code[i + 1]);
      if (SG_CODE_BUILDERP(arg)) {
	Sg_Printf(vm->logPort, UC(" %S\n"), arg);
	vm_dump_code_rec(SG_CODE_BUILDER(arg), indent + 2);
      } else {
	Sg_Printf(vm->logPort, UC(" %#S"), arg);
      }
    }
    if (info->hasSrc) {
      if (SG_PAIRP(cb->src)) {
	SgObject src = Sg_Assv(SG_MAKE_INT(i), cb->src);
	if (SG_FALSEP(src)) {
	  Sg_Printf(vm->logPort, UC(" ;; #f"));
	} else {
	  Sg_Printf(vm->logPort, UC(" ;; %#20S"), SG_CDR(src));
	}
      }
    }
    Sg_Printf(vm->logPort, UC("\n"));
    i += 1 + info->argc;
  }
}

void Sg_VMDumpCode(SgCodeBuilder *cb)
{
  vm_dump_code_rec(cb, 0);
}

SgObject Sg_CurrentOutputPort()
{
  SgVM *vm = Sg_VM();
  return vm->currentOutputPort;
}

SgObject Sg_CurrentErrorPort()
{
  SgVM *vm = Sg_VM();
  return vm->currentErrorPort;
}


SgObject Sg_CurrentInputPort()
{
  SgVM *vm = Sg_VM();
  return vm->currentInputPort;
}

SgObject Sg_VMCurrentLibrary()
{
  return Sg_VM()->currentLibrary;
}

/* compiler */
SgObject Sg_Compile(SgObject o, SgObject e)
{
  static SgObject compiler = SG_UNDEF;
  SgObject compiled;
  /* compiler is initialized after VM. so we need to look it up first */
  /* TODO lock */
  if (SG_UNDEFP(compiler)) {
    SgObject compile_library = Sg_FindLibrary(SG_INTERN("(sagittarius compiler)"), FALSE);
    SgGloc *g = Sg_FindBinding(compile_library, SG_INTERN("compile"), SG_FALSE);
    compiler = SG_GLOC_GET(g);
  }
  return Sg_Apply2(compiler, o, e);
}

/* 
   env: library for now.
   vm-current-library will be given env and after eval, we restore it.
 */
SgObject Sg_VMEval(SgObject sexp, SgObject env)
{
  SgObject v = SG_NIL;
  SgVM *vm = theVM;

  v = Sg_Compile(sexp, env);

  ASSERT(SG_CODE_BUILDERP(v));
  /* TODO replace and restore current library*/
  SG_CLOSURE(vm->closureForEvaluate)->code = v;
  vm->cl = vm->closureForEvaluate;
  vm->dc = vm->closureForEvaluate;
  vm->pc = SG_CODE_BUILDER(v)->code;
  return SG_UNDEF;
}

/* 
   env: library for now.
 */
SgObject Sg_Eval(SgObject sexp, SgObject env)
{
  SgObject v = SG_NIL;
  SgVM *vm = theVM;

  vm->state = COMPILING;
  v = Sg_Compile(sexp, env);
  vm->state = RUNNING;

  ASSERT(SG_CODE_BUILDERP(v));
  /* TODO replace and restore current library*/
  SG_CLOSURE(vm->closureForEvaluate)->code = v;
  return evaluate_safe(vm->closureForEvaluate, SG_CODE_BUILDER(v)->code);
}

static SgObject pass1_import = SG_UNDEF;

SgObject Sg_Environment(SgObject lib, SgObject spec)
{
  SgObject cp;
  if (SG_UNDEFP(pass1_import)) {
    /* TODO lock */
    SgLibrary *complib = Sg_FindLibrary(SG_INTERN("(sagittarius compiler)"), FALSE);
    SgGloc *g = Sg_FindBinding(complib, SG_INTERN("pass1/import"), SG_UNBOUND);
    if (SG_UNBOUNDP(g)) {
      /* something wrong */
      Sg_Panic("pass1/import was not found. loading error?");
    }
    pass1_import = SG_GLOC_GET(g);
  }
  /* make spec look like import-spec */
  spec = Sg_Cons(SG_INTERN("import"), spec);
  Sg_ApplySafe(pass1_import, SG_LIST2(spec, lib));
  return lib;
}

/* TODO check stack expantion */
#define CHECK_STACK(size, vm)	/* dummy */

void Sg_VMPushCC(SgCContinuationProc *after, void **data, int datasize)
{
  int i;
  SgContFrame *cc;
  SgObject *s;
  SgVM *vm = Sg_VM();

  CHECK_STACK(CONT_FRAME_SIZE + datasize, vm);
  s = SP(vm);
  cc = (SgContFrame*)s;
  s += CONT_FRAME_SIZE;
  cc->prev = CONT(vm);
  cc->size = datasize;
  cc->pc = (SgWord*)after;
  cc->fp = NULL;
  cc->cl = CL(vm);
  cc->dc = DC(vm);
  FP(vm) = s;
  for (i = 0; i < datasize; i++) {
    PUSH(s, SG_OBJ(data[i]));
  }
  CONT(vm) = cc;
  FP(vm) = SP(vm) = s;
}

#if 0
/* this version's apply has a bug. */
/* Apply families */
static void make_call_frame(SgVM *vm, SgWord *pc);
static SgWord apply_calls_w_halt[][5] = {
  { MERGE_INSN_VALUE1(CALL, 0), RET, HALT },
  { MERGE_INSN_VALUE1(CALL, 1), RET, HALT },
  { MERGE_INSN_VALUE1(CALL, 2), RET, HALT },
  { MERGE_INSN_VALUE1(CALL, 3), RET, HALT },
  { MERGE_INSN_VALUE1(CALL, 4), RET, HALT }
};
#endif

/*
  TODO:
  convert to direct call.
 */
SgObject Sg_Apply0(SgObject proc)
{
#if 0
  SgVM *vm = Sg_VM();
  make_call_frame(vm, apply_calls_w_halt[0] + 2);
  AC(vm) = proc;
  return evaluate_safe(apply_calls_w_halt[0], 3);
#endif
  return Sg_Apply(proc, SG_NIL);
}

SgObject Sg_Apply1(SgObject proc, SgObject arg)
{
#if 0
  SgVM *vm = Sg_VM();
  make_call_frame(vm, apply_calls_w_halt[1] + 2);
  PUSH(SP(vm), arg);
  AC(vm) = proc;
  return evaluate_safe(apply_calls_w_halt[1], 3);
#endif
  SgPair f;
  f.car = arg;
  f.cdr = SG_NIL;
  return Sg_Apply(proc, &f);
}

SgObject Sg_Apply2(SgObject proc, SgObject arg0, SgObject arg1)
{
#if 0
  SgVM *vm = Sg_VM();
  make_call_frame(vm, apply_calls_w_halt[2] + 2);
  PUSH(SP(vm), arg0);
  PUSH(SP(vm), arg1);
  AC(vm) = proc;
  return evaluate_safe(apply_calls_w_halt[2], 3);
#endif
  SgPair f, s;
  f.car = arg0;
  f.cdr = &s;
  s.car = arg1;
  s.cdr = SG_NIL;
  return Sg_Apply(proc, &f);
}

SgObject Sg_Apply3(SgObject proc, SgObject arg0, SgObject arg1, SgObject arg2)
{
#if 0
  SgVM *vm = Sg_VM();
  make_call_frame(vm, apply_calls_w_halt[3] + 2);
  PUSH(SP(vm), arg0);
  PUSH(SP(vm), arg1);
  PUSH(SP(vm), arg2);
  AC(vm) = proc;
  return evaluate_safe(apply_calls_w_halt[3], 3);
#endif
  SgPair f, s, t;
  f.car = arg0;
  f.cdr = &s;
  s.car = arg1;
  s.cdr = &t;
  t.car = arg2;
  t.cdr = SG_NIL;
  return Sg_Apply(proc, &f);
}

SgObject Sg_Apply4(SgObject proc, SgObject arg0, SgObject arg1, SgObject arg2, SgObject arg3)
{
#if 0
  SgVM *vm = Sg_VM();
  make_call_frame(vm, apply_calls_w_halt[4] + 2);
  PUSH(SP(vm), arg0);
  PUSH(SP(vm), arg1);
  PUSH(SP(vm), arg2);
  PUSH(SP(vm), arg3);
  AC(vm) = proc;
  return evaluate_safe(apply_calls_w_halt[4], 3);
#endif
  SgPair f, s, t, fo;
  f.car = arg0;
  f.cdr = &s;
  s.car = arg1;
  s.cdr = &t;
  t.car = arg2;
  t.cdr = &fo;
  fo.car = arg3;
  fo.cdr = SG_NIL;
  return Sg_Apply(proc, &f);
}

SgObject Sg_Apply(SgObject proc, SgObject args)
{
  SgVM *vm = Sg_VM();
  SgObject program;

  vm->applyCode[3] = SG_WORD(proc);
  vm->applyCode[5] = SG_WORD(args);

  program = (vm->cl) ? vm->cl : vm->closureForEvaluate;
  return evaluate_safe(program, vm->applyCode);
}

SgObject Sg_ApplySafe(SgObject proc, SgObject args)
{
  SgVM *vm = Sg_VM();
  Registers r;
  SgObject ret;
  SAVE_REGS(vm, &r);
  ret = Sg_Apply(proc, args);
  RESTORE_REGS(vm, &r);
  return ret;
}

static SgWord apply_callN[2] = {
  MERGE_INSN_VALUE2(APPLY, 2, 1),
  RET
};

static SgWord apply_calls[][5] = {
  { MERGE_INSN_VALUE1(TAIL_CALL, 0), RET },
  { MERGE_INSN_VALUE1(TAIL_CALL, 1), RET },
  { MERGE_INSN_VALUE1(TAIL_CALL, 2), RET },
  { MERGE_INSN_VALUE1(TAIL_CALL, 3), RET },
  { MERGE_INSN_VALUE1(TAIL_CALL, 4), RET }
};

/*
  VMApply families.

  NB: make sure before call these functions, we need to call Sg_VMPushCC.
 */
SgObject Sg_VMApply(SgObject proc, SgObject args)
{
  int argc = Sg_Length(args);
  int reqstack;
  SgVM *vm = Sg_VM();

  if (argc < 0) Sg_Error(UC("improper list not allowed: %S"), args);
  /* TODO should we check tail posision? */
  reqstack = SG_FRAME_SIZE + 1;
  CHECK_STACK(reqstack, vm);
  PUSH(SP(vm), proc);
  PC(vm) = apply_callN;
  /* return Sg_CopyList(args); */
  return Sg_CopyList(args);;
}

SgObject Sg_VMApply0(SgObject proc)
{
  Sg_VM()->pc = apply_calls[0];
  return proc;
}

SgObject Sg_VMApply1(SgObject proc, SgObject arg)
{
  SgVM *vm = Sg_VM();
  CHECK_STACK(1, vm);
  PUSH(SP(vm), arg);
  vm->pc = apply_calls[1];
  return proc;
}

SgObject Sg_VMApply2(SgObject proc, SgObject arg0, SgObject arg1)
{
  SgVM *vm = Sg_VM();
  CHECK_STACK(2, vm);
  PUSH(SP(vm), arg0);
  PUSH(SP(vm), arg1);
  vm->pc = apply_calls[2];
  return proc;
}

SgObject Sg_VMApply3(SgObject proc, SgObject arg0, SgObject arg1, SgObject arg2)
{
  SgVM *vm = Sg_VM();
  CHECK_STACK(3, vm);
  PUSH(SP(vm), arg0);
  PUSH(SP(vm), arg1);
  PUSH(SP(vm), arg2);
  vm->pc = apply_calls[3];
  return proc;
}

SgObject Sg_VMApply4(SgObject proc, SgObject arg0, SgObject arg1, SgObject arg2, SgObject arg3)
{
  SgVM *vm = Sg_VM();
  CHECK_STACK(4, vm);
  PUSH(SP(vm), arg0);
  PUSH(SP(vm), arg1);
  PUSH(SP(vm), arg2);
  PUSH(SP(vm), arg3);
  vm->pc = apply_calls[4];
  return proc;
}

/*
  dynamic-wind
  memo:
  about cont(call) frame.
  in sagittarius scheme, a call frame and cont frame are the same. we push
  call/cont frame when closure was called and dynamic-wind was called. the
  structure of call/cont frame is like this:
  +-----------+ <- sp
  | arg 0 - n |
  +-----------+ <- current fp
  |   size    |
  |    fp     | the order of frame structure does not matter(see vm.h)
  |    cl     |
  |    dc     |
  |    pc     |
  |   prev    |
  +-----------+
  the difference between Sg_VMPushCC and make_call_frame is how to treat
  fp when a frame was made and size. Sg_VMPushCC sets fp when it's called but,
  make_call_frame does not.
 */
static SgCContinuationProc dynamic_wind_before_cc;
static SgCContinuationProc dynamic_wind_body_cc;
static SgCContinuationProc dynamic_wind_after_cc;

SgObject Sg_VMDynamicWind(SgObject before, SgObject thunk, SgObject after)
{
  void *data[3];
  /* TODO should we check type? */
  data[0] = (void*)before;
  data[1] = (void*)thunk;
  data[2] = (void*)after;
  
  Sg_VMPushCC(dynamic_wind_before_cc, data, 3);
  return Sg_VMApply0(before);
}

static SgObject dynamic_wind_before_cc(SgObject result, void **data)
{
  SgObject before = SG_OBJ(data[0]);
  SgObject body = SG_OBJ(data[1]);
  SgObject after = SG_OBJ(data[2]);
  SgObject prev;
  void *d[2];
  SgVM *vm = Sg_VM();
  
  prev = vm->dynamicWinders;
  d[0] = (void*)after;
  d[1] = (void*)prev;
  vm->dynamicWinders = Sg_Acons(before, after, prev);
  Sg_VMPushCC(dynamic_wind_body_cc, d, 2);
  return Sg_VMApply0(body);
}

static SgObject dynamic_wind_body_cc(SgObject result, void **data)
{
  SgObject after = SG_OBJ(data[0]);
  SgObject prev = SG_OBJ(data[1]);
  void *d[1];
  SgVM *vm = Sg_VM();
  
  vm->dynamicWinders = prev;
  d[0] = (void*)result;
  Sg_VMPushCC(dynamic_wind_after_cc, d, 1);
  return Sg_VMApply0(after);
}

static SgObject dynamic_wind_after_cc(SgObject result, void **data)
{
  SgObject ac = SG_OBJ(data[0]);
  return ac;
}

SgObject Sg_VMDynamicWindC(SgSubrProc *before,
			   SgSubrProc *body,
			   SgSubrProc *after,
			   void *data)
{
  SgObject beforeproc, bodyproc, afterproc;
  beforeproc = before ? Sg_MakeSubr(before, data, 0, 0, SG_FALSE) : Sg_NullProc();
  bodyproc = body ? Sg_MakeSubr(body, data, 0, 0, SG_FALSE) : Sg_NullProc();
  afterproc = after ? Sg_MakeSubr(after, data, 0, 0, SG_FALSE) : Sg_NullProc();
  return Sg_VMDynamicWind(beforeproc, bodyproc, afterproc);
}


/* 
;; with-expantion-handler
;; image of with-exception-handler implementation
(define with-exception-handler 
  (lambda (handler thunk)
    (let ((parent (current-exception-handler)))
      (let ((parent-save (parent-exception-handler))
	    (current-save (current-exception-handler))
	    (new-current (lambda (condition)
			   (let ((current-save2 (current-exception-handler)))
			     (dynamic-wind
				 (lambda ()
				   (current-exception-handler parent))
				 (lambda () (handler condition))
				 (lambda () (current-exception-handler current-save2)))))))
	(dynamic-wind
	    (lambda () 
	      (parent-exception-handler parent)
	      (current-exception-handler new-current))
	    thunk
	    (lambda ()
	      (parent-exception-handler parent-save)
	      (current-exception-handler current-save)))))))
 */
static SgObject install_xhandler(SgObject *args, int argc, void *data)
{
  SgVM *vm = Sg_VM();
  if (SG_PAIRP(SG_OBJ(data))) {
    vm->parentExHandler = SG_CAR(SG_OBJ(data));
    vm->exceptionHandler = SG_CDR(SG_OBJ(data));
  } else {
    vm->exceptionHandler = SG_OBJ(data);
  }
  return vm->ac;
}

static SgObject handler_runner(SgObject *args, int argc, void *data)
{
  SgObject handler = SG_CAR(SG_OBJ(data));
  SgObject condition = SG_CDR(SG_OBJ(data));
  return Sg_VMApply1(handler, condition);
}

static SgObject handler_body(SgObject *args, int argc, void *data)
{
  SgVM *vm = Sg_VM();
  SgObject save = vm->exceptionHandler;
  SgObject parent = SG_CAR(SG_OBJ(data));
  SgObject handler = SG_CDR(SG_OBJ(data));
  SgObject before  = Sg_MakeSubr(install_xhandler, parent, 0, 0, SG_INTERN("install-xhandler(body-before)"));
  SgObject after   = Sg_MakeSubr(install_xhandler, save, 0, 0, SG_INTERN("install-xhandler(body-after)"));
  SgObject thunk   = Sg_MakeSubr(handler_runner, Sg_Cons(handler, args[0]), 0, 0, SG_INTERN("handler-body"));
  return Sg_VMDynamicWind(before, thunk, after);
}

SgObject Sg_VMWithExceptionHandler(SgObject handler, SgObject thunk)
{
  SgVM *vm = Sg_VM();
  SgObject parent = vm->exceptionHandler;
  SgObject psave  = vm->parentExHandler;
  SgObject csave  = vm->exceptionHandler;
  SgObject new_current = Sg_MakeSubr(handler_body, Sg_Cons(parent, handler), 1, 0, SG_INTERN("new-current"));
  SgObject before      = Sg_MakeSubr(install_xhandler, Sg_Cons(parent, new_current), 0, 0, SG_INTERN("install-xhandler(with-before)"));
  SgObject after       = Sg_MakeSubr(install_xhandler, Sg_Cons(psave, csave), 0, 0, SG_INTERN("install-xhandler(with-after)"));
  return Sg_VMDynamicWind(before, thunk, after);
}

#if 0
static SgObject install_ehandler(SgObject *args, int argc, void *data)
{
  SgContinuation *c = (SgContinuation*)data;
  SgVM *vm = Sg_VM();
  vm->exceptionHandler = DEFAULT_EXCEPTION_HANDLER;
  vm->escapePoint = c;
  return SG_UNDEF;
}

static SgObject discard_ehandler(SgObject *args, int argc, void *data)
{
  SgContinuation *c = (SgContinuation*)data;
  SgVM *vm = Sg_VM();
  vm->escapePoint = c->prev;
  vm->exceptionHandler = c->xhandler;
  return SG_UNDEF;
}

SgObject Sg_VMWithExceptionHandler(SgObject handler, SgObject thunk)
{
  SgContinuation *c = SG_NEW(SgContinuation);
  SgObject before, after;
  SgVM *vm = Sg_VM();

  c->prev = vm->escapePoint;
  c->ehandler = handler;
  c->xhandler = vm->exceptionHandler;
  c->winders = vm->dynamicWinders;
  c->cstack = vm->cstack;
  c->cont = vm->cont;
  
  vm->escapePoint = c;

  before = Sg_MakeSubr(install_ehandler, c, 0, 0, SG_FALSE);
  after  = Sg_MakeSubr(discard_ehandler, c, 0, 0, SG_FALSE);
  return Sg_VMDynamicWind(before, thunk, after);
}
#endif

#define SKIP(vm, n)        (PC(vm) += (n))
#define FETCH_OPERAND(pc)  SG_OBJ((*(pc)++))
#define PEEK_OPERAND(pc)   SG_OBJ((*(pc)))

#define REFER_LOCAL(vm, n)   *(FP(vm) + n)
#define INDEX_CLOSURE(vm, n) (SG_CLOSURE(DC(vm))->frees[n])

static inline Stack* save_stack()
{
  SgVM *vm = Sg_VM();
  int size = vm->sp - vm->stack;
  Stack *s = SG_NEW2(Stack *, sizeof(Stack) + sizeof(SgObject)*(size - 1));
  s->size = size;
  memcpy(s->stack, vm->stack, sizeof(SgObject) * size);
  return s;
}

static inline int restore_stack(Stack *s, SgObject *to)
{
  memcpy(to, s->stack, s->size * sizeof(SgObject));
  return s->size;
}

static inline SgContinuation* make_continuation(Stack *s)
{
  SgVM *vm = Sg_VM();
  SgContinuation *c = SG_NEW(SgContinuation);
  c->stack = s;
  c->winders = vm->dynamicWinders;
  c->cont = vm->cont;
  c->cstack = vm->cstack;
  
  return c;
}

static SgWord return_code[1] = {SG_WORD(RET)};

#define PC_TO_RETURN return_code

static SgObject throw_continuation_cc(SgObject, void **);

static SgObject throw_continuation_body(SgObject handlers,
					SgContinuation *c,
					SgObject args)
{
  SgVM *vm = Sg_VM();
  int argc;
  /* (if (not (eq? new (current-dynamic-winders))) perform-dynamic-wind) */
  if (SG_PAIRP(handlers)) {
    SgObject handler, chain;
    void *data[3];
    ASSERT(SG_PAIRP(SG_CAR(handlers)));
    handler = SG_CAAR(handlers);
    chain = SG_CDAR(handlers);
    data[0] = (void*)SG_CDR(handlers);
    data[1] = (void*)c;
    data[2] = (void*)args;
    Sg_VMPushCC(throw_continuation_cc, data, 3);
    vm->dynamicWinders = chain;
    return Sg_VMApply0(handler);
  }
  argc = Sg_Length(args);
  
  /* store arguments of the continuation to ac */
  if (argc < 1) {
    /* does this happen? */
    vm->ac = SG_UNDEF;
  } else if (argc > 1) {
    SgValues *v = Sg_MakeValues(argc);
    int i = 0;
    SgObject cp;
    SG_FOR_EACH(cp, args) {
      v->elements[i++] = SG_CAR(cp);
    }
    vm->ac = v;
  } else {
    vm->ac = SG_CAR(args);
  }
  /* restore stack */

  vm->sp = vm->stack + restore_stack(c->stack, vm->stack);
  vm->fp = vm->sp - argc;

  vm->cont = c->cont;
  vm->pc = return_code;
  vm->dynamicWinders = c->winders;

  return vm->ac;
}
static SgObject throw_continuation_cc(SgObject result, void **data)
{
  SgObject handlers = SG_OBJ(data[0]);
  SgContinuation *c = (SgContinuation*)data[1];
  SgObject args = SG_OBJ(data[2]);
  return throw_continuation_body(handlers, c, args);
}

static SgObject throw_continuation_calculate_handlers(SgContinuation *c,
						      SgVM *vm)
{
  SgObject target = Sg_Reverse(c->winders);
  SgObject current = vm->dynamicWinders;
  SgObject h = SG_NIL, t = SG_NIL, p;

  SG_FOR_EACH(p, current) {
    ASSERT(SG_PAIRP(SG_CAR(p)));
    if (!SG_FALSEP(Sg_Memq(SG_CAR(p), target))) break;
    SG_APPEND1(h, t, Sg_Cons(SG_CDAR(p), SG_CDR(p)));
  }
  SG_FOR_EACH(p, target) {
    SgObject chain;
    ASSERT(SG_PAIRP(SG_CAR(p)));
    if (!SG_FALSEP(Sg_Memq(SG_CAR(p), current))) continue;
    chain = Sg_Memq(SG_CAR(p), c->winders);
    ASSERT(SG_PAIRP(chain));
    SG_APPEND1(h, t, Sg_Cons(SG_CAAR(p), SG_CDR(chain)));
  }
  return h;
}

static SgObject throw_continuation(SgObject *argframes, int argc, void *data)
{
  SgContinuation *c = (SgContinuation*)data;
  SgObject args = SG_NIL, t = SG_NIL;
  SgObject handlers_to_call;
  SgVM *vm = Sg_VM();
  int i;

  for (i = 0; i < argc; i++) {
    SG_APPEND1(args, t, argframes[i]);
  }

  if (c->cstack && vm->cstack != c->cstack) {
    SgCStack *cs;
    for (cs = vm->cstack; cs; cs = cs->prev) {
      if (c->cstack == cs) break;
    }
    if (cs != NULL) {
      vm->escapeReason = SG_VM_ESCAPE_CONT;
      vm->escapeData[0] = c;
      vm->escapeData[1] = args;
      longjmp(vm->cstack->jbuf, 1);
    }
  }

  handlers_to_call = throw_continuation_calculate_handlers(c, vm);
  return throw_continuation_body(handlers_to_call, c, args);
}

SgObject Sg_VMCallCC(SgObject proc)
{
  Stack *stack;
  SgContinuation *cont;
  SgObject contproc;

  stack = save_stack();
  cont = make_continuation(stack);
  contproc = Sg_MakeSubr(throw_continuation, cont, 0, 1,
			 Sg_MakeString(UC("continucation"),
				       SG_LITERAL_STRING));
  return Sg_VMApply1(proc, contproc);
}

/* given load path must be unshifted.
   NB: we don't check the validity of given path.
 */
SgObject Sg_AddLoadPath(SgString *path)
{
  SgVM *vm = Sg_VM();
  vm->loadPath = Sg_Append2X(SG_LIST1(path), vm->loadPath);
  return vm->loadPath;
}

/* returns alist of stack trace. */
SgObject Sg_GetStackTrace()
{
  SgVM *vm = Sg_VM();
  SgObject r = SG_NIL;
  SgObject cur = SG_NIL;
  SgContFrame *cont = CONT(vm);
  SgObject cl = CL(vm);
  SgObject prev = SG_UNDEF;
  int i;
  if (!cl) {
    /* before running */
    return SG_NIL;
  }
  for (i = 0;;) {
    if (SG_PROCEDUREP(cl)) {
      SgObject name = SG_PROCEDURE_NAME(cl);
      SgObject src = SG_NIL;
      if (Sg_EqvP(name, prev)) {
	goto next_cont;
      }
      prev = name;
      switch (SG_PROCEDURE_TYPE(cl)) {
      case SG_PROC_SUBR:
	r = SG_LIST3(SG_INTERN("*cproc*"), name, src);
	break;
      case SG_PROC_CLOSURE:
	if (SG_CLOSURE(cl)->code
	    && SG_CODE_BUILDERP(SG_CLOSURE(cl)->code)) {
	  src = SG_CODE_BUILDER(SG_CLOSURE(cl)->code)->src;
	}
	r = SG_LIST3(SG_INTERN("*proc*"), name, src);
	break;
      }
      i++;
    } else {
      /* should not be here */
      ASSERT(FALSE);
    }
    cur = Sg_Acons(SG_MAKE_INT(i), r, cur);
  next_cont:
    if (cont > vm->stack) {

      SgContFrame *nextCont;
      cl = cont->cl;
      if (!cl) break;
      if (!SG_PROCEDUREP(cl)) {
	break;
      }
      nextCont = cont->prev;
      if (!SG_PTRP(nextCont)) {
	break;
      }
      if (nextCont < vm->stack || vm->stackEnd < nextCont) {
	break;
      }
      cont = nextCont;
    } else {
      break;
    }
  }
  return cur;
}

SgObject Sg_VMThrowException(SgVM *vm, SgObject exception, int continuableP)
{
  if (vm->exceptionHandler != DEFAULT_EXCEPTION_HANDLER) {
    if (continuableP) {
      vm->ac = Sg_Apply1(vm->exceptionHandler, exception);
      return vm->ac;
    } else {
      Sg_Apply1(vm->exceptionHandler, exception);
      if (!SG_FALSEP(vm->parentExHandler)) {
	return Sg_Apply1(vm->parentExHandler, 
			 Sg_Condition(SG_LIST4(Sg_MakeNonContinuableViolation(),
					       Sg_MakeWhoCondition(SG_INTERN("raise")),
					       Sg_MakeMessageCondition(Sg_MakeString(UC("returned from non-continuable exception"),
										     SG_LITERAL_STRING)),
					       Sg_MakeIrritantsCondition(SG_LIST1(exception)))));
      }
      vm->exceptionHandler = DEFAULT_EXCEPTION_HANDLER;
      Sg_Error(UC("error in raise: returned from non-continuable exception\n\nirritants:\n%A"), exception);
    }
  }
  Sg_VMDefaultExceptionHandler(exception);
  return SG_UNDEF;		/* dummy */
}

#ifndef EX_SOFTWARE
/* SRFI-22 requires this. */
#define EX_SOFTWARE 70
#endif

/* default exception handler */
void Sg_VMDefaultExceptionHandler(SgObject e)
{
  SgVM *vm = Sg_VM();
  SgContinuation *c = vm->escapePoint;
  SgObject hp;
  
  if (c) {
    /* never reaches for now. */
  } else {
    SG_FOR_EACH(hp, vm->dynamicWinders) {
      SgObject proc = SG_CDAR(hp);
      vm->dynamicWinders = SG_CDR(hp);
      Sg_Apply0(proc);
    }
  }
  Sg_FlushAllPort(FALSE);
  report_error(e);
  /* jump */
  if (vm->cstack) {
    vm->escapeReason = SG_VM_ESCAPE_ERROR;
    vm->escapeData[0] = c;
    vm->escapeData[1] = e;
    longjmp(vm->cstack->jbuf, 1);
  } else {
    exit(EX_SOFTWARE);
  }
}

static SgObject default_exception_handler_body(SgObject *args,
					       int argc, void *data)
{
  ASSERT(argc == 1);
  Sg_VMDefaultExceptionHandler(args[0]);
  return SG_UNDEF;
}

static SG_DEFINE_SUBR(default_exception_handler_rec, 1, 0,
		      default_exception_handler_body,
		      SG_FALSE, NULL);



#define PUSH_CONT(vm, next_pc)				\
  do {							\
    SgContFrame *newcont = (SgContFrame*)SP(vm);	\
    newcont->prev = CONT(vm);				\
    newcont->fp = FP(vm);				\
    newcont->size = (int)(SP(vm) - FP(vm));		\
    newcont->pc = next_pc;				\
    newcont->cl = CL(vm);				\
    newcont->dc = DC(vm);				\
    CONT(vm) = newcont;					\
    SP(vm) += CONT_FRAME_SIZE;				\
    /* FP(vm) = SP(vm);	*/				\
  } while (0)
  

#define CALL_CCONT(p, v, d) p(v, d)

#define POP_CONT()							\
  do {									\
    if (CONT(vm)->fp == NULL) {						\
      void *data__[SG_CCONT_DATA_SIZE];					\
      SgObject v__ = AC(vm);						\
      SgCContinuationProc *after__;					\
      void **d__ = data__;						\
      void **s__ = (void**)((SgObject*)CONT(vm) + CONT_FRAME_SIZE);	\
      int i__ = CONT(vm)->size;						\
      while (i__-- > 0) {						\
	*d__++ = *s__++;						\
      }									\
      after__ = ((SgCContinuationProc*)CONT(vm)->pc);			\
      if (IN_STACK_P((SgObject*)CONT(vm), vm)) {			\
	SP(vm) = (SgObject*)CONT(vm);					\
      }									\
      FP(vm) = SP(vm);							\
      PC(vm) = PC_TO_RETURN;						\
      CL(vm) = CONT(vm)->cl;						\
      DC(vm) = CONT(vm)->dc;						\
      CONT(vm) = CONT(vm)->prev;					\
      AC(vm) = CALL_CCONT(after__, v__, data__);			\
    } else if (IN_STACK_P((SgObject*)CONT(vm), vm)) {			\
      SgContFrame *cont__ = CONT(vm);					\
      SP(vm) = CONT(vm)->fp + CONT(vm)->size;				\
      FP(vm) = cont__->fp;						\
      PC(vm) = cont__->pc;						\
      CL(vm) = cont__->cl;						\
      DC(vm) = cont__->dc;						\
      CONT(vm) = cont__->prev;						\
    } else {								\
      int size__ = CONT(vm)->size;					\
      FP(vm) = SP(vm) = vm->stack;					\
      PC(vm) = CONT(vm)->pc;						\
      CL(vm) = CONT(vm)->cl;						\
      DC(vm) = CONT(vm)->dc;						\
      if (CONT(vm)->fp && size__) {					\
	SgObject *s__ = CONT(vm)->fp, *d__ = SP(vm);			\
	SP(vm) += size__;						\
	while (size__-- > 0) {						\
	  *d__++ = *s__++;						\
	}								\
      }									\
      CONT(vm) = CONT(vm)->prev;					\
    }									\
  } while (0)


SgObject evaluate_safe(SgObject program, SgWord *code)
{
  SgCStack cstack;
  SgVM * volatile vm = Sg_VM();
  SgWord * volatile prev_pc = PC(vm);

  CHECK_STACK(CONT_FRAME_SIZE, vm);

  ASSERT(SG_PROCEDUREP(program));
  vm->cl = program;
  vm->dc = program;

  if (code != NULL) {
    PC(vm) = code;
  } else {
    ASSERT(SG_CLOSUREP(vm->cl));
    PC(vm) = SG_CODE_BUILDER(SG_CLOSURE(vm->cl)->code)->code;
  }
  cstack.prev = vm->cstack;
  cstack.cont = vm->cont;
  vm->cstack = &cstack;

 restart:
  vm->escapeReason = SG_VM_ESCAPE_NONE;
  if (setjmp(cstack.jbuf) == 0) {
    run_loop();
    AC(vm) = vm->ac;
    if (vm->cont == cstack.cont) {
      PC(vm) = prev_pc;
    }
  } else {
    if (vm->escapeReason == SG_VM_ESCAPE_CONT) {
      SgContinuation *c = (SgContinuation*)vm->escapeData[0];
      if (c->cstack == vm->cstack) {
	SgObject handlers = throw_continuation_calculate_handlers(c, vm);
	PC(vm) = PC_TO_RETURN;
	AC(vm) = throw_continuation_body(handlers, c, vm->escapeData[1]);
	goto restart;
      } else {
	ASSERT(vm->cstack && vm->cstack->prev);
	CONT(vm) = cstack.cont;
	AC(vm) = vm->ac;
	vm->cstack = vm->cstack->prev;
	longjmp(vm->cstack->jbuf, 1);
      }
    } else if (vm->escapeReason == SG_VM_ESCAPE_ERROR) {
      SgContinuation *c = (SgContinuation*)vm->escapeData[0];
      if (c && c->cstack == vm->cstack) {
	CONT(vm) = c->cont;
	PC(vm) = PC_TO_RETURN;
	goto restart;
      } else if (vm->cstack->prev == NULL) {
	exit(EX_SOFTWARE);
      } else {
	CONT(vm) = cstack.cont;
	AC(vm) = vm->ac;
	vm->cstack = vm->cstack->prev;
	longjmp(vm->cstack->jbuf, 1);
      }
    } else {
      Sg_Panic("invalid longjmp");
    }
  }
  vm->cstack = vm->cstack->prev;
  return AC(vm);
}


/*
  This method is for compiled library.
  compiled library can only have one library on its file.
  (it may be changed future but for now)
  Library will be compiled only one compiled code, because
  one R6RS library is one S-expression.
  So, just make a closure and apply it with '()
 */
void Sg_VMExecute(SgObject toplevel)
{
  ASSERT(SG_CODE_BUILDERP(toplevel));
  /* NB: compiled libraries don't need any frame. */
  evaluate_safe(theVM->closureForEvaluate, SG_CODE_BUILDER(toplevel)->code);
}

#if 0
static inline void make_call_frame(SgVM *vm, SgWord *pc)
{
  SgObject *s = SP(vm);
  SgContFrame *cc = (SgContFrame*)s;
  cc->size = SP(vm) - FP(vm);		/* dummy */
  cc->pc = pc;
  cc->cl = CL(vm);
  cc->dc = DC(vm);
  cc->fp = FP(vm);
  cc->prev = CONT(vm);
  s += CONT_FRAME_SIZE;
  SP(vm) = s;
  CONT(vm) = cc;
}
#endif 
static inline SgObject* discard_let_frame(SgVM *vm, int n)
{
  int i;
  for (i = n - 1; 0 <= i; i--) {
    INDEX_SET(FP(vm) + n, i, INDEX(SP(vm), i));
  }
  return (FP(vm) + n);
}

static inline SgObject make_display(int n, SgObject *sp)
{
  SgClosure *cl = cl = SG_NEW2(SgClosure *,
			       sizeof(SgClosure) + (sizeof(SgObject) * n));
  int i;
  SG_SET_HEADER(cl, TC_PROCEDURE);
  SG_PROCEDURE_INIT(cl, 0, FALSE, SG_PROC_CLOSURE, SG_FALSE);
  for (i = 0; i < n; i++) {
    cl->frees[i] = INDEX(sp, i);
  }
  return SG_OBJ(cl);
}

static inline SgObject stack_to_pair_args(SgObject *sp, int nargs)
{
  SgObject args = SG_NIL;
  int i;
  for (i = 0; i < nargs; i++) {
    args = Sg_Cons(INDEX(sp, i), args);
  }
  return args;
}

#if 0
static inline void pair_args_to_stack(SgObject *sp, int offset, SgObject args)
{
  if (SG_NULLP(args)) {
    INDEX_SET(sp, offset, SG_NIL);
  } else {
    SgObject arg;
    SG_FOR_EACH(arg, args) {
      PUSH(sp, SG_CAR(arg));
    }
  }
}
#endif

/* shift-arg-to-top */
static inline SgObject* unshift_args(SgObject *sp, int diff)
{
  int i;
  for (i = 0; i < diff; i++) {
    INDEX_SET(sp + diff - i, 0, INDEX(sp, i));
  }
  return sp + diff;
}

static inline SgObject* shift_args(SgObject *fp, int m, SgObject *sp)
{
  int i;
  for (i = m - 1; 0 <= i; i--) {
    INDEX_SET(fp + m, i, INDEX(sp, i));
  }
  return fp + m;
}

static SgObject process_queued_requests_cc(SgObject result, void **data)
{
  SgVM *vm = Sg_VM();
  vm->ac = data[0];
  return vm->ac;
}

static void process_queued_requests(SgVM *vm)
{
  void *data[1];
  
  /* preserve the current continuation */
  data[0] = (void*)vm->ac;

  Sg_VMPushCC(process_queued_requests_cc, data, 1);
  
  vm->attentionRequest = FALSE;
  if (vm->finalizerPending) Sg_VMFinalizerRun(vm);
}

#define CONST_INSN(vm)				\
  AC(vm) = FETCH_OPERAND(PC(vm))
#define PUSH_INSN(vm)				\
  PUSH(SP(vm), AC(vm))
#define LREF_INSN(vm, code)			\
  INSN_VAL1(val1, code);			\
  AC(vm) = REFER_LOCAL(vm, val1)
#define FREF_INSN(vm, code)			\
  INSN_VAL1(val1, code);			\
  AC(vm) = INDEX_CLOSURE(vm, val1)

#define GREF_INSN(vm)							\
  SgObject var = FETCH_OPERAND(PC(vm));					\
  if (SG_GLOCP(var)) {							\
    AC(vm) = SG_GLOC_GET(SG_GLOC(var));					\
  } else {								\
    SgObject value;							\
    SgGloc *g;								\
    ASSERT(SG_IDENTIFIERP(var));					\
    value = Sg_FindBinding(SG_IDENTIFIER(var)->library,			\
			   SG_IDENTIFIER(var)->name, SG_UNBOUND);	\
    if (SG_UNBOUNDP(value)) {						\
      Sg_AssertionViolation(SG_INTERN("vm"),				\
			    Sg_Sprintf(UC("unbound variable %S")),	\
			    var);					\
      return SG_UNDEF;							\
    }									\
    ASSERT(SG_GLOCP(value));						\
    g = SG_GLOC(value);							\
    AC(vm) = SG_GLOC_GET(g);						\
    *(PC(vm) - 1) = SG_WORD(g);						\
  }

#define TAIL_CALL_INSN(vm, code)			\
  {							\
    INSN_VAL1(val1, code);				\
    SP(vm) = shift_args(FP(vm), val1, SP(vm));		\
  }

#define LOCAL_CALL_INSN(vm, c)		\
  {						\
    SgClosure *cl;				\
    SgCodeBuilder *cb;				\
    INSN_VAL1(val1, c);				\
    ASSERT(SG_CLOSUREP(AC(vm)));		\
    cl = SG_CLOSURE(AC(vm));			\
    cb = SG_CODE_BUILDER(cl->code);		\
    DC(vm) = AC(vm);				\
    CL(vm) = AC(vm);				\
    PC(vm) = cb->code;				\
    FP(vm) = SP(vm) - val1;			\
  }

#define BUILTIN_TWO_ARGS(vm, proc)		\
  do {									\
    SgObject s = INDEX(SP(vm), 0);		\
    AC(vm) = proc(s, AC(vm));			\
	SP(vm) -= 1;						\
  } while (0)

#define BUILTIN_TWO_ARGS_COMPARE(vm, proc)	\
  do {										\
    SgObject s = INDEX(SP(vm), 0);		\
    AC(vm) = SG_MAKE_BOOL(proc(s, AC(vm)));	\
    SP(vm) -= 1;							\
  } while(0)

#define BUILTIN_ONE_ARG(vm, proc)		\
  AC(vm) = proc(AC(vm));

#define BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, proc, code)		\
  INSN_VAL1(val1, code);					\
  AC(vm) = proc(AC(vm), SG_MAKE_INT(val1));

#define BRANCH_TEST2(test)			\
  {						\
    SgObject n = FETCH_OPERAND(PC(vm));		\
    int t = test(INDEX(SP(vm), 0), AC(vm));	\
    AC(vm) = SG_MAKE_BOOL(t);			\
    if (SG_FALSEP(AC(vm))) {			\
      PC(vm) += SG_INT_VALUE(n) - 1;		\
    }						\
    SP(vm) -= 1;				\
  }
#define BRANCH_TEST1(test)			\
  {						\
    SgObject n = FETCH_OPERAND(PC(vm));		\
    int t = test(AC(vm));			\
    AC(vm) = SG_MAKE_BOOL(t);			\
    if (SG_FALSEP(AC(vm))) {			\
      PC(vm) += SG_INT_VALUE(n) - 1;		\
    }						\
  }

#define RET_INSN()				\
  do {						\
  /*						\
    if (CONT(vm) == NULL) {			\
      break;					\
    }						\
  */						\
    POP_CONT();					\
  } while (0)					\

/*
  print call frames
  
  call frame and let frame model
  before calling
  +----------------+ <-- sp 
  |   arguments    |
  +----------------+
  |      size      |
  |      prev  ----|----------+
  |       :        |          |
  |       fp       |          |
  +----------------+ <-- cont |
  |   argument n   |          |
  |       :        |          |
  |   argument 0   |          |
  +----------------+ <-- fp   | prev
  |      size      |          |
  |       pc       |          |
  |       cl       |          |
  |       dc       |          |
  |       fp       |          |
  +----------------+ <--------+

  after calling
  +----------------+ <-- sp 
  |   arguments    |
  +----------------+ <-- fp 
  |      size      |
  |      prev  ----|----------+
  |       :        |          |
  |       fp       |          |
  +----------------+ <-- cont |
  |   argument n   |          |
  |       :        |          |
  |   argument 0   |          |
  +----------------+          | prev
  |      size      |          |
  |       pc       |          |
  |       cl       |          |
  |       dc       |          |
  |       fp       |          |
  +----------------+ <--------+

  we can follow the stack with cont register.
  to avoid segmentation fault, we need to be careful with let frame.
  so make sure if it's fp or not before touch an stack element.
 */
static void print_frames(SgVM *vm)
{
  SgContFrame *cont = CONT(vm);
  SgObject *stack = vm->stack, sp = SP(vm);
  SgObject *current = sp;
  SgString *fmt = Sg_MakeString(UC("+   o=~39,,,,39a +~%"), SG_LITERAL_STRING);
  SgString *clfmt = Sg_MakeString(UC("+   cl=~38,,,,39s +~%"), SG_LITERAL_STRING);
  SgString *dcfmt = Sg_MakeString(UC("+   dc=~38,,,,39s +~%"), SG_LITERAL_STRING);
  int something_printed = FALSE;
  Sg_Printf(vm->logPort, UC("stack: 0x%x\n"), stack);
  Sg_Printf(vm->logPort, UC("+---------------------------------------------+ <== sp(0x%x)\n"), sp);
  /* we print frames from top */
  while (stack < current && current <= sp) {
    if (((uintptr_t)current) == ((uintptr_t)cont + sizeof(SgContFrame))) {
      /* print call frame */
      /* frame | frame case*/
      if (something_printed) {
	Sg_Format(vm->logPort, fmt, SG_LIST1(*current), TRUE);
	something_printed = FALSE;
	Sg_Printf(vm->logPort, UC("+---------------------------------------------+\n"));
      }
      Sg_Printf(vm->logPort, UC("+ size=%#38d +\n"), cont->size);
      Sg_Printf(vm->logPort, UC("+   pc=%#38x +\n"), cont->pc);
      Sg_Format(vm->logPort, clfmt, SG_LIST1(cont->cl), TRUE);
      Sg_Format(vm->logPort, dcfmt, SG_LIST1(cont->dc), TRUE);
      Sg_Printf(vm->logPort, UC("+   fp=%#38x +\n"), cont->fp);
      Sg_Printf(vm->logPort, UC("+ prev=%#38x +\n"), cont->prev);
      if (cont == CONT(vm)) {
	Sg_Printf(vm->logPort, UC("+---------------------------------------------+ <== cont(0x%x)\n"), cont);
      } else if (cont->prev) {
	Sg_Printf(vm->logPort, UC("+---------------------------------------------+ <== prev(0x%x)\n"), cont);
      }
      current = cont;
      cont = cont->prev;
      /* check if prev cont has arg or not. */
      continue;
    }
    /* this might be fp or dc of let frame */
    if ((stack <= *current && *current <= sp)) {
      Sg_Printf(vm->logPort, UC("+   p=%#39x +\n"), *current);
    } else if (!(*current)) {
      /* why does this happen? */
      Sg_Printf(vm->logPort, UC("+   p=%#39x +\n"), *current);
    } else {
      /* assume it's an object */
      Sg_Format(vm->logPort, fmt, SG_LIST1(*current), TRUE);
    }
    something_printed = TRUE;
    current--;
  }
  Sg_Printf(vm->logPort, UC("+---------------------------------------------+ <== bottom(0x%x)\n"), stack);
  Sg_Write(SG_MAKE_CHAR('\n'), vm->logPort, 1);
}

void Sg_VMPrintFrame()
{
  print_frames(Sg_VM());
}

#ifdef __GNUC__
# define SWITCH(val)        goto *dispatch_table[val];
# define CASE(insn)         SG_CPP_CAT(LABEL_, insn) :
# define DISPATCH            /* empty */
# define NEXT							\
  do {								\
    if (vm->attentionRequest) goto process_queue;		\
    c = (SgWord)FETCH_OPERAND(PC(vm));				\
    goto *dispatch_table[INSN(c)];				\
  } while (0)
# define DEFAULT            LABEL_DEFAULT :
#else
# define SWITCH(val)        switch (val)
# define CASE(insn)         case insn :
# define NEXT               goto dispatch;
# define DISPATCH           dispatch:
# define DEFAULT            default:
#endif

SgObject run_loop()
{
  SgVM *vm = Sg_VM();
  
  vm->callCode[0] = SG_WORD(CALL);
  vm->callCode[1] = SG_WORD(HALT);

#ifdef __GNUC__
  static void *dispatch_table[INSTRUCTION_COUNT] = {
#define DEFINSN(insn, vals, argc, src, label) && SG_CPP_CAT(LABEL_, insn),
#include "vminsn.c"
#undef DEFINSN
  };
#endif	/* __GNUMC__ */

  for (;;) {
    SgWord c;
    int val1, val2;

    DISPATCH;

	c = (SgWord)FETCH_OPERAND(PC(vm));
    SWITCH(INSN(c)) {
#define VM_LOOP
#include "vminsn.c"
#undef VM_LOOP
      DEFAULT {
	Sg_Panic("unknown instruction appeard. %08x", c);
      }
    }
  process_queue:
    CHECK_STACK(CONT_FRAME_SIZE, vm);
    PUSH_CONT(vm, PC(vm));
    process_queued_requests(vm);
    POP_CONT();
    NEXT;

  }
  return SG_UNDEF;		/* dummy */
}

void Sg__InitVM()
{  
  SgWord *callCode = SG_NEW_ARRAY(SgWord, 2);
  SgWord *applyCode = SG_NEW_ARRAY(SgWord,  7);

  SgCodeBuilder *closureForEvaluateCode = Sg_MakeCodeBuilder(-1);

  applyCode[0] = SG_WORD(FRAME);
  applyCode[1] = SG_WORD(SG_MAKE_INT(6));
  applyCode[2] = SG_WORD(CONST_PUSH);
  applyCode[3] = SG_WORD(SG_UNDEF);
  applyCode[4] = SG_WORD(CONST);
  applyCode[5] = SG_WORD(SG_UNDEF);
  applyCode[6] = SG_WORD(MERGE_INSN_VALUE1(APPLY, 2));
  applyCode[7] = SG_WORD(HALT);

  /* TODO multi thread and etc */
  rootVM = theVM = Sg_NewVM(NULL, Sg_MakeString(UC("root"), SG_LITERAL_STRING));
  rootVM->applyCode = applyCode;
  rootVM->callCode = callCode;
  rootVM->libraries = Sg_MakeHashTableSimple(SG_HASH_EQ, 64);
  rootVM->currentLibrary = Sg_FindLibrary(SG_INTERN("user"), TRUE);

  closureForEvaluateCode->src = Sg_MakeString(UC("<top-level>"), SG_LITERAL_STRING);
  closureForEvaluateCode->name = SG_INTERN("closure-for-evaluate-code");
  rootVM->closureForEvaluate = Sg_MakeClosure(closureForEvaluateCode, NULL);

  /* initialization is not here. in reader.c */
  rootVM->defaultConstructors = SG_NIL;

  /* load path */
  rootVM->loadPath = SG_LIST2(Sg_MakeString(UC(SAGITTARIUS_SITE_LIB_PATH), SG_LITERAL_STRING),
			      Sg_MakeString(UC(SAGITTARIUS_SHARE_LIB_PATH), SG_LITERAL_STRING));

  SG_PROCEDURE_NAME(&default_exception_handler_rec) = Sg_MakeString(UC("default-exception-handler"),
								    SG_LITERAL_STRING);

}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
