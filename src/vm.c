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
#include "sagittarius/bignum.h"
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
#include "sagittarius/weak.h"
#include "sagittarius/thread.h"

static SgInternalMutex global_lock;

static SgVM *rootVM = NULL;

#if _MSC_VER
static __declspec(thread) SgVM *theVM;
#else
#include <pthread.h>
static pthread_key_t the_vm_key;
#define theVM ((SgVM*)pthread_getspecific(the_vm_key))
#endif

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

static void vm_finalize(SgObject obj, void *data)
{
  SgVM *vm = SG_VM(obj);
  Sg_DestroyMutex(&vm->vmlock);
  Sg_DestroyCond(&vm->cond);
}

SgVM* Sg_NewVM(SgVM *proto, SgObject name)
{
  SgVM *v = SG_NEW(SgVM);
  SgWord *callCode = SG_NEW_ARRAY(SgWord, 2);
  SgWord *applyCode = SG_NEW_ARRAY(SgWord,  7);
  unsigned long sec, usec;
  SgCodeBuilder *closureForEvaluateCode = Sg_MakeCodeBuilder(-1);
  SG_SET_HEADER(v, TC_VM);

  applyCode[0] = SG_WORD(FRAME);
  applyCode[1] = SG_WORD(SG_MAKE_INT(6));
  applyCode[2] = SG_WORD(CONST_PUSH);
  applyCode[3] = SG_WORD(SG_UNDEF);
  applyCode[4] = SG_WORD(CONST);
  applyCode[5] = SG_WORD(SG_UNDEF);
  applyCode[6] = SG_WORD(MERGE_INSN_VALUE1(APPLY, 2));
  applyCode[7] = SG_WORD(HALT);

  v->applyCode = applyCode;
  v->callCode = callCode;
 
  closureForEvaluateCode->src = Sg_MakeString(UC("<top-level>"), SG_LITERAL_STRING);
  closureForEvaluateCode->name = SG_INTERN("closure-for-evaluate-code");
  v->closureForEvaluate = Sg_MakeClosure(closureForEvaluateCode, NULL);

  v->name = name;
  v->threadState = SG_VM_NEW;
  v->stack = SG_NEW_ARRAY(SgObject, SG_VM_STACK_SIZE);
  v->sp = v->fp = v->stack;
  v->stackEnd = v->stack + SG_VM_STACK_SIZE;
  v->cont = (SgContFrame *)v->sp;
  v->ac = SG_NIL;
  v->cl = v->dc = NULL;

  v->attentionRequest = FALSE;
  v->finalizerPending = FALSE;
  v->stopRequest = FALSE;
  v->escapePoint = NULL;
  v->escapeReason = SG_VM_ESCAPE_NONE;
  v->escapeData[0] = NULL;
  v->escapeData[1] = NULL;
  v->defaultEscapeHandler = SG_FALSE;
  v->cache = SG_NIL;

  v->dynamicWinders = SG_NIL;
  v->parentExHandler = SG_FALSE;
  v->exceptionHandler = DEFAULT_EXCEPTION_HANDLER;
  v->parameters = Sg_MakeHashTableSimple(SG_HASH_EQ, 64);
  /* TODO use equal hash for source info might not be good. */
  v->sourceInfos = Sg_MakeHashTableSimple(SG_HASH_EQUAL, 4000);
  v->toplevelVariables = SG_NIL;
  v->commandLineArgs = SG_NIL;

  /* from proto */
  /* if proto was NULL, this will be initialized Sg__InitVM */
  /* TODO I'm not sure if I should use the same libraries and currentLibrary
     from proto. Should I copy it?
   */
  v->libraries = proto ? proto->libraries: SG_UNDEF;
  v->currentLibrary = proto ? proto->currentLibrary: SG_UNDEF;
  v->loadPath = proto ? proto->loadPath: SG_NIL;
  v->dynamicLoadPath = proto ? proto->dynamicLoadPath: SG_NIL;
  v->cstack = proto ? proto->cstack : NULL;
  v->flags = proto? proto->flags : 0;
  v->currentInputPort = proto 
    ? proto->currentInputPort
    : Sg_MakeTranscodedInputPort(Sg_StandardInputPort(),
				 Sg_IsUTF16Console(Sg_StandardIn())
				 ? Sg_MakeNativeConsoleTranscoder()
				 : Sg_MakeNativeTranscoder());
  v->currentOutputPort = proto
    ? proto->currentOutputPort
    : Sg_MakeTranscodedOutputPort(Sg_StandardOutputPort(),
				  Sg_IsUTF16Console(Sg_StandardOut())
				  ? Sg_MakeNativeConsoleTranscoder()
				  : Sg_MakeNativeTranscoder());
  v->currentErrorPort = proto 
    ? proto->currentErrorPort
    : Sg_MakeTranscodedOutputPort(Sg_StandardErrorPort(),
				  Sg_IsUTF16Console(Sg_StandardError())
				  ? Sg_MakeNativeConsoleTranscoder()
				  : Sg_MakeNativeTranscoder());
  v->logPort = proto ? proto->logPort : v->currentErrorPort;
  /* macro env */
  v->usageEnv = proto ? proto->usageEnv : SG_FALSE;
  v->macroEnv = proto ? proto->macroEnv : SG_FALSE;

  /* thread, mutex, etc */
  SG_INTERNAL_THREAD_INIT(&v->thread);
  Sg_InitMutex(&v->vmlock, FALSE);
  Sg_InitCond(&v->cond);
  v->inspector = NULL;
  v->canceller = NULL;
  v->thunk = NULL;
  v->specific = SG_FALSE;
  v->result = SG_UNDEF;
  v->resultException = SG_UNDEF;

  /* uptime */
  Sg_GetTimeOfDay(&sec, &usec);
  v->uptimeSec = sec;
  v->uptimeUsec = usec;

  Sg_RegisterFinalizer(SG_OBJ(v), vm_finalize, NULL);
  return v;
}

/*
  Current VM.
 */
SgVM* Sg_VM()
{
  return theVM;
}

int Sg_AttachVM(SgVM *vm)
{
  if (SG_INTERNAL_THREAD_INITIALIZED_P(&vm->thread)) return FALSE;
  if (theVM != NULL) return FALSE;

#if _MSC_VER
  theVM = vm;
#else
  if (pthread_setspecific(the_vm_key, vm) != 0) return FALSE;
#endif
  Sg_SetCurrentThread(&vm->thread);
  vm->state = SG_VM_RUNNABLE;
  return TRUE;
}

int Sg_SetCurrentVM(SgVM *vm)
{
#if _MSC_VER
  theVM = vm;
#else
  if (pthread_setspecific(the_vm_key, vm) != 0) return FALSE;
#endif
  return TRUE;
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

/* some flags */
/* bench mark said, it does not make that much difference.
   and made call/cc so slow.
   maybe we need to do call/cc performance tuning first.
 */
/* #define USE_STACK_DISPLAY 1 */
#define CLEAN_STACK 1
/* #define PROF_INSN 1 */
/*
  clear stack.
  for now, it's only used after compile.
  benchmark said it did not make that much difference, however it definitely
  reduced some GC counts. well just put it.
 */
#if CLEAN_STACK
#define CLEAR_STACK(vm)							\
  memset(SP(vm),0,((vm)->stackEnd-SP(vm))*sizeof(SgObject))
#else
#define CLEAR_STACK(vm)		/* dummy */
#endif


static inline void report_error(SgObject exception)
{
  static const int MAX_STACK_TRACE = 20;
  SgObject error = SG_NIL, stackTrace = SG_NIL;
  SgObject cur;
  SgPort *buf = SG_PORT(Sg_MakeStringOutputPort(-1));
  SgVM *vm = Sg_VM();

  if (SG_PAIRP(exception)) {
    error = SG_CAR(exception);
    stackTrace = SG_CDR(exception);
  } else {
    error = exception;
    stackTrace = Sg_GetStackTrace();
  }
  Sg_Printf(buf,
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
      Sg_Printf(buf,
		UC("      ... (more stack dump truncated)\n"));
      break;
    }

    proc = SG_CDR(obj);		/* (proc name src) */
    if (SG_EQ(SG_CAR(proc), SG_INTERN("*proc*"))) {
      tmp = SG_CAR(SG_CDDR(proc));
      if (!SG_PAIRP(tmp)) {
	goto no_src;
      } else {
	src = Sg_LastPair(tmp);
	src = SG_CDAR(src);
	info = Sg_HashTableRef(SG_HASHTABLE(vm->sourceInfos),
			       src, SG_FALSE);
	/* info = SG_SOURCE_INFO(src); */
      }
      if (SG_FALSEP(info) || !info) {
	Sg_Printf(buf,
		  UC("  [%A] %A: %A (location *unknown*)\n"
		     "    src: %#50S\n"),
		  index, SG_CAR(proc), SG_CADR(proc),
		  Sg_UnwrapSyntax(src));
      } else {
	file = SG_CAR(info);
	line = SG_CDR(info);
	Sg_Printf(buf,
		  UC("  [%A] %A: %A (location %S (line %A))\n"
		     "    src: %#50S\n"),
		  index, SG_CAR(proc), SG_CADR(proc),
		  file, line,
		  Sg_UnwrapSyntax(src));
      }
      
    } else {
    no_src:
      /* *cproc* does not have any source info */
      Sg_Printf(buf,
		UC("  [%A] %A: %A \n"),
		index, SG_CAR(proc), SG_CADR(proc));
    }
  }
  Sg_Write(Sg_GetStringFromStringPort(buf), SG_PORT(Sg_StandardErrorPort()), SG_WRITE_DISPLAY);
  Sg_FlushAllPort(FALSE);
}

void Sg_ReportError(SgObject e)
{
  SgVM *vm = Sg_VM();

  if (SG_VM_RUNTIME_FLAG_IS_SET(vm, SG_ERROR_BEING_REPORTED)) {
    Sg_Abort("Unhandled error occurred during reporting an error. Process aborted.\n");
  }
  SG_VM_RUNTIME_FLAG_SET(vm, SG_ERROR_BEING_REPORTED);
  SG_UNWIND_PROTECT {
    if (SG_PROCEDUREP(vm->defaultEscapeHandler)) {
      Sg_Apply1(vm->defaultEscapeHandler, e);
    } else {
      Sg_FlushAllPort(FALSE);
      report_error(e);
    }
  }
  SG_WHEN_ERROR {
    SG_VM_RUNTIME_FLAG_CLEAR(vm, SG_ERROR_BEING_REPORTED);
  }
  SG_END_PROTECT;
  SG_VM_RUNTIME_FLAG_CLEAR(vm, SG_ERROR_BEING_REPORTED);
}

void Sg_VMSetToplevelVariable(SgSymbol *name, SgObject value)
{
  SgVM *vm = Sg_VM();
  SgGloc *g = Sg_MakeGloc(name, vm->currentLibrary);
  SG_GLOC_SET(g, value);
  vm->toplevelVariables = Sg_Acons(name, g, vm->toplevelVariables);
}

void Sg_VMProcessTime(unsigned long *sec, unsigned long *usec)
{
  /* we need to get process time from rootVM */
  *sec = rootVM->uptimeSec;
  *usec = rootVM->uptimeUsec;
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
    Sg_Printf(vm->logPort, UC("%d: %A"), i, Sg_MakeStringC(info->name));
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
	  Sg_Printf(vm->logPort, UC(" ;; %#20S"), Sg_UnwrapSyntax(SG_CDR(src)));
	}
      } else {
	Sg_Printf(vm->logPort, UC(" ;; #f"));
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

  /* compiler is initialized after VM. so we need to look it up first */
  if (SG_UNDEFP(compiler)) {
    SgObject compile_library;
    SgGloc *g;
    Sg_LockMutex(&global_lock);
    compile_library = Sg_FindLibrary(SG_INTERN("(sagittarius compiler)"), FALSE);
    g = Sg_FindBinding(compile_library, SG_INTERN("compile"), SG_FALSE);
    compiler = SG_GLOC_GET(g);
    Sg_UnlockMutex(&global_lock);
  }
  return Sg_Apply2(compiler, o, e);
}

/* 
   env: library for now.
 */
SgObject Sg_Eval(SgObject sexp, SgObject env)
{
  SgObject v = SG_NIL;
  SgVM *vm = theVM;
  SgObject r = SG_UNDEF, save = vm->currentLibrary;

  if (vm->state != IMPORTING) vm->state = COMPILING;
  v = Sg_Compile(sexp, env);
  /* store cache */
  if (vm->state == IMPORTING) SG_SET_CAR(vm->cache, Sg_Cons(v, SG_CAR(vm->cache)));
  if (vm->state != IMPORTING) vm->state = RUNNING;
  CLEAR_STACK(vm);

  ASSERT(SG_CODE_BUILDERP(v));
  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    Sg_VMDumpCode(v);
  }
  if (!SG_FALSEP(env)) {
    vm->currentLibrary = env;
  }
  SG_CLOSURE(vm->closureForEvaluate)->code = v;
  r = evaluate_safe(vm->closureForEvaluate, SG_CODE_BUILDER(v)->code);
  vm->currentLibrary = save;
  return r;
}

static SgObject pass1_import = SG_UNDEF;

SgObject Sg_Environment(SgObject lib, SgObject spec)
{
  if (SG_UNDEFP(pass1_import)) {
    SgLibrary *complib;
    SgGloc *g;
    Sg_LockMutex(&global_lock);
    complib = Sg_FindLibrary(SG_INTERN("(sagittarius compiler)"), FALSE);
    g = Sg_FindBinding(complib, SG_INTERN("pass1/import"), SG_UNBOUND);
    if (SG_UNBOUNDP(g)) {
      /* something wrong */
      Sg_Panic("pass1/import was not found. loading error?");
    }
    pass1_import = SG_GLOC_GET(g);
    Sg_UnlockMutex(&global_lock);
  }
  /* make spec look like import-spec */
  spec = Sg_Cons(SG_INTERN("import"), spec);
  Sg_ApplySafe(pass1_import, SG_LIST2(spec, lib));
  return lib;
}

static void print_frames(SgVM *vm);
static void expand_stack(SgVM *vm);

#define MOSTLY_FALSE(expr) expr

/* TODO check stack expantion */
#define CHECK_STACK(size, vm)					\
  do {								\
    if (MOSTLY_FALSE(SP(vm) >= (vm)->stackEnd - (size))) {	\
      expand_stack(vm);						\
      /* Sg_Panic("stack overflow"); */				\
    }								\
  } while (0)

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


static SgObject install_ehandler(SgObject *args, int argc, void *data)
{
  SgContinuation *c = (SgContinuation*)data;
  SgVM *vm = Sg_VM();
  vm->exceptionHandler = DEFAULT_EXCEPTION_HANDLER;
  vm->escapePoint = c;
  SG_VM_RUNTIME_FLAG_CLEAR(vm, SG_ERROR_BEING_REPORTED);
  return SG_UNDEF;
}

static SgObject discard_ehandler(SgObject *args, int argc, void *data)
{
  SgContinuation *c = (SgContinuation*)data;
  SgVM *vm = Sg_VM();
  vm->escapePoint = c->prev;
  vm->exceptionHandler = c->xhandler;
  if (c->errorReporting) {
    SG_VM_RUNTIME_FLAG_SET(vm, SG_ERROR_BEING_REPORTED);
  }
  return SG_UNDEF;
}

SgObject Sg_VMWithErrorHandler(SgObject handler, SgObject thunk)
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
  c->errorReporting = SG_VM_RUNTIME_FLAG_IS_SET(vm, SG_ERROR_BEING_REPORTED);

  vm->escapePoint = c;

  before = Sg_MakeSubr(install_ehandler, c, 0, 0, SG_FALSE);
  after  = Sg_MakeSubr(discard_ehandler, c, 0, 0, SG_FALSE);
  return Sg_VMDynamicWind(before, thunk, after);
}

typedef struct display_closure_rec
{
  SG_HEADER;
  SgObject *mark;
  SgObject *prev;
  SgObject  frees[];
} display_closure;
#define DCLOSURE_SIZE_SHIFT 11
#define DCLOSURE(obj)  ((display_closure*)obj)
#define DCLOSUREP(obj) (SG_PTRP(obj) && IS_TYPE(obj, TC_DISPLAY))
#define DCLOSURE_SIZE(obj) (SG_HDR(obj) >> DCLOSURE_SIZE_SHIFT)
#define DCLOSURE_SIZE_SET(obj, size)		\
  (SG_SET_HEADER_ATTRIBUTE(obj, (size << DCLOSURE_SIZE_SHIFT)))

#define SKIP(vm, n)        (PC(vm) += (n))
#define FETCH_OPERAND(pc)  SG_OBJ((*(pc)++))
#define PEEK_OPERAND(pc)   SG_OBJ((*(pc)))

#define REFER_LOCAL(vm, n)   *(FP(vm) + n)
#define INDEX_CLOSURE(vm, n)				\
  (SG_CLOSUREP(DC(vm)) ? SG_CLOSURE(DC(vm))->frees[n]	\
    		       : DCLOSURE(DC(vm))->frees[n])

static inline Stack* save_stack(SgVM* vm, void *end)
{
  int size = (SgObject*)end - vm->stack;
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

/*
  we reuse stack area. so we need to save current stack to somewhere.
  for now we do like this.

  +---------+        +---------+
  |   old   |        |   new   |
  |  stack  |   -->  |  stack  |
  |         |        |         |
  |         |        +---------+
  |         |        |  cont   |
  +---------+        +---------+

  detail drawing
+---------------------------------------------+ <== sp(0x51ec24)
+   p=                                      0 +
+   o=#(9 (#<identifier lambda#(tests r6rs te +
+---------------------------------------------+
+ size=                                     2 +
+   pc=                            0x6a5d8dcc +
+   cl=#<closure #f>                          +
+   dc=#<closure #f>                          +
+   fp=                              0x51ec00 +
+ prev=                              0x51ebe8 +
+---------------------------------------------+ <== cont(0x51ec08)
+   p=                               0x51ebe8 +
+   o=(#(9 (#<identifier lambda#(tests r6rs t +
+   o=#<closure pass2/lifted-define>          +
+---------------------------------------------+ <== fp(0x51ec00)
+ size=                                     3 +
+   pc=                            0x6a5d8df4 +
+   cl=#<closure #f>                          +
+   dc=#<closure #f>                          +
+   fp=                              0x51ebdc +
+ prev=                              0x51ebc4 +
+---------------------------------------------+ <== prev(0x51ebe8)

  We need the last frame pointer to refer local variables.
  So copy from fp - CONT_FRAME_SIZE address.
 */
static SgObject restore_stack_cc(SgObject ac, void **data)
{
  Stack *stack = (Stack*)data[0];
  SgVM *vm = Sg_VM();
  SP(vm) = vm->stack + restore_stack(stack, vm->stack);
  FP(vm) = (SgObject*)data[1];
  return ac;
}

static SgObject save_disp(SgVM *vm)
{
  static const int D_SIZE = sizeof(display_closure)/sizeof(SgObject);
  SgObject dc = DC(vm);
  SgClosure *r = NULL;
  while (!SG_CLOSUREP(dc)) {
    int freec = DCLOSURE_SIZE(dc) - D_SIZE, i;
    SgClosure * cl = SG_NEW2(SgClosure *,
			     sizeof(SgClosure) + (sizeof(SgObject) * freec));
    SG_SET_HEADER(cl, TC_PROCEDURE);
    SG_PROCEDURE_INIT(cl, 0, FALSE, SG_PROC_CLOSURE, SG_FALSE);
    for (i = 0; i < freec; i++) {
      cl->frees[i] = DCLOSURE(dc)->frees[i];
    }
    cl->prev = r;
    r = cl;
    dc = DCLOSURE(dc)->prev;
  }
  SG_CLOSURE(dc)->prev = r;
  return dc;
}

static void expand_stack(SgVM *vm)
{
  Stack *stack = save_stack(vm, CONT(vm));
  SgObject *s = vm->stack, *fp_diff = FP(vm) - CONT_FRAME_SIZE, *sp = SP(vm);
  SgObject dc;
  void *data[2];
  int diff = SP(vm) - FP(vm);

  if (SG_VM_LOG_LEVEL(vm, SG_INFO_LEVEL)) {
    Sg_Printf(vm->logPort, UC("expanding stack\n"));
  }
  /* save display closure */
#if USE_STACK_DISPLAY
  dc = save_disp(vm);
#endif

  /* clear stack */
  while (s != fp_diff) *s++ = NULL;
  SP(vm) = vm->stack;
  /* create marker cont frame */
  data[0] = stack;
  data[1] = FP(vm);
  Sg_VMPushCC(restore_stack_cc, data, 2);
  /* slide the last frame to top */
  memmove(SP(vm), fp_diff, (sp - fp_diff) * sizeof(SgObject));
  /* FIXME, this doesn't work */
  SP(vm) += (sp - fp_diff);
  FP(vm) = SP(vm) - diff;
#if USE_STACK_DISPLAY
  DC(vm) = dc;
  CONT(vm)->dc = dc;
#endif

  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    print_frames(vm);
  }
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
  SgVM *vm = Sg_VM();

  stack = save_stack(vm, SP(vm));
  cont = make_continuation(stack);
  contproc = Sg_MakeSubr(throw_continuation, cont, 0, 1,
			 Sg_MakeString(UC("continucation"),
				       SG_LITERAL_STRING));
  return Sg_VMApply1(proc, contproc);
}

/* given load path must be unshifted.
   NB: we don't check the validity of given path.
 */
static SgString* replace_file_separator(SgString *path)
{
  SgPort *ret = SG_PORT(Sg_MakeStringOutputPort(SG_STRING_SIZE(path)));
  int i;
  for (i = 0; i < SG_STRING_SIZE(path); i++) {
    /* we need to check both '/' and '\\' */
    SgChar c = SG_STRING_VALUE_AT(path, i);
    switch (c) {
    case '/':
    case '\\':
      Sg_PutuzUnsafe(ret, Sg_NativeFileSeparator());
      break;
    default:
      Sg_PutcUnsafe(ret, c);
      break;
    }
  }
  return SG_STRING(Sg_GetStringFromStringPort(ret));
}

SgObject Sg_AddLoadPath(SgString *path)
{
  SgVM *vm = Sg_VM();
  vm->loadPath = Sg_Append2X(SG_LIST1(replace_file_separator(path)), vm->loadPath);
  return vm->loadPath;
}

SgObject Sg_AddDynamicLoadPath(SgString *path)
{
  SgVM *vm = Sg_VM();
  vm->dynamicLoadPath = Sg_Append2X(SG_LIST1(replace_file_separator(path)), vm->dynamicLoadPath);
  return vm->dynamicLoadPath;
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
    if ((uintptr_t)cont > (uintptr_t)vm->stack) {

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
      if ((uintptr_t)nextCont < (uintptr_t)vm->stack ||
	  (uintptr_t)vm->stackEnd < (uintptr_t)nextCont) {
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
    SgObject result = SG_FALSE;
    SgObject target, current;
    /* never reaches for now. */
    vm->escapePoint = c->prev;
    SG_UNWIND_PROTECT {
      result = Sg_Apply1(c->ehandler, e);
      target = c->winders;
      current = vm->dynamicWinders;
      for (hp = current; SG_PAIRP(hp) && (hp != target); hp = SG_CDR(hp)) {
	SgObject proc = SG_CDAR(hp);
	vm->dynamicWinders = SG_CDR(hp);
	Sg_Apply0(proc);
      }
    }
    SG_WHEN_ERROR {
      SG_NEXT_HANDLER;
    }
    SG_END_PROTECT;
    vm->ac = result;
    vm->cont = c->cont;
    if (c->errorReporting) {
      SG_VM_RUNTIME_FLAG_SET(vm, SG_ERROR_BEING_REPORTED);
    }
  } else {
    Sg_ReportError(e);
    SG_FOR_EACH(hp, vm->dynamicWinders) {
      SgObject proc = SG_CDAR(hp);
      vm->dynamicWinders = SG_CDR(hp);
      Sg_Apply0(proc);
    }
  }
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
  SgObject usave = vm->usageEnv, msave = vm->macroEnv;

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
	vm->usageEnv = usave;
	vm->macroEnv = msave;
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
	vm->usageEnv = usave;
	vm->macroEnv = msave;
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

/*
  discards let frame.

  this function might be a little bit tricky. we may have created a display
  closure before, and we don't want to remove it. so we need to detect where
  it is exactly located. see below for the layout of display closure.
  before:
  +------------------+ <- sp
  |    local var 2   |
  +------------------+
  |    local var 1   |
  +------------------+
  | Display closure  |
  |      frees      -+--> [free var 1, free var 2]
  +------------------+ <-- *butt
  |      FP          |
  |      DC          |
  +------------------+
  |      var         |
  +------------------+
  |      var         |
  +------------------+ <- fp
  | Previous display |
  +------------------+
  | Previous frame   |
  +------------------+

  after:
  +------------------+ <- sp
  |    local var 2   |
  +------------------+
  |    local var 1   |
  +------------------+ <- fp
  | Display closure  |
  |      frees      -+--> [free var 1, free var 2]
  +------------------+ <- *butt
  | Previous frame   |
  +------------------+
 */
#if USE_STACK_DISPLAY
static inline SgObject* discard_let_frame(SgVM *vm, int n, int freec)
{
  if (freec > 0) {
    int display_size = 0, prev_size = 0;
    int size = 0, i;
    SgObject dc, prev, *butt;
    display_closure *tmp;
    display_size = sizeof(display_closure) + (sizeof(SgObject) * freec);
    size = display_size/sizeof(SgObject);
    /* check display closure */
    dc = DC(vm);
    /* we don't have to consider root display
       pattern 1: (prev2 prev1 dc) -> (prev2 dc)
       pattern 2: (prev1 dc)       -> (dc)
       pattern 3: (dc)             -> (dc) ;; never happen
       prev2 in pattern1 and prev1 in pattern 2 could be a closure.
     */
    ASSERT(DCLOSUREP(dc));	/* sanity check */
    prev = DCLOSURE(dc)->prev;
    if (!prev) {
      /* pattern3 and already replaced */
    } else if (DCLOSUREP(prev) && !DCLOSURE(prev)->mark) {
      /* cut prev closure */
      SgObject prev2 = DCLOSURE(prev)->prev;
      prev_size = DCLOSURE_SIZE(prev);
      DCLOSURE(dc)->prev = prev2;
    } else if (SG_CLOSUREP(prev) && !SG_CLOSURE(prev)->mark) {
      DCLOSURE(dc)->prev = NULL;
    }
    
    butt = SP(vm) - (n + size);
    memmove(FP(vm) - prev_size, butt, sizeof(SgObject) * n + display_size);
    /* tmp = DCLOSURE(FP(vm) - prev_size); */
    /* *tmp = *DCLOSURE(butt); /\* copy header *\/ */
    /* for (i = 0; i < freec + n; i++) { */
    /*   tmp->frees[i] = DCLOSURE(butt)->frees[i]; */
    /* } */
    DC(vm) = FP(vm) - prev_size;
    FP(vm) += (size - prev_size);
  } else {
    int i;
    for (i = n - 1; 0 <= i; i--) {
      INDEX_SET(FP(vm) + n, i, INDEX(SP(vm), i));
    }
  }
  return FP(vm) + n;

}
#else
static inline SgObject* discard_let_frame(SgVM *vm, int n, int freec)
{
  int i;
  if (freec > 0) {
    SgObject prev;
    ASSERT(DCLOSUREP(DC(vm)));
    prev = DCLOSURE(DC(vm))->prev;
    if (prev) {
      if (SG_CLOSUREP(prev) && !SG_CLOSURE(prev)->mark) {
	DCLOSURE(DC(vm))->prev = NULL;
      } else if (DCLOSUREP(prev) && !DCLOSURE(prev)->mark) {
	SgObject prev2 = DCLOSURE(prev)->prev;
	DCLOSURE(DC(vm))->prev = prev2;
      }
    }
  }
  for (i = n - 1; 0 <= i; i--) {
    INDEX_SET(FP(vm) + n, i, INDEX(SP(vm), i));
  }
  return (FP(vm) + n);
}
#endif
/*
  DISPLAY instruction will be called after LET_FRAME and its free variables.
  so let's make display closure on the stack.
  before:
  *1 fp is not available yet.
  +------------------+ <- sp
  |    free var 1    |
  +------------------+
  |    free var 2    |
  +------------------+ 
  |      FP          |
  |      DC          |
  +------------------+
  after:
  +------------------+ <- sp
  | Display closure  |
  |      frees      -+--> [free var 1, free var 2]
  +------------------+ 
  |      FP          |
  |      DC          |
  +------------------+

 */
#if USE_STACK_DISPLAY
static inline void make_display(int n, SgVM *vm)
{
  SgObject *tfp = SP(vm) - n;	/* temporary fp */
  display_closure *cl = (display_closure *)SP(vm);
  const int size = sizeof(display_closure) + (sizeof(SgObject) * n);
  int i;

  CHECK_STACK(size/sizeof(SgObject), vm);
  SG_SET_HEADER(cl, TC_DISPLAY);
  cl->prev = DC(vm);
  cl->mark = NULL;
  DCLOSURE_SIZE_SET(cl, size/sizeof(SgObject));
  for (i = 0; i < n; i++) {
    cl->frees[i] = INDEX(SP(vm), i);
  }
  /* shift display closure */
  memmove(tfp, cl, size);
  DC(vm) = tfp;
  SP(vm) = tfp + size/sizeof(SgObject);
}
#else
static inline void make_display(int n, SgVM *vm) 
{
  const int size = sizeof(display_closure) + (sizeof(SgObject) * n);
  display_closure *cl = SG_NEW2(display_closure *, size);
  int i;
  SG_SET_HEADER(cl, TC_DISPLAY);
  /* SG_PROCEDURE_INIT(cl, 0, FALSE, SG_PROC_CLOSURE, SG_FALSE); */
  for (i = 0; i < n; i++) {
    cl->frees[i] = INDEX(SP(vm), i);
  }
  cl->prev = DC(vm);
  DCLOSURE_SIZE_SET(cl, size/sizeof(SgObject));
  DC(vm) = cl;
  SP(vm) -= n;
}
#endif

static inline void leave_process(SgVM *vm, int freec)
{
    SgObject* sp = FP(vm);
#if USE_STACK_DISPLAY
    if (freec > 0) {
      ASSERT(DCLOSUREP(DC(vm)));
      sp -= DCLOSURE_SIZE(DC(vm));
    }
#endif
    FP(vm)=(SgObject*)INDEX(sp, 0);
    DC(vm)=INDEX(sp, 1);
    SP(vm)=(sp - SG_LET_FRAME_SIZE);
}

static inline SgObject* shift_args(SgObject *fp, int m, SgObject *sp);
/* shiftj

  before:
  +------------------+ <- sp
  |    local var 2   |
  +------------------+
  |    local var 1   |
  +------------------+ <- fp1
  | Display closure  |
  |      mark       -+--> fp2
  +------------------+
  |    let frame     |
  +------------------+
  |     old var 2    |
  +------------------+
  |     old var 1    |
  +------------------+ <- fp2
  | Display closure  |
  +------------------+
  |    let frame     |
  +------------------+

  after:
  +------------------+ <- sp
  |    local var 2   |
  +------------------+
  |    local var 1   |
  +------------------+ <- fp2
  | Display closure  |
  +------------------+
  |    let frame     |
  +------------------+
 */
static inline void shiftj_process(SgVM *vm, int depth, int diff)
{
  int skip_marks = depth - 1, count = 0;
  for (;;) {
    SgObject mark;
    if (SG_CLOSUREP(DC(vm))) {
      FP(vm) = SG_CLOSURE(DC(vm))->mark;
    } else {
      FP(vm) = DCLOSURE(DC(vm))->mark;
    }
    if (count == skip_marks && FP(vm)) {
      break;
    }
    if (FP(vm)) {
      count++;
    }
    if (SG_CLOSUREP(DC(vm))) {
      DC(vm)=SG_CLOSURE(DC(vm))->prev;
    } else {
      DC(vm)=DCLOSURE(DC(vm))->prev;
    }
  }
  ASSERT(SG_CLOSUREP(DC(vm)) || DCLOSUREP(DC(vm)));
  SP(vm) = shift_args(FP(vm), diff, SP(vm));
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

  if (vm->stopRequest) {
    SG_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(vm->vmlock);
    if (vm->stopRequest) {
      vm->stopRequest = FALSE;
      vm->threadState = SG_VM_STOPPED;
      Sg_NotifyAll(&vm->cond);
      while (vm->threadState == SG_VM_STOPPED) {
	Sg_Wait(&vm->cond, &vm->vmlock);
      }
    }
    SG_INTERNAL_MUTEX_SAFE_LOCK_END();
  }
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
    if (!SG_IDENTIFIERP(var)) {						\
      Sg_Error(UC("[internal error] (GREF) identifier required but got %S\n"), var); \
    }									\
    ASSERT(SG_IDENTIFIERP(var));					\
    value = Sg_FindBinding(SG_IDENTIFIER(var)->library,			\
			   SG_IDENTIFIER(var)->name, SG_UNBOUND);	\
    if (SG_UNBOUNDP(value)) {						\
      Sg_AssertionViolation(SG_INTERN("vm"),				\
			    Sg_Sprintf(UC("unbound variable %A"), var),	\
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

#define LOCAL_CALL_INSN(vm, c)						\
  {									\
    SgClosure *cl;							\
    SgCodeBuilder *cb;							\
    INSN_VAL1(val1, c);							\
    ASSERT(SG_CLOSUREP(AC(vm)));					\
    if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL) && vm->state == RUNNING) {	\
      Sg_Printf(vm->logPort, UC("calling %S\n"), AC(vm));		\
      if (SG_VM_LOG_LEVEL(vm, SG_TRACE_LEVEL) && vm->state == RUNNING) { \
	print_frames(vm);						\
      }									\
    }									\
    cl = SG_CLOSURE(AC(vm));						\
    cb = SG_CODE_BUILDER(cl->code);					\
    DC(vm) = AC(vm);							\
    CL(vm) = AC(vm);							\
    PC(vm) = cb->code;							\
    FP(vm) = SP(vm) - val1;						\
  }

#define BUILTIN_TWO_ARGS(vm, proc)		\
  do {						\
    AC(vm) = proc(INDEX(SP(vm), 0), AC(vm));	\
    SP(vm)--;					\
  } while (0)

#define BUILTIN_TWO_ARGS_COMPARE(vm, proc)		\
  do {							\
    AC(vm) = SG_MAKE_BOOL(proc(INDEX(SP(vm), 0),	\
			       AC(vm)));		\
    SP(vm)--;						\
  } while(0)

#define BUILTIN_ONE_ARG(vm, proc)		\
  AC(vm) = proc(AC(vm));

#define BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, proc, code)		\
  AC(vm) = proc(SG_MAKE_INT(val1), AC(vm));

#define BRANCH_TEST2(test)			\
  {						\
    SgObject n = FETCH_OPERAND(PC(vm));		\
    if (!test(INDEX(SP(vm), 0), AC(vm))) {	\
      PC(vm) += SG_INT_VALUE(n) - 1;		\
      AC(vm) = SG_FALSE;			\
    } else {					\
      AC(vm) = SG_TRUE;				\
    }						\
    SP(vm)--;					\
  }
#define BRANCH_TEST1(test)			\
  {						\
    SgObject n = FETCH_OPERAND(PC(vm));		\
    if (!test(AC(vm))) {			\
      PC(vm) += SG_INT_VALUE(n) - 1;		\
      AC(vm) = SG_FALSE;			\
    } else {					\
      AC(vm) = SG_TRUE;				\
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
  SgObject *stack = vm->stack, *sp = SP(vm);
  SgObject *current = sp - 1;
  int below_cont = FALSE, size = cont->size, c_func = FALSE;
  SgString *fmt   = Sg_MakeString(UC("+    o=~38,,,,39a +~%"), SG_LITERAL_STRING);
  SgString *clfmt = Sg_MakeString(UC("+   cl=~38,,,,39s +~%"), SG_LITERAL_STRING);
  SgString *dcfmt = Sg_MakeString(UC("+   dc=~38,,,,39s +~%"), SG_LITERAL_STRING);

  Sg_Printf(vm->logPort, UC("stack: 0x%x\n"), stack);
  Sg_Printf(vm->logPort, UC("0x%x +---------------------------------------------+ <== sp(0x%x)\n"), sp, sp);
  /* first we dump from top until cont frame. */
  while ((stack < current && current <= sp)) {
    if (current == (SgObject*)cont + CONT_FRAME_SIZE) {
      break;
    }
    Sg_Printf(vm->logPort, UC("0x%x +   p=%#39x +\n"), current, *current);
    current--;
  }
  /* now we know we just need to trace cont frames
     memo: if cont has let frame, we just dump it as pointer.
   */
  while (stack < current && current <= sp) {
    int i;
    /* the very first arguments are ignored */
    while (current > (SgObject *)cont + CONT_FRAME_SIZE) {
      Sg_Printf(vm->logPort, UC("0x%x +   p=%#39x +\n"), current, *current);
      current--;
    }
    Sg_Printf(vm->logPort, UC("0x%x +   p=%#39x +\n"), current, *current);

    /* todo, dump display closure. */
    Sg_Printf(vm->logPort, UC("0x%x +---------------------------------------------+\n"), current);
    Sg_Printf(vm->logPort, UC("0x%x + size=%#38d +\n"),
	      (uintptr_t)cont + offsetof(SgContFrame, size), cont->size);
    Sg_Printf(vm->logPort, UC("0x%x +   pc=%#38x +\n"),
	      (uintptr_t)cont + offsetof(SgContFrame, pc), cont->pc);
    Sg_Printf(vm->logPort, UC("0x%x "), (uintptr_t)cont + offsetof(SgContFrame, cl));
    Sg_Format(vm->logPort, clfmt, SG_LIST1(cont->cl), TRUE);
    Sg_Printf(vm->logPort, UC("0x%x "), (uintptr_t)cont + offsetof(SgContFrame, dc));
    Sg_Format(vm->logPort, dcfmt, SG_LIST1(cont->dc), TRUE);
    Sg_Printf(vm->logPort, UC("0x%x +   fp=%#38x +\n"),
	      (uintptr_t)cont + offsetof(SgContFrame, fp), cont->fp);
    Sg_Printf(vm->logPort, UC("0x%x + prev=%#38x +\n"),
	      (uintptr_t)cont + offsetof(SgContFrame, prev), cont->prev);
    if (cont == CONT(vm)) {
      Sg_Printf(vm->logPort, UC("0x%x +---------------------------------------------+ <== cont(0x%x)\n"), cont, cont);
    } else if (cont->prev) {
      Sg_Printf(vm->logPort, UC("0x%x +---------------------------------------------+ <== prev(0x%x)\n"), cont, cont);
    }
    if (cont->fp == NULL) c_func = TRUE;
    else c_func = FALSE;

    current = cont;
    size = cont->size;
    /* cont's size is argc of previous cont frame */
    /* dump arguments */
    for (i = 0; i < size; i++) {
      if (c_func) {
	Sg_Printf(vm->logPort, UC("0x%x +   p=%#39x +\n"), current-i-1, *(current-i-1));
      } else {
	Sg_Printf(vm->logPort, UC("0x%x "), current-i-1);
	Sg_Format(vm->logPort, fmt, SG_LIST1(*(current-i-1)), TRUE);
      }
    }
    current -= (size + 1);
    cont = cont->prev;
    /* check if prev cont has arg or not. */
    continue;
  }
  Sg_Printf(vm->logPort, UC("0x%x +---------------------------------------------+\n"), stack);
}

void Sg_VMPrintFrame()
{
  print_frames(Sg_VM());
}

#if PROF_INSN
#define COUNT_INSN(c)   if (vm->state == RUNNING) called_instructions[INSN(c)]++
static int called_instructions[INSTRUCTION_COUNT] = {0};
static void show_inst_count(void *data)
{
  int i;
  for (i = 0; i < INSTRUCTION_COUNT; i++) {
    if (called_instructions[i]) {
      InsnInfo *info = Sg_LookupInsnName(i);
      fprintf(stderr, "INSN: %s(%d)\n", info->name, called_instructions[i]);
    }
  }
}
#else
#define COUNT_INSN(c)		/* dummy */
#endif


#ifdef __GNUC__
# define SWITCH(val)        goto *dispatch_table[val];
# define CASE(insn)         SG_CPP_CAT(LABEL_, insn) :
# define DISPATCH            /* empty */
# define NEXT							\
  do {								\
    if (vm->attentionRequest) goto process_queue;		\
    c = (SgWord)FETCH_OPERAND(PC(vm));				\
    COUNT_INSN(c);						\
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
    int val1 = 0, val2 = 0;

    DISPATCH;
    if (vm->attentionRequest) goto process_queue;
    c = (SgWord)FETCH_OPERAND(PC(vm));
    COUNT_INSN(c);
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
  SgObject initialEnv = Sg_MakeVector(4, SG_UNDEF);
  /* TODO multi thread and etc */
#ifdef _MSC_VER
  rootVM = theVM = Sg_NewVM(NULL, Sg_MakeString(UC("root"), SG_LITERAL_STRING));
#else
  if (pthread_key_create(&the_vm_key, NULL) != 0) {
    Sg_Panic("pthread_key_create failed.");
  }
  rootVM = Sg_NewVM(NULL, Sg_MakeString(UC("root"), SG_LITERAL_STRING));
  Sg_SetCurrentVM(rootVM);
  /*
  if (pthread_setspecific(the_vm_key, rootVM) != 0) {
    Sg_Panic("pthread_setspecific failed.");
  }
  */
#endif
  Sg_SetCurrentThread(&rootVM->thread);
  rootVM->threadState = SG_VM_RUNNABLE;
  rootVM->libraries = Sg_MakeHashTableSimple(SG_HASH_EQ, 64);
  rootVM->currentLibrary = Sg_FindLibrary(SG_INTERN("user"), TRUE);

  /* initialization is not here. in reader.c */
  rootVM->defaultConstructors = SG_NIL;

  /* load path */
  rootVM->loadPath = Sg_GetDefaultLoadPath();
  rootVM->dynamicLoadPath = Sg_GetDefaultDynamicLoadPath();

  /* env */
  SG_VECTOR_ELEMENT(initialEnv, 0) = rootVM->currentLibrary;
  SG_VECTOR_ELEMENT(initialEnv, 1) = SG_NIL;
  rootVM->usageEnv = initialEnv;
  rootVM->macroEnv = initialEnv;

  SG_PROCEDURE_NAME(&default_exception_handler_rec) = Sg_MakeString(UC("default-exception-handler"),
								    SG_LITERAL_STRING);
  Sg_InitMutex(&global_lock, TRUE);

#if PROF_INSN
  Sg_AddCleanupHandler(show_inst_count, NULL);
#endif
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
