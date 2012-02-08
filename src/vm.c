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
#include "sagittarius/generic.h"
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

static void box_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<box 0x%x>"), obj);
}
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_BoxClass, box_print);

static inline SgObject make_box(SgObject value)
{
  SgBox *b = SG_NEW(SgBox);
  SG_SET_CLASS(b, SG_CLASS_BOX);
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

static void vm_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  char buf[50];
  SgVM *vm = SG_VM(obj);
  Sg_Printf(port, UC("#<thread %A"), vm->name);
  switch (vm->threadState) {
  case SG_VM_NEW:
    Sg_Putz(port, " new");
    break;
  case SG_VM_RUNNABLE:
    Sg_Putz(port, " runnable");
    break;
  case SG_VM_STOPPED:
    Sg_Putz(port, " stopped");
    break;
  case SG_VM_TERMINATED:
    Sg_Putz(port, " terminated");
    break;
  default:
    Sg_Putz(port, " (unknonw state)");
    break;
  }
  snprintf(buf, sizeof(buf), " %p>", vm);
  Sg_Putz(port, buf);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_VMClass, vm_print);

SgVM* Sg_NewVM(SgVM *proto, SgObject name)
{
  SgVM *v = SG_NEW(SgVM);
  SgWord *callCode = SG_NEW_ARRAY(SgWord, 2);
  SgWord *applyCode = SG_NEW_ARRAY(SgWord,  7);
  unsigned long sec, usec;
  SgCodeBuilder *closureForEvaluateCode = Sg_MakeCodeBuilder(-1);
  SG_SET_CLASS(v, SG_CLASS_VM);

  applyCode[0] = SG_WORD(FRAME);
  applyCode[1] = SG_WORD(SG_MAKE_INT(6));
  applyCode[2] = SG_WORD(CONST_PUSH);
  applyCode[3] = SG_WORD(SG_UNDEF);
  applyCode[4] = SG_WORD(CONST);
  applyCode[5] = SG_WORD(SG_UNDEF);
  applyCode[6] = SG_WORD(MERGE_INSN_VALUE1(APPLY, 2));
  applyCode[7] = SG_WORD(RET);

  v->applyCode = applyCode;
  v->callCode = callCode;
 
  closureForEvaluateCode->src = SG_NIL;
  closureForEvaluateCode->name = SG_INTERN("*toplevel*");
  v->closureForEvaluate = Sg_MakeClosure(closureForEvaluateCode, NULL);

  v->name = name;
  v->threadState = SG_VM_NEW;
  v->stack = SG_NEW_ARRAY(SgObject, SG_VM_STACK_SIZE);
  v->sp = v->fp = v->stack;
  v->stackEnd = v->stack + SG_VM_STACK_SIZE;
  v->cont = (SgContFrame *)v->sp;
  v->ac = SG_NIL;
  v->cl = NULL;
  /* v->fpOffset = 0; */

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
  v->sourceInfos = Sg_MakeHashTableSimple(SG_HASH_EQ, 4000);
  v->toplevelVariables = SG_NIL;
  v->commandLineArgs = SG_NIL;

  /* from proto */
  /* if proto was NULL, this will be initialized Sg__InitVM */
  /* TODO I'm not sure if I should use the same libraries and currentLibrary
     from proto. Should I copy it?
   */
  v->currentLoadPath = SG_FALSE; /* should this be inherited from proto? */
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
  vm->threadState = SG_VM_RUNNABLE;
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

readtable_t* Sg_CurrentReadTable()
{
  return Sg_VM()->currentReadTable;
}
void Sg_SetCurrentReadTable(readtable_t *newtable)
{
  Sg_VM()->currentReadTable = newtable;
}

/* some flags */
/* bench mark said, it does not make that much difference.
   and made call/cc so slow.
   maybe we need to do call/cc performance tuning first.
 */
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
	       "%A\n"), Sg_DescribeCondition(error));

  if (!SG_NULLP(stackTrace)) {
    Sg_Printf(buf, UC("stack trace:\n"));
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

static SgObject eval_restore_env(SgObject *args, int argc, void *data)
{
  theVM->currentLibrary = SG_LIBRARY(data);
  return SG_UNDEF;
}

SgObject Sg_VMEval(SgObject sexp, SgObject env)
{
  SgObject v = SG_NIL;
  SgVM *vm = theVM;

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
    SgObject body = Sg_MakeClosure(v, NULL);
    SgObject before = Sg_MakeSubr(eval_restore_env, env, 0, 0, SG_FALSE);
    SgObject after = Sg_MakeSubr(eval_restore_env, vm->currentLibrary, 0, 0, SG_FALSE);
    return Sg_VMDynamicWind(before, body, after);
  } else {
    /* Since we have LIBRARY instruction, we always need to restore current library */
    SgObject body = Sg_MakeClosure(v, NULL);
    SgObject before = Sg_NullProc();
    SgObject after = Sg_MakeSubr(eval_restore_env, vm->currentLibrary, 0, 0, SG_FALSE);
    return Sg_VMDynamicWind(before, body, after);
  }
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
  Sg_Apply2(pass1_import, spec, lib);
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

#define C_CONT_MARK NULL

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
  /* cc->fp = NULL; */
  cc->fp = C_CONT_MARK;
  cc->cl = CL(vm);
  for (i = 0; i < datasize; i++) {
    PUSH(s, SG_OBJ(data[i]));
  }
  CONT(vm) = cc;
  FP(vm) = SP(vm) = s;
  /* vm->fpOffset = CALC_OFFSET(vm, 0); */
}

/* #define USE_LIGHT_WEIGHT_APPLY 1 */

#define PUSH_CONT(vm, next_pc)				\
  do {							\
    SgContFrame *newcont = (SgContFrame*)SP(vm);	\
    newcont->prev = CONT(vm);				\
    newcont->fp = FP(vm);				\
    /* newcont->fp = (SgObject*)newcont - FP(vm); */	\
    /* newcont->fp = vm->fpOffset; */			\
    newcont->size = (int)(SP(vm) - FP(vm));		\
    newcont->pc = next_pc;				\
    newcont->cl = CL(vm);				\
    CONT(vm) = newcont;					\
    SP(vm) += CONT_FRAME_SIZE;				\
    /* FP(vm) = SP(vm);	*/				\
  } while (0)


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

SgObject Sg_Apply0(SgObject proc)
{
#if USE_LIGHT_WEIGHT_APPLY
  SgVM *vm = Sg_VM();
  PUSH_CONT(vm, apply_calls[0]);
  return evaluate_safe(proc, apply_calls[0]);
#else
  return Sg_Apply(proc, SG_NIL);
#endif
}

SgObject Sg_Apply1(SgObject proc, SgObject arg)
{
#if USE_LIGHT_WEIGHT_APPLY
  SgVM *vm = Sg_VM();
  PUSH_CONT(vm, apply_calls[1]);
  PUSH(SP(vm), arg);
  return evaluate_safe(proc, apply_calls[1]);
#else
  SgPair f;
  f.car = arg;
  f.cdr = SG_NIL;
  return Sg_Apply(proc, &f);
#endif
}

SgObject Sg_Apply2(SgObject proc, SgObject arg0, SgObject arg1)
{
#if USE_LIGHT_WEIGHT_APPLY
  SgVM *vm = Sg_VM();
  PUSH_CONT(vm, apply_calls[2]);
  PUSH(SP(vm), arg0);
  PUSH(SP(vm), arg1);
  return evaluate_safe(proc, apply_calls[2]);
#else
  SgPair f, s;
  f.car = arg0;
  f.cdr = &s;
  s.car = arg1;
  s.cdr = SG_NIL;
  return Sg_Apply(proc, &f);
#endif
}

SgObject Sg_Apply3(SgObject proc, SgObject arg0, SgObject arg1, SgObject arg2)
{
#if USE_LIGHT_WEIGHT_APPLY
  SgVM *vm = Sg_VM();
  PUSH_CONT(vm, apply_calls[3]);
  PUSH(SP(vm), arg0);
  PUSH(SP(vm), arg1);
  PUSH(SP(vm), arg2);
  return evaluate_safe(proc, apply_calls[3]);
#else
  SgPair f, s, t;
  f.car = arg0;
  f.cdr = &s;
  s.car = arg1;
  s.cdr = &t;
  t.car = arg2;
  t.cdr = SG_NIL;
  return Sg_Apply(proc, &f);
#endif
}

SgObject Sg_Apply4(SgObject proc, SgObject arg0, SgObject arg1, SgObject arg2, SgObject arg3)
{
#if USE_LIGHT_WEIGHT_APPLY
  SgVM *vm = Sg_VM();
  PUSH_CONT(vm, apply_calls[4]);
  PUSH(SP(vm), arg0);
  PUSH(SP(vm), arg1);
  PUSH(SP(vm), arg2);
  PUSH(SP(vm), arg3);
  return evaluate_safe(proc, apply_calls[4]);
#else
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
#endif
}

SgObject Sg_Apply(SgObject proc, SgObject args)
{
  SgVM *vm = Sg_VM();
  SgObject program;

  vm->applyCode[3] = SG_WORD(proc);
  vm->applyCode[5] = SG_WORD(args);

  program = (CL(vm)) ? CL(vm) : vm->closureForEvaluate;
  return evaluate_safe(program, vm->applyCode);
}

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

static SgObject with_error_handler(SgObject handler, SgObject thunk,
				   int rewindBefore)
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
  c->rewindBefore = rewindBefore;

  vm->escapePoint = c;

  before = Sg_MakeSubr(install_ehandler, c, 0, 0, SG_FALSE);
  after  = Sg_MakeSubr(discard_ehandler, c, 0, 0, SG_FALSE);
  return Sg_VMDynamicWind(before, thunk, after);
}

SgObject Sg_VMWithErrorHandler(SgObject handler, SgObject thunk,
			       int rewindBefore)
{
  return with_error_handler(handler, thunk, rewindBefore);
}


#define SKIP(vm, n)        (PC(vm) += (n))
#define FETCH_OPERAND(pc)  SG_OBJ((*(pc)++))
#define PEEK_OPERAND(pc)   SG_OBJ((*(pc)))

#define REFER_LOCAL(vm, n)   *(FP(vm) + n)
#define INDEX_CLOSURE(vm, n)  SG_CLOSURE(CL(vm))->frees[n]

#if USE_ONE_PATH_CALL_CC

#define FORWARDED_CONT_P(c) ((c)&&((c)->size == -1))
#define FORWARDED_CONT(c)   ((c)->prev)

/*
  save a cont frame with its arguments.
 */
static SgContFrame* save_a_cont(SgContFrame *c)
{
  SgObject *s, *d;
  SgContFrame *csave;
  int i;
  const size_t size = sizeof(SgContFrame) + c->size * sizeof(SgObject);
  csave = SG_NEW2(SgContFrame *, size);
  csave->env = NULL;
  /* copy cont frame */
  if (c->fp != C_CONT_MARK) {
    *csave = *c;		/* copy the frame */
    if (c->size) {
      /* copy the args */
      s = (SgObject*)c - c->size;
      d = (SgObject*)csave + CONT_FRAME_SIZE;
      csave->env = d;
      for (i = 0; i < c->size; i++) {
	*d++ = *s++;
      }
    }
  } else {
    /* C continuation */
    s = (SgObject*)c;
    d = (SgObject*)csave;
    for (i =CONT_FRAME_SIZE + c->size; i > 0; i--) {
      /* C continuation frame contains opaque pointer */
      *d++ = *s++;
    }
  }
  return csave;
}

/*
  save continuation frame to heap.
  we do with 2 passes.
  pass1: save cont frame to heap
  pass2: update cstack etc.
 */
static void save_cont(SgVM *vm)
{
  SgContFrame *c = CONT(vm), *prev = NULL, *tmp;
  SgCStack *cstk;
  SgContinuation *ep;
  /* struct offset_saver saver = {NULL, 0, NULL}; */

  if (!IN_STACK_P((SgObject*)c, vm)) return;

  do {
    SgContFrame *csave = save_a_cont(c);
    /* make the orig frame forwarded */
    if (prev) prev->prev = csave;

    prev = csave;
    tmp = c->prev;
    c->prev = csave;
    c->size = -1;
    c = tmp;
  } while (IN_STACK_P((SgObject*)c, vm));

  if (FORWARDED_CONT_P(vm->cont)) {
    vm->cont = FORWARDED_CONT(vm->cont);
  }
  for (cstk = vm->cstack; cstk; cstk = cstk->prev) {
    if (FORWARDED_CONT_P(cstk->cont)) {
      cstk->cont = FORWARDED_CONT(cstk->cont);
    }
  }
  for (ep = vm->escapePoint; ep; ep = ep->prev) {
    if (FORWARDED_CONT_P(ep->cont)) {
      ep->cont = FORWARDED_CONT(ep->cont);
    } 
  }
}

static void expand_stack(SgVM *vm)
{
  SgObject *p;

  if (SG_VM_LOG_LEVEL(vm, SG_WARN_LEVEL)) {
    Sg_Printf(vm->logPort,
	      UC("expanding stack (fp=%d, sp=%d)\n"),
	      FP(vm) - vm->stack, SP(vm) - vm->stack);
  }

  save_cont(vm);
  memmove(vm->stack, FP(vm), (SP(vm) - FP(vm))*sizeof(SgObject));
  SP(vm) -= FP(vm) - vm->stack;
  FP(vm) = vm->stack;
  /* for GC friendliness */
  for (p = SP(vm); p < vm->stackEnd; p++) *p = NULL;
}

#else

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

  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    print_frames(vm);
  }
}
#endif

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
#if USE_ONE_PATH_CALL_CC
  /* do nothing */
#else
  vm->sp = vm->stack + restore_stack(c->stack, vm->stack);
  vm->fp = vm->sp - argc;
#endif
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
  SgContinuation *cont;
  SgObject contproc;
  SgVM *vm = Sg_VM();
  
#if USE_ONE_PATH_CALL_CC
  save_cont(vm);
  cont = SG_NEW(SgContinuation);
#else
  Stack *stack;
  stack = save_stack(vm, SP(vm));
  cont = SG_NEW(SgContinuation);
  cont->stack = stack;
#endif
  cont->winders = vm->dynamicWinders;
  cont->cont = vm->cont;
  cont->cstack = vm->cstack;
  cont->prev = NULL;
  cont->ehandler = SG_FALSE;


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
  SgObject r = SG_NIL, cur = SG_NIL, prev = SG_UNDEF;
  SgContFrame *cont = CONT(vm);
  SgObject cl = CL(vm);
  SgWord *pc = PC(vm);
  int i;
  if (!cl) {
    /* before running */
    return SG_NIL;
  }
  if (vm->state == COMPILING || vm->state == IMPORTING) return SG_NIL;
  /* get current posision's src */

  for (i = 0;;) {
    if (SG_PROCEDUREP(cl)) {
      SgObject name = SG_PROCEDURE_NAME(cl);
      if (SG_EQ(prev, name)) goto next_cont;
      prev = name;
      switch (SG_PROCEDURE_TYPE(cl)) {
      case SG_PROC_SUBR:
	if (SG_FALSEP(name)) goto next_cont;
	r = SG_LIST3(SG_INTERN("*cproc*"), name, SG_NIL);
	break;
      case SG_PROC_CLOSURE:
	if (SG_CLOSURE(cl)->code
	    && SG_CODE_BUILDERP(SG_CLOSURE(cl)->code)) {
	  /* we need to check pc-1(for *CALL, or os) or pc-2(for GREF_*CALL) */
	  SgCodeBuilder *cb = SG_CODE_BUILDER(SG_CLOSURE(cl)->code);
	  InsnInfo *info;
	  SgObject src = SG_FALSE;
	  int index = -1, i;
	  /* search previous src, max 2 */
	  for (i = 0; i < 3; i++) {
	    info = Sg_LookupInsnName(INSN(*(pc-i)));
	    if (info && info->hasSrc) break;
	  }
	  /* for sanity */
	  if (info && info->hasSrc) {
	    index = (pc - i) - cb->code;
	  }
	  if (index > 0) {
	    src = Sg_Assv(SG_MAKE_INT(index), cb->src);
	  }
	  if (SG_FALSEP(src)) {
	    src = cb->src;
	  } else {
	    /* need to be alist */
	    src = SG_LIST1(src);
	  }
	  r = SG_LIST3(SG_INTERN("*proc*"), name, src);
	} else {
	  r = SG_LIST3(SG_INTERN("*proc*"), name, SG_NIL);
	}
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
      pc = cont->pc;
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
    if (c->rewindBefore) {
      target = c->winders;
      current = vm->dynamicWinders;
      for (hp = current; SG_PAIRP(hp) && (hp != target); hp = SG_CDR(hp)) {
	SgObject proc = SG_CDAR(hp);
	vm->dynamicWinders = SG_CDR(hp);
	Sg_Apply0(proc);
      }
    }
    vm->escapePoint = c->prev;
    SG_UNWIND_PROTECT {
      result = Sg_Apply1(c->ehandler, e);
      if (!c->rewindBefore) {
	target = c->winders;
	current = vm->dynamicWinders;
	for (hp = current; SG_PAIRP(hp) && (hp != target); hp = SG_CDR(hp)) {
	  SgObject proc = SG_CDAR(hp);
	  vm->dynamicWinders = SG_CDR(hp);
	  Sg_Apply0(proc);
	}
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

#define CALL_CCONT(p, v, d) p(v, d)

#define POP_CONT()							\
  do {									\
    /* if (CONT(vm)->fp == NULL) { */					\
    if (CONT(vm)->fp == C_CONT_MARK) {					\
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
      CONT(vm) = CONT(vm)->prev;					\
      /* (vm)->fpOffset = CALC_OFFSET(vm, 0); */			\
      AC(vm) = CALL_CCONT(after__, v__, data__);			\
    } else if (IN_STACK_P((SgObject*)CONT(vm), vm)) {			\
      SgContFrame *cont__ = CONT(vm);					\
      /* FP(vm) = (SgObject*)cont__ - cont__->fp; */			\
      FP(vm) = cont__->fp;						\
      SP(vm) = FP(vm) + cont__->size;					\
      PC(vm) = cont__->pc;						\
      CL(vm) = cont__->cl;						\
      CONT(vm) = cont__->prev;						\
    } else {								\
      int size__ = CONT(vm)->size;					\
      FP(vm) = SP(vm) = vm->stack;					\
      PC(vm) = CONT(vm)->pc;						\
      CL(vm) = CONT(vm)->cl;						\
      if (CONT(vm)->env && size__) {					\
	SgObject *s__ = CONT(vm)->env, *d__ = SP(vm);			\
	SP(vm) += size__;						\
	while (size__-- > 0) {						\
	  *d__++ = *s__++;						\
	}								\
      }									\
      CONT(vm) = CONT(vm)->prev;					\
    }									\
  } while (0)

static SgWord boundaryFrameMark = NOP;
#define BOUNDARY_FRAME_MARK_P(cont) ((cont)->pc == &boundaryFrameMark)

SgObject evaluate_safe(SgObject program, SgWord *code)
{
  SgCStack cstack;
  SgVM * volatile vm = Sg_VM();
  SgWord * volatile prev_pc = PC(vm);
  SgObject usave = vm->usageEnv, msave = vm->macroEnv;

  CHECK_STACK(CONT_FRAME_SIZE, vm);
  PUSH_CONT(vm, &boundaryFrameMark);
  FP(vm) = (SgObject*)CONT(vm) + CONT_FRAME_SIZE;

  ASSERT(SG_PROCEDUREP(program));
  CL(vm) = program;

  if (code != NULL) {
    PC(vm) = code;
  } else {
    ASSERT(SG_CLOSUREP(CL(vm)));
    PC(vm) = SG_CODE_BUILDER(SG_CLOSURE(CL(vm))->code)->code;
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
      POP_CONT();
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
	POP_CONT();
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
	POP_CONT();
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
  CLEAR_STACK(vm);
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
  SgObject *f = fp + m;
  for (i = m - 1; 0 <= i; i--) {
    INDEX_SET(f, i, INDEX(sp, i));
  }
  /* memmove(fp, sp-m, m*sizeof(SgObject)); */
  return f;
}

/* for call-next-method. is there not a better way? */
static inline SgObject* shift_one_args(SgObject *sp, int m)
{
  int i;
  SgObject *tsp = sp+1;
  for (i=0; i<m; i++) {
    INDEX_SET(tsp, i, INDEX(sp, i));
  }
  return tsp;
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

#define RET_INSN()						\
  do {								\
    if (CONT(vm) == NULL || BOUNDARY_FRAME_MARK_P(CONT(vm))) {	\
      /* no more continuation */				\
      return AC(vm);						\
    }								\
    POP_CONT();							\
  } while (0)							\

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
    if (!BOUNDARY_FRAME_MARK_P(cont)) {
      Sg_Printf(vm->logPort, UC("0x%x + size=%#38d +\n"),
		(uintptr_t)cont + offsetof(SgContFrame, size), cont->size);
      Sg_Printf(vm->logPort, UC("0x%x +   pc=%#38x +\n"),
		(uintptr_t)cont + offsetof(SgContFrame, pc), cont->pc);
      Sg_Printf(vm->logPort, UC("0x%x "), (uintptr_t)cont + offsetof(SgContFrame, cl));
      Sg_Format(vm->logPort, clfmt, SG_LIST1(cont->cl), TRUE);
      Sg_Printf(vm->logPort, UC("0x%x +   fp=%#38x +\n"),
		(uintptr_t)cont + offsetof(SgContFrame, fp), cont->fp);
      Sg_Printf(vm->logPort, UC("0x%x + prev=%#38x +\n"),
		(uintptr_t)cont + offsetof(SgContFrame, prev), cont->prev);
      if (cont == CONT(vm)) {
	Sg_Printf(vm->logPort, UC("0x%x +---------------------------------------------+ <== cont(0x%x)\n"), cont, cont);
      } else if (cont->prev) {
	Sg_Printf(vm->logPort, UC("0x%x +---------------------------------------------+ <== prev(0x%x)\n"), cont, cont);
      }
      if (cont->fp == C_CONT_MARK) c_func = TRUE;
      else c_func = FALSE;
	
      current = cont;
      size = cont->size;
      /* cont's size is argc of previous cont frame */
      /* dump arguments */
      if (IN_STACK_P((SgObject*)cont, vm)) {
	if (!c_func) {
	  for (i = 0; i < size; i++, current--) {
	    Sg_Printf(vm->logPort, UC("0x%x +   p=%#39x +\n"), current, *(current));
	  }
	}
      } else {
	if (!c_func) {
	  for (i = 0; i < size; i++) {
	    Sg_Printf(vm->logPort, UC("0x%x +   p=%#39x +\n"), cont->env+i, *(cont->env+i));
	  }
	}
	break;
      }
      cont = cont->prev;
      /* check if prev cont has arg or not. */
      continue;
    } else {
      for (i = 0; i < CONT_FRAME_SIZE; i++) {
	Sg_Printf(vm->logPort, UC("0x%x +   p=%#39x +\n"), current-i, *(current-i));
      }
      break;
    }
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
  vm->callCode[1] = SG_WORD(RET);

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
