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

#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
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
  Sg_RemoveLibrary(vm->currentLibrary);
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
  unsigned long sec, usec;
  int i;
  SG_SET_CLASS(v, SG_CLASS_VM);

  v->name = name;
  v->threadState = SG_VM_NEW;
  v->stack = SG_NEW_ARRAY(SgObject, SG_VM_STACK_SIZE);
  v->sp = v->fp = v->stack;
  v->stackEnd = v->stack + SG_VM_STACK_SIZE;
  v->cont = (SgContFrame *)v->sp;
  v->ac = SG_NIL;
  v->cl = NULL;
  for (i = 0; i < DEFAULT_VALUES_SIZE; i++) v->values[i] = SG_UNDEF;
  v->valuesCount = 1;

  v->attentionRequest = FALSE;
  v->finalizerPending = FALSE;
  v->stopRequest = FALSE;
  v->escapePoint = NULL;
  v->escapeReason = SG_VM_ESCAPE_NONE;
  v->escapeData[0] = NULL;
  v->escapeData[1] = NULL;
  v->defaultEscapeHandler = SG_FALSE;
  v->cache = SG_NIL;
  v->cstack = NULL;

  v->dynamicWinders = SG_NIL;
  v->parentExHandler = SG_FALSE;
  v->exceptionHandler = DEFAULT_EXCEPTION_HANDLER;

  v->sourceInfos = Sg_MakeWeakHashTableSimple(SG_HASH_EQ, SG_WEAK_KEY,
					      4000, SG_FALSE);
  v->commandLineArgs = SG_NIL;

  /* from proto */
  /* if proto was NULL, this will be initialized Sg__InitVM */
  v->currentLoadPath = SG_FALSE; /* should this be inherited from proto? */
  if (proto) {
    SgObject nl = 
      Sg_MakeChildLibrary(v, Sg_MakeSymbol(SG_MAKE_STRING("child"), FALSE));
    Sg_ImportLibrary(nl, proto->currentLibrary);
    SG_LIBRARY_DEFINEED(nl) = SG_FALSE;
    v->currentLibrary = nl;
    v->parameters = Sg_HashTableCopy(proto->parameters, TRUE);
  } else {
    v->currentLibrary = SG_UNDEF;
    v->parameters = Sg_MakeHashTableSimple(SG_HASH_EQ, 64);
  }
  /* child thread should not affect parent load-path*/
  v->loadPath = proto ? Sg_CopyList(proto->loadPath): SG_NIL;
  v->dynamicLoadPath = proto ? Sg_CopyList(proto->dynamicLoadPath): SG_NIL;
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

  v->currentLoadingPort = proto
    ? proto->currentLoadingPort
    : v->currentInputPort;

  v->logPort = proto ? proto->logPort : v->currentErrorPort;
  /* macro env */
  v->usageEnv = proto ? proto->usageEnv : SG_FALSE;
  v->macroEnv = proto ? proto->macroEnv : SG_FALSE;
  v->transEnv = v->history = SG_NIL;

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

#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
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
#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
  theVM = vm;
#else
  if (pthread_setspecific(the_vm_key, vm) != 0) return FALSE;
#endif
  return TRUE;
}

int Sg_MainThreadP()
{
  return theVM == rootVM;
}

#define Sg_VM() theVM

/* values  */
SgObject Sg_Values(SgObject args)
{
  return Sg_VMValues(theVM, args);
}

SgObject Sg_VMValues(SgVM *vm, SgObject args)
{
  SgObject cp;
  int nvals, len = -1, init = FALSE;
  if (!SG_PAIRP(args)) {
    vm->valuesCount = 0;
    return SG_UNDEF;
  }
  nvals = 1;

  SG_FOR_EACH(cp, SG_CDR(args)) {
    if (nvals < DEFAULT_VALUES_SIZE+1) {
      SG_VALUES_SET(vm, nvals-1, SG_CAR(cp));
    } else {
      if (len < 0) {
	len = Sg_Length(cp); /* get rest... */
      }
      if (!init) {
	if (!vm->extra_values || vm->extra_values->buffer_size < len) {
	  SG_ALLOC_VALUES_BUFFER(vm, len);
	}
	init = TRUE;
      }
      SG_VALUES_SET(vm, nvals-1, SG_CAR(cp));
    }
    nvals++;
  }
  vm->valuesCount = nvals;
  vm->ac = SG_CAR(args);
  return vm->ac;
}

SgObject Sg_Values2(SgObject v1, SgObject v2)
{
  return Sg_VMValues2(theVM, v1, v2);
}

SgObject Sg_VMValues2(SgVM *vm, SgObject v1, SgObject v2)
{
  vm->valuesCount = 2;
  vm->values[0] = v2;
  vm->ac = v1;
  return v1;
}

SgObject Sg_Values3(SgObject v1, SgObject v2, SgObject v3)
{
  return Sg_VMValues3(theVM, v1, v2, v3);
}

SgObject Sg_VMValues3(SgVM *vm, SgObject v1, SgObject v2, SgObject v3)
{
  vm->valuesCount = 3;
  vm->values[0] = v2;
  vm->values[1] = v3;
  vm->ac = v1;
  return v1;
}


/* some flags */
/* bench mark said, it does not make that much difference.
   and made call/cc so slow.
   maybe we need to do call/cc performance tuning first.
 */
#define CLEAN_STACK 1
/* #define PROF_INSN 1 */
/* #define SHOW_CALL_TRACE 1 */
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


static inline void report_error(SgObject exception, SgObject out)
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
	  info = Sg_WeakHashTableRef(SG_WEAK_HASHTABLE(vm->sourceInfos),
				     src, SG_FALSE);
	  /* info = SG_SOURCE_INFO(src); */
	}
	if (SG_FALSEP(info) || !info) {
	  Sg_Printf(buf,
		    UC("  [%A] %A\n"
		       "    src: %#50S\n"),
		    index, SG_CADR(proc),
		    Sg_UnwrapSyntax(src));
	} else {
	  file = SG_CAR(info);
	  line = SG_CDR(info);
	  Sg_Printf(buf,
		    UC("  [%A] %A\n"
		       "    src: %#50S\n"
		       "    %S:%A\n"),
		    index, SG_CADR(proc),
		    Sg_UnwrapSyntax(src),
		    file, line);
	}
      
      } else {
      no_src:
	/* *cproc* does not have any source info */
	Sg_Printf(buf,
		  UC("  [%A] %A\n"),
		  index, SG_CADR(proc));
      }
    }
  }
  Sg_Write(Sg_GetStringFromStringPort(buf), out, SG_WRITE_DISPLAY);
  Sg_FlushAllPort(FALSE);
}

void Sg_ReportError(SgObject e, SgObject out)
{
  SgVM *vm = Sg_VM();

  if (SG_VM_RUNTIME_FLAG_IS_SET(vm, SG_ERROR_BEING_REPORTED)) {
    Sg_Abort("Unhandled error occurred during reporting an error."
	     " Process aborted.\n");
  }
  SG_VM_RUNTIME_FLAG_SET(vm, SG_ERROR_BEING_REPORTED);
  SG_UNWIND_PROTECT {
    /* FIXME this is ugly... */
    if ((SG_EQ(vm->currentErrorPort, out) ||
	 SG_EQ(vm->currentOutputPort, out)) &&
	SG_PROCEDUREP(vm->defaultEscapeHandler)) {
      Sg_Apply1(vm->defaultEscapeHandler, e);
    } else {
      Sg_FlushAllPort(FALSE);
      report_error(e, out);
    }
  }
  SG_WHEN_ERROR {
    SG_VM_RUNTIME_FLAG_CLEAR(vm, SG_ERROR_BEING_REPORTED);
  }
  SG_END_PROTECT;
  SG_VM_RUNTIME_FLAG_CLEAR(vm, SG_ERROR_BEING_REPORTED);
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

#define write_indent()							\
  Sg_Write(SG_MAKE_STRING(";; "), vm->logPort, SG_WRITE_DISPLAY);	\
  for (ind = 0; ind < indent; ind++) {					\
    Sg_Write(SG_MAKE_CHAR(' '), vm->logPort, SG_WRITE_DISPLAY);		\
  }

  write_indent();
  Sg_Printf(vm->logPort, UC("size: %d\n"), size);
  for (i = 0; i < size;) {
    int need_line_break = TRUE;
    SgObject s;
    SgPort *out = SG_PORT(Sg_MakeStringOutputPort(-1));
    info = Sg_LookupInsnName(INSN(code[i]));

    write_indent();
    Sg_Printf(out, UC("%4d: %A"), i, Sg_MakeStringC(info->name));
    if (info->instValues != 0) {
      int val1, val2;
      switch (info->instValues) {
      case 1:
	INSN_VAL1(val1, code[i]);
	Sg_Printf(out, UC("(%d)"), val1);
	break;
      case 2:
	INSN_VAL2(val1, val2, code[i]);
	Sg_Printf(out, UC("(%d %d)"), val1, val2);
      }
    }
    if (info->argc != 0) {
      /* for now insn argument is only one */
      SgObject arg = SG_OBJ(code[i + 1]);
      if (SG_CODE_BUILDERP(arg)) {
	s = Sg_GetStringFromStringPort(out);
	Sg_Puts(vm->logPort, SG_STRING(s));
	Sg_Printf(vm->logPort, UC(" %S\n"), arg);
	vm_dump_code_rec(SG_CODE_BUILDER(arg), indent + 2);
	need_line_break = FALSE;
      } else {
	Sg_Printf(out, UC(" %#S"), arg);
	s = Sg_GetStringFromStringPort(out);
	Sg_Puts(vm->logPort, SG_STRING(s));
	if (info->hasSrc) {
	  if (SG_PAIRP(cb->src)) {
	    SgObject src = Sg_Assv(SG_MAKE_INT(i), cb->src);
	    int len = SG_STRING_SIZE(s);
	    for (; len<32; len++) {
	      Sg_Putc(vm->logPort, ' ');
	    }
	    if (!SG_FALSEP(src)) {
	      Sg_Printf(vm->logPort, UC("; %#30.1S"), 
			Sg_UnwrapSyntax(SG_CDR(src)));
	    }
	  }
	}
      }
    } else {
      s = Sg_GetStringFromStringPort(out);
      Sg_Puts(vm->logPort, SG_STRING(s));
    }
    if (need_line_break) {
      Sg_Printf(vm->logPort, UC("\n"));
    }
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

SgObject Sg_CurrentLoadingPort()
{
  SgVM *vm = Sg_VM();
  return vm->currentLoadingPort;
}

SgObject Sg_VMCurrentLibrary()
{
  return Sg_VM()->currentLibrary;
}

/* compiler */
SgObject Sg_Compile(SgObject o, SgObject e)
{
  static SgObject compiler = SG_UNDEF;
  SgObject r, save;
  /* compiler is initialized after VM. so we need to look it up first */
  if (SG_UNDEFP(compiler)) {
    SgObject compile_library;
    SgGloc *g;
    Sg_LockMutex(&global_lock);
    compile_library=Sg_FindLibrary(SG_INTERN("(sagittarius compiler)"), FALSE);
    g = Sg_FindBinding(compile_library, SG_INTERN("compile"), SG_FALSE);
    compiler = SG_GLOC_GET(g);
    Sg_UnlockMutex(&global_lock);
  }
  save = Sg_VM()->currentLibrary;
  Sg_VM()->history = SG_NIL;
  if (SG_LIBRARYP(e)) {
    Sg_VM()->currentLibrary = e;
  }
#define restore_vm()				\
  do {						\
    Sg_VM()->history = SG_NIL;			\
    Sg_VM()->currentLibrary = save;		\
  } while(0)

  SG_UNWIND_PROTECT {
    r = Sg_Apply2(compiler, o, e);
    restore_vm();
  }
  SG_WHEN_ERROR {
    restore_vm();
    SG_NEXT_HANDLER;
  }
  SG_END_PROTECT;

#undef restore_vm;
  return r;
}

/* 
   env: library for now.
 */
SgObject Sg_Eval(SgObject sexp, SgObject env)
{
  SgObject v = SG_NIL, c;
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
  c = Sg_MakeClosure(v, NULL);
  r = evaluate_safe(c, SG_CODE_BUILDER(v)->code);
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
  SgObject v = SG_NIL, body, before, after;
  SgVM *vm = theVM;

  if (vm->state != IMPORTING) vm->state = COMPILING;
  v = Sg_Compile(sexp, env);
  /* Now we are checking with this defined variable during compilation,
     and if a macro have eval in it blow resets the defined variables.
     to avoid it we need to keep it. */
  /* SG_LIBRARY_DEFINEED(vm->currentLibrary) = SG_NIL; */
  /* store cache */
  if (vm->state == IMPORTING) {
    SG_SET_CAR(vm->cache, Sg_Cons(v, SG_CAR(vm->cache)));
  }
  if (vm->state != IMPORTING) vm->state = RUNNING;
  CLEAR_STACK(vm);

  ASSERT(SG_CODE_BUILDERP(v));
  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    Sg_VMDumpCode(v);
  }
  vm->valuesCount = 1;

  body = Sg_MakeClosure(v, NULL);
  if (!SG_FALSEP(env)) {
    before = Sg_MakeSubr(eval_restore_env, env, 0, 0, SG_FALSE);
  } else {
    before = Sg_NullProc();
  }
  after = Sg_MakeSubr(eval_restore_env, vm->currentLibrary, 0, 0, SG_FALSE);
  return Sg_VMDynamicWind(before, body, after);
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

/* it does not improve performance */
/* #ifdef __GNUC__ */
#if 0
#define MOSTLY_FALSE(expr) __builtin_expect(!!(expr), FALSE)
#else
#define MOSTLY_FALSE(expr) expr
#endif

#define CHECK_STACK(size, vm)					\
  do {								\
    if (MOSTLY_FALSE(SP(vm) >= (vm)->stackEnd - (size))) {	\
      expand_stack(vm);						\
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
  cc->fp = C_CONT_MARK;
  cc->cl = CL(vm);
  for (i = 0; i < datasize; i++) {
    PUSH(s, SG_OBJ(data[i]));
  }
  CONT(vm) = cc;
  FP(vm) = SP(vm) = s;
}

/* #define USE_LIGHT_WEIGHT_APPLY 1 */

#define PUSH_CONT(vm, next_pc)				\
  do {							\
    SgContFrame *newcont = (SgContFrame*)SP(vm);	\
    newcont->prev = CONT(vm);				\
    newcont->size = (int)(SP(vm) - FP(vm));		\
    newcont->pc = next_pc;				\
    newcont->cl = CL(vm);				\
    newcont->fp = FP(vm);				\
    CONT(vm) = newcont;					\
    SP(vm) += CONT_FRAME_SIZE;				\
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

/* dummy closure */
static SgClosure internal_toplevel_closure =
  { SG__PROCEDURE_INITIALIZER(SG_CLASS_STATIC_TAG(Sg_ProcedureClass),
			      0, 0, SG_PROC_CLOSURE, SG_FALSE, SG_FALSE),
    SG_FALSE,};

static SgObject apply_rec(SgVM *vm, SgObject proc, SgObject rest, int nargs)
{
  SgObject program;
  SgWord code[3];
  code[0] = SG_WORD(MERGE_INSN_VALUE1(APPLY_VALUES, nargs));
  code[1] = SG_WORD(rest);
  code[2] = SG_WORD(RET);

  AC(vm) = proc;
  program = (CL(vm)) ? CL(vm) : SG_OBJ(&internal_toplevel_closure);
  return evaluate_safe(program, code);  
}


SgObject Sg_Apply0(SgObject proc)
{
  return apply_rec(theVM, proc, SG_NIL, 0);
}

SgObject Sg_Apply1(SgObject proc, SgObject arg)
{
  SgVM *vm = theVM;
  vm->values[0] = arg;
  return apply_rec(theVM, proc, SG_NIL, 1);
}

SgObject Sg_Apply2(SgObject proc, SgObject arg0, SgObject arg1)
{
  SgVM *vm = theVM;
  vm->values[0] = arg0;
  vm->values[1] = arg1;
  return apply_rec(theVM, proc, SG_NIL, 2);
}

SgObject Sg_Apply3(SgObject proc, SgObject arg0, SgObject arg1, SgObject arg2)
{
  SgVM *vm = theVM;
  vm->values[0] = arg0;
  vm->values[1] = arg1;
  vm->values[2] = arg2;
  return apply_rec(theVM, proc, SG_NIL, 3);
}

SgObject Sg_Apply4(SgObject proc, SgObject arg0, SgObject arg1,
		   SgObject arg2, SgObject arg3)
{
  SgVM *vm = theVM;
  vm->values[0] = arg0;
  vm->values[1] = arg1;
  vm->values[2] = arg2;
  vm->values[3] = arg3;
  return apply_rec(theVM, proc, SG_NIL, 4);
}

SgObject Sg_Apply(SgObject proc, SgObject args)
{
  SgVM *vm = theVM;
  int nargs = Sg_Length(args), i;
  if (nargs < 0) {
    Sg_Error(UC("improper list not allowed: %S"), args);
  }

  for (i = 0; i < nargs; i++) {
    if (i == DEFAULT_VALUES_SIZE) break;
    vm->values[i] = SG_CAR(args);
    args = SG_CDR(args);
  }
  return apply_rec(vm, proc, args, nargs);
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
  void *d[3];
  SgVM *vm = Sg_VM();
  
  vm->dynamicWinders = prev;
  d[0] = (void*)result;
  d[1] = (void*)(intptr_t)vm->valuesCount;
  if (vm->valuesCount > 1) {
    SgObject *array = SG_NEW_ARRAY(SgObject, vm->valuesCount - 1);
    int i;
    for (i = 0; i < vm->valuesCount-1; i++) {
      array[i] = SG_VALUES_REF(vm, i);
    }
    d[2] = array;
  }
  Sg_VMPushCC(dynamic_wind_after_cc, d, 3);
  return Sg_VMApply0(after);
}

static SgObject dynamic_wind_after_cc(SgObject result, void **data)
{
  SgObject ac = SG_OBJ(data[0]);
  int nvals = (int)(intptr_t)(data[1]);
  SgVM *vm = theVM;
  vm->valuesCount = nvals;
  if (nvals > 1) {
    int i;
    SgObject *array = (SgObject*)data[2];
    for (i = 0; i < nvals-1; i++) {
      SG_VALUES_SET(vm, i, array[i]);
    }
  }
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
  SgObject before  = Sg_MakeSubr(install_xhandler, parent, 0, 0, SG_FALSE);
  SgObject after   = Sg_MakeSubr(install_xhandler, save, 0, 0, SG_FALSE);
  SgObject thunk   = Sg_MakeSubr(handler_runner, Sg_Cons(handler, args[0]),
				 0, 0, SG_FALSE);
  return Sg_VMDynamicWind(before, thunk, after);
}

SgObject Sg_VMWithExceptionHandler(SgObject handler, SgObject thunk)
{
  SgVM *vm = Sg_VM();
  SgObject parent = vm->exceptionHandler;
  SgObject psave  = vm->parentExHandler;
  SgObject csave  = vm->exceptionHandler;
  SgObject new_current = Sg_MakeSubr(handler_body, Sg_Cons(parent, handler),
				     1, 0, SG_FALSE);
  SgObject before      = Sg_MakeSubr(install_xhandler,
				     Sg_Cons(parent, new_current),
				     0, 0, SG_FALSE);
  SgObject after       = Sg_MakeSubr(install_xhandler, Sg_Cons(psave, csave),
				     0, 0, SG_FALSE);
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
  c->floating = SG_VM_FLOATING_EP(vm);
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


static SgWord boundaryFrameMark = NOP;
#define BOUNDARY_FRAME_MARK_P(cont) ((cont)->pc == &boundaryFrameMark)

#define SKIP(vm, n)        (PC(vm) += (n))
#define FETCH_OPERAND(pc)  SG_OBJ((*(pc)++))
#define PEEK_OPERAND(pc)   SG_OBJ((*(pc)))

#define REFER_LOCAL(vm, n)   *(FP(vm) + n)
#define INDEX_CLOSURE(vm, n)  SG_CLOSURE(CL(vm))->frees[n]
#define REFER_GLOBAL(vm, ret)						\
  do {									\
    ret = FETCH_OPERAND(PC(vm));					\
    if (SG_GLOCP(ret)) {						\
      ret = SG_GLOC_GET(SG_GLOC(ret));					\
    } else {								\
      SgObject lib = SG_IDENTIFIER_LIBRARY(ret);			\
      SgObject value = Sg_FindBinding(SG_FALSEP(lib)			\
				      ? vm->currentLibrary : lib,	\
				      SG_IDENTIFIER_NAME(ret),		\
				      SG_UNBOUND);			\
      if (SG_GLOCP(value)) {						\
	ret = SG_GLOC_GET(SG_GLOC(value));				\
	*(PC(vm)-1) = SG_WORD(value);					\
      } else {								\
	Sg_UndefinedViolation((ret),					\
			      Sg_Sprintf(UC("unbound variable %S"),	\
					 SG_IDENTIFIER_NAME(ret)));	\
      }									\
    }									\
  } while (0)


#define FORWARDED_CONT_P(c) ((c)&&((c)->size == -1))
#define FORWARDED_CONT(c)   ((c)->prev)

/*
  save a cont frame with its arguments.
 */
static SgContFrame* save_a_cont(SgContFrame *c)
{
  SgObject *s, *d;
  int i;
  const size_t argsize = (c->size > 0) ? (c->size * sizeof(SgObject)) : 0;
  const size_t size = sizeof(SgContFrame) + argsize;
  SgContFrame *csave = SG_NEW2(SgContFrame *, size);

  /* copy cont frame */
  if (c->fp != C_CONT_MARK) {
    *csave = *c;		/* copy the frame */
    if (c->size > 0) {
      /* copy the args */
      s = (SgObject*)c - c->size;
      d = csave->env;
      for (i = 0; i < c->size; i++) {
	*d++ = *s++;
      }
    }
  } else {
    /* C continuation */
    s = (SgObject*)c;
    d = (SgObject*)csave;
    for (i = CONT_FRAME_SIZE + c->size; i > 0; i--) {
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
static void save_cont_rec(SgVM *vm, int partialP)
{
  SgContFrame *c = CONT(vm), *prev = NULL, *tmp;
  SgCStack *cstk;
  SgContinuation *ep;

  if (IN_STACK_P((SgObject*)c, vm)) {
    do {
      SgContFrame *csave;
      if (partialP && BOUNDARY_FRAME_MARK_P(c)) break;
      csave = save_a_cont(c);
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
    for (ep = SG_VM_FLOATING_EP(vm); ep; ep = ep->floating) {
      if (FORWARDED_CONT_P(ep->cont)) {
	ep->cont = FORWARDED_CONT(ep->cont);
      } 
    }
  }
}

static void save_cont(SgVM *vm)
{
  save_cont_rec(vm, FALSE);
}

static void save_partial_cont(SgVM *vm)
{
  save_cont_rec(vm, TRUE);
}

static void expand_stack(SgVM *vm)
{
  SgObject *p;

  if (SG_VM_LOG_LEVEL(vm, SG_WARN_LEVEL)) {
    Sg_Printf(vm->logPort,
	      UC(";; expanding stack in %S (fp=%d, sp=%d)\n"),
	      CL(vm), FP(vm) - vm->stack, SP(vm) - vm->stack);
  }

  save_cont(vm);
  memmove(vm->stack, FP(vm), (SP(vm) - FP(vm))*sizeof(SgObject));
  SP(vm) -= FP(vm) - vm->stack;
  FP(vm) = vm->stack;

  /* for GC friendliness */
  for (p = SP(vm); p < vm->stackEnd; p++) *p = NULL;
}

static SgWord return_code[1] = {SG_WORD(RET)};

#define PC_TO_RETURN return_code

static SgObject throw_continuation_cc(SgObject, void **);

static SgObject throw_continuation_body(SgObject handlers,
					SgContinuation *c,
					SgObject args)
{
  SgVM *vm = Sg_VM();
  /* (if (not (eq? new (current-dynamic-winders))) perform-dynamic-wind) */
  if (SG_PAIRP(handlers)) {
    SgObject handler, chain;
    void *data[3];
    handler = SG_CAAR(handlers);
    chain = SG_CDAR(handlers);
    data[0] = (void*)SG_CDR(handlers);
    data[1] = (void*)c;
    data[2] = (void*)args;
    Sg_VMPushCC(throw_continuation_cc, data, 3);
    vm->dynamicWinders = chain;
    return Sg_VMApply0(handler);
  } else {
    /* 
       if the target continuation is a full continuation, we can abandon
       the current continuation. however, if the target continuation is
       partial, we must return to the current continuation after executing
       the partial continuation.
    */
    if (c->cstack == NULL) save_cont(vm);

    vm->cont = c->cont;
    vm->pc = return_code;
    vm->dynamicWinders = c->winders;

    /* store arguments of the continuation to ac */
    if (SG_NULLP(args)) {		/* no value */
      /* does this happen? */
      vm->ac = SG_UNDEF;
      vm->valuesCount = 0;
    } else if (SG_NULLP(SG_CDR(args))) { /* usual case */
      vm->ac = SG_CAR(args);
      vm->valuesCount = 1;
    } else {			/* multi values */
      SgObject ap;
      int argc = Sg_Length(args), i;
      /* when argc == DEFAULT_VALUES_SIZE+1, it must be in pre-allocated
	 buffer */
      if (argc > DEFAULT_VALUES_SIZE+1) {
	SG_ALLOC_VALUES_BUFFER(vm, argc - DEFAULT_VALUES_SIZE -1);
      }
      vm->ac = SG_CAR(args);
      for (i = 0, ap = SG_CDR(args); SG_PAIRP(ap); i++, ap = SG_CDR(ap)) {
	SG_VALUES_SET(vm, i, SG_CAR(ap));
      }
      vm->valuesCount = argc;
    }
  
    return vm->ac;
  }
}
static SgObject throw_continuation_cc(SgObject result, void **data)
{
  SgObject handlers = SG_OBJ(data[0]);
  SgContinuation *c = (SgContinuation*)data[1];
  SgObject args = SG_OBJ(data[2]);
  return throw_continuation_body(handlers, c, args);
}

/* remove and re-order continuation's handlers */
static SgObject remove_common_winders(SgObject current, SgObject escapes)
{
  SgObject r = SG_NIL, p;
  SG_FOR_EACH(p, escapes) {
    if (SG_FALSEP(Sg_Memq(SG_CAR(p), current))) {
      r = Sg_Cons(SG_CAR(p), r);
    }
  }
  return r;
}

static SgObject throw_continuation_calculate_handlers(SgContinuation *c,
						      SgVM *vm)
{
  SgObject current = vm->dynamicWinders;
  SgObject target = remove_common_winders(current, c->winders);
  SgObject h = SG_NIL, t = SG_NIL, p;

  SG_FOR_EACH(p, current) {
    if (!SG_FALSEP(Sg_Memq(SG_CAR(p), c->winders))) break;
    SG_APPEND1(h, t, Sg_Cons(SG_CDAR(p), SG_CDR(p)));
  }
  SG_FOR_EACH(p, target) {
    SgObject chain = Sg_Memq(SG_CAR(p), c->winders);
    SG_APPEND1(h, t, Sg_Cons(SG_CAAR(p), SG_CDR(chain)));
  }
  return h;
}

static SgObject throw_continuation(SgObject *argframes, int argc, void *data)
{
  SgContinuation *c = (SgContinuation*)data;
  SgObject handlers_to_call;
  SgVM *vm = Sg_VM();

  if (c->cstack && vm->cstack != c->cstack) {
    SgCStack *cs;
    for (cs = vm->cstack; cs; cs = cs->prev) {
      if (c->cstack == cs) break;
    }
    if (cs != NULL) {
      vm->escapeReason = SG_VM_ESCAPE_CONT;
      vm->escapeData[0] = c;
      vm->escapeData[1] = argframes[0];
      longjmp(vm->cstack->jbuf, 1);
    }
  }

  handlers_to_call = throw_continuation_calculate_handlers(c, vm);
  return throw_continuation_body(handlers_to_call, c, argframes[0]);
}

SgObject Sg_VMCallCC(SgObject proc)
{
  SgContinuation *cont;
  SgObject contproc;
  SgVM *vm = Sg_VM();
  
  save_cont(vm);
  cont = SG_NEW(SgContinuation);
  cont->winders = vm->dynamicWinders;
  cont->cont = vm->cont;
  cont->cstack = vm->cstack;
  cont->prev = NULL;
  cont->ehandler = SG_FALSE;


  contproc = Sg_MakeSubr(throw_continuation, cont, 0, 1,
			 SG_MAKE_STRING("continucation"));
  return Sg_VMApply1(proc, contproc);
}

/*
  call with partial contnuation.
 */
SgObject Sg_VMCallPC(SgObject proc)
{
  SgContinuation *cont;
  SgContFrame *c, *cp;
  SgObject contproc;
  SgVM *vm = Sg_VM();

  /*
    save the continuation.
   */
  save_partial_cont(vm);
  for (c = vm->cont, cp = NULL;
       c && !BOUNDARY_FRAME_MARK_P(c);
       cp = c, c = c->prev)
    /* do nothing */;

  if (cp != NULL) cp->prev = NULL; /* cut the dynamic chain */

  cont = SG_NEW(SgContinuation);
  cont->winders = vm->dynamicWinders;
  cont->cont = (cp? vm->cont : NULL);
  cont->prev = NULL;
  cont->ehandler = SG_FALSE;
  cont->cstack = NULL;		/* so that the partial continuation can be
				   run on any cstack state. */


  contproc = Sg_MakeSubr(throw_continuation, cont, 0, 1,
			 SG_MAKE_STRING("partial continucation"));
  /* Remove the saved continuation chain */
  vm->cont = c;
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
  if (SG_STRING_SIZE(path) != 0) {
    vm->loadPath = Sg_Cons(replace_file_separator(path), vm->loadPath);
  }
  return vm->loadPath;
}

SgObject Sg_AddDynamicLoadPath(SgString *path)
{
  SgVM *vm = Sg_VM();
  if (SG_STRING_SIZE(path) != 0) {
    vm->dynamicLoadPath = Sg_Cons(replace_file_separator(path),
				  vm->dynamicLoadPath);
  }
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
	  intptr_t index = -1, i;
	  if (SG_FALSEP(name)) {
	    /* try codebuilder name */
	    name = Sg_CodeBuilderFullName(cb);
	  }
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
	    if (SG_PAIRP(cb->src)) {
	      src = Sg_Assv(SG_MAKE_INT(index), cb->src);
	    }
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
      default: break;		/* do nothing */
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
					       Sg_MakeMessageCondition(SG_MAKE_STRING("returned from non-continuable exception")),
					       Sg_MakeIrritantsCondition(SG_LIST1(exception)))));
      }
      vm->exceptionHandler = DEFAULT_EXCEPTION_HANDLER;
      Sg_Error(UC("error in raise: returned from non-continuable exception\n\n"
		  "irritants:\n%A"), exception);
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
    SgObject result = SG_FALSE, dvals[DEFAULT_VALUES_SIZE], *rvals;
    SgObject target, current;
    int valscount = 0, i, ext_count = 0;
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
    SG_VM_FLOATING_EP_SET(vm, c);

    rvals = dvals;
    SG_UNWIND_PROTECT {
      result = Sg_Apply1(c->ehandler, e);
      if ((valscount = vm->valuesCount) > 1) {
	if (valscount > DEFAULT_VALUES_SIZE+1) {
	  rvals = SG_NEW_ARRAY(SgObject, valscount -1);
	}
	for (i = 0; i < valscount - 1; i++) {
	  rvals[i] = SG_VALUES_REF(vm, i);
	}
      }
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
      SG_VM_FLOATING_EP_SET(vm, c->floating);
      SG_NEXT_HANDLER;
    }
    SG_END_PROTECT;

    /* install the continuation */
    if (valscount > DEFAULT_VALUES_SIZE+1) {
      SG_ALLOC_VALUES_BUFFER(vm, ext_count);
    }
    for (i = 0; i < valscount-1; i++) SG_VALUES_SET(vm, i, rvals[i]);

    vm->ac = result;
    vm->cont = c->cont;
    SG_VM_FLOATING_EP_SET(vm, c->floating);
    if (c->errorReporting) {
      SG_VM_RUNTIME_FLAG_SET(vm, SG_ERROR_BEING_REPORTED);
    }
  } else {
    Sg_ReportError(e, vm->currentErrorPort);
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

#define TAIL_POS(vm)  (*PC(vm) == RET)

#define POP_CONT()							\
  do {									\
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
      AC(vm) = after__(v__, data__);					\
    } else if (IN_STACK_P((SgObject*)CONT(vm), vm)) {			\
      SgContFrame *cont__ = CONT(vm);					\
      CONT(vm) = cont__->prev;						\
      PC(vm) = cont__->pc;						\
      CL(vm) = cont__->cl;						\
      FP(vm) = cont__->fp;						\
      SP(vm) = FP(vm) + cont__->size;					\
    } else {								\
      int size__ = CONT(vm)->size;					\
      FP(vm) = SP(vm) = vm->stack;					\
      PC(vm) = CONT(vm)->pc;						\
      CL(vm) = CONT(vm)->cl;						\
      if (size__) {							\
	SgObject *s__ = CONT(vm)->env, *d__ = SP(vm);			\
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
    } else if (vm->cont == NULL) {
      /* we're finished with executing partial continuation */
      vm->cont = cstack.cont;
      POP_CONT();
      PC(vm) = prev_pc;
    }
    /* FIXME
       We actually have ghost continuation problem. if we raise an error
       here then SRFI-41 test can not pass. But I have no idea where.
       so for now just ignore... */
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
  evaluate_safe(SG_OBJ(&internal_toplevel_closure),
		SG_CODE_BUILDER(toplevel)->code);
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
  int i;
  SgObject cp;
  SgVM *vm = Sg_VM();
  vm->ac = data[0];
  vm->valuesCount = (int)(intptr_t)data[1];
  if (vm->valuesCount > 1) {
    for (i=0,cp=SG_OBJ(data[2]); i<vm->valuesCount-1; i++, cp=SG_CDR(cp)) {
      SG_VALUES_SET(vm, i, SG_CAR(cp));
    }
  }
  if (vm->valuesCount < DEFAULT_VALUES_SIZE) {
    vm->extra_values = NULL;
  }
  return vm->ac;
}

static void process_queued_requests(SgVM *vm)
{
  void *data[3];
  /* preserve the current continuation */
  data[0] = (void*)vm->ac;
  data[1] = (void*)(intptr_t)vm->valuesCount;
  if (vm->valuesCount > 1) {
    int i;
    SgObject h = SG_NIL, t = SG_NIL;
    for (i = 0; i < vm->valuesCount-1; i++) {
      SG_APPEND1(h, t, SG_VALUES_REF(vm, i));
    }
    data[2] = h;
  }

  Sg_VMPushCC(process_queued_requests_cc, data, 3);
  
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
  int size = cont->size, c_func = FALSE;
  /* SgString *fmt   = SG_MAKE_STRING("+    o=~38,,,,39a +~%"); */
  SgString *clfmt = SG_MAKE_STRING("+   cl=~38,,,,39s +~%");

  Sg_Printf(vm->logPort, UC(";; stack: 0x%x, cont: 0x%x\n"), stack, cont);
  Sg_Printf(vm->logPort, UC(";; 0x%x +---------------------------------------------+ < sp\n"), sp);
  /* first we dump from top until cont frame. */
  while ((stack < current && current <= sp)) {
    if (current == (SgObject*)cont + CONT_FRAME_SIZE) {
      break;
    }
    Sg_Printf(vm->logPort, UC(";; 0x%x +   p=%#39x +\n"), current, *current);
    current--;
  }
  /* now we know we just need to trace cont frames
     memo: if cont has let frame, we just dump it as pointer.
   */
  while (stack < current && current <= sp) {
    int i;
    /* the very first arguments are ignored */
    while (current > (SgObject *)cont + CONT_FRAME_SIZE) {
      Sg_Printf(vm->logPort, UC(";; 0x%x +   p=%#39x +\n"), current, *current);
      current--;
    }
    Sg_Printf(vm->logPort, UC(";; 0x%x +   p=%#39x +\n"), current, *current);

    /* todo, dump display closure. */
    Sg_Printf(vm->logPort, UC(";; 0x%x +---------------------------------------------+\n"), current);
    if (!BOUNDARY_FRAME_MARK_P(cont)) {
      Sg_Printf(vm->logPort, UC(";; 0x%x + size=%#38d +\n"),
		(uintptr_t)cont + offsetof(SgContFrame, size), cont->size);
      Sg_Printf(vm->logPort, UC(";; 0x%x +   pc=%#38x +\n"),
		(uintptr_t)cont + offsetof(SgContFrame, pc), cont->pc);
      Sg_Printf(vm->logPort, UC(";; 0x%x "), (uintptr_t)cont + offsetof(SgContFrame, cl));
      Sg_Format(vm->logPort, clfmt, SG_LIST1(cont->cl), TRUE);
      Sg_Printf(vm->logPort, UC(";; 0x%x +   fp=%#38x +\n"),
		(uintptr_t)cont + offsetof(SgContFrame, fp), cont->fp);
      Sg_Printf(vm->logPort, UC(";; 0x%x + prev=%#38x +\n"),
		(uintptr_t)cont + offsetof(SgContFrame, prev), cont->prev);
      if (cont == CONT(vm)) {
	Sg_Printf(vm->logPort, UC(";; 0x%x +---------------------------------------------+ < cont\n"), cont);
      } else if (cont->prev) {
	Sg_Printf(vm->logPort, UC(";; 0x%x +---------------------------------------------+ < prev\n"), cont);
      }
      if (cont->fp == C_CONT_MARK) c_func = TRUE;
      else c_func = FALSE;
	
      current = (SgObject*)cont;
      size = cont->size;
      /* cont's size is argc of previous cont frame */
      /* dump arguments */
      if (IN_STACK_P((SgObject*)cont, vm)) {
	if (!c_func) {
	  for (i = 0; i < size; i++, current--) {
	    Sg_Printf(vm->logPort, UC(";; 0x%x +   p=%#39x +\n"), current, *(current));
	  }
	}
      } else {
	if (!c_func) {
	  for (i = 0; i < size; i++) {
	    Sg_Printf(vm->logPort, UC(";; 0x%x +   p=%#39x +\n"), cont->env+i, *(cont->env+i));
	  }
	}
	break;
      }
      cont = cont->prev;
      /* check if prev cont has arg or not. */
      continue;
    } else {
      for (i = 0; i < CONT_FRAME_SIZE; i++) {
	Sg_Printf(vm->logPort, UC(";; 0x%x +   p=%#39x +\n"), current-i, *(current-i));
      }
      break;
    }
  }
  Sg_Printf(vm->logPort, UC(";; 0x%x +---------------------------------------------+\n"), stack);
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

#define NEXT1					\
  do {						\
    vm->valuesCount = 1;			\
    NEXT;					\
  } while (0)

#ifdef _MSC_VER
# pragma warning( push )
# pragma warning( disable : 4102 4101)
#endif

SgObject run_loop()
{
  SgVM *vm = Sg_VM();
  
#ifdef __GNUC__
  static void *dispatch_table[INSTRUCTION_COUNT] = {
#define DEFINSN(insn, vals, argc, src, label) && SG_CPP_CAT(LABEL_, insn),
#include "vminsn.c"
#undef DEFINSN
  };
#endif	/* __GNUC__ */

  for (;;) {
    SgWord c;

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
#ifdef _MSC_VER
# pragma warning( pop )
#endif

void Sg__InitVM()
{
  /* this env is p1env and it must be 4 elements vector for now. */
  SgObject initialEnv = Sg_MakeVector(4, SG_UNDEF);
#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
  rootVM = theVM = Sg_NewVM(NULL, SG_MAKE_STRING("root"));
#else
  if (pthread_key_create(&the_vm_key, NULL) != 0) {
    Sg_Panic("pthread_key_create failed.");
  }
  rootVM = Sg_NewVM(NULL, SG_MAKE_STRING("root"));
  Sg_SetCurrentVM(rootVM);
#endif
  Sg_SetCurrentThread(&rootVM->thread);
  rootVM->threadState = SG_VM_RUNNABLE;
  rootVM->currentLibrary = Sg_FindLibrary(SG_INTERN("user"), TRUE);
  /* mark as this is toplevel library. */
  SG_LIBRARY_DEFINEED(rootVM->currentLibrary) = SG_FALSE;
  
  /* env */
  SG_VECTOR_ELEMENT(initialEnv, 0) = rootVM->currentLibrary;
  SG_VECTOR_ELEMENT(initialEnv, 1) = SG_NIL;
  rootVM->usageEnv = initialEnv;
  rootVM->macroEnv = initialEnv;

  /* load path */
  rootVM->loadPath = Sg_GetDefaultLoadPath();
  rootVM->dynamicLoadPath = Sg_GetDefaultDynamicLoadPath();

  SG_PROCEDURE_NAME(&default_exception_handler_rec) =
    SG_MAKE_STRING("default-exception-handler");
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
