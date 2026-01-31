/* vm.c                                            -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2021  Takashi Kato <ktakashi@ymail.com>
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
#include <stddef.h>
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/private/vm.h"
#include "sagittarius/private/bignum.h"
#include "sagittarius/private/code.h"
#include "sagittarius/private/core.h"
#include "sagittarius/private/closure.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/file.h"
#include "sagittarius/private/generic.h"
#include "sagittarius/private/hashtable.h"
#include "sagittarius/private/identifier.h"
#include "sagittarius/private/kernel.h"
#include "sagittarius/private/library.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/port.h"
#include "sagittarius/private/transcoder.h"
#include "sagittarius/private/reader.h"
#include "sagittarius/private/string.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/instruction.h"
#include "sagittarius/private/writer.h"
#include "sagittarius/private/number.h"
#include "sagittarius/private/macro.h"
#include "sagittarius/private/values.h"
#include "sagittarius/private/vector.h"
#include "sagittarius/private/compare.h"
#include "sagittarius/private/system.h"
#include "sagittarius/private/exceptions.h"
#include "sagittarius/private/profiler.h"
#include "sagittarius/private/gloc.h"
#include "sagittarius/private/weak.h"
#include "sagittarius/private/thread.h"
#include "sagittarius/private/unicode.h"

static SgInternalMutex global_lock;

static SgKernel *root = NULL;	/* main kernel */

#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
static __declspec(thread) SgVM *theVM;
#else
#include <pthread.h>
static pthread_key_t the_vm_key;
#define theVM ((SgVM*)pthread_getspecific(the_vm_key))
#endif

static SgSubr default_exception_handler_rec;
static SgPair default_exception_handler = {
  SG_OBJ(&default_exception_handler_rec),
  SG_NIL,
  SG_NIL
};
#define DEFAULT_EXCEPTION_HANDLER SG_OBJ(&default_exception_handler)

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

static SgObject evaluate_safe(SgObject program, SgWord *code);
static SgObject run_loop();

static void vm_finalize(SgObject obj, void *data)
{
  SgVM *vm = SG_VM(obj);
  Sg_RemoveLibrary(vm->currentLibrary);
  Sg_DestroyMutex(&vm->vmlock);
  Sg_DestroyCond(&vm->cond);
#ifdef _WIN32
  CloseHandle((&vm->thread)->event);
#endif  
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

static SgObject copy_generics(SgObject lib)
{
  SgObject h = SG_NIL, t = SG_NIL, gs;
  /* copying cdr part of slot of alist.
     adding method is done by destructively to avoid
     to affect it in parent thread. */
  SG_FOR_EACH(gs, SG_LIBRARY_GENERICS(lib)) {
    SgObject g = SG_CAR(gs);
    /* well, it won't hurt anyway :) */
    SG_APPEND1(h, t, Sg_CopyList(g));
  }
  return h;
}

SgVM* Sg_NewThreadVM(SgVM *proto, SgObject name)
{
  SgVM *v = SG_NEW(SgVM);
  int i;
  SG_SET_CLASS(v, SG_CLASS_VM);

  v->name = name;
  v->threadErrorP = FALSE;
  v->threadState = SG_VM_NEW;
  for (i = 0; i < DEFAULT_VALUES_SIZE; i++) v->values[i] = SG_UNDEF;
  v->valuesCount = 1;

  v->attentionRequest = FALSE;
  v->finalizerPending = FALSE;
  v->stopRequest = FALSE;
  v->escapePoint = NULL;
  v->escapeReason = SG_VM_ESCAPE_NONE;
  v->escapeData[0] = NULL;
  v->escapeData[1] = NULL;
  v->cache = SG_NIL;
  v->cstack = NULL;
  v->prompts = NULL;

  v->dynamicWinders = SG_NIL;
  v->exceptionHandlers = DEFAULT_EXCEPTION_HANDLER;

  /* from proto */
  /* if proto was NULL, this will be initialized Sg__InitVM */
  if (proto) {
    SgObject nl = 
      Sg_MakeChildLibrary(v, Sg_MakeSymbol(SG_MAKE_STRING("child"), FALSE));
    v->kernel = proto->kernel;	/* use the proto's kernel */

    Sg_ImportLibrary(nl, proto->currentLibrary);
    SG_LIBRARY_DEFINEED(nl) = SG_FALSE;
    v->currentLibrary = nl;
    v->parameters = Sg_WeakHashTableCopy(proto->parameters);
    SG_LIBRARY_GENERICS(nl) = copy_generics(proto->currentLibrary);
    if (!SG_FALSEP(proto->sandbox)) {
      v->sandbox = Sg_HashTableCopy(proto->sandbox, TRUE);
    } else {
      v->sandbox = SG_FALSE;
    }
  } else {
    v->kernel = root;		/* should never happen with my usage */
    v->currentLibrary = SG_UNDEF;
    v->parameters = Sg_MakeWeakHashTableSimple(SG_HASH_EQ, SG_WEAK_KEY, 64, 
					       SG_FALSE);
    v->sandbox = SG_FALSE;
  }
  /* child thread should not affect parent load-path*/
  v->loadPath = proto ? Sg_CopyList(proto->loadPath): SG_NIL;
  v->dynamicLoadPath = proto ? Sg_CopyList(proto->dynamicLoadPath): SG_NIL;
  /* default no overwrite */
  v->flags = proto? proto->flags : 0;

  /* the very initial ones will be initialised in Sg_InitPort() */
  v->currentInputPort = proto ? proto->currentInputPort : NULL;
  v->currentOutputPort = proto ? proto->currentOutputPort : NULL;
  v->currentErrorPort = proto ? proto->currentErrorPort : NULL;

  v->logPort = proto ? proto->logPort : v->currentErrorPort;

  /* thread, mutex, etc */
  SG_INTERNAL_THREAD_INIT(&v->thread);
  Sg_InitMutex(&v->vmlock, FALSE);
  Sg_InitCond(&v->cond);
  v->inspector = NULL;
  v->canceller = NULL;
  v->thunk = NULL;
  v->specific = SG_FALSE;
  v->result = SG_UNDEF;

  Sg_RegisterFinalizer(SG_OBJ(v), vm_finalize, NULL);
  return v;
}

SgVM* Sg_SetVMStack(SgVM *vm, SgObject *stack, int stackSize)
{
  vm->stack = stack;
  vm->sp = vm->fp = vm->stack;
  vm->stackEnd = vm->stack + stackSize;
  vm->cont = (SgContFrame *)vm->sp;
  vm->ac = SG_NIL;
  vm->cl = NULL;
  return vm;
}

SgVM* Sg_NewVM(SgVM *proto, SgObject name)
{
  SgVM *vm = Sg_NewThreadVM(proto, name);
  SgObject *stack = SG_NEW_ARRAY(SgObject, SG_VM_STACK_SIZE);
  return Sg_SetVMStack(vm, stack, SG_VM_STACK_SIZE);
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

static SgVM *root_vm()
{
  SgKernel *kernel = SG_KERNEL(theVM->kernel);
  if (kernel) {
    return SG_VM(kernel->threads->value);
  }
  return NULL;
}

int Sg_MainThreadP()
{
  return theVM == root_vm();
}

int Sg_RootVMP(SgVM *vm)
{
  return vm == root_vm();
}

#define Sg_VM() theVM

/* some convenient accessors */
#define PC(vm)             (vm)->pc
#define AC(vm)             (vm)->ac
#define CL(vm)             (vm)->cl
#define FP(vm)             (vm)->fp
#define SP(vm)             (vm)->sp
#define CONT(vm)           (vm)->cont

#define INDEX(sp, n)        (*((sp) - (n) - 1))
#define INDEX_SET(sp, n, v) (*((sp) - (n) - 1) = (v))
#define PUSH(sp, o)         (*(sp)++ = (o))
#define POP(sp)             (*(--(sp)))

/* is the given pointer in the stack? */
#define IN_STACK_P(ptr, vm)				\
  ((uintptr_t)((ptr) - vm->stack) < SG_VM_STACK_SIZE)


/* values  */
SgObject Sg_Values(SgObject args)
{
  return Sg_VMValues(theVM, args);
}

SgObject Sg_VMValues(SgVM *vm, SgObject args)
{
  SgObject cp;
  int nvals, init = FALSE;
  int len = -1;
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
	/* we can't allow too many values so cast it... */
	len = (int)Sg_Length(cp); /* get rest... */
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

SgObject Sg_Values4(SgObject v1, SgObject v2, SgObject v3, SgObject v4)
{
  return Sg_VMValues4(theVM, v1, v2, v3, v4);
}

SgObject Sg_VMValues4(SgVM *vm, SgObject v1,
		      SgObject v2, SgObject v3, SgObject v4)
{
  vm->valuesCount = 4;
  vm->values[0] = v2;
  vm->values[1] = v3;
  vm->values[2] = v4;
  vm->ac = v1;
  return v1;
}

SgObject Sg_Values5(SgObject v1, SgObject v2, SgObject v3, SgObject v4,
		    SgObject v5)
{
  return Sg_VMValues5(theVM, v1, v2, v3, v4, v5);
}

SgObject Sg_VMValues5(SgVM *vm, SgObject v1,
		      SgObject v2, SgObject v3, SgObject v4, SgObject v5)
{
  vm->valuesCount = 5;
  vm->values[0] = v2;
  vm->values[1] = v3;
  vm->values[2] = v4;
  vm->values[3] = v5;
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

static const int MAX_STACK_TRACE = 20;

static void format_stack_trace(SgObject stackTrace, SgObject out)
{
  SgObject cur;
  SgStringPort s;
  SgPort *buf = SG_PORT(Sg_InitStringOutputPort(&s, -1));
  Sg_Printf(buf, UC("stack trace:\n"));
  stackTrace = Sg_Reverse(stackTrace);

  SG_FOR_EACH(cur, stackTrace) {
    SgObject obj, index, proc, tmp, src, file, info, line, name;

    obj = SG_CAR(cur);
    index = SG_CAR(obj);
    if (SG_INT_VALUE(index) > MAX_STACK_TRACE) {
      Sg_Printf(buf, UC("      ... (more stack dump truncated)\n"));
      break;
    }

    proc = SG_CDR(obj);		/* (proc name src) */
    name = SG_PROCEDURE_NAME(SG_CADR(proc));

    if (SG_EQ(SG_CAR(proc), SG_INTERN("*proc*"))) {
      tmp = SG_CAR(SG_CDDR(proc));
      if (!SG_PAIRP(tmp)) {
	goto no_src;
      } else {
	src = Sg_LastPair(tmp);
	src = SG_CDAR(src);
	if (SG_PAIRP(src)) {
	  info = Sg_GetPairAnnotation(src, SG_INTERN("source-info"));
	} else {
	  info = SG_FALSE;
	}
      }
      if (SG_FALSEP(info) || !info) {
	Sg_PrintfShared(buf,
			UC("  [%A] %A\n"
			   "    src: %#50S\n"),
			index, name,
			Sg_UnwrapSyntax(src));
      } else {
	file = SG_CAR(info);
	line = SG_CDR(info);
	Sg_PrintfShared(buf,
			UC("  [%A] %A\n"
			   "    src: %#50S\n"
			   "    %S:%A\n"),
			index, name,
			Sg_UnwrapSyntax(src),
			file, line);
      }
      
    } else {
    no_src:
      /* *cproc* does not have any source info */
      Sg_Printf(buf, UC("  [%A] %A\n"), index, name);
    }
  }
  Sg_Write(Sg_GetStringFromStringPort(&s), out, SG_WRITE_DISPLAY);
}

void Sg_FormatStackTrace(SgObject stackTrace, SgObject out)
{
  format_stack_trace(stackTrace, out);
}


/* we need to check pc-1(for *CALL, or os) or pc-2(for GREF_*CALL) */
static SgObject get_closure_source(SgObject cl, SgWord *pc) 
{
  SgCodeBuilder *cb = SG_CODE_BUILDER(SG_CLOSURE(cl)->code);
  InsnInfo *info;
  SgObject src = SG_FALSE;
  intptr_t index = -1, j;
  SgObject name = SG_PROCEDURE_NAME(cl);

  if (SG_FALSEP(name)) {
    /* try codebuilder name */
    name = Sg_CodeBuilderFullName(cb);
  }
  /* before FRAME insn there must be a insn which has src info */
  for (j = 1;; j++) {
    if (Sg_GCBase(SG_OBJ(*(pc-j)))) continue;
    info = Sg_LookupInsnName(INSN(*(pc-j)));
    if (info && info->hasSrc) break;
  }
  /* for sanity */
  if (info && info->hasSrc) {
    index = (pc-j) - cb->code;
  }
  if (index > 0) {
    if (SG_PAIRP(cb->src)) {
      src = Sg_Assv(SG_MAKE_INT(index), cb->src);
    }
  }
  return src;
}

static void* search_different_cont(SgVM *vm, SgObject c, SgContFrame *cont)
{
  SgContFrame *currCont = cont->prev;
  SgContFrame *nextCont = (SgContFrame *)SG_STACK_TRACE_CONDITION(c)->cont;

  while (!IN_STACK_P((SgObject *)currCont, vm)) {
    /* Check if current stack frame contains next stack frame */
    if (nextCont == currCont) return NULL;
    /* cont frame loops when it's on the heap, then this is the bottom */
    if (currCont == currCont->prev->prev) return nextCont;
    currCont = currCont->prev;
  }
  return nextCont;
}

static inline void report_error(SgObject error, SgObject out)
{
  SgVM *vm = Sg_VM();
  SgObject next = SG_FALSE;
  SgPort *buf = SG_PORT(Sg_MakeStringOutputPort(-1));
  SgContFrame *cont = NULL;
  SgObject stackTrace = SG_NIL;
  int attached = FALSE;

  if (Sg_ConditionP(error)) {
    if (Sg_CompoundConditionP(error)) {
      SgObject cp;
      SG_FOR_EACH(cp, Sg_CompoundConditionComponent(error)) {
	if (SG_STACK_TRACE_CONDITION_P(SG_CAR(cp))) {
	  stackTrace = SG_STACK_TRACE_CONDITION(SG_CAR(cp))->trace;
	  attached = TRUE;
	  next = SG_STACK_TRACE_CONDITION(SG_CAR(cp))->cause;
	  cont = (SgContFrame *)SG_STACK_TRACE_CONDITION(SG_CAR(cp))->cont;
	  break;
	}
      }
    } else if (SG_STACK_TRACE_CONDITION_P(error)) {
      stackTrace = SG_STACK_TRACE_CONDITION(error)->trace;
      attached = TRUE;
      next = SG_STACK_TRACE_CONDITION(error)->cause;
      cont = (SgContFrame *)SG_STACK_TRACE_CONDITION(error)->cont;
    } 
  }
  if (SG_NULLP(stackTrace)) {
    stackTrace = Sg_VMGetStackTraceOf(vm, SG_STACK_TRACE_SOURCE, 0);
  }
  if (!Sg_CompileConditionP(error)) {
    Sg_Printf(buf,
	      UC("Unhandled exception\n"
		 "  %A\n"), Sg_DescribeCondition(error));
    /* If we simply check the vm state, then stack trace of terminated thread
       won't be shown and that makes extremly difficult to debug multi thread
       execution. so as long as stack trace is attached on the condition, then
       just show it.*/
    if ((attached || vm->state == RUNNING) && !SG_NULLP(stackTrace)) {
      while (1) {
	format_stack_trace(stackTrace, buf);
	if (SG_STACK_TRACE_CONDITION_P(next)) {
	  cont = search_different_cont(vm, next, cont);
	  if (!cont) break;
	  stackTrace = SG_STACK_TRACE_CONDITION(next)->trace;
	  next = SG_STACK_TRACE_CONDITION(next)->cause;
	  Sg_PutuzUnsafe(buf, UC("Nested "));
	} else {
	  break;
	}
      }
    }
  } else {
    Sg_Printf(buf, UC("%A"), Sg_DescribeCondition(error));
    /* if it's not &syntax nor &undefined, then most likely macro
       procedure error in that case, stack trace might be useful to
       show
     */
    if (!Sg_SyntaxViolationP(error)
	&& !Sg_ConditionContainsP(error, SG_CLASS_UNDEFINED_CONDITION)
	&& !SG_NULLP(stackTrace)) {
      while (1) {
	format_stack_trace(stackTrace, buf);
	if (SG_STACK_TRACE_CONDITION_P(next)) {
	  cont = search_different_cont(vm, next, cont);
	  if (!cont) break;
	  stackTrace = SG_STACK_TRACE_CONDITION(next)->trace;
	  next = SG_STACK_TRACE_CONDITION(next)->cause;
	  Sg_PutuzUnsafe(buf, UC("Nested "));
	} else {
	  break;
	}
      }
    }
  }
  /* for some reason, certain Windows platform failed to create
     stdout, at that moment, there is no stderr ready so out might
     be NULL.
     NB: in the case, it's an unrecoverable error, so just dump
         native stack trace.*/
  Sg_Write(Sg_GetStringFromStringPort(SG_STRING_PORT(buf)), 
	   out, SG_WRITE_DISPLAY);
  Sg_FlushAllPort(FALSE);
}

void Sg_ReportError(SgObject e, SgObject out)
{
  report_error(e, out);
}

void Sg_ReportErrorInternal(volatile SgObject e, SgObject out)
{
  SgVM *vm = Sg_VM();

  if (SG_VM_RUNTIME_FLAG_IS_SET(vm, SG_ERROR_BEING_REPORTED)) {
    Sg_Abort("Unhandled error occurred during reporting an error."
	     " Process aborted.\n");
  }
  SG_VM_RUNTIME_FLAG_SET(vm, SG_ERROR_BEING_REPORTED);
  SG_UNWIND_PROTECT {
    if (Sg_MainThreadP()) {
      Sg_FlushAllPort(FALSE);
      Sg_ReportError(e, out);
    }
  }
  SG_WHEN_ERROR {
    SG_VM_RUNTIME_FLAG_CLEAR(vm, SG_ERROR_BEING_REPORTED);
  }
  SG_END_PROTECT;
  SG_VM_RUNTIME_FLAG_CLEAR(vm, SG_ERROR_BEING_REPORTED);
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
      if (!info->label && SG_CODE_BUILDERP(arg)) {
	s = Sg_GetStringFromStringPort(SG_STRING_PORT(out));
	Sg_Puts(vm->logPort, SG_STRING(s));
	Sg_Printf(vm->logPort, UC(" %S\n"), arg);
	vm_dump_code_rec(SG_CODE_BUILDER(arg), indent + 2);
	need_line_break = FALSE;
      } else {
	if (info->label) Sg_Printf(out, UC(" %d"), arg);
	else Sg_Printf(out, UC(" %#S"), arg);
	s = Sg_GetStringFromStringPort(SG_STRING_PORT(out));
	Sg_Puts(vm->logPort, SG_STRING(s));
	if (info->hasSrc) {
	  if (SG_PAIRP(cb->src)) {
	    SgObject src = Sg_Assv(SG_MAKE_INT(i), cb->src);
	    long len = SG_STRING_SIZE(s);
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
      s = Sg_GetStringFromStringPort(SG_STRING_PORT(out));
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

SgObject Sg_VMCurrentLibrary()
{
  return Sg_VM()->currentLibrary;
}


void Sg_EnableSandbox()
{
  SgVM *vm = Sg_VM();
  if (SG_FALSEP(vm->sandbox)) {
    vm->sandbox = Sg_MakeHashTableSimple(SG_HASH_EQUAL, 32);
  }
}
void Sg_DisableSandbox()
{
  SgVM *vm = Sg_VM();
  vm->sandbox = SG_FALSE;
}


void Sg_VMAcquireGlobalLock()
{
  Sg_LockMutex(&global_lock);
}

void Sg_VMReleaseGlobalLock()
{
  Sg_UnlockMutex(&global_lock);
}

static SgObject compiler = SG_UNDEF;
static void init_compiler()
{
  SgObject compile_library;
  SgGloc *g;
  Sg_LockMutex(&global_lock);
  compile_library=Sg_FindLibrary(SG_INTERN("(sagittarius compiler)"), FALSE);
  g = Sg_FindBinding(compile_library, SG_INTERN("compile"), SG_FALSE);
  compiler = SG_GLOC_GET(g);
  Sg_UnlockMutex(&global_lock);
}

/* compiler */
#define define_compiler(name, apply)				\
  SgObject SG_CPP_CAT(Sg_, name)(SgObject o, SgObject e)	\
  {								\
    /* compiler is initialized after VM. so we need to		\
       look it up first */					\
    if (SG_UNDEFP(compiler)) {					\
      init_compiler();						\
    }								\
    return apply(compiler, o, e);				\
  }
define_compiler(Compile, Sg_Apply2)
define_compiler(VMCompile, Sg_VMApply2)
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
  if (vm->state == IMPORTING) {
    int b = vm->flags;
    SG_SET_CAR(vm->cache, Sg_Cons(Sg_Cons(b? SG_TRUE: SG_FALSE, v),
				  SG_CAR(vm->cache)));
  }
  if (vm->state != IMPORTING) vm->state = RUNNING;

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
  SgVM *vm = theVM;
  vm->currentLibrary = SG_LIBRARY(data);
  if (vm->state == IMPORTING) {
    
  }
  return SG_UNDEF;
}

static SgObject next_eval_cc(SgObject v, void **data)
{
  SgObject body, before, after, env = data[0];
  SgVM *vm = theVM;
  /* Now we are checking with this defined variable during compilation,
     and if a macro have eval in it blow resets the defined variables.
     to avoid it we need to keep it. */
  /* SG_LIBRARY_DEFINEED(vm->currentLibrary) = SG_NIL; */
  /* store cache */
  if (vm->state == IMPORTING) {
    int b = vm->flags;
    SG_SET_CAR(vm->cache, Sg_Cons(Sg_Cons(SG_MAKE_INT(b), v),
				  SG_CAR(vm->cache)));
  }
  if (vm->state != IMPORTING) vm->state = RUNNING;
  CLEAR_STACK(vm);

  /* ASSERT(SG_CODE_BUILDERP(v)); */
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

SgObject Sg_VMEval(SgObject sexp, SgObject env)
{
  SgVM *vm = theVM;
  void *data[1];
  data[0] = env;
  Sg_VMPushCC(next_eval_cc, data, 1);
  if (vm->state != IMPORTING) vm->state = COMPILING;
  return Sg_VMCompile(sexp, env);
}

static SgObject pass1_import = SG_UNDEF;

static void init_pass1_import()
{
  SgLibrary *complib;
  SgGloc *g;
  Sg_LockMutex(&global_lock);
  complib = Sg_FindLibrary(SG_INTERN("(sagittarius compiler pass1)"), FALSE);
  g = Sg_FindBinding(complib, SG_INTERN("pass1/import"), SG_UNBOUND);
  if (SG_UNBOUNDP(g)) {
    /* something wrong */
    Sg_Panic("pass1/import was not found. loading error?");
  }
  pass1_import = SG_GLOC_GET(g);
  Sg_UnlockMutex(&global_lock);
}

/* well this is actually not used, and i don't think will ever be used
   but in case... */
SgObject Sg_Environment(SgObject lib, SgObject spec)
{
  if (SG_UNDEFP(pass1_import)) {
    init_pass1_import();
  }
  /* make spec look like import-spec */
  spec = Sg_Cons(SG_INTERN("import"), spec);
  Sg_Apply2(pass1_import, spec, lib);
  return lib;
}

static SgObject environment_cc(SgObject result, void **data)
{
  return SG_OBJ(data[0]);
}

SgObject Sg_VMEnvironment(SgObject lib, SgObject spec)
{
  void *data[1];
  if (SG_UNDEFP(pass1_import)) {
    init_pass1_import();
  }
  /* make spec look like import-spec */
  spec = Sg_Cons(SG_INTERN("import"), spec);
  data[0] = lib;
  Sg_VMPushCC(environment_cc, data, 1);
  return Sg_VMApply2(pass1_import, spec, lib);
}

static void print_frames(SgVM *vm, SgContFrame *cont);
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
  cc->type = 0;
  cc->pc = (SgWord*)after;
  cc->fp = C_CONT_MARK;
  cc->cl = CL(vm);
  for (i = 0; i < datasize; i++) {
    PUSH(s, SG_OBJ(data[i]));
  }
  CONT(vm) = cc;
  FP(vm) = SP(vm) = s;
}

static SgWord boundaryFrameMark = NOP;
#define BOUNDARY_FRAME_MARK &boundaryFrameMark

enum {
  NORMAL_FRAME = 0,
  BOUNDARY_FRAME = 1,
  PROMPT_FRAME = 2
};

/* #define USE_LIGHT_WEIGHT_APPLY 1 */
#define PUSH_CONT_REC(vm, next_pc, typ)		\
  do {						\
    SgContFrame *newcont;			\
    newcont = (SgContFrame*)SP(vm);		\
    newcont->type = typ;			\
    newcont->prev = CONT(vm);			\
    newcont->size = (int)(SP(vm) - FP(vm));	\
    newcont->pc = (SgWord *)next_pc;		\
    newcont->cl = CL(vm);			\
    newcont->fp = FP(vm);			\
    CONT(vm) = newcont;				\
    SP(vm) += CONT_FRAME_SIZE;			\
  } while (0)

#define PUSH_CONT(vm, next_pc) PUSH_CONT_REC(vm, next_pc, NORMAL_FRAME)
#define PUSH_BOUNDARY_CONT(vm)					\
  PUSH_CONT_REC(vm, BOUNDARY_FRAME_MARK, BOUNDARY_FRAME)
#define PUSH_PROMPT_CONT(vm, tag) PUSH_CONT_REC(vm, tag, PROMPT_FRAME)

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
  int nargs = (int)Sg_Length(args), i;
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
  int argc = (int)Sg_Length(args);
  int reqstack;
  SgVM *vm = Sg_VM();

  if (argc < 0) Sg_Error(UC("improper list not allowed: %S"), args);
  /* TODO should we check tail posision? */
  reqstack = SG_FRAME_SIZE + 1;
  CHECK_STACK(reqstack, vm);
  PUSH(SP(vm), proc);
  PC(vm) = apply_callN;
  /* return Sg_CopyList(args); */
  return Sg_CopyList(args);
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
  } else {
    d[2] = NULL;
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

static SgObject install_ehandler(SgObject *args, int argc, void *data)
{
  SgContinuation *c = (SgContinuation*)data;
  SgVM *vm = Sg_VM();
  vm->exceptionHandlers = DEFAULT_EXCEPTION_HANDLER;
  vm->escapePoint = c;
  SG_VM_RUNTIME_FLAG_CLEAR(vm, SG_ERROR_BEING_REPORTED);
  return SG_UNDEF;
}

static SgObject discard_ehandler(SgObject *args, int argc, void *data)
{
  SgContinuation *c = (SgContinuation*)data;
  SgVM *vm = Sg_VM();
  vm->escapePoint = c->prev;
  vm->exceptionHandlers = c->xhandler;
  if (c->errorReporting) {
    SG_VM_RUNTIME_FLAG_SET(vm, SG_ERROR_BEING_REPORTED);
  }
  return SG_UNDEF;
}

SgObject Sg_VMWithErrorHandler(SgObject handler, SgObject thunk,
			       int rewindBefore)
{
  SgContinuation *c = SG_NEW(SgContinuation);
  SgObject before, after;
  SgVM *vm = Sg_VM();

  c->prev = vm->escapePoint;
  c->ehandler = handler;
  c->xhandler = vm->exceptionHandlers;
  c->winders = vm->dynamicWinders;
  c->cstack = vm->cstack;
  c->cont = vm->cont;
  c->floating = SG_VM_FLOATING_EP(vm);
  c->errorReporting = SG_VM_RUNTIME_FLAG_IS_SET(vm, SG_ERROR_BEING_REPORTED);
  c->rewindBefore = rewindBefore;

  before = Sg_MakeSubr(install_ehandler, c, 0, 0, SG_FALSE);
  after  = Sg_MakeSubr(discard_ehandler, c, 0, 0, SG_FALSE);
  return Sg_VMDynamicWind(before, thunk, after);
}

#define NORMAL_FRAME_MARK_P(cont) ((cont)->type == NORMAL_FRAME)
#define BOUNDARY_FRAME_MARK_P(cont) ((cont)->type == BOUNDARY_FRAME)
#define PROMPT_FRAME_MARK_P(cont) ((cont)->type == PROMPT_FRAME)

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
  SgContFrame *c = CONT(vm), *prev = NULL;
  SgCStack *cstk;
  SgContinuation *ep;
  SgPromptNode *node;

  if (!(IN_STACK_P((SgObject *)c, vm))) return;
  
  do {
    SgContFrame *csave, *tmp;
    if (partialP && BOUNDARY_FRAME_MARK_P(c)) break;
    csave = save_a_cont(c);
    /* make the orig frame forwarded */
    if (prev) prev->prev = csave;
    
    prev = csave;
    tmp = c->prev;
    c->prev = csave;
    c->size = -1;
    c = tmp;
  } while (IN_STACK_P((SgObject *)c, vm));

  if (FORWARDED_CONT_P(vm->cont)) {
    vm->cont = FORWARDED_CONT(vm->cont);
  }
  for (cstk = vm->cstack; cstk; cstk = cstk->prev) {
    if (FORWARDED_CONT_P(cstk->cont)) {
      cstk->cont = FORWARDED_CONT(cstk->cont);
    }
  }
  for (node = vm->prompts; node; node = node->next) {
    if (FORWARDED_CONT_P(node->frame)) {
      node->frame = FORWARDED_CONT(node->frame);
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

static void save_cont(SgVM *vm)
{
  save_cont_rec(vm, FALSE);
}

static void save_partial_cont(SgVM *vm)
{
  save_cont_rec(vm, TRUE);
}

/* static void save_cont_to_tag(SgVM *vm, SgObject tag) */
/* { */
/*   save_cont_rec(vm, FALSE, tag); */
/* } */

static void expand_stack(SgVM *vm)
{
  SgObject *p;
  int i, size;

  if (SG_VM_LOG_LEVEL(vm, SG_WARN_LEVEL)) {
    Sg_Printf(vm->logPort,
	      UC(";; expanding stack in %S of %S (fp=%d, sp=%d)\n"),
	      CL(vm), vm, FP(vm) - vm->stack, SP(vm) - vm->stack);
  }

  save_cont(vm);
  /* seems this is a bit faster (it's really a bit) */
  size = (int)((SP(vm) - FP(vm)));
  for (p = FP(vm), i = 0; i < size; i++, p++) {
    vm->stack[i] = *p;
  }
  /* memmove(vm->stack, FP(vm), (SP(vm) - FP(vm))*sizeof(SgObject)); */
  SP(vm) -= FP(vm) - vm->stack;
  FP(vm) = vm->stack;

  /* for GC friendliness */
  for (p = SP(vm); p < vm->stackEnd; p++) *p = NULL;
}

static SgWord return_code[1] = {SG_WORD(RET)};

#define PC_TO_RETURN return_code

static SgContFrame * copy_a_cont(SgContFrame *c)
{
  const size_t argsize = (c->size > 0) ? (c->size * sizeof(SgObject)) : 0;
  const size_t size = sizeof(SgContFrame) + argsize;
  SgContFrame *copy = SG_NEW2(SgContFrame *, size);
  memcpy(copy, c, size);
  return copy;
}

static int bottom_cont_frame_p(SgVM *vm, SgContFrame *cont)
{
  return !cont
    || (SgObject *)cont == vm->stack
    || cont == cont->prev
    || cont == cont->prev->prev;
}

/* check if the cur is after border frame */
static int cont_frame_after_p(SgVM *vm, SgContFrame *border, SgContFrame *cur)
{
  SgContFrame *cont = cur;
  do {
    if (cont == border) return TRUE;
    cont = cont->prev;
  } while (!bottom_cont_frame_p(vm, cont));
  return FALSE;
}

static SgPromptNode *search_prompt_node(SgVM *vm)
{
  SgPromptNode *node = vm->prompts;
  SgContFrame *cont = vm->cont;
  if (!node) return NULL;	/* no prompt */
  
  while (node->next) {
    if (cont_frame_after_p(vm, node->frame, cont)) break;
    node = node->next;
  }
  return node;			/* return the last node */
}

static SgPromptNode *insert_prompt(SgVM *vm, SgPromptNode *node,
				   SgPrompt *prompt, SgContFrame *frame)
{
  SgPromptNode *n = SG_NEW(SgPromptNode);
  n->prompt = prompt;
  n->frame = frame;
  if (node) {
    /* [ node )->[ next ) <= (c)
       [ node )->[ c )->[ next)
     */
    n->next = node->next;
    node->next = n;
  } else {
    vm->prompts = n;
  }
  return n;
}

static SgPromptNode *search_prompt_node_by_tag(SgVM *vm, SgObject tag)
{
  SgPromptNode *node = vm->prompts;
  
  /* search tag */
  while (node) {
    if (node->prompt->tag == tag) {
      return node;
    }
    node = node->next;
  }
  return NULL;
}


static int cont_prompt_match_p(SgContFrame *c, SgPrompt *prompt)
{
  return c && PROMPT_FRAME_MARK_P(c) && ((SgPrompt *)(c->pc)) == prompt;
}


static SgContFrame * splice_cont(SgVM *vm, SgContFrame *saved,
				 SgPrompt *prompt)
{
  SgContFrame *cur = vm->cont, *c = saved->prev, *cp = NULL, *top = NULL;
  SgPromptNode *node = search_prompt_node(vm);
  cp = top = copy_a_cont(saved);

  if (PROMPT_FRAME_MARK_P(top)) {
    node = insert_prompt(vm, node, (SgPrompt *)top->pc, top);
  }
  
  while (!cont_prompt_match_p(cp, prompt)) {
    SgContFrame *cs = copy_a_cont(c);
    if (PROMPT_FRAME_MARK_P(cs)) {
      node = insert_prompt(vm, node, (SgPrompt *)cs->pc, cs);
    }
    cp->prev = cs;
    cp = cs;
    c = c->prev;
  }
  cp->prev = cur;
  return top;
}

static SgObject throw_continuation_cc(SgObject, void **);
static SgObject merge_winders(SgObject, SgObject);
static SgObject take_prompt_winders(SgPrompt *, SgObject);

static SgObject throw_continuation_body(SgObject handlers,
					SgContinuation *c,
					SgObject args,
					SgPrompt *prompt)
{
  SgVM *vm = Sg_VM();
  /* (if (not (eq? new (current-dynamic-winders))) perform-dynamic-wind) */
  if (SG_PAIRP(handlers)) {
    SgObject handler, chain, next;
    void *data[4];
    handler = SG_CAAR(handlers);
    chain = SG_CDAR(handlers);
    data[0] = (void*)SG_CDR(handlers);
    data[1] = (void*)c;
    data[2] = (void*)args;
    data[3] = prompt;
    Sg_VMPushCC(throw_continuation_cc, data, 4);
    /* FIXME best to reconstruct chain itself during the computation... */
    next = (prompt)
      ? merge_winders(vm->dynamicWinders, chain)
      : chain;
    vm->dynamicWinders = next;
    /* Sg_Printf(Sg_StandardErrorPort(), UC("dw: %A\n"), vm->dynamicWinders); */
    return Sg_VMApply0(handler);
  } else {
    /* 
       if the target continuation is a full continuation, we can abandon
       the current continuation. however, if the target continuation is
       partial, we must return to the current continuation after executing
       the partial continuation.
    */
    if (c->cstack == NULL) save_cont(vm);
    if (prompt) {
      /* if the tag is there, then it's composable continuation
	 means, we add the continuation frame atop of the current
	 continuation.
	 As the continuation is not oneshot, we need to copy the
	 cont frame from the continuation.
       */
      vm->cont = splice_cont(vm, c->cont, prompt);
      if (c->winders != vm->dynamicWinders) {
	/* continuation is invoked outside of the winder's dynamic extent.
	   Merge it and take only the prompt ones.
	 */
	vm->dynamicWinders = merge_winders(c->winders, vm->dynamicWinders);
      }
    } else {
      vm->cont = c->cont;
      vm->dynamicWinders = c->winders;
    }
    vm->pc = return_code;
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
      int argc = (int)Sg_Length(args), i;
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
  SgObject tag = SG_OBJ(data[3]);
  return throw_continuation_body(handlers, c, args, tag);
}

/* remove and re-order continuation's handlers */
static SgObject remove_common_winders(SgObject current, SgObject escapes)
{
  SgObject r = SG_NIL, p, cr = Sg_Reverse(current);
  SG_FOR_EACH(p, escapes) {
    if (SG_FALSEP(Sg_Memq(SG_CAR(p), cr))) {
      r = Sg_Cons(SG_CAR(p), r);
    }
  }
  return r;
}

/* Reconstruct escape winders
   When 2 composable continuation is invoked, then the latter continuation
   must have the winders during the invocation. However, if we simply
   let the process go, then before thunk may not get into the list.
 */
static SgObject merge_winders(SgObject current, SgObject escapes)
{
  SgObject h = SG_NIL, t = SG_NIL;

  if (SG_NULLP(current)) return escapes;
  if (SG_NULLP(escapes)) return current;
  
  /* merge first current, then escape */
#define do_merge(source, checker)			\
  while (!SG_NULLP(source)) {				\
    if (SG_FALSEP(Sg_Memq(SG_CAR(source), checker))) {	\
      SG_APPEND1(h, t, SG_CAR(source));			\
      source = SG_CDR(source);				\
      continue;						\
    }							\
    break;						\
  }

  do_merge(current, escapes);
  do_merge(escapes, current);
  while (!SG_NULLP(escapes)) {
    SG_APPEND1(h, t, SG_CAR(escapes));
    escapes = SG_CDR(escapes);
  }

  return h;
}

/*
  when the continuation is captured with call/comp, then the prompt
  is also provided.
  
 */
static SgObject take_prompt_winders(SgPrompt *prompt, SgObject winders)
{
  /* now we only need the uncommon winders, e.g.
     (call/prompt         ;; p1
       (lambda ()
         (dynamic-wind
	   pre1
	   (lambda ()
	     (call/prompt ;; p2
	       (lambda ()
	         (dynamic-wind
		   pre2
		   (lambda () (call/comp (lambda (k) k)))
		   post2))))
	   post1)))
     when the returned `k` is invoked then we only want to invoke
     winders of below p2
   */
  SgObject p;
  if (SG_NULLP(winders)) return winders;

  SG_FOR_EACH(p, winders) {
    if (SG_FALSEP(Sg_Memq(SG_CAR(p), prompt->winders))) return p;
  }
  /* no uncommon winders */
  return SG_NIL;
}

/* TODO clean up */
static SgObject throw_cont_compute_handlers(SgContinuation *c,
					    SgPrompt *prompt,
					    SgVM *vm)
{
  SgObject current = vm->dynamicWinders;
  SgObject escapes = c->winders;
  SgObject target = remove_common_winders(current, escapes);
  SgObject h = SG_NIL, t = SG_NIL, p;

  if (prompt) target = take_prompt_winders(prompt, target);

  /* When the continuation is partial continuation,
     then the after thunk is already installed in the dynamic extent.
     So, skip the current ones.
   */
  if (c->cstack) {
    if (prompt) current = take_prompt_winders(prompt, current);
    SG_FOR_EACH(p, current) {
      if (!SG_FALSEP(Sg_Memq(SG_CAR(p), escapes))) break;
      SG_APPEND1(h, t, Sg_Cons(SG_CDAR(p), SG_CDR(p)));
    }
  }
  SG_FOR_EACH(p, target) {
    SgObject chain = Sg_Memq(SG_CAR(p), escapes);
    SG_APPEND1(h, t, Sg_Cons(SG_CAAR(p), SG_CDR(chain)));
  }

  return h;
}

static SgObject throw_continuation(SgObject *argv, int argc, void *data)
{
  SgContinuation *c = (SgContinuation*)SG_CAR(data);
  SgObject handlers_to_call;
  SgVM *vm = Sg_VM();
  SgPrompt *prompt = (SgPrompt *)SG_CDR(data);

  if (c->cstack && vm->cstack != c->cstack) {
    SgCStack *cs;
    for (cs = vm->cstack; cs; cs = cs->prev) {
      if (c->cstack == cs) break;
    }
    if (cs != NULL) {
      vm->escapeReason = SG_VM_ESCAPE_CONT;
      vm->escapeData[0] = c;
      vm->escapeData[1] = Sg_Cons(argv[0], prompt);
      longjmp(vm->cstack->jbuf, 1);
    }
    save_cont(vm);
  }
  handlers_to_call = throw_cont_compute_handlers(c, prompt, vm);

  return throw_continuation_body(handlers_to_call, c, argv[0], prompt);
}

static SgObject sym_continuation = SG_FALSE;
static SgObject make_cont_subr(SgContinuation *cont, SgPrompt *prompt)
{
  return Sg_MakeSubr(throw_continuation, Sg_Cons(cont, prompt), 0, 1,
		     sym_continuation);
}

int Sg_ContinuationP(SgObject o)
{
  return SG_SUBRP(o) && SG_EQ(SG_PROCEDURE_NAME(o), sym_continuation);
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


  contproc = make_cont_subr(cont, NULL);
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

  contproc = make_cont_subr(cont, NULL);
  /* Remove the saved continuation chain */
  vm->cont = c;
  return Sg_VMApply1(proc, contproc);  
}

/* call-with-composable-continuation */
SgObject Sg_VMCallComp(SgObject proc, SgObject tag)
{
  SgContinuation *cont;
  SgObject contproc;
  SgVM *vm = Sg_VM();
  SgPromptNode *node = search_prompt_node_by_tag(vm, tag);

  if (!node) goto err;
  /*
    NOT DOING IT FOR NOW.
    save the continuation up to the tag.
    here, we save everything even the tag doesn't exist in the continuation.
   */
  /* save_cont_to_tag(vm, tag); */
  /*
    Ideally, we should save only the required continuation frame,
    to reduce memory usage. Though, the current save_cont expects
    the continuation frame to be in stack and if it's partially saved,
    then it won't save the remaining frames next time.
    e.g.
      1st [heap][heap][prompt][stack][stack]
      2nd call will not save [stack], and full continuation created by
      call/cc would fail to restore.
    So for now, we save all the continuation frame, then when the saved
    continuation is invoked, we simply put them on top of the current
    continuation (as composable continuation requires)
    
    If the memory usage becomes a problem, we will revisit.
   */
  save_cont(vm);

  cont = SG_NEW(SgContinuation);
  /* For composable continuations, only save winders up to the prompt */
  cont->winders = take_prompt_winders(node->prompt, vm->dynamicWinders);
  cont->cont = vm->cont;
  cont->prev = NULL;
  cont->ehandler = SG_FALSE;
  cont->cstack = NULL;		/* so that this continuation can be
				   run on any cstack state. */
  contproc = make_cont_subr(cont, node->prompt);

  return Sg_VMApply1(proc, contproc);
 err:
  Sg_Error(UC("No continuation tag: %S"), tag);
  return SG_UNDEF;		/* dummy */
}

/* call-with-continuation-prompt

   This is basically just put a boundary continuation
   before executing the `proc`.
 */
static SgPrompt *make_prompt(SgObject tag, SgObject handler, SgVM *vm)
{
  SgPrompt *prompt = SG_NEW(SgPrompt);
  prompt->tag = tag;
  prompt->handler = handler;
  prompt->cstack = vm->cstack;
  prompt->winders = vm->dynamicWinders;
  return prompt;
}

static void install_prompt(SgVM *vm, SgPrompt *prompt)
{
  SgPromptNode *node = SG_NEW(SgPromptNode);
  node->prompt = prompt;
  node->frame = vm->cont;
  node->next = vm->prompts;
  vm->prompts = node;
}

static void remove_prompt(SgVM *vm, SgPrompt *prompt)
{
  SgPromptNode *node = vm->prompts, *prev = NULL;
  while (node) {
    if (node->prompt == prompt) {
      if (prev) {
	prev->next = node->next;
      } else {
	vm->prompts = node->next;
      }
      break;
    }
    prev = node;
    node = node->next;
  }
}

SgObject Sg_VMCallCP(SgObject proc, SgObject tag,
		     SgObject handler, SgObject args)
{
  SgVM *vm = theVM;
  int nargs = (int)Sg_Length(args);
  SgPrompt *prompt = make_prompt(tag, handler, vm);
  
  if (nargs < 0) {
    Sg_Error(UC("improper list not allowed: %S"), args);
  }

  CHECK_STACK(CONT_FRAME_SIZE, vm);
  PUSH_PROMPT_CONT(vm, prompt);
  FP(vm) = SP(vm);
  install_prompt(vm, prompt);
  
  return Sg_VMApply(proc, args);
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
  return SG_STRING(Sg_GetStringFromStringPort(SG_STRING_PORT(ret)));
}

SgObject Sg_AddLoadPath(SgString *path, int appendP)
{
  SgVM *vm = Sg_VM();
  if (SG_STRING_SIZE(path) != 0) {
    path = replace_file_separator(path);
    if (appendP && !SG_NULLP(vm->loadPath)) {
      SgObject last = Sg_LastPair(vm->loadPath);
      SG_SET_CDR(last, SG_LIST1(path));
    } else {
      vm->loadPath = Sg_Cons(path, vm->loadPath);
    }
  }
  return vm->loadPath;
}

SgObject Sg_AddDynamicLoadPath(SgString *path, int appendP)
{
  SgVM *vm = Sg_VM();
  if (SG_STRING_SIZE(path) != 0) {
    path = replace_file_separator(path);
    if (appendP && !SG_NULLP(vm->dynamicLoadPath)) {
      SgObject last = Sg_LastPair(vm->dynamicLoadPath);
      SG_SET_CDR(last, SG_LIST1(path));
    } else {
      vm->dynamicLoadPath = Sg_Cons(path, vm->dynamicLoadPath);
    }
  }
  return vm->dynamicLoadPath;
}

static SgObject collect_arguments(SgVM *vm, SgObject *fp, int argc, SgObject cl)
{
  SgObject h = SG_NIL, t = SG_NIL, r = SG_NIL;
  int i;
  if (!cl || !fp || argc < 0) return SG_NIL;

  /* free variable */
  if (SG_PROCEDURE_TYPE(cl) == SG_PROC_CLOSURE) {
    int freec = SG_CODE_BUILDER_FREEC(SG_CLOSURE(cl)->code);
    for (i = 0; i < freec; i++) {
      SG_APPEND1(h, t, Sg_Cons(SG_MAKE_INT(i), SG_CLOSURE(cl)->frees[i]));
    }
    r = Sg_Acons(SG_INTERN("free"), h, r);
    h = SG_NIL, t = SG_NIL;
  }

  /* local variable */
  /* if (argc > 0) fprintf(stderr, "fp = %p, ", fp); */
  for (i = 0; i < argc;) {
    SgObject v = *(fp + i);
    if (IN_STACK_P((SgObject *)v, vm)) {
      /* probably a LOCAL_CALL frame, so skip */
      i += SG_FRAME_SIZE;
    } else {
      /* fprintf(stderr, "%d:v = %p, ", i, v); */
      SG_APPEND1(h, t, Sg_Cons(SG_MAKE_INT(i++), v));
    }
  }
  /* if (argc > 0) fprintf(stderr, "\n"); */
  return Sg_Acons(SG_INTERN("local"), h, r);
}

static SgObject get_source_info(SgObject cl, SgWord *pc)
{
  SgObject src = get_closure_source(cl, pc);
  if (SG_FALSEP(src)) {
    src = SG_CODE_BUILDER(SG_CLOSURE(cl)->code)->src;
  } else {
    /* need to be alist */
    src = SG_LIST1(src);
  }
  return src;
}

static SgObject get_stack_trace(SgVM *vm, SgContFrame *cont, SgObject cl,
				SgWord *pc, SgObject *fp, int count,
				SgVMStackTraceInfo flags, int framesToSkip)
{
  SgObject r = SG_NIL, cur = SG_NIL, *argp = fp;
  int i, frames = 0;

  for (i = 0;; frames++) {
    if (SG_PROCEDUREP(cl) && frames < framesToSkip) goto next;
    if (SG_PROCEDUREP(cl)) {
      SgObject args = (flags & SG_STACK_TRACE_ARGUMENTS)
	? collect_arguments(vm, argp, count, cl)
	: SG_NIL;
      switch (SG_PROCEDURE_TYPE(cl)) {
      case SG_PROC_SUBR:
	r = SG_LIST4(SG_INTERN("*cproc*"), cl, SG_NIL, args);
	break;
      case SG_PROC_CLOSURE:
	if (SG_CLOSURE(cl)->code
	    && SG_CODE_BUILDERP(SG_CLOSURE(cl)->code)) {
	  SgObject src = (flags & SG_STACK_TRACE_SOURCE)
	    ? get_source_info(cl, pc)
	    : SG_NIL;
	  r = SG_LIST4(SG_INTERN("*proc*"), cl, src, args);
	} else {
	  r = SG_LIST4(SG_INTERN("*proc*"), cl, SG_NIL, args);
	}
	break;
      default: break;		/* never happen? */
      }
      i++;
    } else {
      /* should not be here */
      ASSERT(FALSE);
    }

    cur = Sg_Acons(SG_MAKE_INT(i), r, cur);
  next:
    if (!IN_STACK_P((SgObject *)cont, vm) ||
	(uintptr_t)cont > (uintptr_t)vm->stack) {

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
      if (IN_STACK_P((SgObject *)nextCont, vm) && 
	  ((uintptr_t)nextCont < (uintptr_t)vm->stack ||
	   (uintptr_t)vm->stackEnd < (uintptr_t)nextCont)) {
	break;
      }
      count = cont->size;	/* size = previous frame's arguments count */
      if (cont->fp == C_CONT_MARK) argp = NULL;
      /* on the stack, i hope */
      else argp = cont->fp;
      cont = nextCont;
    } else {
      break;
    }
  }
  return cur;
}

SgObject Sg_VMGetStackTraceOf(SgVM *vm, SgVMStackTraceInfo flags,
			      int framesToSkip)
{
  SgContFrame *cont = CONT(vm);
  SgObject cl = CL(vm);
  SgWord *pc = PC(vm);

  if (!cl) {
    /* before running */
    return SG_NIL;
  }
  /* fprintf(stderr, "stack = %p, sp = %p\n", vm->stack, SP(vm)); */
  return get_stack_trace(vm, cont, cl, pc, FP(vm), (int)(SP(vm) - FP(vm)), flags,
			 framesToSkip);
}

/*
;; image of the definitions

 */
static SgObject raise_proc = SG_FALSE;
static SgObject raise_continuable_proc = SG_FALSE;

static SgObject raise_cc(SgObject result, void **data)
{
  SgObject e = SG_OBJ(data[0]);
  SgObject p = SG_OBJ(data[1]);
  return Sg_VMApply1(p, e);
}

/*
  This change makes raise or raise-continuable and saving raised condition
  a bit more expensive than before (and may cause memory explosion). The 
  basic idea of this change is that using continuation frame as a stack 
  trace so that nested stack trace can be detected easily. To make this 
  happen, we save all frames into the heap as if call/cc is called when 
  raise/raise-contiuable is called.
  
  Above sounds kinda horrible however if we implement segmented stacks type
  call/cc described the blow paper, then this performance penalty wouldn't be
  a problem.
  - Representing Control in the Presence of First-Class Continuations
    URL: http://www.cs.indiana.edu/~dyb/papers/stack.ps
  Not sure if this happens in near future but we may review the current
  implementation of call/cc if the performance would be an issue.
*/
SgObject Sg_VMAttachStackTrace(SgVM *vm, SgObject condition, int skipTop)
{
  if (Sg_CompoundConditionP(condition)) {
    save_cont(vm);
    condition = Sg_AddStackTrace(condition, vm, skipTop? 1: 0);
  }
  return condition;
}

SgObject Sg_VMThrowException(SgVM *vm, SgObject exception, int continuableP)
{
  exception = Sg_VMAttachStackTrace(vm, exception, FALSE);
  /* should never happen but I usually make mistake so lean to safer side. */
  if (SG_NULLP(vm->exceptionHandlers)) {
    vm->exceptionHandlers = DEFAULT_EXCEPTION_HANDLER;
  }

  if (vm->exceptionHandlers != DEFAULT_EXCEPTION_HANDLER) {
    /* 
       To avoid calling exception handers outside of current continuation
       (c.f. using Sg_Apply families), we need call raise/raise-continuable
       defined in Scheme (see boot/lib/errors.scm). To do it, we set flag
       here and escape from current C stack. If the run_loop procedure
       sees the flag, then it handles this call properly.
     */
    void *data[2];
    data[0] = exception;
    if (continuableP) {
      data[1] = raise_continuable_proc;
    } else {
      data[1] = raise_proc;
    }
    vm->escapeReason = SG_VM_ESCAPE_RAISE;
    Sg_VMPushCC(raise_cc, data, 2);
    longjmp(vm->cstack->jbuf, 1);
  }
  /* short cut, if there's no exception handlers, then we don't have to
     call it. we know what should happen.
   */
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
    Sg_ReportErrorInternal(e, vm->currentErrorPort);
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
    /* exit(EX_SOFTWARE); */
    Sg_Exit(EX_SOFTWARE);
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

static SgContFrame *skip_prompt_frame(SgContFrame *cont)
{
  while (PROMPT_FRAME_MARK_P(cont)) {
    remove_prompt(theVM, (SgPrompt *)cont->pc);
    cont = cont->prev;
  }
  return cont;
}

#define POP_CONT()							\
  do {									\
    SgContFrame *cont__ = skip_prompt_frame(CONT(vm));			\
    if (cont__->fp == C_CONT_MARK) {					\
      void *data__[SG_CCONT_DATA_SIZE];					\
      SgObject v__ = AC(vm);						\
      SgCContinuationProc *after__;					\
      void **d__ = data__;						\
      void **s__ = (void**)((SgObject*)cont__ + CONT_FRAME_SIZE);	\
      int i__ = cont__->size;						\
      while (i__-- > 0) {						\
	*d__++ = *s__++;						\
      }									\
      after__ = ((SgCContinuationProc*)cont__->pc);			\
      if (IN_STACK_P((SgObject*)cont__, vm)) {				\
	SP(vm) = (SgObject*)cont__;					\
      }									\
      FP(vm) = SP(vm);							\
      PC(vm) = PC_TO_RETURN;						\
      CL(vm) = cont__->cl;						\
      CONT(vm) = cont__->prev;						\
      AC(vm) = after__(v__, data__);					\
    } else if (IN_STACK_P((SgObject *)cont__, vm)) {			\
      CONT(vm) = cont__->prev;						\
      PC(vm) = cont__->pc;						\
      CL(vm) = cont__->cl;						\
      FP(vm) = cont__->fp;						\
      SP(vm) = FP(vm) + cont__->size;					\
    } else {								\
      int size__ = cont__->size;					\
      FP(vm) = SP(vm) = vm->stack;					\
      PC(vm) = cont__->pc;						\
      CL(vm) = cont__->cl;						\
      CONT(vm) = cont__->prev;						\
      if (size__) {							\
	SgObject *s__ = cont__->env, *d__ = SP(vm);			\
	SP(vm) += size__;						\
	while (size__-- > 0) {						\
	  *d__++ = *s__++;						\
	}								\
      }									\
    }									\
  } while (0)

/* 
   Finalizers also occupy memory space and if we don't invoke
   them, then it only grows. This case only happens only on
   multi thread environment and the execution raised an error
   or numbers of GC happened after returning from run_loop (
   not even sure this would happen though).

   Seems this would cause more problem than profit.
 */
#define RUN_FINALIZER(vm)				\
  do {							\
    if ((vm)->finalizerPending) Sg_VMFinalizerRun(vm);	\
  } while (0)

static SgObject default_abort_handler(SgObject *argv, int argc, void *data)
{
  SgObject tag = SG_OBJ(data);

  if (argc != 1) {
    Sg_WrongNumberOfArgumentsAtLeastViolation(SG_INTERN("default-abort-handler"),
					      1, argc, SG_NIL);
  }
  
  if (!SG_PROCEDUREP(argv[0])) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("default-abort-handler"),
				    SG_MAKE_STRING("procedure"),
				    argv[0], SG_NIL);

  }
  return Sg_VMCallCP(argv[0], tag, SG_FALSE, SG_NIL);
}

static SgObject abort_invoke_handler(SgPrompt *prompt, SgObject args)
{
  SgObject handler;
  if (SG_FALSEP(prompt->handler)) {
    handler = Sg_MakeSubr(default_abort_handler, prompt->tag, 1, 0,
			  SG_INTERN("default-abort-handler"));
  } else {
    handler = prompt->handler;
  }
  return Sg_VMApply(handler, args);
}

/*
  Abort continuation goes 2 pass,
  1. search the tag
  2. pop the continuation until the tag

  If the tag is not found, then an error will be raised
 */
static int prompt_winder_in_scope_p(SgPromptNode *node, SgObject winders)
{
  SgVM *vm = theVM;
  SgContFrame *cont = vm->cont;

  /* check if the prompt is in the same dynamic-extent (stack) */
  while (!bottom_cont_frame_p(vm, cont)) {
    if (cont == node->frame) return TRUE; /* must be invoked */
    cont = cont->prev;
  }
  /*
    the prompt is not in the same dynamic-extent
    check if winder is defined in the same prompt or not
   */
  return !SG_NULLP(take_prompt_winders(node->prompt, SG_CAAR(winders)));
}

static SgObject abort_cc(SgObject, void **);
static SgObject abort_body(SgPromptNode *node, SgObject winders, SgObject args)
{
  SgVM *vm = theVM;
  if (SG_PAIRP(winders) && prompt_winder_in_scope_p(node, winders)) {
    SgObject winder, chain;
    void *data[3];
    winder = SG_CAAR(winders);
    chain = SG_CDAR(winders);
    data[0] = node;
    data[1] = SG_CDR(winders);
    data[2] = args;
    Sg_VMPushCC(abort_cc, data, 3);

    vm->dynamicWinders = chain;
    return Sg_VMApply0(winder);
  } else {
    /* make sure the node is in the prompt chain

       NOTE: when this abort_cc frame is captured in a continuation,
       the captured node is not loger valid.
    */
    SgPromptNode *cur_node = search_prompt_node_by_tag(vm, node->prompt->tag);
    if (!cur_node) Sg_Error(UC("Stale prompt: %S"), node->prompt->tag);
    SgPrompt *prompt = cur_node->prompt;
    SgContFrame *cont = vm->cont;

    /* remove the prompt in the aborting cont frame from the chain */
    while (!cont_prompt_match_p(cont, prompt)) {
      if (PROMPT_FRAME_MARK_P(cont)) remove_prompt(vm, (SgPrompt *)cont->pc);
      cont = cont->prev;
    }
    if (prompt->cstack != vm->cstack) {
      vm->escapeReason = SG_VM_ESCAPE_ABORT;
      vm->escapeData[0] = cur_node;
      vm->escapeData[1] = args;
      longjmp(prompt->cstack->jbuf, 1);
    }
    /* reset the cont frame after the winder invocation */
    remove_prompt(vm, prompt);
    vm->cont = cur_node->frame->prev;
    return abort_invoke_handler(prompt, args);
  }
}

static SgObject abort_cc(SgObject r, void **data)
{
  return abort_body((SgPromptNode *)(data[0]), SG_OBJ(data[1]), SG_OBJ(data[2]));
}

SgObject Sg_VMAbortCC(SgObject tag, SgObject args)
{
  SgVM *vm = theVM;
  SgObject h;
  SgPromptNode *node = search_prompt_node_by_tag(vm, tag);
  SgContinuation c;

  if (!node) Sg_Error(UC("No continuation tag: %S"), tag);

  /* compose fake continuation to compute winders */
  c.winders = node->prompt->winders;
  c.cstack = node->prompt->cstack;
  h = throw_cont_compute_handlers(&c, node->prompt, vm);

  return abort_body(node, h, args);
}

SgObject evaluate_safe(SgObject program, SgWord *code)
{
  SgCStack cstack;
  SgVM * volatile vm = theVM;
  SgWord * volatile prev_pc = PC(vm);

  CHECK_STACK(CONT_FRAME_SIZE, vm);
  PUSH_BOUNDARY_CONT(vm);
  
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
    /* RUN_FINALIZER(vm); */
    if (vm->cont == cstack.cont) {
      POP_CONT();
      PC(vm) = prev_pc;
    } else if (vm->cont == NULL) {
      /* we're finished with executing partial continuation */
      vm->cont = cstack.cont;
      POP_CONT();
      PC(vm) = prev_pc;
    } else {
      /* The VM's SP and FP registers are initialised as the same
	 pointer, this causes call/cc save duplicated boundary mark
	 and make it cyclic.
	 e.g.) This would an error
	   (import (rnrs) (srfi :18))

	   (define (thunk) (guard (e (else (raise e))) (print 'ok)))
	   (thread-join! (thread-start! (make-thread thunk)))

	 Above case the last and the second last frame's prev indicates
	 the last frame as it should be, however the last frame's
	 prev indicates the second last frame.
	 To detect such situation, we need to allow cyclic boundaries. 
         NOTE: a lot of things are depending on the behaviour so we
	       can't change it...*/
      /* TODO this check might be too naive */
      if (vm->cont->prev && vm->cont->prev->prev && 
	  vm->cont == vm->cont->prev->prev && vm->cont->prev == cstack.cont) {
	POP_CONT();
	PC(vm) = prev_pc;
      } else {
	Sg_Error(UC("attempt to return from C continuation boundary."));
      }
    }
    
  } else {
    /* error, let finalizer run first here */
    /* RUN_FINALIZER(vm); */
    if (vm->escapeReason == SG_VM_ESCAPE_CONT) {
      SgContinuation *c = (SgContinuation*)vm->escapeData[0];
      if (c->cstack == vm->cstack) {
	SgObject ed = vm->escapeData[1];
	SgPrompt *p = (SgPrompt *)SG_CDR(ed);
	SgObject handlers = throw_cont_compute_handlers(c, p, vm);
	PC(vm) = PC_TO_RETURN;
	AC(vm) = throw_continuation_body(handlers, c, SG_CAR(ed), p);
	goto restart;
      } else {
 	ASSERT(vm->cstack && vm->cstack->prev);
	CONT(vm) = cstack.cont;
	AC(vm) = vm->ac;
	POP_CONT();
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
	/* exit(EX_SOFTWARE); */
	Sg_Exit(EX_SOFTWARE);
      } else {
	CONT(vm) = cstack.cont;
	POP_CONT();
	vm->cstack = vm->cstack->prev;
	longjmp(vm->cstack->jbuf, 1);
      }
    } else if (vm->escapeReason == SG_VM_ESCAPE_RAISE) {
      PC(vm) = PC_TO_RETURN;
      goto restart;
    } else if (vm->escapeReason == SG_VM_ESCAPE_ABORT) {
      SgPromptNode *node = (SgPromptNode *)vm->escapeData[0];
      if (node->prompt->cstack == vm->cstack) {
	SgObject args = SG_OBJ(vm->escapeData[1]);
	remove_prompt(vm, node->prompt);
	CONT(vm) = node->frame->prev;
	PC(vm) = PC_TO_RETURN;
	AC(vm) = abort_invoke_handler(node->prompt, args);
	goto restart;
      } else {
	CONT(vm) = cstack.cont;
	AC(vm) = vm->ac;
	POP_CONT();
	vm->cstack = vm->cstack->prev;
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
SgObject Sg_VMExecute(SgObject toplevel)
{
  ASSERT(SG_CODE_BUILDERP(toplevel));
  /* NB: compiled libraries don't need any frame. */
  return evaluate_safe(SG_OBJ(&internal_toplevel_closure),
		       SG_CODE_BUILDER(toplevel)->code);
}

/*
  Shifts argument frame

  from
  fp          m       sp
  +---+---+---+---+---+
  | r | g | g | a | a |
  +---+---+---+---+---+
  r = remain, g = garbage, a = argument

  to
  fp         sp/m
  +---+---+---+
  | r | a | a |
  +---+---+---+

  return SP = fp+m
 */
static inline SgObject* shift_args(SgObject *fp, int m, SgObject *sp)
{
  /* TODO Use SIMD? */
#if 1
  int i;
  SgObject *f = fp + m;
  for (i = m - 1; 0 <= i; i--) {
    INDEX_SET(f, i, INDEX(sp, i));
  }
  return f;
#else
  /* seems this is slower */
  memmove(fp, sp-m, m*sizeof(SgObject));
  return fp+m;
#endif
}

/* for call-next-method. is there not a better way? 
   
  Shifts stack frame.

  from
  sp      
  +---+---+
  | a | a |
  +---+---+
  a = argument

  to
  sp
  +---+---+---+
  | r | a | a |
  +---+---+---+
  r = will next method

  return SP
 */
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
  SG_RESET_INTERRUPTED_THREAD(vm);
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
  } else {
    data[2] = SG_NIL;
  }

  Sg_VMPushCC(process_queued_requests_cc, data, 3);
  
  vm->attentionRequest = FALSE;

  if (vm->finalizerPending) Sg_VMFinalizerRun(vm);

  if (vm->stopRequest) {
    SG_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(vm->vmlock);
    switch (vm->stopRequest) {
    case SG_VM_REQUEST_SUSPEND:
      vm->stopRequest = FALSE;
      vm->threadState = SG_VM_STOPPED;
      Sg_NotifyAll(&vm->cond);
      while (vm->threadState == SG_VM_STOPPED) {
	Sg_Wait(&vm->cond, &vm->vmlock);
      }
      break;
    case SG_VM_REQUEST_TERMINATE:
      vm->threadState = SG_VM_TERMINATED;
      break;
    }
    SG_INTERNAL_MUTEX_SAFE_LOCK_END();
    if (vm->threadState == SG_VM_TERMINATED) {
      Sg_ExitThread(&vm->thread, NULL);
    }
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
static void print_argument(SgVM *vm, SgContFrame *cont,
			   SgObject *loc, SgObject p)
{
#if 1
  Sg_Printf(vm->logPort, UC(";; %p +   p=%#39p +\n"), loc, p);
#else
  SgString *fmt = SG_MAKE_STRING("+   p=~39,,,,39s +~%");
  if (cont->fp == C_CONT_MARK && p) {
    Sg_Printf(vm->logPort, UC(";; %p +   p=%#39p +\n"), loc, p);
  } else {
    Sg_Printf(vm->logPort, UC(";; %p "), loc);
    Sg_Format(vm->logPort, fmt, SG_LIST1(p), TRUE);
  }
#endif
}

/* dladdr is not in POSIX, for now enable it only on Mac
   NOTE: It should also be available on BSD systems, but I'm lazy
 */
#if defined(HAVE_DLFCN_H) && __APPLE__
# include <dlfcn.h>
static int print_c_pc(SgVM *vm, SgObject pfmt, SgContFrame *cont)
{
  if (cont->fp == C_CONT_MARK) {
    Dl_info info;
    /* pc == after function */
    if (dladdr((void *)cont->pc, &info) && info.dli_sname) {
      SgObject pc = SG_INTERN("pc");
      SgObject name = Sg_Utf8sToUtf32s(info.dli_sname, strlen(info.dli_sname));
      Sg_Printf(vm->logPort, UC(";; %p "),
		(uintptr_t)cont + offsetof(SgContFrame, pc));
      Sg_Format(vm->logPort, pfmt, SG_LIST2(pc, name), TRUE);
      return TRUE;
    }
  }
  return FALSE;
}
#else
static int print_c_pc(SgVM *vm, SgObject pfmt, SgContFrame *cont)
{
  return FALSE;
}
#endif

static void print_pc(SgVM *vm, SgObject pfmt, SgContFrame *cont)
{
  SgObject pc = SG_INTERN("pc");
  if (PROMPT_FRAME_MARK_P(cont)) {
    SgPrompt *p = (SgPrompt *)cont->pc;
    Sg_Printf(vm->logPort, UC(";; %p "),
	      (uintptr_t)cont + offsetof(SgContFrame, pc));
    Sg_Format(vm->logPort, pfmt,
	      SG_LIST2(pc, Sg_Cons(p->tag, p->handler)), TRUE);
  } else if (!print_c_pc(vm, pfmt, cont)) {
    Sg_Printf(vm->logPort, UC(";; %p +   pc=%#38p +\n"),
	      (uintptr_t)cont + offsetof(SgContFrame, pc), cont->pc);
  }
}


static SgContFrame * print_cont1(SgContFrame *cont, SgVM *vm)
{
  int size = cont->size;
  SgString *pfmt = SG_MAKE_STRING("+   ~a=~38,,,,38s +~%");
  SgObject cl = SG_INTERN("cl");

  /* cont's size is argc of previous cont frame */
  /* dump arguments */
  if (IN_STACK_P((SgObject*)cont, vm)) {
    SgObject *current = (SgObject *)cont;
    int i;
    for (i = 0; i < size; i++, current--) {
      print_argument(vm, cont, current-1, *(current-1));
    }
    if (size > 0) {
      Sg_Printf(vm->logPort, 
	UC(";; %p +---------------------------------------------+ < args\n"),
	current);
    }
  } else {
    int i;
    for (i = 0; i < size; i++) {
      print_argument(vm, cont, cont->env+i, *(cont->env+i));
    }
    if (size > 0) {
      Sg_Printf(vm->logPort, 
	UC(";; %p +---------------------------------------------+ < args/heap\n"),
	cont->env+size);
    }
  }

  Sg_Printf(vm->logPort, UC(";; %p +   fp=%#38p +\n"),
	    (uintptr_t)cont + offsetof(SgContFrame, fp), cont->fp);
  Sg_Printf(vm->logPort, UC(";; %p "),
	    (uintptr_t)cont + offsetof(SgContFrame, cl));
  if (cont->cl) {
    Sg_Format(vm->logPort, pfmt, SG_LIST2(cl, cont->cl), TRUE);
  } else {
    Sg_Format(vm->logPort, pfmt, SG_LIST2(cl, SG_FALSE), TRUE);
  }
  print_pc(vm, pfmt, cont);
#if SIZEOF_LONG == 8
  Sg_Printf(vm->logPort, UC(";; %p + type=%#38d +\n"),
	    (uintptr_t)cont + offsetof(SgContFrame, type), cont->type);
  Sg_Printf(vm->logPort, UC(";; %p + size=%#38d +\n"),
	    (uintptr_t)cont + offsetof(SgContFrame, size), size);
#else
  Sg_Printf(vm->logPort, UC(";; %p + type=%#38d +\n"),
	    (uintptr_t)cont, cont->type);
  Sg_Printf(vm->logPort, UC(";; %p + size=%#38d +\n"),
	    (uintptr_t)cont, size);
#endif
  Sg_Printf(vm->logPort, UC(";; %p + prev=%#38p +\n"),
	    (uintptr_t)cont + offsetof(SgContFrame, prev), cont->prev);

  Sg_Printf(vm->logPort, 
	    UC(";; %p +---------------------------------------------+ < %s%s"),
	    cont,
	    (cont == CONT(vm)) ? UC("cont")
	    : (cont->prev) ? UC("prev")
	    : UC("prompt"),
	    !IN_STACK_P((SgObject *)cont, vm) ? UC("/heap") : UC(""));

  if (BOUNDARY_FRAME_MARK_P(cont)) Sg_Printf(vm->logPort, UC("(boundary)"));
  if (PROMPT_FRAME_MARK_P(cont)) {
    Sg_Printf(vm->logPort, UC("%S(%p)"), ((SgPrompt *)(cont->pc))->tag,
	      cont->pc);
  }
  Sg_Printf(vm->logPort, UC("\n"));

  return cont->prev;
}

static void print_frames(SgVM *vm, SgContFrame *cont)
{
  SgObject *stack = vm->stack, *sp = SP(vm);
  SgPromptNode *node = vm->prompts;

  Sg_Printf(vm->logPort, UC(";; stack: %p, cont: %p\n"), stack, vm->cont);
  Sg_Printf(vm->logPort, UC(";; sp: %p, fp: %p, pc: %p\n"), sp, vm->fp, vm->pc);

  Sg_Printf(vm->logPort, 
    UC(";; %p +---------------------------------------------+ < top%s%s%s\n"),
    cont, 
    !IN_STACK_P((SgObject *)cont, vm) ? UC("/heap") : UC(""), 
    (intptr_t)cont == (intptr_t)sp-1 ? UC("/sp") : UC(""), 
    (intptr_t)vm->fp == (intptr_t)cont ? UC("/fp") : UC(""));
  while (!bottom_cont_frame_p(vm, cont)) {
    cont = print_cont1(cont, vm);
  }
  
  Sg_Printf(vm->logPort,
    UC(";; %p +---------------------------------------------+ < bottom\n"),
    stack);

  if (node) {
    Sg_Printf(vm->logPort, UC(";; Prompt chain\n"));
    Sg_Printf(vm->logPort, UC(";; [%S:%S %p]"),
	      node->prompt->tag, node->prompt->handler, node->prompt);
    node = node->next;
    while (node) {
      Sg_Printf(vm->logPort, UC(" => [%S:%S %p]"),
		node->prompt->tag, node->prompt->handler, node->prompt);
      node = node->next;
    }
    Sg_Printf(vm->logPort, UC("\n"));
  }
  
}

void Sg_VMPrintFrame()
{
  print_frames(Sg_VM(), CONT(Sg_VM()));
}

void Sg_VMPrintFrameOf(SgVM *vm)
{
  print_frames(vm, CONT(vm));
}

#ifdef PROF_INSN
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

#define FETCH_OPERAND(pc)  SG_OBJ((*(pc)++))
#define PEEK_OPERAND(pc)   ((intptr_t)(*(pc)))

#ifdef __GNUC__
# define SWITCH(val)        goto *dispatch_table[val];
# define CASE(insn)         	/* dummy */
# define DISPATCH		/* empty */
# define NEXT							\
  do {								\
    c = (SgWord)FETCH_OPERAND(PC(vm));				\
    COUNT_INSN(c);						\
    goto *dispatch_table[INSN(c)];				\
  } while (0)
# define DEFAULT            	/* dummy */
#else
# define SWITCH(val)        switch (val)
# define CASE(insn)         case insn :
# define NEXT               goto dispatch;
# define DISPATCH           dispatch:
# define DEFAULT            default:
#endif

/* 
   VM interruption happens only certain situation like the followings:
    - GC
    - Thread interrpution (only POSIX)
    - Thread stop/termination
   Thread related thing does not really matter since it just proceed the
   process. So what we need to care is GC. Thus just referring variable
   doesn't have to check such as LREF.   
 */
#define CHECK_ATTENTION							\
  do { if (vm->attentionRequest) goto process_queue; } while (0)

#ifdef _MSC_VER
# pragma warning( push )
# pragma warning( disable : 4102 4101)
#endif

SgObject run_loop()
{
  SgVM *vm = Sg_VM();

#ifdef __GNUC__
  static void *dispatch_table[INSTRUCTION_COUNT] = {
#define DEFINSN(insn, vals, argc, src, label) && SG_CPP_CAT(label_, insn),
#include "vminsn.c"
#undef DEFINSN
  };
#endif	/* __GNUC__ */

  for (;;) {
    SgWord c;

    DISPATCH;
    /* if (vm->attentionRequest) goto process_queue; */
    c = (SgWord)FETCH_OPERAND(PC(vm));
    COUNT_INSN(c);
    SWITCH(INSN(c)) {
#define VM_LOOP
#include "vminsn.c"
#undef VM_LOOP
      DEFAULT {
#ifdef _MSC_VER
	__assume(0);
#else
	Sg_Panic("unknown instruction appeard. %08x", c);
#endif
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

#undef REFER_GLOBAL
#undef FIND_GLOBAL
}
#ifdef _MSC_VER
# pragma warning( pop )
#endif

void Sg__InitVM()
{
  SgVM *rootVM;
  /* this env is p1env and it must be 5 elements vector for now. */
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
  root = Sg_NewKernel(rootVM);	/* hmmmm, should this be here? */

  rootVM->threadState = SG_VM_RUNNABLE;
  rootVM->currentLibrary = Sg_FindLibrary(SG_INTERN("user"), FALSE);
  /* mark as this is toplevel library. */
  SG_LIBRARY_DEFINEED(rootVM->currentLibrary) = SG_FALSE;
  
  /* load path */
  rootVM->loadPath = Sg_GetDefaultLoadPath();
  rootVM->dynamicLoadPath = Sg_GetDefaultDynamicLoadPath();

  SG_PROCEDURE_NAME(&default_exception_handler_rec) =
    SG_MAKE_STRING("default-exception-handler");
  Sg_InitMutex(&global_lock, TRUE);

  SG_PROCEDURE_NAME(&internal_toplevel_closure) = SG_INTERN("%internal-closure");
#ifdef PROF_INSN
  Sg_AddCleanupHandler(show_inst_count, NULL);
#endif
  sym_continuation = Sg_MakeSymbol(SG_MAKE_STRING("continuation"), FALSE);
}

void Sg__PostInitVM()
{
  SgObject lib = Sg_FindLibrary(SG_INTERN("(core errors)"), FALSE);
  SgObject b = Sg_FindBinding(lib, SG_INTERN("raise"), SG_UNBOUND);
  if (SG_UNBOUNDP(b)) {
    Sg_Panic("`raise` was not found.");
  }
  raise_proc = SG_GLOC_GET(SG_GLOC(b));
  b = Sg_FindBinding(lib, SG_INTERN("raise-continuable"), SG_UNBOUND);
  if (SG_UNBOUNDP(b)) {
    Sg_Panic("`raise-continuable` was not found.");
  }
  raise_continuable_proc = SG_GLOC_GET(SG_GLOC(b));

  lib = Sg_FindLibrary(SG_INTERN("(sagittarius compiler)"), FALSE);
  b = Sg_FindBinding(lib, SG_INTERN("init-compiler"), SG_UNBOUND);
  if (SG_UNBOUNDP(b)) {
    Sg_Panic("`init-compiler` was not found.");
  }
  Sg_Apply0(SG_GLOC_GET(SG_GLOC(b)));
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
