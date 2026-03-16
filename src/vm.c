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
#include "sagittarius/private/exceptions.h"
#include "sagittarius/private/file.h"
#include "sagittarius/private/generic.h"
#include "sagittarius/private/hashtable.h"
#include "sagittarius/private/identifier.h"
#include "sagittarius/private/kernel.h"
#include "sagittarius/private/library.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/parameter.h"
#include "sagittarius/private/port.h"
#include "sagittarius/private/transcoder.h"
#include "sagittarius/private/record.h"
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
static INLINE SgObject run_loop();
static INLINE void ** vm_new_cont(SgCContinuationProc *after, int datasize);

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

static SgClass *violation_cpl[] = {
  SG_VIOLATION_CPL,
  NULL
};

static void cont_violation_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<&continuation %S>"),
	    SG_CONTINUATION_VIOLATION_PROMPT_TAG(o));
}

static SgObject cont_violation_allocate(SgClass *klass, SgObject initargs)
{
  SgContinuationViolation *c = SG_ALLOCATE(SgContinuationViolation, klass);
  SG_SET_CLASS(c, klass);
  return SG_OBJ(c);
}

static void cont_violation_tag_set(SgContinuationViolation *c, SgObject tag)
{
  if (!SG_CONTINUATION_VIOLATIONP(c)) {
    Sg_Error(UC("&continuation required but got %S"), c);
  }
  SG_CONTINUATION_VIOLATION_PROMPT_TAG(c) = tag;
}

static SgObject cont_violation_tag(SgContinuationViolation *c)
{
  return SG_CONTINUATION_VIOLATION_PROMPT_TAG(c);
}

static SgSlotAccessor cont_violation_slot[] = {
  SG_CLASS_SLOT_SPEC("prompt-tag", 0, cont_violation_tag, cont_violation_tag_set),
  {{ NULL }}
};

SG_DEFINE_BASE_CLASS(Sg_ContinuationViolationClass, SgContinuationViolation,
		     cont_violation_printer, NULL, NULL, cont_violation_allocate,
		     violation_cpl);

static void dw_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  if (SG_WRITE_MODE(ctx) == SG_WRITE_DISPLAY) {
    Sg_Printf(port, UC("#<dynamic-winder %p>"), obj);
  } else {
    Sg_Printf(port, UC("#<dynamic-winder %p %S:%S>"),
	      obj,
	      SG_DYNAMIC_WINDER_BEFORE(obj),
	      SG_DYNAMIC_WINDER_AFTER(obj));
  }
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_DynamicWinderClass, dw_print);

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

static SgObject currentInputPort = SG_FALSE;
static SgObject currentOutputPort = SG_FALSE;
static SgObject currentErrorPort = SG_FALSE;

SgObject Sg_CurrentOutputPort()
{
  SgObject r;
  SG_CALL_SUBR0(r, currentOutputPort);
  return r;
}

SgObject Sg_CurrentErrorPort()
{
  SgObject r;
  SG_CALL_SUBR0(r, currentErrorPort);
  return r;
}

SgObject Sg_CurrentInputPort()
{
  SgObject r;
  SG_CALL_SUBR0(r, currentInputPort);
  return r;
}

void Sg_SetCurrentOutputPort(SgObject p)
{
  if (!SG_OUTPUT_PORTP(p)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("current-output-port"),
				    SG_MAKE_STRING("output port"),
				    p, SG_NIL);
  }
  Sg_VM()->currentOutputPort = p;
}

void Sg_SetCurrentErrorPort(SgObject p)
{
  if (!SG_OUTPUT_PORTP(p)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("current-error-port"),
				    SG_MAKE_STRING("output port"),
				    p, SG_NIL);
  }
  Sg_VM()->currentErrorPort = p;
}

void Sg_SetCurrentInputPort(SgObject p)
{
  if (!SG_INPUT_PORTP(p)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("current-input-port"),
				    SG_MAKE_STRING("input port"),
				    p, SG_NIL);
  }
  Sg_VM()->currentInputPort = p;
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
  void **data = vm_new_cont(next_eval_cc, 1);
  data[0] = env;
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
  if (SG_UNDEFP(pass1_import)) {
    init_pass1_import();
  }
  /* make spec look like import-spec */
  spec = Sg_Cons(SG_INTERN("import"), spec);

  void **data = vm_new_cont(environment_cc, 1);
  data[0] = lib;
  return Sg_VMApply2(pass1_import, spec, lib);
}

static void print_frames(SgVM *vm, SgContFrame *cont);
static void print_prompts(SgVM *vm, SgPromptNode *node);
static void print_marks(SgVM *vm, SgContMarks *marks);
static void print_summary(SgVM *vm);
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

static void push_cont_marks(SgVM *vm, SgContFrame *cont)
{
  SgContMarks *cm = SG_NEW(SgContMarks);
  cm->frame = cont;
  cm->entries = NULL;
  cm->prev = vm->marks;
  vm->marks = cm;
}

static INLINE void ** vm_new_cont(SgCContinuationProc *after, int datasize)
{
  SgVM *vm = Sg_VM();

  CHECK_STACK(CONT_FRAME_SIZE + datasize, vm);
  SgObject *s = SP(vm);
  SgContFrame *cc = (SgContFrame*)s;
  s += CONT_FRAME_SIZE;
  cc->prev = CONT(vm);
  cc->size = datasize;
  cc->type = 0;
  cc->pc = (SgWord*)after;
  cc->fp = C_CONT_MARK;
  cc->cl = CL(vm);
  push_cont_marks(vm, cc);
  CONT(vm) = cc;
  FP(vm) = SP(vm) = s + datasize;
  return s;
}

void Sg_VMPushCC(SgCContinuationProc *after, void **data, int datasize)
{
  SgObject *s = vm_new_cont(after, datasize);
  for (int i = 0; i < datasize; i++) {
    PUSH(s, SG_OBJ(data[i]));
  }
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
    push_cont_marks(vm, newcont);		\
    CONT(vm) = newcont;				\
    SP(vm) += CONT_FRAME_SIZE;			\
  } while (0)

#define PUSH_CONT(vm, next_pc) PUSH_CONT_REC(vm, next_pc, NORMAL_FRAME)
#define PUSH_BOUNDARY_CONT(vm)					\
  PUSH_CONT_REC(vm, BOUNDARY_FRAME_MARK, BOUNDARY_FRAME)
#define PUSH_PROMPT_CONT(vm, tag) do {		\
    PUSH_CONT_REC(vm, tag, PROMPT_FRAME);	\
    FP(vm) = SP(vm);				\
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

static SgObject make_dynamic_winder(SgObject before, SgObject after)
{
  SgDynamicWinder *dw = SG_NEW(SgDynamicWinder);
  SG_SET_CLASS(dw, SG_CLASS_DYNAMIC_WINDER);
  dw->before = before;
  dw->after = after;
  dw->marks = theVM->marks;
  return SG_OBJ(dw);
}

SgObject Sg_VMDynamicWind(SgObject before, SgObject thunk, SgObject after)
{
  void **data = vm_new_cont(dynamic_wind_before_cc, 3);
  data[0] = (void*)before;
  data[1] = (void*)thunk;
  data[2] = (void*)after;

  return Sg_VMApply0(before);
}

static SgObject dynamic_wind_before_cc(SgObject result, void **data)
{
  SgObject before = SG_OBJ(data[0]);
  SgObject body = SG_OBJ(data[1]);
  SgObject after = SG_OBJ(data[2]);
  SgVM *vm = Sg_VM();
  SgObject dw = make_dynamic_winder(before, after);
  SgObject prev = vm->dynamicWinders;
  void **d = vm_new_cont(dynamic_wind_body_cc, 2);
  d[0] = (void*)dw;
  d[1] = (void*)prev;
  vm->dynamicWinders = Sg_Cons(dw, prev);

  return Sg_VMApply0(body);
}

static SgObject dynamic_wind_body_cc(SgObject result, void **data)
{
  SgObject dw = SG_OBJ(data[0]);
  SgObject prev = SG_OBJ(data[1]);
  SgVM *vm = Sg_VM();
  
  vm->dynamicWinders = prev;
  void **d = vm_new_cont(dynamic_wind_after_cc, 3);
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
  return Sg_VMApply0(SG_DYNAMIC_WINDER_AFTER(dw));
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
static void save_cont(SgVM *vm)
{
  SgContFrame *c = CONT(vm), *prev = NULL;
  SgCStack *cstk;
  SgContinuation *ep;
  SgPromptNode *node;
  SgContMarks *marks;

  if (!(IN_STACK_P((SgObject *)c, vm))) return;
  
  do {
    SgContFrame *csave, *tmp;
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
  for (marks = vm->marks; marks; marks = marks->prev) {
    if (FORWARDED_CONT_P(marks->frame)) {
      marks->frame = FORWARDED_CONT(marks->frame);
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

/* Forward declaration */
static void rebuild_prompts_from_cont(SgVM *vm);

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

static SgPromptNode *search_prompt_node(SgVM *vm, SgObject tag)
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

static int has_barrier_node(SgVM *vm, SgObject tag)
{
  SgPromptNode *node = vm->prompts;
  
  /* search tag */
  while (node) {
    if (node->prompt->tag == tag) return FALSE;
    if (node->prompt->barrierP) return TRUE;
    node = node->next;
  }
  return FALSE;
}

/*
  Check if invoking a continuation would enter a barrier from outside.
  This happens when:
  - The continuation was captured inside a barrier
  - We're currently outside that barrier
  - Invoking would restore to inside the barrier = "entering" = error
  
  Returns the barrier prompt if entering would occur, NULL otherwise.
*/
static SgPrompt *check_barrier_entry(SgVM *vm, SgContinuation *c)
{
  SgContFrame *cont = c->cont;
  
  /* Scan saved continuation frames for barrier prompts */
  while (!bottom_cont_frame_p(vm, cont)) {
    if (PROMPT_FRAME_MARK_P(cont)) {
      SgPrompt *p = (SgPrompt *)cont->pc;
      if (p->barrierP) {
        /* Check if this barrier is in current prompt chain */
        SgPromptNode *node = vm->prompts;
        int found = FALSE;
        while (node) {
          if (node->prompt == p) {
            found = TRUE;
            break;
          }
          node = node->next;
        }
        if (!found) {
          /* Barrier in saved state but not current -> entering barrier */
          return p;
        }
      }
    }
    cont = cont->prev;
  }
  return NULL; /* ok, no barrier entry */
}

static int cont_prompt_match_p(SgContFrame *c, SgPrompt *prompt)
{
  return c && PROMPT_FRAME_MARK_P(c) && ((SgPrompt *)(c->pc)) == prompt;
}


static SgContFrame * splice_cont(SgVM *vm, SgContFrame *saved,
				 SgPrompt *prompt)
{
  SgContFrame *cur = vm->cont, *c = saved->prev, *cp = NULL, *top = NULL;
  SgPromptNode *node = vm->prompts;
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

static SgContMarks * splice_marks(SgVM *vm, SgContMarks *marks)
{
  /* here we need to copy the mark chain like the cont */
  SgContMarks *head = NULL, *tail = NULL;

  if (!marks) {
    /* No marks to splice, just return current marks */
    return vm->marks;
  }

  while (marks) {
    SgContMarks *copy = SG_NEW(SgContMarks);
    copy->frame = marks->frame;
    copy->entries = marks->entries;
    copy->prev = head;
    if (!head) head = tail = copy;
    else head = copy;
    marks = marks->prev;
  }
  tail->prev = vm->marks;
  return head;
}

static SgObject merge_winders(SgObject, SgObject);
static SgObject take_prompt_winders(SgPrompt *, SgObject);
static SgObject capture_prompt_winders(SgPrompt *, SgObject);

/* Forward declarations for delimited call/cc support */
static SgPrompt *make_prompt(SgObject tag, SgObject handler, SgVM *vm);
static void install_prompt(SgVM *, SgPrompt *);
static void remove_prompt(SgVM *, SgPrompt *);
static SgPromptNode * remove_prompts(SgVM *, SgObject);
static void continuation_violation(SgObject who, SgObject message,
				   SgObject promptTag);

/*
  Unified continuation invocation infrastructure.
  
  All continuation operations (throw_continuation_body, throw_delimited_continuation_body,
  abort_body, call_in_cont_body) share a common pattern:
  1. Process handlers one by one (calling before/after thunks)
  2. Update dynamic winders appropriately
  3. Execute operation-specific completion logic
  
  This infrastructure unifies the handler iteration and provides
  a clean dispatch mechanism for the completion step.
 */

/* Operation types for continuation invocation */
typedef enum {
  CONT_OP_THROW,          /* Regular continuation throw */
  CONT_OP_THROW_DELIMITED,/* Delimited continuation throw */
  CONT_OP_ABORT,          /* Abort to prompt */
  CONT_OP_CALL_IN_CONT    /* Call proc in continuation context */
} ContOpType;

/* Winder merge policy */
typedef enum {
  WINDER_MERGE,  /* Merge with current dynamic winders */
  WINDER_SET     /* Directly set dynamic winders */
} WinderPolicy;

/* Context for continuation invocation operations */
typedef struct ContInvokeCtxRec {
  ContOpType op;           /* Operation type */
  WinderPolicy winder_policy; /* How to update winders */
  SgObject handlers;       /* Remaining handlers to process */
  SgContinuation *cont;    /* Target continuation (NULL for abort) */
  SgPrompt *prompt;        /* Associated prompt */
  SgObject arg1;           /* Primary argument (args for throw/abort, proc for call_in_cont) */
  SgObject arg2;           /* Secondary argument (args for call_in_cont) */
} ContInvokeCtx;

/* Forward declarations */
static SgObject cont_invoke_body(ContInvokeCtx *ctx);
static SgObject cont_invoke_cc(SgObject result, void **data);
static SgObject cont_invoke_complete(ContInvokeCtx *ctx);

#define CONT_ERR(who, msg, tag)						\
  continuation_violation(SG_INTERN(who), SG_MAKE_STRING(msg), tag)


/* Forward declarations for operations used by completion handlers */
static SgObject throw_continuation_end(SgVM *, SgObject);
static SgPromptNode * search_prompt_node(SgVM *, SgObject);
static SgObject throw_cont_compute_handlers(SgContinuation *, SgPrompt *, SgVM *);
static SgObject abort_invoke_handler(SgPrompt *, SgObject);
static SgObject strip_delimied_cc_handlers(SgObject);
static int winder_in_scope_p(SgVM *, SgObject);
static SgContMarks * strip_marks(SgVM *, SgPromptNode *);

/*
  Unified continuation invocation: handler iteration callback.
  This is called after each handler (before/after thunk) completes.
 */
static SgObject cont_invoke_cc(SgObject result, void **data)
{
  ContInvokeCtx ctx;
  SgVM *vm = theVM;
  SgContMarks *saved_marks;

  ctx.op = (ContOpType)(intptr_t)data[0];
  ctx.winder_policy = (WinderPolicy)(intptr_t)data[1];
  ctx.handlers = SG_OBJ(data[2]);
  ctx.cont = (SgContinuation*)data[3];
  ctx.prompt = (SgPrompt*)data[4];
  ctx.arg1 = SG_OBJ(data[5]);
  ctx.arg2 = SG_OBJ(data[6]);
  saved_marks = (SgContMarks*)data[7];

  /* Restore marks that were saved before calling the handler, UNLESS
     this is a composable continuation (shift/reset) where we want to
     keep the invocation context's marks for `temporarily` to work. */
  if (!(ctx.op == CONT_OP_THROW &&
        ctx.prompt &&
        ctx.cont &&
        ctx.cont->type == SG_COMPOSABLE_CONTINUATION)) {
    vm->marks = saved_marks;
  }

  /* For abort operation, skip handlers no longer in scope.
     Handler format: ((marks . thunk) . chain) - extract thunk for scope check */
  if (ctx.op == CONT_OP_ABORT) {
    while (SG_PAIRP(ctx.handlers) &&
	   !winder_in_scope_p(vm, SG_CDR(SG_CAAR(ctx.handlers)))) {
      ctx.handlers = SG_CDR(ctx.handlers);
    }
  }
  /* For delimited continuation, strip handlers outside prompt scope */
  if (ctx.op == CONT_OP_THROW_DELIMITED) {
    ctx.handlers = strip_delimied_cc_handlers(ctx.handlers);
  }

  return cont_invoke_body(&ctx);
}

/*
  Unified continuation invocation: main iteration body.
  Processes handlers one by one, then calls completion handler.
 */
static SgObject cont_invoke_body(ContInvokeCtx *ctx)
{
  SgVM *vm = theVM;

  if (SG_PAIRP(ctx->handlers)) {
    /* Handler format: ((marks . thunk) . chain) */
    SgObject handler_entry = SG_CAAR(ctx->handlers);
    SgContMarks *winder_marks = (SgContMarks *)SG_CAR(handler_entry);
    SgObject handler = SG_CDR(handler_entry);
    SgObject chain = SG_CDAR(ctx->handlers);

    /* Save current marks for restoration after handler returns */
    SgContMarks *saved_marks = vm->marks;

    void **data = vm_new_cont(cont_invoke_cc, 8);
    /* Pack context into data array for CC callback */
    data[0] = (void*)(intptr_t)ctx->op;
    data[1] = (void*)(intptr_t)ctx->winder_policy;
    data[2] = (void*)SG_CDR(ctx->handlers);
    data[3] = (void*)ctx->cont;
    data[4] = (void*)ctx->prompt;
    data[5] = (void*)ctx->arg1;
    data[6] = (void*)ctx->arg2;
    data[7] = (void*)saved_marks;  /* Save marks for restoration */

    /* Update dynamic winders based on policy */
    if (ctx->winder_policy == WINDER_MERGE && ctx->prompt) {
      vm->dynamicWinders = merge_winders(vm->dynamicWinders, chain);
    } else {
      vm->dynamicWinders = chain;
    }

    /* Restore the winder's marks so handler runs with the parameterization
       that was active when the dynamic-wind was installed.
       
       EXCEPTION: For COMPOSABLE continuations (shift/reset with
       SG_COMPOSABLE_CONTINUATION type), keep current marks so handlers
       like `temporarily` work correctly with the invocation context.
       This is needed because `temporarily` swaps parameter values using
       the current parameterization, not the captured one. */
    if (!(ctx->op == CONT_OP_THROW &&
          ctx->cont &&
          ctx->cont->type == SG_COMPOSABLE_CONTINUATION)) {
      vm->marks = winder_marks;
    }

    return Sg_VMApply0(handler);
  } else {
    /* All handlers processed - dispatch to operation-specific completion */
    return cont_invoke_complete(ctx);
  }
}

/*
  Unified continuation invocation: operation-specific completion.
  Called after all handlers have been processed.
 */
static SgObject cont_invoke_complete(ContInvokeCtx *ctx)
{
  SgVM *vm = theVM;

  switch (ctx->op) {
  case CONT_OP_THROW: {
    /* Regular continuation throw completion */
    SgContinuation *c = ctx->cont;
    SgPrompt *prompt = ctx->prompt;
    SgObject args = ctx->arg1;

    if (c->cstack == NULL) save_cont(vm);
    if (prompt) {
      /* Composable continuation: add frames atop current continuation */
      vm->cont = splice_cont(vm, c->cont, prompt);
      vm->marks = splice_marks(vm, c->marks);
      if (c->winders != vm->dynamicWinders) {
	SgObject to_merge = capture_prompt_winders(prompt, c->winders);
	vm->dynamicWinders = merge_winders(to_merge, vm->dynamicWinders);
      }
    } else {
      /* Full continuation: replace current */
      vm->cont = c->cont;
      vm->marks = c->marks;
      vm->dynamicWinders = c->winders;
      rebuild_prompts_from_cont(vm);
    }
    return throw_continuation_end(vm, args);
  }

  case CONT_OP_THROW_DELIMITED: {
    /* Delimited continuation throw completion */
    SgContinuation *c = ctx->cont;
    SgPrompt *prompt = ctx->prompt;
    SgObject args = ctx->arg1;

    SgPromptNode *node = remove_prompts(vm, prompt->tag);
    if (!node) {
      CONT_ERR("delimited-continuation",
	       "Stale prompt in delimited continuation", prompt->tag);
    }

    remove_prompt(vm, node->prompt);
    vm->cont = node->frame->prev;
    vm->marks = strip_marks(vm, node);

    /* Create new prompt for the continuation */
    SgPrompt *new_prompt = make_prompt(prompt->tag, SG_FALSE, vm);

    CHECK_STACK(CONT_FRAME_SIZE, vm);
    PUSH_PROMPT_CONT(vm, new_prompt);
    install_prompt(vm, new_prompt);

    if (!SG_FALSEP(node->prompt->handler)) {
      if (node->prompt != prompt) {
	save_cont(vm);
	vm->cont = splice_cont(vm, c->cont, prompt);
	vm->marks = splice_marks(vm, c->marks);
      }
      return throw_continuation_end(vm, args);
    }

    /* Continue as composable continuation */
    {
      ContInvokeCtx throw_ctx;
      SgObject wind_handlers = throw_cont_compute_handlers(c, prompt, vm);
      throw_ctx.op = CONT_OP_THROW;
      throw_ctx.winder_policy = WINDER_MERGE;
      throw_ctx.handlers = wind_handlers;
      throw_ctx.cont = c;
      throw_ctx.prompt = prompt;
      throw_ctx.arg1 = args;
      throw_ctx.arg2 = SG_NIL;
      return cont_invoke_body(&throw_ctx);
    }
  }

  case CONT_OP_ABORT: {
    /* Abort to prompt completion */
    SgPrompt *prompt = ctx->prompt;
    SgObject args = ctx->arg1;

    SgPromptNode *cur_node = remove_prompts(vm, prompt->tag);
    if (!cur_node)
      CONT_ERR("abort-current-continuation", "Stale prompt", prompt->tag);
    prompt = cur_node->prompt;

    if (prompt->cstack != vm->cstack) {
      vm->escapeReason = SG_VM_ESCAPE_ABORT;
      vm->escapeData[0] = cur_node;
      vm->escapeData[1] = args;
      longjmp(prompt->cstack->jbuf, 1);
    }

    remove_prompt(vm, prompt);
    vm->cont = cur_node->frame->prev;
    vm->marks = strip_marks(vm, cur_node);
    return abort_invoke_handler(prompt, args);
  }

  case CONT_OP_CALL_IN_CONT: {
    /* Call proc in continuation context completion */
    SgContinuation *c = ctx->cont;
    SgPrompt *prompt = ctx->prompt;
    SgObject proc = ctx->arg1;
    SgObject args = ctx->arg2;

    if (c->cstack == NULL) save_cont(vm);
    if (prompt && c->type == SG_COMPOSABLE_CONTINUATION) {
      vm->cont = splice_cont(vm, c->cont, prompt);
      vm->marks = splice_marks(vm, c->marks);
      if (c->winders != vm->dynamicWinders) {
	SgObject to_merge = capture_prompt_winders(prompt, c->winders);
	vm->dynamicWinders = merge_winders(to_merge, vm->dynamicWinders);
      }
    } else if (prompt && c->type == SG_DELIMIETED_CONTINUATION) {
      vm->cont = c->cont;
      {
	SgContMarks *marks = c->marks;
	while (marks && !marks->entries) {
	  marks = marks->prev;
	}
	vm->marks = marks;
      }
      vm->dynamicWinders = c->winders;
      rebuild_prompts_from_cont(vm);
    } else {
      vm->cont = c->cont;
      vm->marks = c->marks;
      vm->dynamicWinders = c->winders;
      rebuild_prompts_from_cont(vm);
    }

    if (SG_NULLP(args)) return Sg_VMApply0(proc);
    return Sg_VMApply(proc, args);
  }

  default:
    Sg_Error(UC("Unknown continuation operation type: %d"), ctx->op);
    return SG_UNDEF;
  }
}

/* Forward declaration */
static SgObject throw_delimited_continuation_body(SgObject,
						  SgContinuation *,
						  SgObject,
						  SgPrompt *);

static SgObject throw_continuation_body(SgObject handlers,
					SgContinuation *c,
					SgObject args,
					SgPrompt *prompt)
{
  ContInvokeCtx ctx;
  ctx.op = CONT_OP_THROW;
  ctx.winder_policy = prompt ? WINDER_MERGE : WINDER_SET;
  ctx.handlers = handlers;
  ctx.cont = c;
  ctx.prompt = prompt;
  ctx.arg1 = args;
  ctx.arg2 = SG_NIL;
  return cont_invoke_body(&ctx);
}

static SgObject throw_continuation_end(SgVM *vm, SgObject args)
{
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

/* Returns only winders from the input list that are NOT in prompt->winders.
   Stops when we reach a winder that IS in prompt->winders.
   This is used when capturing a composable continuation to only include
   winders installed after the prompt. Returns a new list (copies winders). */
static SgObject capture_prompt_winders(SgPrompt *prompt, SgObject winders)
{
  SgObject p, h = SG_NIL, t = SG_NIL;
  if (SG_NULLP(winders)) return winders;

  SG_FOR_EACH(p, winders) {
    if (SG_FALSEP(Sg_Memq(SG_CAR(p), prompt->winders))) {
      SG_APPEND1(h, t, SG_CAR(p));
    } else {
      /* Found a winder that's in the prompt's winders - stop here */
      break;
    }
  }
  return h;
}

/* Returns the tail of the winders list starting from the first winder
   that is NOT in prompt->winders. This preserves list structure for
   chain computation in handler invocation. */
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
    SgObject seen_afters = SG_NIL;
    if (prompt) current = take_prompt_winders(prompt, current);
    SG_FOR_EACH(p, current) {
      SgDynamicWinder *winder = SG_DYNAMIC_WINDER(SG_CAR(p));
      if (!SG_FALSEP(Sg_Memq(SG_CAR(p), escapes))) {
        /* Track after thunks from common winders to skip duplicates */
        if (SG_FALSEP(Sg_Memq(winder->after, seen_afters))) {
          seen_afters = Sg_Cons(winder->after, seen_afters);
        }
        continue;
      }
      /* Skip non-common winders whose after was already in a common winder */
      if (!SG_FALSEP(Sg_Memq(winder->after, seen_afters))) {
        continue;
      }
      /* Handler format: ((marks . thunk) . chain) */
      SG_APPEND1(h, t, Sg_Cons(Sg_Cons(SG_OBJ(winder->marks), winder->after), SG_CDR(p)));
    }
  }

  SG_FOR_EACH(p, target) {
    SgDynamicWinder *winder = SG_DYNAMIC_WINDER(SG_CAR(p));
    SgObject chain = Sg_Memq(SG_CAR(p), escapes);
    SgObject next_winders = SG_CDR(chain);
    /* For composable continuations, don't include winders that were
       in place when the prompt was created - they're outside the
       continuation's scope and shouldn't be added to vm->dynamicWinders */
    if (prompt && SG_PAIRP(next_winders)) {
      SgObject w = SG_CAR(next_winders);
      if (!SG_FALSEP(Sg_Memq(w, prompt->winders))) {
        next_winders = SG_NIL;
      }
    }
    /* Handler format: ((marks . thunk) . chain) */
    SG_APPEND1(h, t, Sg_Cons(Sg_Cons(SG_OBJ(winder->marks), winder->before), next_winders));
  }

  return h;
}

static SgObject throw_continuation(SgObject *argv, int argc, void *data)
{
  SgContinuation *c = (SgContinuation*)SG_CAR(data);
  SgObject handlers;
  SgVM *vm = Sg_VM();
  SgPrompt *prompt = (SgPrompt *)SG_CDR(data);
  SgPrompt *barrier;

  /* Check if we're trying to enter a barrier from outside */
  barrier = check_barrier_entry(vm, c);
  if (barrier) {
    CONT_ERR("continuation",
	     "Cannot apply continuation across barrier", barrier->tag);
  }

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
  if (c->type == SG_DELIMIETED_CONTINUATION) {
    /* here we emulate abort/cc */
    SgContinuation abort_c;
    SgPromptNode *node = search_prompt_node(vm, prompt->tag);
    SgPrompt *p;
    if (!node) {
      /* Prompt not on stack - raise continuation violation */
      CONT_ERR("continuation",
	       "Prompt not found for delimited continuation", prompt->tag);
    }
    p = node->prompt;
    abort_c.winders = p->winders;
    abort_c.cstack = p->cstack;
    handlers = throw_cont_compute_handlers(&abort_c, p, vm);
    return throw_delimited_continuation_body(handlers, c, argv[0], prompt);
  } else {
    handlers = throw_cont_compute_handlers(c, prompt, vm);
    /* Sg_Printf(Sg_StandardErrorPort(), UC("comp: vm->dw: %A\n"), vm->dynamicWinders); */
    /* Sg_Printf(Sg_StandardErrorPort(), UC("comp:  c->dw: %A\n"), c->winders); */
    /* Sg_Printf(Sg_StandardErrorPort(), UC("comp: hndlrs: %A\n"), handlers); */
    return throw_continuation_body(handlers, c, argv[0], prompt);
  }
}

static SgObject sym_continuation = SG_FALSE;
static SgObject make_cont_subr(SgContinuation *cont, SgPrompt *prompt)
{
  return Sg_MakeSubr(throw_continuation, Sg_Cons(cont, prompt), 0, 1,
		     sym_continuation);
}

static void continuation_violation(SgObject who,
				   SgObject message,
				   SgObject promptTag)
{
  SgContinuationViolation *c = SG_NEW(SgContinuationViolation);
  SG_SET_CLASS(c, SG_CLASS_CONTINUATION_VIOLATION);
  c->promptTag = promptTag;
  Sg_Raise(Sg_Condition(SG_LIST3(c, Sg_MakeWhoCondition(who),
				 Sg_MakeMessageCondition(message))),
	   FALSE);
}
    
int Sg_ContinuationP(SgObject o)
{
  return SG_SUBRP(o) && SG_EQ(SG_PROCEDURE_NAME(o), sym_continuation);
}

int Sg_ComposableContinuationP(SgObject o)
{
  return Sg_ContinuationP(o) &&
    ((SgContinuation *)SG_CAR(SG_SUBR_DATA(o)))->type == SG_COMPOSABLE_CONTINUATION;
}

int Sg_ContinuationPromptAvailableP(SgObject tag, SgObject k)
{
  SgContFrame *cont = NULL;
  SgPrompt *boundary = NULL;
  SgVM *vm = theVM;
  int type = SG_FULL_CONTINUATION;
  if (SG_FALSEP(k)) {
    cont = vm->cont;
  } else if (Sg_ContinuationP(k)) {
    SgContinuation *c = (SgContinuation *)SG_CAR(SG_SUBR_DATA(k));
    boundary = (SgPrompt *)SG_CDR(SG_SUBR_DATA(k));
    cont = c->cont;
    type = c->type;
  } else {
    Sg_Error(UC("continuation or #f is required but got %S"), k);
  }
  while (!bottom_cont_frame_p(vm, cont)) {
    if (PROMPT_FRAME_MARK_P(cont)) {
      SgPrompt *p = (SgPrompt *)cont->pc;
      /* a bit weird, but Racket and SRFI-226 seems like this */
      if (type == SG_COMPOSABLE_CONTINUATION) {
	/* composable, excludes boundary */
	if (p == boundary) return FALSE;
	if (p->tag == tag) return TRUE;
      } else {
	/* delimited, includes boundary */
	if (p->tag == tag) return TRUE;
	if (p == boundary) return FALSE;
      }
    }
    cont = cont->prev;
  }
  return FALSE;
}

/* Deep copy mark entries */
static SgMarkEntry* copy_mark_entries(SgMarkEntry *entries)
{
  SgMarkEntry *new_head = NULL, *new_tail = NULL;
  while (entries) {
    SgMarkEntry *copy = SG_NEW(SgMarkEntry);
    copy->key = entries->key;
    copy->value = entries->value;
    copy->next = NULL;
    if (!new_head) {
      new_head = new_tail = copy;
    } else {
      new_tail->next = copy;
      new_tail = copy;
    }
    entries = entries->next;
  }
  return new_head;
}

/* Deep copy the entire marks chain with entries */
static SgContMarks* deep_copy_marks(SgContMarks *marks)
{
  SgContMarks *new_chain = NULL, *tail = NULL;
  while (marks) {
    SgContMarks *copy = SG_NEW(SgContMarks);
    copy->frame = marks->frame;
    copy->entries = copy_mark_entries(marks->entries);
    copy->prev = NULL;
    if (!new_chain) {
      new_chain = tail = copy;
    } else {
      tail->prev = copy;
      tail = copy;
    }
    marks = marks->prev;
  }
  return new_chain;
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
  cont->type = SG_FULL_CONTINUATION;
  /* Deep copy marks to preserve capture-time values */
  cont->marks = deep_copy_marks(vm->marks);

  contproc = make_cont_subr(cont, NULL);
  return Sg_VMApply1(proc, contproc);
}

/* copy until the prompt node, deep-copying entries */
static SgContMarks * copy_marks(SgVM *vm, SgPromptNode *node)
{
  SgContMarks *marks = vm->marks, *new_chain = NULL;
  /* Copy marks until we reach the prompt's frame (exclusive) */
  while (marks && marks->frame != node->frame) {
    SgContMarks *copy = SG_NEW(SgContMarks);
    copy->frame = marks->frame;
    copy->entries = copy_mark_entries(marks->entries);
    copy->prev = new_chain;
    new_chain = copy;
    marks = marks->prev;
  }
  return new_chain;
}

static SgContMarks * strip_marks(SgVM *vm, SgPromptNode *node)
{
  SgContMarks *marks = vm->marks;
  while (marks && marks->frame != node->frame) marks = marks->prev;
  return marks;
}

/* call-with-composable-continuation */
SgObject Sg_VMCallComp(SgObject proc, SgObject tag)
{
  SgContinuation *cont;
  SgObject contproc;
  SgVM *vm = Sg_VM();
  SgPromptNode *node;

  if (has_barrier_node(vm, tag)) {
    CONT_ERR("call-with-composable-continuation",
	     "Cannot capture past continuation barrier", tag);
  }
  
  node = search_prompt_node(vm, tag);
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
  cont->type = SG_COMPOSABLE_CONTINUATION;
  cont->marks = copy_marks(vm, node);

  contproc = make_cont_subr(cont, node->prompt);

  return Sg_VMApply1(proc, contproc);
 err:
  CONT_ERR("call-with-composable-continuation", "Prompt tag not found", tag);
  return SG_UNDEF;		/* dummy */
}

/*
  Delimited call/cc - Racket-like call/cc with prompt support

  This creates a non-composable continuation that:
  1. When invoked, aborts to the prompt (running post-thunks)
  2. After abort, reinstalls a new prompt and applies the composable continuation

  The continuation procedure has data: (cont . tag)
  where cont is the composable continuation and tag identifies the boundary.
*/

static SgObject throw_delimited_continuation_body(SgObject handlers,
						  SgContinuation *c,
						  SgObject args,
						  SgPrompt *prompt)
{
  ContInvokeCtx ctx;
  ctx.op = CONT_OP_THROW_DELIMITED;
  ctx.winder_policy = WINDER_SET;  /* Always directly set winders */
  ctx.handlers = handlers;
  ctx.cont = c;
  ctx.prompt = prompt;
  ctx.arg1 = args;
  ctx.arg2 = SG_NIL;
  return cont_invoke_body(&ctx);
}

/* Strip the delimited cc handlers.
   If the cont frame is captured, then the captured frame may contain
   outside of the prompt chain.
   The VM dynamicWinders should contain only the prompt winders,
   so the handler is not in the winders, we should strip it.
   Handler format: ((marks . thunk) . chain)
 */
static SgObject strip_delimied_cc_handlers(SgObject handlers)
{
  SgVM *vm = theVM;
  while (!SG_NULLP(handlers)) {
    SgObject handler_entry = SG_CAAR(handlers);
    SgObject handler = SG_CDR(handler_entry);  /* Extract thunk from (marks . thunk) */
    SgObject cp;
    SG_FOR_EACH(cp, vm->dynamicWinders) {
      if (SG_EQ(SG_DYNAMIC_WINDER_BEFORE(SG_CAR(cp)), handler)
	  || SG_EQ(SG_DYNAMIC_WINDER_AFTER(SG_CAR(cp)), handler))
	goto end;
    }
    handlers = SG_CDR(handlers);
  }
 end:
  return handlers;
}

/* call-with-current-continuation with prompt delimiting (Racket-like call/cc) */
SgObject Sg_VMCallDelimitedCC(SgObject proc, SgObject tag)
{
  SgContinuation *cont;
  SgObject contproc;
  SgVM *vm = Sg_VM();
  SgPromptNode *node = search_prompt_node(vm, tag);

  if (!node) goto err;

  save_cont(vm);

  cont = SG_NEW(SgContinuation);
  /* Use capture_prompt_winders to only include winders installed
     AFTER the prompt.  Unlike take_prompt_winders which returns a
     tail, this returns a NEW list containing only the winders not in
     prompt->winders, stopping at the first common winder. */
  cont->winders = capture_prompt_winders(node->prompt, vm->dynamicWinders);
  cont->cont = vm->cont;
  cont->prev = NULL;
  cont->ehandler = SG_FALSE;
  cont->cstack = NULL;
  cont->type = SG_DELIMIETED_CONTINUATION;
  /* Deep copy marks to preserve capture-time values */
  cont->marks = copy_marks(vm, node);

  /* Pass the capture-time prompt so we know where to stop when splicing */
  contproc = make_cont_subr(cont, node->prompt);

  return Sg_VMApply1(proc, contproc);
 err:
  CONT_ERR("call-with-delimited-current-continuation",
	   "Prompt tag not found", tag);
  return SG_UNDEF;
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
  prompt->barrierP = FALSE;
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
      return;
    }
    prev = node;
    node = node->next;
  }
}

/* remove prompts up to the tag */
static SgPromptNode * remove_prompts(SgVM *vm, SgObject tag)
{
  SgPromptNode *cur_node = search_prompt_node(vm, tag);
  if (!cur_node) return NULL;
  SgPrompt *prompt = cur_node->prompt;
  SgContFrame *cont = vm->cont;
  
  /* remove the prompt in the aborting cont frame from the chain */
  while (!cont_prompt_match_p(cont, prompt)) {
    if (PROMPT_FRAME_MARK_P(cont)) remove_prompt(vm, (SgPrompt *)cont->pc);
    cont = cont->prev;
  }
  return cur_node;
}

static SgObject make_prompt_tag(SgObject name)
{
  return SG_LIST1(name);
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
  install_prompt(vm, prompt);
  
  return Sg_VMApply(proc, args);
}

static SgPrompt * make_barrier_prompt(SgVM *vm)
{
  SgObject tag = make_prompt_tag(Sg_Gensym(SG_MAKE_STRING("barrier")));
  SgPrompt *p = make_prompt(tag, SG_FALSE, vm);
  p->barrierP = TRUE;
  return p;
} 

SgObject Sg_VMCallCB(SgObject thunk)
{
  SgVM *vm = theVM;
  SgPrompt *prompt = make_barrier_prompt(vm);
  CHECK_STACK(CONT_FRAME_SIZE, vm);
  PUSH_PROMPT_CONT(vm, prompt);
  install_prompt(vm, prompt);
  return Sg_VMApply0(thunk);
}

/* this will be wrapped by the with-continuation-mark macro
   entries is a vector of pairs: #((key1 . value1) (key2 . value2) ...) */
SgObject Sg_VMCallCM(SgObject entries, SgObject thunk)
{
  SgVM *vm = theVM;
  long i, len = SG_VECTOR_SIZE(entries);
  
  /* If the current mark frame is associated with a prompt frame,
     we need to create a new mark frame to avoid polluting the prompt's marks.
     Marks added inside a prompt should be stripped when aborting.
     We use NULL as the frame pointer so strip_marks will walk past this frame. */
  if (vm->marks && vm->marks->frame && PROMPT_FRAME_MARK_P(vm->marks->frame)) {
    SgContMarks *cm = SG_NEW(SgContMarks);
    cm->frame = NULL; /* NULL frame means this is a "virtual" frame for marks only */
    cm->entries = NULL;
    cm->prev = vm->marks;
    vm->marks = cm;
  }
  
  for (i = 0; i < len; i++) {
    SgObject entry = SG_VECTOR_ELEMENT(entries, i);
    SgObject key = SG_CAR(entry);
    SgObject value = SG_CDR(entry);
    SgMarkEntry *e;
    int found = FALSE;
    
    /* Check if key already exists in current frame - if so, replace value */
    for (e = vm->marks->entries; e != NULL; e = e->next) {
      if (e->key == key || SG_EQ(e->key, key)) {
        e->value = value;
        found = TRUE;
        break;
      }
    }
    
    if (!found) {
      /* Key doesn't exist, add new entry */
      e = SG_NEW(SgMarkEntry);
      e->key = key;
      e->value = value;
      e->next = vm->marks->entries;
      vm->marks->entries = e;
    }
  }
  return Sg_VMApply0(thunk);
}

/* Get the value of a mark in the immediate continuation frame only.
   Returns SG_UNBOUND if not found.  

   Skips empty frames created by internal let bindings etc, but stops
   at prompts. */
static SgObject immediate_cm(SgObject key, SgObject fallback)
{
  SgVM *vm = theVM;
  SgContMarks *marks = vm->marks;
  
  /* "Immediate" means only the topmost frame - do NOT traverse
     to previous frames. This ensures tail-position semantics work correctly.
     If the current frame has no entries, there's no immediate mark. */
  if (marks && marks->entries) {
    SgMarkEntry *entry = marks->entries;
    while (entry) {
      if (entry->key == key || SG_EQ(entry->key, key)) {
	return entry->value;
      }
      entry = entry->next;
    }
  }
  return fallback;
}


/* call-with-immediate-continuation-mark as a C function.
   This avoids the extra frames created by a Scheme wrapper. */
SgObject Sg_VMCallImmediateCM(SgObject key, SgObject proc, SgObject fallback)
{
  SgObject value = immediate_cm(key, fallback);
  return Sg_VMApply1(proc, value);
}

SgObject Sg_MakeContinuationPromptTag(SgObject name)
{
  return make_prompt_tag(name);
}

/* for now we don't make specific type for continuation mark set
   but using a vector
 */
static SgObject cont_mark_set_sym = SG_FALSE;

int Sg_ContinuationMarkSetP(SgObject o)
{
  return SG_VECTORP(o) && SG_VECTOR_SIZE(o) == 2 &&
    SG_VECTOR_ELEMENT(o, 0) == cont_mark_set_sym;
}

static SgObject continuation_marks(SgContFrame *cont,
				   SgContMarks *marks,
				   SgObject tag)
{
  SgObject r;
  SgObject frames = SG_NIL;
  SgContMarks *cur = marks;
  SgVM *vm = theVM;
  SgPromptNode *prompt_node = NULL;

  /* Find the prompt node for the given tag to determine the boundary */
  if (!SG_FALSEP(tag)) {
    SgPromptNode *node = vm->prompts;
    while (node) {
      if (node->prompt->tag == tag) {
	prompt_node = node;
	break;
      }
      node = node->next;
    }
  }

  /* Collect marks from each frame.
     When we encounter the prompt boundary frame, we include its marks
     then stop. Marks from frames BEFORE the prompt are not visible.
  */
  while (cur) {
    SgMarkEntry *entry = cur->entries;
    SgObject frame_marks = SG_NIL;
    SgObject tail = SG_NIL;
    int is_prompt_boundary = (prompt_node && cur->frame == prompt_node->frame);
    
    /* Collect all entries in this frame, preserving order (most recent first) */
    while (entry) {
      SgObject pair = Sg_Cons(Sg_Cons(entry->key, entry->value), SG_NIL);
      if (SG_NULLP(frame_marks)) {
	frame_marks = tail = pair;
      } else {
	SG_SET_CDR(tail, pair);
	tail = pair;
      }
      entry = entry->next;
    }
    
    /* Add this frame even if empty (needed for
       call-with-immediate-continuation-mark) */
    frames = Sg_Cons(frame_marks, frames);
    
    /* If this was the prompt boundary, stop */
    if (is_prompt_boundary) {
      break;
    }
    
    cur = cur->prev;
  }
  
  /* Reverse to get the correct order (most recent frame first) */
  frames = Sg_ReverseX(frames);

  
  /* Create mark set vector:
     [0] = cont_mark_set_sym (type marker)
     [1] = list of frames (each frame is alist of key-value pairs)
  */
  r = Sg_MakeVector(2, SG_FALSE);
  SG_VECTOR_ELEMENT(r, 0) = cont_mark_set_sym;
  SG_VECTOR_ELEMENT(r, 1) = frames;
  return r;  
}

SgObject Sg_ContinuationMarks(SgObject k, SgObject tag)
{
  SgContinuation *c;
  if (!Sg_ContinuationP(k)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("continuation-marks"),
				    SG_MAKE_STRING("continuation"),
				    k, SG_NIL);
  }
  c = SG_CAR(SG_SUBR_DATA(k));
  return continuation_marks(c->cont, c->marks, tag);
}

SgObject Sg_CurrentContinuationMarks(SgObject tag)
{
  SgVM *vm = theVM;
  return continuation_marks(vm->cont, vm->marks, tag);
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
    void **data = vm_new_cont(raise_cc, 2);
    data[0] = exception;
    if (continuableP) {
      data[1] = raise_continuable_proc;
    } else {
      data[1] = raise_proc;
    }
    vm->escapeReason = SG_VM_ESCAPE_RAISE;
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

static void rewind_until(SgObject target)
{
  SgVM *vm = theVM;
  SgObject current = vm->dynamicWinders, hp;
  for (hp = current; SG_PAIRP(hp) && (hp != target); hp = SG_CDR(hp)) {
    SgObject proc = SG_DYNAMIC_WINDER_AFTER(SG_CAR(hp));
    vm->dynamicWinders = SG_CDR(hp);
    Sg_Apply0(proc);
  }
}

/* default exception handler */
void Sg_VMDefaultExceptionHandler(SgObject e)
{
  SgVM *vm = Sg_VM();
  SgContinuation *c = vm->escapePoint;
  
  if (c) {
    SgObject result = SG_FALSE, dvals[DEFAULT_VALUES_SIZE], *rvals;
    int valscount = 0, i, ext_count = 0;
    /* never reaches for now. */
    if (c->rewindBefore) rewind_until(c->winders);
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
      if (!c->rewindBefore) rewind_until(c->winders);
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
    vm->marks = c->marks;
    SG_VM_FLOATING_EP_SET(vm, c->floating);
    if (c->errorReporting) {
      SG_VM_RUNTIME_FLAG_SET(vm, SG_ERROR_BEING_REPORTED);
    }
  } else {
    Sg_ReportErrorInternal(e, vm->currentErrorPort);
    rewind_until(SG_NIL);
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

/* Rebuild vm->prompts from the continuation frame chain.
   This is needed after restoring a full continuation, since
   the prompt chain may not match the new cont chain.
*/
static void rebuild_prompts_from_cont(SgVM *vm)
{
  SgContFrame *cont = vm->cont;
  SgPromptNode *head = NULL;
  
  /* Walk the cont chain and collect prompt frames in order */
  while (cont && !bottom_cont_frame_p(vm, cont)) {
    if (PROMPT_FRAME_MARK_P(cont)) {
      SgPromptNode *node = SG_NEW(SgPromptNode);
      node->prompt = (SgPrompt *)cont->pc;
      node->frame = cont;
      node->next = NULL;
      /* Build list in reverse (head insertion) so prompts are
         in correct order (most recent first) */
      node->next = head;
      head = node;
    }
    cont = cont->prev;
  }
  vm->prompts = head;
}

static SgContFrame *skip_prompt_frame(SgVM *vm)
{
  SgContFrame *cont = vm->cont;
  SgContMarks *marks = vm->marks;
  while (PROMPT_FRAME_MARK_P(cont)) {
    remove_prompt(theVM, (SgPrompt *)cont->pc);
    if (marks) marks = marks->prev;
    cont = cont->prev;
  }
  vm->marks = marks;
  return cont;
}

#define POP_CONT()							\
  do {									\
    SgContFrame *cont__ = skip_prompt_frame(vm);			\
    if (vm->marks) vm->marks = vm->marks->prev;				\
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

/* Check if a winder (after-thunk) is still in the current dynamicWinders.
   This is needed because when an abort_cc frame is captured in a delimited
   continuation and later restored, some stored winders may no longer be
   in scope. According to Racket semantics, "the destination for a jump
   is recomputed after each pre-thunk or post-thunk completes." */
static int winder_in_scope_p(SgVM *vm, SgObject winder)
{
  SgObject cp;
  SG_FOR_EACH(cp, vm->dynamicWinders) {
    if (SG_EQ(SG_DYNAMIC_WINDER_BEFORE(SG_CAR(cp)), winder)
	|| SG_EQ(SG_DYNAMIC_WINDER_AFTER(SG_CAR(cp)), winder))
      return TRUE;
  }
  return FALSE;
}

static SgObject abort_body(SgPrompt *prompt, SgObject winders, SgObject args)
{
  ContInvokeCtx ctx;
  SgVM *vm = theVM;

  /* Skip winders that are no longer in the current dynamic extent.
     This handles the case where an abort_cc frame was captured in a
     delimited continuation and the stored winders include items
     outside the captured scope.
     Handler format: ((marks . thunk) . chain) - extract thunk for scope check */
  while (SG_PAIRP(winders) && !winder_in_scope_p(vm, SG_CDR(SG_CAAR(winders)))) {
    winders = SG_CDR(winders);
  }

  ctx.op = CONT_OP_ABORT;
  ctx.winder_policy = WINDER_SET;  /* Always directly set winders */
  ctx.handlers = winders;
  ctx.cont = NULL;  /* abort doesn't have a continuation */
  ctx.prompt = prompt;
  ctx.arg1 = args;
  ctx.arg2 = SG_NIL;
  return cont_invoke_body(&ctx);
}

SgObject Sg_VMAbortCC(SgObject tag, SgObject args)
{
  SgVM *vm = theVM;
  SgObject h;
  SgPromptNode *node = search_prompt_node(vm, tag);
  SgContinuation c;

  if (!node) CONT_ERR("abort-current-continuation", "No continuation tag", tag);

  /* compose fake continuation to compute winders */
  c.winders = node->prompt->winders;
  c.cstack = node->prompt->cstack;
  h = throw_cont_compute_handlers(&c, node->prompt, vm);

  return abort_body(node->prompt, h, args);
}

static SgObject call_in_cont_body(SgObject handlers,
				  SgContinuation *c,
				  SgObject proc,
				  SgObject args,
				  SgPrompt *prompt)
{
  ContInvokeCtx ctx;
  ctx.op = CONT_OP_CALL_IN_CONT;
  ctx.winder_policy = prompt ? WINDER_MERGE : WINDER_SET;
  ctx.handlers = handlers;
  ctx.cont = c;
  ctx.prompt = prompt;
  ctx.arg1 = proc;   /* proc in arg1 */
  ctx.arg2 = args;   /* args in arg2 */
  return cont_invoke_body(&ctx);
}

/* Compute handlers specifically for call-in-continuation.
   Unlike throw_cont_compute_handlers, this always computes AFTER handlers
   for exiting current dynamic-winds, regardless of cstack. */
static SgObject call_in_cont_compute_handlers(SgContinuation *c,
                                              SgPrompt *prompt,
                                              SgVM *vm)
{
  SgObject current = vm->dynamicWinders;
  SgObject escapes = c->winders;
  SgObject target = remove_common_winders(current, escapes);
  SgObject h = SG_NIL, t = SG_NIL, p;

  if (prompt) {
    target = take_prompt_winders(prompt, target);
    current = take_prompt_winders(prompt, current);
  }

  /* Always compute AFTER handlers for exiting current dynamic-winds */
  SG_FOR_EACH(p, current) {
    SgDynamicWinder *winder = SG_DYNAMIC_WINDER(SG_CAR(p));
    if (!SG_FALSEP(Sg_Memq(SG_CAR(p), escapes))) break;
    /* Handler format: ((marks . thunk) . chain) */
    SG_APPEND1(h, t, Sg_Cons(Sg_Cons(SG_OBJ(winder->marks), winder->after), SG_CDR(p)));
  }

  /* Then compute BEFORE handlers for entering continuation's dynamic-winds */
  SG_FOR_EACH(p, target) {
    SgDynamicWinder *winder = SG_DYNAMIC_WINDER(SG_CAR(p));
    SgObject chain = Sg_Memq(SG_CAR(p), escapes);
    SgObject next_winders = SG_CDR(chain);
    if (prompt && SG_PAIRP(next_winders)) {
      SgObject w = SG_CAR(next_winders);
      if (!SG_FALSEP(Sg_Memq(w, prompt->winders))) {
        next_winders = SG_NIL;
      }
    }
    /* Handler format: ((marks . thunk) . chain) */
    SG_APPEND1(h, t, Sg_Cons(Sg_Cons(SG_OBJ(winder->marks), winder->before), next_winders));
  }

  return h;
}

SgObject Sg_VMCallInCont(SgContinuation *c, SgPrompt *prompt, SgObject proc, SgObject args)
{
  SgVM *vm = Sg_VM();
  SgObject handlers;
  SgPrompt *barrier;

  /* Check barrier crossing */
  barrier = check_barrier_entry(vm, c);
  if (barrier) {
    continuation_violation(SG_INTERN("call-in-continuation"),
      SG_MAKE_STRING("Cannot call in continuation across barrier"),
      barrier->tag);
  }

  /* Handle C stack crossing for full continuations */
  if (c->cstack && vm->cstack != c->cstack) {
    SgCStack *cs;
    for (cs = vm->cstack; cs; cs = cs->prev) {
      if (c->cstack == cs) break;
    }
    if (cs != NULL) {
      /* Need to unwind C stack */
      vm->escapeReason = SG_VM_ESCAPE_CONT;
      vm->escapeData[0] = c;
      /* Pack proc and args for later - handled in evaluate_safe */
      vm->escapeData[1] = Sg_Cons(Sg_Cons(proc, args), prompt);
      longjmp(vm->cstack->jbuf, 1);
    }
    save_cont(vm);
  }

  /* Compute handlers for dynamic-wind transitions */
  handlers = call_in_cont_compute_handlers(c, prompt, vm);

  return call_in_cont_body(handlers, c, proc, args, prompt);
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
  void **data = vm_new_cont(process_queued_requests_cc, 3);
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
  return FALSE;
}
#elif _WIN32
# include <dbghelp.h>
# pragma comment(lib, "dbghelp.lib")
static int print_c_pc(SgVM *vm, SgObject pfmt, SgContFrame *cont)
{
#define SYM_LEN 256
  char buffer[sizeof(SYMBOL_INFO) + sizeof(char)*SYM_LEN];
  SYMBOL_INFO *sym = (SYMBOL_INFO *)buffer;
  DWORD64 displacement = 0;
  sym->SizeOfStruct = sizeof(SYMBOL_INFO);
  sym->MaxNameLen = SYM_LEN;
  if (SymFromAddr(GetCurrentProcess(), (DWORD64)cont->pc, &displacement, sym)) {
    SgObject pc = SG_INTERN("pc");
    SgObject name = Sg_Utf8sToUtf32s(sym->Name, sym->NameLen);
    Sg_Printf(vm->logPort, UC(";; %p "),
	      (uintptr_t)cont + offsetof(SgContFrame, pc));
    Sg_Format(vm->logPort, pfmt, SG_LIST2(pc, name), TRUE);
    return TRUE;
  }
  return FALSE;
#undef SYM_LEN
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
  } else if (cont->fp != C_CONT_MARK || !print_c_pc(vm, pfmt, cont)) {
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

static void print_prompts(SgVM *vm, SgPromptNode *node)
{
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

static SgObject entries2obj(SgMarkEntry *entries)
{
  SgObject h = SG_NIL, t = SG_NIL;
  while (entries) {
    SG_APPEND1(h, t, Sg_Cons(entries->key, entries->value));
    entries = entries->next;
  }
  return h;
}

static void print_mark(SgVM *vm, SgContMarks *mark)
{
  SgObject entries = entries2obj(mark->entries);
  Sg_Printf(vm->logPort, UC("[%p:%p] %S\n"), mark, mark->frame, entries);
}

static void print_marks(SgVM *vm, SgContMarks *marks)
{
  if (marks) {
    Sg_Printf(vm->logPort, UC(";; Continuation marks\n"));
    /* for now */
    Sg_Printf(vm->logPort, UC(";; "));
    print_mark(vm, marks);
    marks = marks->prev;
    while (marks) {
      Sg_Printf(vm->logPort, UC(";; => "));
      print_mark(vm, marks);
      marks = marks->prev;
    }
    Sg_Printf(vm->logPort, UC("\n"));
  }
}

static void print_summary(SgVM *vm)
{
  int n = 0;
  SgContFrame *cont = vm->cont;
  SgContMarks *marks = vm->marks;
  while (!bottom_cont_frame_p(vm, cont)) {
    n++;
    cont = cont->prev;
  }
  Sg_Printf(vm->logPort, UC(";; current cont: %d\n"), n);
  n = 0;
  while (marks) {
    n++;
    marks = marks->prev;
  }
  Sg_Printf(vm->logPort, UC(";; current marks: %d\n"), n);
}

static void print_frames(SgVM *vm, SgContFrame *cont)
{
  SgObject *stack = vm->stack, *sp = SP(vm);
  SgPromptNode *node = vm->prompts;

  print_summary(vm);
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

  print_prompts(vm, node);
  print_marks(vm, vm->marks);
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

static INLINE SgObject run_loop()
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
  cont_mark_set_sym = Sg_MakeSymbol(SG_MAKE_STRING("continuation mark set"), FALSE);
#ifdef _WIN32
  SymInitialize(GetCurrentProcess(), NULL, TRUE);
#endif
}

static SgObject current_input_port(UNUSED(SgObject x)) {
  return Sg_VM()->currentInputPort;
}

static SgObject current_output_port(UNUSED(SgObject x)) {
  return Sg_VM()->currentOutputPort;
}

static SgObject current_error_port(UNUSED(SgObject x)) {
  return Sg_VM()->currentErrorPort;
}

static void set_current_input_port(UNUSED(SgObject x), SgObject value) {
  Sg_SetCurrentInputPort(value);
}

static void set_current_output_port(UNUSED(SgObject x), SgObject value) {
  Sg_SetCurrentOutputPort(value);
}

static void set_current_error_port(UNUSED(SgObject x), SgObject value) {
  Sg_SetCurrentErrorPort(value);
}


void Sg__PostInitVM()
{
  SgObject lib = Sg_FindLibrary(SG_INTERN("(core errors)"), FALSE);
  SgObject clib = Sg_FindLibrary(SG_INTERN("(core)"), FALSE);
  SgObject b = Sg_FindBinding(lib, SG_INTERN("raise"), SG_UNBOUND);
  if (SG_UNBOUNDP(b)) {
    Sg_Panic("`raise` was not found.");
  }
  SG_INIT_CONDITION(SG_CLASS_CONTINUATION_VIOLATION, clib, "&continuation",
		    cont_violation_slot);
  SG_INIT_CONDITION_PRED(SG_CLASS_CONTINUATION_VIOLATION, clib,
			 "continuation-violation?");
  SG_INIT_CONDITION_CTR(SG_CLASS_CONTINUATION_VIOLATION, clib,
			"make-continuation-violation", 1);
  SG_INIT_CONDITION_ACC(cont_violation_tag, clib,
			"&continuation-violation-prompt-tag");
  raise_proc = SG_GLOC_GET(SG_GLOC(b));
  b = Sg_FindBinding(lib, SG_INTERN("raise-continuable"), SG_UNBOUND);
  if (SG_UNBOUNDP(b)) {
    Sg_Panic("`raise-continuable` was not found.");
  }
  raise_continuable_proc = SG_GLOC_GET(SG_GLOC(b));

#define ADD_PARAMETER(var, name, ref, set)			\
  do {								\
    SgObject n__ = SG_INTERN(name);				\
    (var) = Sg_MakeCoreParameter(n__, SG_UNDEF, ref, set);	\
    Sg_InsertBinding(clib, n__, var);				\
  } while (0)

  ADD_PARAMETER(currentInputPort, "current-input-port",
		current_input_port, set_current_input_port);
  ADD_PARAMETER(currentOutputPort, "current-output-port",
		current_output_port, set_current_output_port);
  ADD_PARAMETER(currentErrorPort, "current-error-port",
		current_error_port, set_current_error_port);
  
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
