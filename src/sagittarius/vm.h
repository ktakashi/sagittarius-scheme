/* -*- C -*- */
/*
 * vm.h
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
#ifndef SAGITTARIUS_VM_H_
#define SAGITTARIUS_VM_H_

#include "sagittariusdefs.h"
#include "subr.h"		/* for SgSubrProc */
#include "thread.h"
#include "clos.h"
#ifdef HAVE_SETJMP_H
# include <setjmp.h>
#else
# error TODO implement own set jmp
#endif

#define SG_VM_STACK_SIZE      10000

SG_CLASS_DECL(Sg_BoxClass);
SG_CLASS_DECL(Sg_VMClass);

#define SG_CLASS_BOX (&Sg_BoxClass)
#define SG_CLASS_VM  (&Sg_VMClass)

struct SgBoxRec
{
  SG_HEADER;
  SgObject value;
};

#define SG_BOX(obj)  ((SgBox*)(obj))
#define SG_BOXP(obj) SG_XTYPEP(obj, SG_CLASS_BOX)

/* continuation frame */
typedef struct SgContFrameRec
{
  struct SgContFrameRec *prev; 	/* previous frame */
  int            size;		/* size of argument frame */
  SgWord        *pc;		/* next PC */
  SgObject       cl;		/* cl register value */
  SgObject      *fp;		/* fp register value */
  SgObject      *env;		/* saved arguments */
} SgContFrame;

#define CONT_FRAME_SIZE (sizeof(SgContFrame)/sizeof(SgObject))

typedef SgObject SgCContinuationProc(SgObject result, void **data);

/*
  We need to treat c-stack not the same way as Scheme stack, otherwise
  it'll consume c-stack infinitely.
 */
typedef struct SgCStackRec
{
  struct SgCStackRec *prev;
  SgContFrame *cont;
  jmp_buf      jbuf;
} SgCStack;

typedef struct SgContinucationRec
{
  struct SgContinucationRec * prev;
  struct SgContinucationRec * floating;
  SgContFrame *cont;
  SgObject     winders;
  SgCStack    *cstack;
  SgObject     ehandler;
  SgObject     xhandler;
  int          errorReporting;
  int          rewindBefore;
} SgContinuation;

#define SG_CONTINUATION(obj)  ((SgContinuation*)obj)
#define SG_VM_FLOATING_EP(vm)			\
  ((vm)->escapePoint? (vm)->escapePoint->floating : (vm)->escapePointFloating)
#define SG_VM_FLOATING_EP_SET(vm, ep)		\
  do {						\
    if ((vm)->escapePoint) {			\
      (vm)->escapePoint->floating = (ep);	\
    } else {					\
      (vm)->escapePointFloating = (ep);		\
    }						\
  } while (0)


typedef struct SgVMProfilerRec SgVMProfiler;

typedef enum {
  COMPILING,
  COMPILED,
  RUNNING,
  IMPORTING,
  FINISHED
} SgVMState;

typedef enum {
  SG_VM_ESCAPE_NONE,
  SG_VM_ESCAPE_CONT,
  SG_VM_ESCAPE_ERROR,
  SG_VM_ESCAPE_EXIT
} SgVMEscapeReason;

enum {
  SG_VM_NEW,			/* This VM is just created and not attached
				   to the running thread. */
  SG_VM_RUNNABLE,		/* This VM is attached to a thread which is
				   runnable or blocked. */
  SG_VM_STOPPED,		/* The thread attached to this VM is stopped
				   by the inspector thread for debugging*/
  SG_VM_TERMINATED		/* The thread attached to this VM is
				   terminated. */
};

enum {
  SG_ERROR_BEING_HANDLED  = (1L << 0),
  SG_ERROR_BEING_REPORTED = (1L << 1)
};

#define SG_VM_RUNTIME_FLAG_IS_SET(vm, flag) ((vm)->runtimeFlags & (flag))
#define SG_VM_RUNTIME_FLAG_SET(vm, flag)    ((vm)->runtimeFlags |= (flag))
#define SG_VM_RUNTIME_FLAG_CLEAR(vm, flag)  ((vm)->runtimeFlags &= ~(flag))

#define DEFAULT_VALUES_SIZE 32

typedef struct values_buffer_t
{
  int buffer_size;
  SgObject values_buffer[1];
} SgValuesBuffer;

#define SG_ALLOC_VALUES_BUFFER(vm, size)				\
  do {									\
    (vm)->extra_values =						\
      SG_NEW2(SgValuesBuffer*,						\
	      sizeof(SgValuesBuffer)+sizeof(SgObject)*((size)-1));	\
    (vm)->extra_values->buffer_size = (size);				\
  } while (0)

#define SG_VALUES_REF(vm, i)						\
  (((i) < DEFAULT_VALUES_SIZE)						\
   ? (vm)->values[i]							\
   : (vm)->extra_values->values_buffer[(i)-DEFAULT_VALUES_SIZE])

#define SG_VALUES_SET(vm, i, v)						\
  do {									\
    if ((i) < DEFAULT_VALUES_SIZE) {					\
      (vm)->values[i] = (v);						\
    } else {								\
      (vm)->extra_values->values_buffer[(i)-DEFAULT_VALUES_SIZE] = (v);	\
    }									\
  } while (0)

struct SgVMRec
{
  SG_HEADER;
  SgInternalThread thread;	/* the system thread executing this VM. */
  int threadState;		/* thread state. */
  SgInternalMutex  vmlock;	/* mutex to be used to lock this VM. */
  SgInternalCond   cond;
  SgVM *canceller;
  SgVM *inspector;
  SgObject name;		/* Scheme thread name. */
  SgObject specific;		/* Scheme thread specific data. */
  SgProcedure *thunk;		/* Entry point of this VM */
  SgObject result;		/* thread result */
  SgObject resultException;	/* thread exception */

  unsigned int flags;		/* flags */
  unsigned int runtimeFlags;	/* flags for runtime */
  /* Registers */
  SgWord   *pc;			/* program counter */
  SgObject  ac;			/* accumelator */
  SgObject  cl;			/* current closure */
  SgObject *fp;			/* frame pointer */
  SgObject *sp;			/* stack pointer */
  SgContFrame  *cont;     	/* saved continuation frame */
  /* values buffer */
  int      valuesCount;
  SgObject values[DEFAULT_VALUES_SIZE];
  SgValuesBuffer *extra_values;

  /* macro expansion */
  SgObject usageEnv;
  SgObject macroEnv;
  SgObject transEnv;
  /* to store macro expansion history alist */
  SgObject history;
  /* 
     load path
   */
  SgObject loadPath;
  SgObject dynamicLoadPath;
  SgObject currentLoadPath;
  /* 
     command line args.
     this is a list of args
   */
  SgObject commandLineArgs;
  /*
    Stack:
    TODO: if we use child vm, do I need to create a new stack or
    share this?
   */

  SgObject  *stack;		/* for convenient */
  SgObject  *stackEnd;

  /* Ports */
  SgPort    *currentOutputPort;
  SgPort    *currentInputPort;
  SgPort    *currentErrorPort;
  SgPort    *logPort;

  /* return point */
  SgCStack  *cstack;
  SgContinuation *escapePoint;
  SgContinuation *escapePointFloating;
  SgVMEscapeReason escapeReason;
  void      *escapeData[2];
  SgObject   defaultEscapeHandler; /* for multi threading */
  /* error */
  SgObject   error;

  /* libraries */
  SgLibrary   *currentLibrary;
  /* dynamic winders */
  SgObject    dynamicWinders;
  /* exception handler */
  SgObject    exceptionHandler;
  SgObject    parentExHandler;

  /* parameters */
  SgObject    parameters;

  /* source info */
  SgObject    sourceInfos;

  /* gc related */
  int finalizerPending;
  int attentionRequest;
  int stopRequest;

  /* statistics */
  SgVMState state;
  int profilerRunning;
  SgVMProfiler *profiler;
  /* for threads modules */
  unsigned long uptimeSec;
  unsigned long uptimeUsec;

  /* temporary storage for compiled cache
     storage structure:
     ((#<code-builder> ...)
      (#<code-builder> ...)
      ...)
     compiled codes are always appended the first list.
     when importing a library, first list will be appended.
   */
  SgObject cache;

  /* current loading port */
  SgObject currentLoadingPort;
};

/*
  flag 32bit
  log level optimization reader/mode   misc(cache)
  00000000   00000000     00000000    00000000
 */
typedef enum {
  /* cache mode */
  SG_DISABLE_CACHE    = 0x00000001,
  SG_NO_DEBUG_INFO    = 0x00000002,
  /* reader mode */
  SG_R6RS_MODE        = 0x00000100, /* 00000001 */
  SG_COMPATIBLE_MODE  = 0x00000200, /* 00000010 */
  SG_NO_OVERWRITE     = 0x00000400, /* 00000100 */

  /* optimization */
  SG_NO_INLINE_ASM    = 0x00010000,
  SG_NO_INLINE_LOCAL  = 0x00020000,
  SG_NO_LAMBDA_LIFT   = 0x00040000,
  SG_NO_LIBRARY_INLINING = 0x00080000,
  SG_NO_CONST_INLINING = 0x00100000,
  /* log level */
  SG_FATAL_LEVEL      = 0x01000000,
  SG_ERROR_LEVEL      = 0x02000000,
  SG_WARN_LEVEL       = 0x04000000,
  SG_INFO_LEVEL       = 0x08000000,
  SG_DEBUG_LEVEL      = 0x10000000,
  SG_TRACE_LEVEL      = 0x20000000,
  SG_LOG_LEVEL_MASK   = 0xff000000,
  
} VMFlags;

#define SG_VM(obj) ((SgVM *)obj)
#define SG_VMP(obj) SG_XTYPEP(obj, SG_CLASS_VM)

#define SG_VM_SET_FLAG(vm, flag)    ((vm)->flags = ((vm)->flags | (flag)))
#define SG_VM_UNSET_FLAG(vm, flag)  ((vm)->flags = ((vm)->flags & (~(flag))))
#define SG_VM_IS_SET_FLAG(vm, flag) (((vm)->flags & (flag)))

#define SG_VM_LOG_LEVEL(vm, level)  (((vm)->flags & SG_LOG_LEVEL_MASK) >= level)

#define PC(vm)             (vm)->pc
#define AC(vm)             (vm)->ac
#define CL(vm)             (vm)->cl
#define FP(vm)             (vm)->fp
#define SP(vm)             (vm)->sp
#define CONT(vm)           (vm)->cont

#if 0
#define CALC_OFFSET(vm, offset)  ((SgObject*)CONT(vm)-FP(vm))
#else
#define CALC_OFFSET(vm, offset) /* dummy */
#endif

#define INDEX(sp, n)        (*((sp) - (n) - 1))
#define INDEX_SET(sp, n, v) (*((sp) - (n) - 1) = (v))
#define PUSH(sp, o)         (*(sp)++ = (o))
#define POP(sp)             (*(--(sp)))


#define SG_CCONT_DATA_SIZE 6

#define IN_STACK_P(ptr, vm)				\
  ((uintptr_t)((ptr) - vm->stack) < SG_VM_STACK_SIZE)

#define SG_LET_FRAME_SIZE           2
#define SG_FRAME_SIZE               CONT_FRAME_SIZE


/* from Gauche */
/* Unwind protect */
#define SG_UNWIND_PROTECT			\
  do {						\
    SgCStack cstack;				\
    cstack.prev = Sg_VM()->cstack;		\
    cstack.cont = NULL;				\
    Sg_VM()->cstack = &cstack;			\
    if (setjmp(cstack.jbuf) == 0) {
           
#define SG_WHEN_ERROR				\
    } else {

#define SG_NEXT_HANDLER					\
      do {						\
	if (Sg_VM()->cstack->prev) {			\
	  Sg_VM()->cstack = Sg_VM()->cstack->prev;	\
	  longjmp(Sg_VM()->cstack->jbuf, 1);		\
	}						\
	else Sg_Exit(1);				\
      } while (0)

#define SG_END_PROTECT				\
    }						\
    Sg_VM()->cstack = Sg_VM()->cstack->prev;	\
  } while (0)


SG_CDECL_BEGIN

SG_EXTERN SgVM*    Sg_NewVM(SgVM *proto, SgObject name);
SG_EXTERN SgObject Sg_Compile(SgObject sexp, SgObject env);
SG_EXTERN SgObject Sg_Apply(SgObject proc, SgObject args);
SG_EXTERN SgObject Sg_Apply0(SgObject proc);
SG_EXTERN SgObject Sg_Apply1(SgObject proc, SgObject arg);
SG_EXTERN SgObject Sg_Apply2(SgObject proc, SgObject arg0, SgObject arg1);
SG_EXTERN SgObject Sg_Apply3(SgObject proc, SgObject arg0, SgObject arg1, SgObject arg2);
SG_EXTERN SgObject Sg_Apply4(SgObject proc, SgObject arg0, SgObject arg1, SgObject arg2, SgObject arg3);
SG_EXTERN SgObject Sg_VMApply0(SgObject proc);
SG_EXTERN SgObject Sg_VMApply1(SgObject proc, SgObject arg);
SG_EXTERN SgObject Sg_VMApply2(SgObject proc, SgObject arg0, SgObject arg1);
SG_EXTERN SgObject Sg_VMApply3(SgObject proc, SgObject arg0, SgObject arg1, SgObject arg2);
SG_EXTERN SgObject Sg_VMApply4(SgObject proc, SgObject arg0, SgObject arg1, SgObject arg2, SgObject arg3);
SG_EXTERN SgObject Sg_VMApply(SgObject proc, SgObject args);
SG_EXTERN SgObject Sg_VMCallCC(SgObject proc);
SG_EXTERN SgObject Sg_VMCallPC(SgObject proc);
SG_EXTERN SgVM*    Sg_VM();	/* get vm */
SG_EXTERN int      Sg_SetCurrentVM(SgVM *vm);
SG_EXTERN int      Sg_AttachVM(SgVM *vm);
SG_EXTERN void     Sg_VMDumpCode(SgCodeBuilder *cb);

SG_EXTERN SgObject Sg_AddLoadPath(SgString *path);
SG_EXTERN SgObject Sg_AddDynamicLoadPath(SgString *path);

/* eval */
SG_EXTERN SgObject Sg_Eval(SgObject sexp, SgObject env);
SG_EXTERN SgObject Sg_VMEval(SgObject sexp, SgObject env);
SG_EXTERN SgObject Sg_Environment(SgObject lib, SgObject spec);

/* dynamic-wind */
SG_EXTERN void     Sg_VMPushCC(SgCContinuationProc *after, void **data, int datasize);
SG_EXTERN SgObject Sg_VMDynamicWind(SgObject before, SgObject thunk, SgObject after);
/* c-friendly wrapper from Gauche */
SG_EXTERN SgObject Sg_VMDynamicWindC(SgSubrProc *before, SgSubrProc *body, SgSubrProc *after, void *data);

/* IO */
SG_EXTERN SgObject Sg_CurrentOutputPort();
SG_EXTERN SgObject Sg_CurrentErrorPort();
SG_EXTERN SgObject Sg_CurrentInputPort();
SG_EXTERN SgObject Sg_CurrentLoadingPort();

SG_EXTERN SgObject Sg_VMCurrentLibrary();

/* exception */
SG_EXTERN SgObject Sg_GetStackTrace();
SG_EXTERN SgObject Sg_VMThrowException(SgVM *vm, SgObject exception,
				       int continuableP);
SG_EXTERN void     Sg_VMDefaultExceptionHandler(SgObject exception);
SG_EXTERN SgObject Sg_VMWithExceptionHandler(SgObject handler, SgObject thunk);
SG_EXTERN SgObject Sg_VMWithErrorHandler(SgObject handler, SgObject thunk,
					 int rewindBefore);
SG_EXTERN void     Sg_ReportError(SgObject e, SgObject out);

/* finalizer */
SG_EXTERN SgObject Sg_VMFinalizerRun(SgVM *vm);

/* debuging */
SG_EXTERN void     Sg_VMPrintFrame();

/* process time */
SG_EXTERN void     Sg_VMProcessTime(unsigned long *sec, unsigned long *usec);

/* root? */
SG_EXTERN int      Sg_MainThreadP();

/* values */
SG_EXTERN SgObject Sg_VMValues(SgVM *vm, SgObject args);
SG_EXTERN SgObject Sg_VMValues2(SgVM *vm, SgObject v1, SgObject v2);
SG_EXTERN SgObject Sg_VMValues3(SgVM *vm, SgObject v1,
				SgObject v2, SgObject v3);

SG_CDECL_END

#endif /* SAGITTARIUS_VM_H_ */
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
