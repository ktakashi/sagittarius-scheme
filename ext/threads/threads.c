/* threads.c                                       -*- mode:c; coding:utf-8; -*-
 *
 * multi thread extensions.
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
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
#include <math.h>
#include <time.h>
#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#include <sagittarius.h>
#include <sagittarius/private/kernel.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "threads.h"


SgObject Sg_MakeThread(SgProcedure *thunk, SgObject name)
{
  SgVM *current = Sg_VM(), *vm;
  if (SG_PROCEDURE_REQUIRED(thunk) != 0) {
    Sg_Error(UC("thunk required, but got %S"), thunk);
  }
  vm = Sg_NewThreadVM(current, name);
  vm->thunk = thunk;
  return SG_OBJ(vm);
}

static void thread_cleanup_inner(SgVM *vm)
{
  vm->threadState = SG_VM_TERMINATED;
  if (vm->canceller) {
    /* this thread is cancelled */
    vm->result = Sg_MakeTerminatedThreadException(vm, vm->canceller);
    vm->threadErrorP = TRUE;
  }
  Sg_NotifyAll(&vm->cond);
}

static void thread_cleanup(void *data)
{
  SgVM *vm = SG_VM(data);
  /* change this VM state to TERMINATED, and signals the change
     to the waiting threads. */
  Sg_LockMutex(&vm->vmlock);
  thread_cleanup_inner(vm);
  Sg_UnlockMutex(&vm->vmlock);
}

static void* thread_entry(void *data)
{
  SgVM *vm = SG_VM(data);
  SgObject *stack;
  int i;
/* #ifdef HAVE_ALLOCA */
  /* 
     I have no idea why but after introducing kernel, this causes
     test failure on Linux, and probaly caused by corrupted stack.
     Maybe we should get stack size and do magical computation to
     check if the stack is available or not if we care about the
     heap allocation performance. At this moment, in the era of
     2023, that slight performance doesn't really hurt anybody, so
     use heap.
  */
#if 0
  stack = (SgObject *)alloca(sizeof(SgObject) * SG_VM_STACK_SIZE);
#else
  stack = SG_NEW_ARRAY(SgObject, SG_VM_STACK_SIZE);
#endif
  Sg_SetVMStack(vm, stack, SG_VM_STACK_SIZE);
  thread_cleanup_push(thread_cleanup, vm);
  if (Sg_SetCurrentVM(vm)) {
    SG_UNWIND_PROTECT {
      vm->result = Sg_Apply0(SG_OBJ(vm->thunk));
    } SG_WHEN_ERROR {
      SgObject exc;
      switch (vm->escapeReason) {
      case SG_VM_ESCAPE_CONT:
	vm->result = SG_MAKE_STRING("stale continuation throws");
	vm->threadErrorP = TRUE;
	break;
      default:
	Sg_Panic("unknown escape");
      case SG_VM_ESCAPE_ERROR:
	exc = Sg_MakeUncaughtException(vm, SG_OBJ(vm->escapeData[1]));
	vm->result = exc;
	vm->threadErrorP = TRUE;
	break;
      }
    } SG_END_PROTECT;
  } else {
    /* I'm not sure if this happen */
    /* if this happen, we can not use any Sg_Apply related methods such as
       error creations. so just create string.
     */
    vm->result = SG_MAKE_STRING("set-current-vm failed");
    vm->threadErrorP = TRUE;
  }
  thread_cleanup_pop(TRUE);
  /* for GC friendliness */
  Sg_SetVMStack(vm, NULL, 0);
  for (i = 0; i < SG_VM_STACK_SIZE; i++) *(stack+i) = NULL;
  stack = NULL;
  return NULL;
}

SgObject Sg_ThreadStart(SgVM *vm)
{
  return Sg_StartManagedThread(vm, (SgThreadEntryFunc *)thread_entry, TRUE);
}

SgObject Sg_ThreadJoin(SgVM *vm, SgObject timeout, SgObject timeoutval)
{
  struct timespec ts;
  int intr = FALSE, tout = FALSE, errorP;
  SgObject result = SG_FALSE;

  struct timespec *pts = Sg_GetTimeSpec(timeout, &ts);
  SG_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(vm->vmlock);
  while (vm->threadState != SG_VM_TERMINATED) {
    if (pts) {
      int tr  = Sg_WaitWithTimeout(&vm->cond, &vm->vmlock, pts);
      if (tr == SG_INTERNAL_COND_TIMEDOUT) {
	tout = TRUE;
	break;
      } else if (tr == SG_INTERNAL_COND_INTR) {
	intr = TRUE;
	break;
      }
    } else {
      Sg_Wait(&vm->cond, &vm->vmlock);
    }
  }
  if (!tout) {
    result = vm->result;
    errorP = vm->threadErrorP;
    if (errorP) {
      vm->result = SG_UNDEF;
      vm->threadErrorP = FALSE;
    }
  }
  SG_INTERNAL_MUTEX_SAFE_LOCK_END();

  if (intr) {
    /* TODO should this be continuable? */
    SgObject e = Sg_MakeThreadInterruptException(vm);
    result = Sg_Raise(e, FALSE);
  }
  if (tout) {
    if (SG_UNBOUNDP(timeoutval)) {
      SgObject e = Sg_MakeJoinTimeoutException(vm);
      result = Sg_Raise(e, FALSE);
    } else {
      result = timeoutval;
    }
  } else if (errorP) {
    /* TODO raise for only non-continuable? */
    result = Sg_Raise(result, FALSE);
  }
  return result;
}

SgObject Sg_ThreadSuspend(SgVM *target, SgObject timeout, SgObject timeoutval)
{
  int invalid_state = FALSE;
  SgVM *taker = NULL;
  SgVM *vm = Sg_VM();
  int success = TRUE;

  struct timespec ts, *pts;
  pts = Sg_GetTimeSpec(timeout, &ts);

  Sg_LockMutex(&target->vmlock);
  if (target->threadState != SG_VM_RUNNABLE &&
      target->threadState != SG_VM_STOPPED) {
    invalid_state = TRUE;
  } else if (target->inspector != NULL &&
	     target->inspector != vm &&
	     target->inspector->threadState != SG_VM_TERMINATED) {
    taker = target->inspector;
  } else {
    if (target->inspector != vm) {
      target->inspector = vm;
      target->stopRequest = SG_VM_REQUEST_SUSPEND;
      target->attentionRequest = TRUE;
    }
    while (target->threadState != SG_VM_STOPPED) {
      if (pts) {
	success = Sg_WaitWithTimeout(&target->cond, &target->vmlock, pts) == 0;
	break;
      } else {
	success = Sg_Wait(&target->cond, &target->vmlock) == 0;
      }
    }
  }
  Sg_UnlockMutex(&target->vmlock);
  if (invalid_state) {
    Sg_Error(UC("cannot stop a thread %S since it is in neither runnable or stopped state"),
	     target);
  }
  if (taker != NULL) {
    Sg_Error(UC("target %S is already under inspection by %S"), target, taker);
  }
  if (!success) return timeoutval;
  return SG_OBJ(target);
}

SgObject Sg_ThreadResume(SgVM *target)
{
  int not_stopped = FALSE;
  SgVM *stopped_by_other = NULL;
  Sg_LockMutex(&target->vmlock);
  if (target->inspector == NULL) {
    not_stopped = TRUE;
  } else if (target->inspector != Sg_VM() &&
	     target->inspector->threadState != SG_VM_TERMINATED) {
    stopped_by_other = target->inspector;
  } else {
    target->inspector = NULL;
    target->threadState = SG_VM_RUNNABLE;
    target->stopRequest = FALSE;
    Sg_NotifyAll(&target->cond);
  }
  Sg_UnlockMutex(&target->vmlock);
  if (not_stopped) Sg_Error(UC("target %S is not stopped"), target);
  if (stopped_by_other) Sg_Error(UC("target %S is stopped by other thread %S"),
				 target, stopped_by_other);
  return SG_OBJ(target);
}

SgObject Sg_ThreadSleep(SgObject timeout)
{
  SgInternalCond dummyc;
  SgInternalMutex dummym;
  int intr = FALSE;

  struct timespec ts, *pts;
  pts = Sg_GetTimeSpec(timeout, &ts);

  if (!pts) {
    Sg_Error(UC("thread-sleep! can't take #f as timeout value: %S"), timeout);
  }

  Sg_InitMutex(&dummym, FALSE);
  Sg_InitCond(&dummyc);
  Sg_LockMutex(&dummym);
  /* sleep should sleep second not milli second */
  intr = Sg_WaitWithTimeout(&dummyc, &dummym, pts);
  if (intr == SG_INTERNAL_COND_INTR) {
    /* TODO should this be continuable? */
    SgObject e = Sg_MakeThreadInterruptException(Sg_VM());
    Sg_Raise(e, TRUE);
  }
  Sg_UnlockMutex(&dummym);
  Sg_DestroyMutex(&dummym);
  Sg_DestroyCond(&dummyc);
  return SG_UNDEF;
}

static int wait_for_termination(SgVM *target)
{
  struct timespec ts;
  int r;
  SgObject t = Sg_MakeFlonum(0.001); /* 1ms */

  Sg_GetTimeSpec(t, &ts);
  do {
    r = Sg_WaitWithTimeout(&target->cond, &target->vmlock, &ts);
  } while (r != SG_INTERNAL_COND_TIMEDOUT &&
	   target->threadState != SG_VM_TERMINATED);
  return r == 0;
}

SgObject Sg_ThreadTerminate(SgVM *target)
{
  SgVM *vm = Sg_VM();
  if (target == vm) {
    /* self termination */
    Sg_LockMutex(&target->vmlock);
    if (target->canceller == NULL) {
      target->canceller = vm;
    }
    Sg_UnlockMutex(&target->vmlock);
    Sg_ExitThread(&target->thread, NULL);
    return SG_UNDEF;
  } 
  Sg_LockMutex(&target->vmlock);
  if (target->threadState == SG_VM_RUNNABLE ||
      target->threadState == SG_VM_STOPPED) {
    do {
      if (target->canceller == NULL) {
	target->canceller = vm;
	target->stopRequest = SG_VM_REQUEST_TERMINATE;
	target->attentionRequest = TRUE;

	if (wait_for_termination(target)) break;

	thread_cleanup_inner(target);
	
	Sg_TerminateThread(&target->thread);
      }
    } while (0);
  }
  target->threadState = SG_VM_TERMINATED;
  Sg_UnlockMutex(&target->vmlock);

  return SG_UNDEF;
}

SgObject Sg_ThreadInterrupt(SgVM *target)
{
  SgVM *vm = Sg_VM();
  if (target == vm) {
    Sg_AssertionViolation(SG_INTERN("thread-interrupt!"),
			  SG_MAKE_STRING("attempt to interrupt own"),
			  SG_LIST1(target));
  }
  if (target->threadState != SG_VM_RUNNABLE) {
    Sg_AssertionViolation(SG_INTERN("thread-interrupt!"),
			  SG_MAKE_STRING("thread is not running"),
			  SG_LIST1(target));
  }
  return SG_MAKE_BOOL(Sg_InterruptThread(&target->thread));
}

#if !defined HAVE_NANOSLEEP || defined(_WIN32)
int nanosleep(const struct timespec *req, struct timespec *rem)
{
    time_t msecs = 0;
    time_t sec;
    time_t overflow = 0, c;
    const DWORD MSEC_OVERFLOW = 4294967; /* 4294967*1000 = 0xfffffed8 */

    /* It's very unlikely that we overflow msecs, but just in case... */
    if (req->tv_sec > 0 || (req->tv_sec == 0 && req->tv_nsec > 0)) {
        if (req->tv_sec >= MSEC_OVERFLOW) {
            overflow = req->tv_sec / MSEC_OVERFLOW;
            sec = req->tv_sec % MSEC_OVERFLOW;
        } else {
            sec = req->tv_sec;
        }
        msecs = (sec * 1000 + (req->tv_nsec + 999999)/1000000);
    }
    Sleep((DWORD)msecs);
    for (c = 0; c < overflow; c++) {
        Sleep(MSEC_OVERFLOW * 1000);
    }
    if (rem) {
        rem->tv_sec = rem->tv_nsec = 0;
    }
    return 0;
}
#endif

unsigned long Sg_SysNanosleep(double v)
{
  struct timespec spec, rem;
  spec.tv_sec = (unsigned long)floor(v / 1.0e9);
  spec.tv_nsec = (unsigned long)fmod(v, 1.0e9);
  while (spec.tv_nsec >= 1000000000L) {
    spec.tv_nsec -= 1000000000L;
    spec.tv_sec += 1;
  }
  rem.tv_sec = 0;
  rem.tv_nsec = 0;
  nanosleep(&spec, &rem);
  if (rem.tv_sec == 0 && rem.tv_nsec == 0) return 0;
  else {
    /* returns remaind nanosecond */
    unsigned long r = 0;
    while (rem.tv_sec > 0) {
      r += 1000000000L;
      rem.tv_sec -= 1000000000L;
    }
    r += rem.tv_nsec;
    return r;
  }
}

extern void Sg__Init_threads_stub(SgLibrary *lib);
SG_CDECL_BEGIN
extern void Sg__InitMutex(SgLibrary *lib);
SG_CDECL_END

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__threads()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__threads);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius threads)"),
				  FALSE));
  Sg_InitStaticClassWithMeta(SG_CLASS_VM, UC("<thread>"), lib,
			     NULL, SG_FALSE, NULL, 0);
  Sg__InitMutex(lib);
  Sg__Init_threads_stub(lib);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
