/* mutex.c                                         -*- mode:c; coding:utf-8; -*-
 *
 * multi thread extensions
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
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "threads.h"

static void mutex_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgMutex *mutex = SG_MUTEX(self);
  int locked;
  SgVM *vm;
  SgObject name;
  Sg_LockMutex(&mutex->mutex);
  locked = mutex->locked;
  name = mutex->name;
  vm = mutex->owner;
  Sg_UnlockMutex(&mutex->mutex);

  Sg_Printf(port, UC("#<mutex %S "), name);
  if (locked) {
    if (vm) {
      if (vm->threadState == SG_VM_TERMINATED) {
	Sg_Printf(port, UC("unlocked/abandoned>"));
      } else {
	Sg_Printf(port, UC("locked/owned by %S>"), vm);
      }
    } else {
      Sg_Printf(port, UC("locked/not-owned>"));
    }
  } else {
    Sg_Printf(port, UC("unlocked/not-abandoned>"));
  }
}
static SgObject mutex_allocate(SgClass *klass, SgObject initargs);

SG_DEFINE_BASE_CLASS(Sg_MutexClass, SgMutex,
		     mutex_printer, NULL, NULL, mutex_allocate,
		     NULL);

static SgObject mutex_allocate(SgClass *klass, SgObject initargs)
{
  return NULL; 			/* dummy for now */
}

static void mutex_finalize(SgObject obj, void *data)
{
  SgMutex *mutex = SG_MUTEX(obj);
  Sg_DestroyMutex(&mutex->mutex);
  Sg_DestroyCond(&mutex->cv);
}

static SgMutex * make_mutex(SgObject name, int recursiveP)
{
  SgMutex *m = SG_NEW(SgMutex);
  SG_SET_CLASS(m, SG_CLASS_MUTEX);
  m->name = name;
  m->specific = SG_UNDEF;
  m->locked = FALSE;
  m->owner = NULL;
  Sg_InitMutex(&m->mutex, recursiveP);
  Sg_InitCond(&m->cv);
  Sg_RegisterFinalizer(SG_OBJ(m), mutex_finalize, NULL);
  return m;
}

SgObject Sg_MakeMutex(SgObject name)
{
  return SG_OBJ(make_mutex(name, FALSE));
}

static SgObject sym_not_owned;
static SgObject sym_abandoned;
static SgObject sym_not_abandoned;

SgObject Sg_MutexState(SgMutex *mutex)
{
  SgObject r;
  Sg_LockMutex(&mutex->mutex);
  if (mutex->locked) {
    if (mutex->owner) {
      if (mutex->owner->threadState == SG_VM_TERMINATED) r = sym_abandoned;
      else r = SG_OBJ(mutex->owner);
    } else {
      r = sym_not_owned;
    }
  } else {
    r = sym_not_abandoned;
  }
  Sg_UnlockMutex(&mutex->mutex);
  return r;
}

SgObject Sg_MutexLock(SgMutex *mutex, SgObject timeout, SgVM *owner)
{
  SgObject r = SG_TRUE;
  SgVM *abandoned = NULL;
  struct timespec ts, *pts;
  int intr = FALSE;

  pts = Sg_GetTimeSpec(timeout, &ts);

  SG_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(mutex->mutex);
  while (mutex->locked) {
    if (mutex->owner && mutex->owner->threadState == SG_VM_TERMINATED) {
      abandoned = mutex->owner;
      mutex->locked = FALSE;
      break;
    }
    if (pts) {
      int tr = Sg_WaitWithTimeout(&mutex->cv, &mutex->mutex, pts);
      if (tr == SG_INTERNAL_COND_TIMEDOUT) {
	r = SG_FALSE;
	break;
      } else if (tr == SG_INTERNAL_COND_INTR) {
	intr = TRUE;
	break;
      }
    } else {
      Sg_Wait(&mutex->cv, &mutex->mutex);
    }
  }
  if (SG_TRUEP(r)) {
    mutex->locked = TRUE;
    mutex->owner = owner;
  }
  SG_INTERNAL_MUTEX_SAFE_LOCK_END();

  /* intr? */
  if (intr) {
    SgObject e = Sg_MakeThreadInterruptException(owner);
    Sg_Raise(e, FALSE);
    r = FALSE;
  }
  if (abandoned) {
    SgObject exc = Sg_MakeAbandonedMutexException(abandoned, mutex);
    r = Sg_Raise(exc, FALSE);
  }
  return r;
}

SgObject Sg_MutexUnlock(SgMutex *mutex, SgConditionVariable *cv, 
			SgObject timeout)
{
  SgObject r = SG_TRUE;
  struct timespec ts, *pts;
  int intr = FALSE;
  SgVM *vm = mutex->owner;	/* original owner */

  pts = Sg_GetTimeSpec(timeout, &ts);

  SG_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(mutex->mutex);
  mutex->locked = FALSE;
  mutex->owner = NULL;
  Sg_Notify(&mutex->cv);
  if (cv) {
    if (pts) {
      int tr = Sg_WaitWithTimeout(&cv->cv, &mutex->mutex, pts);
      if (tr == SG_INTERNAL_COND_TIMEDOUT) {
	r = SG_FALSE;
      } else if (tr == SG_INTERNAL_COND_INTR) {
	intr = TRUE;
      }
    } else {
      Sg_Wait(&cv->cv, &mutex->mutex);
    }
  }
  SG_INTERNAL_MUTEX_SAFE_LOCK_END();

  /* intr? */
  if (intr) {
    SgObject e = Sg_MakeThreadInterruptException((vm) ? vm : SG_VM(SG_FALSE));
    Sg_Raise(e, FALSE);
    r = FALSE;
  }
  return r;
}

/* condition variable */
static void cv_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgConditionVariable *cv = SG_CONDITION_VARIABLE(self);
  Sg_Printf(port, UC("#<condition variable %S>"), cv->name);
}
static SgObject cv_allocate(SgClass *klass, SgObject initargs);

SG_DEFINE_BASE_CLASS(Sg_ConditionVariableClass, SgConditionVariable,
		     cv_printer, NULL, NULL, cv_allocate,
		     NULL);

static SgObject cv_allocate(SgClass *klass, SgObject initargs)
{
  return NULL;			/* dummy for now */
}

static void cv_finalize(SgObject obj, void *data)
{
  SgConditionVariable *cv = SG_CONDITION_VARIABLE(obj);
  Sg_DestroyCond(&cv->cv);
}


SgObject Sg_MakeConditionVariable(SgObject name)
{
  SgConditionVariable *c = SG_NEW(SgConditionVariable);
  SG_SET_CLASS(c, SG_CLASS_CONDITION_VARIABLE);
  c->name = name;
  c->specific = SG_UNDEF;
  Sg_InitCond(&c->cv);
  Sg_RegisterFinalizer(SG_OBJ(c), cv_finalize, NULL);
  return SG_OBJ(c);
}

SgObject Sg_ConditionVariableSignal(SgConditionVariable *cond)
{
  Sg_Notify(&cond->cv);
  return SG_UNDEF;
}

SgObject Sg_ConditionVariableBroadcast(SgConditionVariable *cond)
{
  Sg_NotifyAll(&cond->cv);
  return SG_UNDEF;
}


/* semaphore */
static void sem_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{

}
static SgObject sem_allocate(SgClass *klass, SgObject initargs);

SG_DEFINE_BASE_CLASS(Sg_SemaphoreClass, SgSemaphore,
		     sem_printer, NULL, NULL, sem_allocate,
		     NULL);

static SgObject sem_allocate(SgClass *klass, SgObject initargs)
{
  return NULL; 			/* dummy for now */
}


SgObject Sg_MakeSemaphore(SgObject name, int value)
{
  SgInternalSemaphore *sem
    = Sg_InitSemaphore((SG_FALSEP(name)) ? NULL : SG_STRING(name), value);
  SgSemaphore *z = SG_NEW(SgSemaphore);
  SG_SET_CLASS(z, SG_CLASS_SEMAPHORE);
  z->semaphore = sem;
  return SG_OBJ(z);
}

int Sg_SemaphoreWait(SgSemaphore *sem, SgObject timeout)
{
  struct timespec ts, *pts;
  int r;

  pts = Sg_GetTimeSpec(timeout, &ts);
  r = Sg_WaitSemaphore(sem->semaphore, pts);
  /* TODO intr? */
  if (r == SG_INTERNAL_COND_TIMEDOUT) return FALSE;
  return TRUE;
}
int Sg_SemaphorePost(SgSemaphore *sem)
{
  int r = Sg_PostSemaphore(sem->semaphore);
  if (r) {
    Sg_Error(UC("semaphore-post!: %A"),
	     Sg_GetLastErrorMessageWithErrorCode(r));
  }
  return TRUE;
}
void Sg_SemaphoreClose(SgSemaphore *sem)
{
  Sg_CloseSemaphore(sem->semaphore);
}
void Sg_SemaphoreDestroy(SgSemaphore *sem)
{
  Sg_DestroySemaphore(sem->semaphore);
}


static SgClass *error_cpl[] = {
  SG_ERROR_CONDITION_CPL,
  NULL
};

static void exc_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<%A>"), SG_CLASS(Sg_ClassOf(o))->name);
}

SG_DEFINE_CONDITION_ALLOCATOR(parent_allocate, SgThreadException)
SG_DEFINE_CONDITION_ACCESSOR(thread_thread, SgThreadException,
			     SG_THREAD_EXCEPTIONP, thread)
static SgSlotAccessor parent_slots[] = {
  SG_CLASS_SLOT_SPEC("thread", 0, thread_thread, thread_thread_set),
  { { NULL } }
};
/* conditions */
SG_DEFINE_BASE_CLASS(Sg_ThreadExceptionClass, SgThreadException,
		     exc_printer, NULL, NULL, parent_allocate,
		     error_cpl);

static SgClass *thread_exc_cpl[] = {
  SG_CLASS_THREAD_EXCEPTION,
  SG_ERROR_CONDITION_CPL,
  NULL
};

SG_DEFINE_BASE_CLASS(Sg_JoinTimeoutExceptionClass, SgThreadException,
		     exc_printer, NULL, NULL, parent_allocate,
		     thread_exc_cpl);

SG_DEFINE_CONDITION_ALLOCATOR(abn_allocate, SgAbondanedMutexException)
SG_DEFINE_CONDITION_ACCESSOR(abondand_mutex, SgAbondanedMutexException,
			     SG_ABONDANED_MUTEX_EXCEPTIONP, mutex)
static SgSlotAccessor abn_slots[] = {
  SG_CLASS_SLOT_SPEC("mutex", 0, abondand_mutex, abondand_mutex_set),
  { { NULL } }
};
SG_DEFINE_BASE_CLASS(Sg_AbondanedMutexExceptionClass, SgAbondanedMutexException,
		     exc_printer, NULL, NULL, abn_allocate,
		     thread_exc_cpl);

SG_DEFINE_CONDITION_ALLOCATOR(term_allocate, SgTerminatedThreadException)
SG_DEFINE_CONDITION_ACCESSOR(term_terminator, SgTerminatedThreadException,
			     SG_TERMINATED_THREAD_EXCEPTIONP, terminator)
static SgSlotAccessor term_slots[] = {
  SG_CLASS_SLOT_SPEC("terminator", 0, term_terminator, term_terminator_set),
  { { NULL } }
};
SG_DEFINE_BASE_CLASS(Sg_TerminatedThreadExceptionClass,
		     SgTerminatedThreadException,
		     exc_printer, NULL, NULL, term_allocate,
		     thread_exc_cpl);

SG_DEFINE_CONDITION_ALLOCATOR(uncaught_allocate, SgUncaughtException)
SG_DEFINE_CONDITION_ACCESSOR(uncaught_reason, SgUncaughtException,
			     SG_UNCAUGHT_EXCEPTIONP, reason)
static SgSlotAccessor uncaught_slots[] = {
  SG_CLASS_SLOT_SPEC("reason", 0, uncaught_reason, uncaught_reason_set),
  { { NULL } }
};
SG_DEFINE_BASE_CLASS(Sg_UncaughtExceptionClass,
		     SgUncaughtException,
		     exc_printer, NULL, NULL, uncaught_allocate,
		     thread_exc_cpl);

SG_DEFINE_BASE_CLASS(Sg_ThreadInterruptExceptionClass, SgThreadException,
		     exc_printer, NULL, NULL, parent_allocate,
		     thread_exc_cpl);


SgObject Sg_MakeJoinTimeoutException(SgVM *vm)
{
  SgObject c = parent_allocate(SG_CLASS_JOIN_TIMEOUT_EXCEPTION, SG_NIL);
  SG_THREAD_EXCEPTION(c)->thread = vm;
  return c;
}

SgObject Sg_MakeAbandonedMutexException(SgVM *vm, SgMutex *mutex)
{
  SgObject c = abn_allocate(SG_CLASS_ABONDANED_MUTEX_EXCEPTION, SG_NIL);
  SG_THREAD_EXCEPTION(c)->thread = vm;
  SG_ABONDANED_MUTEX_EXCEPTION(c)->mutex = mutex;
  return c;
}

SgObject Sg_MakeTerminatedThreadException(SgVM *vm, SgVM *terminator)
{
  SgObject c = term_allocate(SG_CLASS_TERMINATED_THREAD_EXCEPTION, SG_NIL);
  SG_THREAD_EXCEPTION(c)->thread = vm;
  SG_TERMINATED_THREAD_EXCEPTION(c)->terminator = terminator;
  return c;
}

SgObject Sg_MakeUncaughtException(SgVM *vm, SgObject reason)
{
  SgObject c = uncaught_allocate(SG_CLASS_UNCAUGHT_EXCEPTION, SG_NIL);
  SG_THREAD_EXCEPTION(c)->thread = vm;
  SG_UNCAUGHT_EXCEPTION(c)->reason = reason;
  return c;
}

SgObject Sg_MakeThreadInterruptException(SgVM *vm)
{
  SgObject c = parent_allocate(SG_CLASS_THREAD_INTERRUPT_EXCEPTION, SG_NIL);
  SG_THREAD_EXCEPTION(c)->thread = vm;
  return c;
}

SG_CDECL_BEGIN
void Sg__InitMutex(SgLibrary *lib)
{
  SG_INIT_CONDITION(SG_CLASS_THREAD_EXCEPTION, lib, 
		    "&thread-exception", parent_slots);
  SG_INIT_CONDITION(SG_CLASS_JOIN_TIMEOUT_EXCEPTION, lib, 
		    "&join-timeout-exception", NULL);
  SG_INIT_CONDITION(SG_CLASS_ABONDANED_MUTEX_EXCEPTION, lib, 
		    "&abandoned-mutex-exception", abn_slots);
  SG_INIT_CONDITION(SG_CLASS_TERMINATED_THREAD_EXCEPTION, lib, 
		    "&terminated-thread-exception", term_slots);
  SG_INIT_CONDITION(SG_CLASS_UNCAUGHT_EXCEPTION, lib, 
		    "&uncaught-exception", uncaught_slots);
  SG_INIT_CONDITION(SG_CLASS_THREAD_INTERRUPT_EXCEPTION, lib, 
		    "&thread-interrupt-exception", NULL);
  /* super class thread-exception */
#define INIT_CTR_PRED(cl, name, n, pred)	\
  SG_INIT_CONDITION_PRED(cl, lib, pred);	\
  SG_INIT_CONDITION_CTR(cl, lib, name, n)
#define INIT_ACC(fn, name) SG_INIT_CONDITION_ACC(fn, lib, name)

  /* &thread-exception */
  INIT_CTR_PRED(SG_CLASS_THREAD_EXCEPTION, "make-thread-exception", 1,
		"thread-exception?");
  INIT_ACC(thread_thread, "&thread-exception-thread");
  /* &join-timeout-exception */
  INIT_CTR_PRED(SG_CLASS_JOIN_TIMEOUT_EXCEPTION, 
		"make-join-timeout-exception", 1, "join-timeout-exception?");

  /* &abandoned-mutex-exception */
  INIT_CTR_PRED(SG_CLASS_ABONDANED_MUTEX_EXCEPTION,
		"make-abandoned-mutex-exception", 2, 
		"abandoned-mutex-exception?");
  INIT_ACC(abondand_mutex, "&abandoned-mutex-exception-mutex");

  /* &terminated-thread-exception */
  INIT_CTR_PRED(SG_CLASS_TERMINATED_THREAD_EXCEPTION,
		"make-terminated-thread-exception", 2,
		"terminated-thread-exception?");
  INIT_ACC(term_terminator, "&terminated-thread-exception-terminator");

  /* &uncaught-exception */
  INIT_CTR_PRED(SG_CLASS_UNCAUGHT_EXCEPTION,
		"make-uncaught-exception", 2, "uncaught-exception?");
  INIT_ACC(uncaught_reason, "&uncaught-exception-reason");

  /* &thread-interrupt-exception */
  INIT_CTR_PRED(SG_CLASS_JOIN_TIMEOUT_EXCEPTION, 
		"make-thread-interrupt-exception", 1, 
		"thread-interrupt-exception?");

  sym_not_owned      = SG_INTERN("not-owned");
  sym_abandoned      = SG_INTERN("abandoned");
  sym_not_abandoned  = SG_INTERN("not-abandoned");

  Sg_InitStaticClassWithMeta(SG_CLASS_MUTEX, UC("<mutex>"), lib, NULL,
			     SG_FALSE, NULL, 0);
  Sg_InitStaticClassWithMeta(SG_CLASS_CONDITION_VARIABLE,
			     UC("<condition-variable>"), lib, NULL,
			     SG_FALSE, NULL, 0);
  Sg_InitStaticClassWithMeta(SG_CLASS_SEMAPHORE, UC("<semaphore>"), lib, NULL,
			     SG_FALSE, NULL, 0);
}
SG_CDECL_END
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/

