/* -*- C -*- */
/*
 * mutex.c: multi thread extensions
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
#define LIBSAGITTARIUS_BODY
#include "threads.h"

static void mutex_printer(SgPort *port, SgObject self, SgWriteContext *ctx)
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

SG_INIT_META_OBJ(Sg_MutexMeta, &mutex_printer, NULL);

static void mutex_finalize(SgObject obj, void *data)
{
  SgMutex *mutex = SG_MUTEX(obj);
  Sg_DestroyMutex(&mutex->mutex);
  Sg_DestroyCond(&mutex->cv);
}

SgObject Sg_MakeMutex(SgObject name)
{
  SgMutex *m = SG_NEW(SgMutex);
  SG_SET_META_OBJ(m, SG_META_MUTEX);
  m->name = name;
  m->specific = SG_UNDEF;
  m->locked = FALSE;
  m->owner = NULL;
  Sg_InitMutex(&m->mutex, FALSE);
  Sg_InitCond(&m->cv);
  Sg_RegisterFinalizer(SG_OBJ(m), mutex_finalize, NULL);
  return SG_OBJ(m);
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
  int msec = -1;
  if (SG_REALP(timeout)) {
    msec = Sg_GetIntegerClamp(timeout, SG_CLAMP_NONE, NULL);
  }

  thread_cleanup_push((void (*)(void*))Sg_UnlockMutex,
		      (void *)&mutex->mutex);
  Sg_LockMutex(&mutex->mutex);
  while (mutex->locked) {
    if (mutex->owner && mutex->owner->threadState == SG_VM_TERMINATED) {
      abandoned = mutex->owner;
      mutex->locked = FALSE;
      break;
    }
    if (SG_REALP(timeout)) {
      int success = Sg_WaitWithTimeout(&mutex->cv, &mutex->mutex, msec);
      if (!success) {
	r = SG_FALSE;
	break;
      } else {
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
  Sg_UnlockMutex(&mutex->mutex);
  thread_cleanup_pop(0);
  if (abandoned) {
    SgObject exc = Sg_MakeAbandonedMutexException(abandoned, mutex);
    r = Sg_Raise(exc, FALSE);
  }
  return r;
}

SgObject Sg_MutexUnlock(SgMutex *mutex, SgConditionVariable *cv, SgObject timeout)
{
  SgObject r = SG_TRUE;

  thread_cleanup_push((void (*)(void*))Sg_UnlockMutex,
		      (void *)&mutex->mutex);

  Sg_LockMutex(&mutex->mutex);
  mutex->locked = FALSE;
  mutex->owner = NULL;
  Sg_Notify(&mutex->cv);
  if (cv) {
    if (SG_REALP(timeout)) {
      int msec = Sg_GetIntegerClamp(timeout, SG_CLAMP_NONE, NULL);
      int success = Sg_WaitWithTimeout(&cv->cv, &mutex->mutex, msec);
      if (!success) {
	r = SG_FALSE;
      }      
    } else {
      Sg_Wait(&cv->cv, &mutex->mutex);
    }
  }
  Sg_UnlockMutex(&mutex->mutex);
  thread_cleanup_pop(0);
  return r;
}

/* condition variable */
static void cv_printer(SgPort *port, SgObject self, SgWriteContext *ctx)
{
  SgConditionVariable *cv = SG_CONDITION_VARIABLE(self);
  Sg_Printf(port, UC("#<condition variable %S>"), cv->name);
}

SG_INIT_META_OBJ(Sg_ConditionVariableMeta, &cv_printer, NULL);

static void cv_finalize(SgObject obj, void *data)
{
  SgConditionVariable *cv = SG_CONDITION_VARIABLE(obj);
  Sg_DestroyCond(&cv->cv);
}


SgObject Sg_MakeConditionVariable(SgObject name)
{
  SgConditionVariable *c = SG_NEW(SgConditionVariable);
  SG_SET_META_OBJ(c, SG_META_CONDITION_VARIABLE);
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

/* thread exceptions */
static SgObject make_join_timeout_exception;
static SgObject make_abandoned_mutex_exception;
static SgObject make_terminated_thread_exception;
static SgObject make_uncaught_exception;

SgObject Sg_MakeJoinTimeoutException(SgVM *vm)
{
  return Sg_Apply1(make_join_timeout_exception, vm);
}

SgObject Sg_MakeAbandonedMutexException(SgVM *vm, SgMutex *mutex)
{
  return Sg_Apply2(make_abandoned_mutex_exception, vm, mutex);
}

SgObject Sg_MakeTerminatedThreadException(SgVM *vm, SgVM *terminator)
{
  return Sg_Apply2(make_terminated_thread_exception, vm, terminator);
}

SgObject Sg_MakeUncaughtException(SgVM *vm, SgObject reason)
{
  return Sg_Apply2(make_uncaught_exception, vm, reason);
}

static SgRecordType thread_exception;
static SgRecordType join_timeout_exception;
static SgRecordType abandoned_mutex_exception;
static SgRecordType terminated_thread_exception;
static SgRecordType uncaught_exception;

void Sg__InitMutex()
{
  SG_DECLARE_EXCEPTIONS("(sagittarius threads impl)", TRUE);
  SgObject null_lib = Sg_FindLibrary(SG_INTERN("null"), FALSE);
  /* TODO should parent be &assertion? */
  SgObject parent = Sg_FindBinding(SG_LIBRARY(null_lib), SG_INTERN("&assertion"), SG_UNBOUND);
  /* field for super class of all thread exceptions. */
  SgObject field = Sg_MakeVector(1, SG_LIST2(SG_INTERN("immutable"), SG_INTERN("thread")));
  SgObject nullfield = Sg_MakeVector(0, SG_UNDEF);
  SgObject parent_rtd = SG_FALSE, parent_rcd = SG_FALSE;

  if (SG_UNBOUNDP(parent)) {
    /* fail safe */
    /* TODO should this be panic? */
    parent = SG_FALSE;
  } else {
    parent = SG_GLOC_GET(SG_GLOC(parent));
    parent_rtd = SG_RECORD_TYPE_RTD(parent);
    parent_rcd = SG_RECORD_TYPE_RCD(parent);
  }
  
  /* super class thread-exception */
  SG_INTERN__CONDITION_SIMPLE(&thread_exception, &thread-exception, parent_rtd, parent_rcd, field);
  SG_INTERN__CONDITION_CTR(&thread_exception, make-thread-exception);
  SG_INTERN__CONDITION_PRED(&thread_exception, thread-exception?);
  SG_INTERN__CONDITION_ACCESSOR(&thread_exception, &thread-exception-thread,
				thread-exception-thread, 0);
  /* join-timeout-exception */
  SG_INTERN__CONDITION_SIMPLE(&join_timeout_exception, &join-timeout-exception,
			      SG_RECORD_TYPE_RTD(&thread_exception),
			      SG_RECORD_TYPE_RCD(&thread_exception), nullfield);
  SG_INTERN__CONDITION_CTR(&join_timeout_exception, make-join-timeout-exception);
  SG_INTERN__CONDITION_PRED(&join_timeout_exception, join-timeout-exception?);
  SG_SET_CONSTRUCTOR(make_join_timeout_exception);

  {
    SgObject mfield = Sg_MakeVector(1, SG_LIST2(SG_INTERN("immutable"), SG_INTERN("mutex")));
    SG_INTERN__CONDITION_SIMPLE(&abandoned_mutex_exception, &abandoned-mutex-exception,
				SG_RECORD_TYPE_RTD(&thread_exception),
				SG_RECORD_TYPE_RCD(&thread_exception), mfield);
    SG_INTERN__CONDITION_CTR(&abandoned_mutex_exception, make-abandoned-mutex-exception);
    SG_INTERN__CONDITION_PRED(&abandoned_mutex_exception, abandoned-mutex-exception?);
    SG_INTERN__CONDITION_ACCESSOR(&abandoned_mutex_exception, &abandoned-mutex-exception-mutex,
				  abandoned-mutex-exception-mutex, 0);
    SG_SET_CONSTRUCTOR(make_abandoned_mutex_exception);
  }
  {
    SgObject tfield = Sg_MakeVector(1, SG_LIST2(SG_INTERN("immutable"), SG_INTERN("terminator")));
    SG_INTERN__CONDITION_SIMPLE(&terminated_thread_exception, &terminated-thread-exception,
				SG_RECORD_TYPE_RTD(&thread_exception),
				SG_RECORD_TYPE_RCD(&thread_exception), tfield);
    SG_INTERN__CONDITION_CTR(&terminated_thread_exception, make-terminated-thread-exception);
    SG_INTERN__CONDITION_PRED(&terminated_thread_exception, terminated-thread-exception?);
    SG_INTERN__CONDITION_ACCESSOR(&terminated_thread_exception, &terminated-thread-exception-terminator,
				  terminated-thread-exception-terminator, 0);
    SG_SET_CONSTRUCTOR(make_terminated_thread_exception);
  }
  {
    SgObject rfield = Sg_MakeVector(1, SG_LIST2(SG_INTERN("immutable"), SG_INTERN("reason")));
    SG_INTERN__CONDITION_SIMPLE(&uncaught_exception, &uncaught-exception,
				SG_RECORD_TYPE_RTD(&thread_exception),
				SG_RECORD_TYPE_RCD(&thread_exception), rfield);
    SG_INTERN__CONDITION_CTR(&uncaught_exception, make-uncaught-exception);
    SG_INTERN__CONDITION_PRED(&uncaught_exception, uncaught-exception?);
    SG_INTERN__CONDITION_ACCESSOR(&uncaught_exception, &uncaught-exception-reason,
				  uncaught-exception-reason, 0);
    SG_SET_CONSTRUCTOR(make_uncaught_exception);
  }

  sym_not_owned      = SG_INTERN("not-owned");
  sym_abandoned      = SG_INTERN("abandoned");
  sym_not_abandoned  = SG_INTERN("not-abandoned");
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/

