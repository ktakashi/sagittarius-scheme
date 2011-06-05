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
      if (vm->state == SG_VM_TERMINATED) {
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

SG_INIT_META_OBJ(Sg_MutexMeta, &mutex_printer);

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
  Sg_InitMutex(&m->mutex, TRUE);
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
      if (mutex->owner->state == SG_VM_TERMINATED) r = sym_abandoned;
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
  /* TODO clean up */
  Sg_LockMutex(&mutex->mutex);
  while (mutex->locked) {
    if (mutex->owner && mutex->owner->state == SG_VM_TERMINATED) {
      abandoned = mutex->owner;
      mutex->locked = FALSE;
      break;
    }
    if (SG_REALP(timeout)) {
      int msec = Sg_GetIntegerClamp(timeout, SG_CLAMP_NONE, NULL);
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
  if (abandoned) {
    /* TODO exception */
  }
  return r;
}

SgObject Sg_MutexUnlock(SgMutex *mutex, SgConditionVariable *cv, SgObject timeout)
{
  SgObject r = SG_TRUE;
  Sg_LockMutex(&mutex->mutex);
  mutex->locked = FALSE;
  mutex->owner = NULL;
  Sg_Notify(&mutex->cv);
  if (cv) {
    if (SG_REALP(timeout)) {
      int msec = Sg_GetIntegerClamp(timeout, SG_CLAMP_NONE, NULL);
      int success = Sg_WaitWithTimeout(&mutex->cv, &mutex->mutex, msec);
      if (!success) {
	r = SG_FALSE;
      }      
    } else {
      Sg_Wait(&mutex->cv, &mutex->mutex);
    }
  }
  Sg_UnlockMutex(&mutex->mutex);
  return r;
}

/* condition variable */
static void cv_printer(SgPort *port, SgObject self, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<condition variable>"));
}

SG_INIT_META_OBJ(Sg_ConditionVariableMeta, &cv_printer);

void Sg__InitMutex()
{
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

