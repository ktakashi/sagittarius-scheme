/* threads.h                                       -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_THREADS_H_
#define SAGITTARIUS_THREADS_H_

#include <sagittarius.h>

SG_CLASS_DECL(Sg_ConditionVariableClass);
#define SG_CLASS_CONDITION_VARIABLE (&Sg_ConditionVariableClass)

typedef struct SgConditionVariableRec
{
  SG_INSTANCE_HEADER;
  SgInternalCond cv;
  SgObject name;
  SgObject specific;
} SgConditionVariable;


#define SG_CONDITION_VARIABLE(obj) ((SgConditionVariable *)obj)
#define SG_CONDITION_VARIABLE_P(obj) SG_XTYPEP(obj, SG_CLASS_CONDITION_VARIABLE)

SG_CLASS_DECL(Sg_MutexClass);
#define SG_CLASS_MUTEX     (&Sg_MutexClass)
typedef struct SgMutexRec
{
  SG_INSTANCE_HEADER;
  SgInternalMutex mutex;
  SgInternalCond  cv;
  SgObject name;
  SgObject specific;
  int locked;
  SgVM *owner;
} SgMutex;

#define SG_MUTEX(obj)     ((SgMutex *)obj)
#define SG_MUTEXP(obj)   SG_XTYPEP(obj, SG_CLASS_MUTEX)

/* conditions */
SG_CLASS_DECL(Sg_ThreadExceptionClass);
SG_CLASS_DECL(Sg_JoinTimeoutExceptionClass);
SG_CLASS_DECL(Sg_AbondanedMutexExceptionClass);
SG_CLASS_DECL(Sg_TerminatedThreadExceptionClass);
SG_CLASS_DECL(Sg_UncaughtExceptionClass);
SG_CLASS_DECL(Sg_ThreadInterruptExceptionClass);

#define SG_CLASS_THREAD_EXCEPTION            (&Sg_ThreadExceptionClass)
#define SG_CLASS_JOIN_TIMEOUT_EXCEPTION      (&Sg_JoinTimeoutExceptionClass)
#define SG_CLASS_ABONDANED_MUTEX_EXCEPTION   (&Sg_AbondanedMutexExceptionClass)
#define SG_CLASS_TERMINATED_THREAD_EXCEPTION (&Sg_TerminatedThreadExceptionClass)
#define SG_CLASS_UNCAUGHT_EXCEPTION          (&Sg_UncaughtExceptionClass)
#define SG_CLASS_THREAD_INTERRUPT_EXCEPTION  (&Sg_ThreadInterruptExceptionClass)

typedef struct SgThreadExceptionRec
{
  SG_INSTANCE_HEADER;
  SgObject thread;
} SgThreadException;
#define SG_THREAD_EXCEPTION(o)  ((SgThreadException *)o)
#define SG_THREAD_EXCEPTIONP(o) SG_ISA(o, SG_CLASS_THREAD_EXCEPTION)

typedef struct SgAbondanedMutexExceptionRec
{
  SgThreadException parent;
  SgObject mutex;
} SgAbondanedMutexException;
#define SG_ABONDANED_MUTEX_EXCEPTION(o)	((SgAbondanedMutexException *)o)
#define SG_ABONDANED_MUTEX_EXCEPTIONP(o)	\
  SG_ISA(o, SG_CLASS_ABONDANED_MUTEX_EXCEPTION)

typedef struct SgTerminatedThreadExceptionRec
{
  SgThreadException parent;
  SgObject terminator;
} SgTerminatedThreadException;
#define SG_TERMINATED_THREAD_EXCEPTION(o) ((SgTerminatedThreadException *)o)
#define SG_TERMINATED_THREAD_EXCEPTIONP(o)		\
  SG_ISA(o, SG_CLASS_TERMINATED_THREAD_EXCEPTION)

typedef struct SgUncaughtExceptionRec
{
  SgThreadException parent;
  SgObject reason;
} SgUncaughtException;
#define SG_UNCAUGHT_EXCEPTION(o)  ((SgUncaughtException *)o)
#define SG_UNCAUGHT_EXCEPTIONP(o) SG_ISA(o, SG_CLASS_UNCAUGHT_EXCEPTION)

SG_CDECL_BEGIN
/*
  Scheme level thread API
 */

extern SgObject Sg_MakeThread(SgProcedure *thunk, SgObject name);
extern SgObject Sg_ThreadStart(SgVM *vm);
extern SgObject Sg_ThreadJoin(SgVM *vm, SgObject timeout, SgObject timeoutval);
extern SgObject Sg_ThreadStop(SgVM *vm, SgObject timeout, SgObject timeoutval);
extern SgObject Sg_ThreadCont(SgVM *vm);
extern SgObject Sg_ThreadSleep(SgObject timeout);
extern SgObject Sg_ThreadTerminate(SgVM *vm);
extern SgObject Sg_ThreadInterrupt(SgVM *vm);
extern unsigned long Sg_SysNanosleep(double nanosecond);

SgObject Sg_MakeConditionVariable(SgObject name);
SgObject Sg_ConditionVariableSignal(SgConditionVariable *cond);
SgObject Sg_ConditionVariableBroadcast(SgConditionVariable *cond);

SgObject Sg_MakeMutex(SgObject name);
SgObject Sg_MutexState(SgMutex *mutex);
SgObject Sg_MutexLock(SgMutex *mutex, SgObject timeout, SgVM *owner);
SgObject Sg_MutexUnlock(SgMutex *mutex, SgConditionVariable *cv, SgObject timeout);

/* thread exceptions */
SgObject Sg_MakeJoinTimeoutException(SgVM *vm);
SgObject Sg_MakeAbandonedMutexException(SgVM *vm, SgMutex *mutex);
SgObject Sg_MakeTerminatedThreadException(SgVM *vm, SgVM *terminator);
SgObject Sg_MakeUncaughtException(SgVM *vm, SgObject reason);
SgObject Sg_MakeThreadInterruptException(SgVM *vm);

SG_CDECL_END
#endif /* ! SAGITTARIUS_THREADS_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
