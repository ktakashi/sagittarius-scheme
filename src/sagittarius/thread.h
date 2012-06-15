/* -*- C -*- */
/*
 * thread.h
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
#ifndef SAGITTARIUS_THREAD_H_
#define SAGITTARIUS_THREAD_H_

#include "sagittariusdefs.h"
#ifdef _WIN32
# include <windows.h>
# ifdef _MSC_VER
#  pragma warning(disable : 4127)
# endif
#endif

/* thread function and some macros */
#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
typedef struct SgInternalMutexRec
{
  HANDLE mutex;
} SgInternalMutex;
typedef struct SgInternalThreadRec
{
  HANDLE  thread;
  void   *returnValue;
} SgInternalThread;
typedef unsigned int SgThreadEntryFunc(void *);
#define SG_INTERNAL_THREAD_INIT(thr)         ((thr)->thread = (HANDLE)NULL)
#define SG_INTERNAL_THREAD_INITIALIZED_P(thr) ((thr)->thread != (HANDLE)NULL)
struct SgInternalCondRec
{
  int waiters_count;
  CRITICAL_SECTION waiters_count_lock;
  HANDLE semaphore;
  HANDLE waiters_done;
  size_t was_broadcast;
};
typedef struct SgInternalCondRec SgInternalCond;

#else
typedef struct SgInternalMutexRec
{
  pthread_mutex_t mutex;
} SgInternalMutex;
typedef struct SgInternalThreadRec
{
  pthread_t thread;
} SgInternalThread;
typedef void* SgThreadEntryFunc(void *);

typedef struct  SgInternalCondRec
{
  pthread_cond_t cond;
} SgInternalCond;

#define SG_INTERNAL_THREAD_INIT(thr)         ((thr)->thread = (pthread_t)NULL)
#define SG_INTERNAL_THREAD_INITIALIZED_P(thr) ((thr)->thread != (pthread_t)NULL)
#endif

/* emulate pthread_cleanup_push/pop*/
#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
/* emulation code from pthread for win32 */
typedef void (* ptw32_cleanup_callback_t)(void *);
typedef struct ptw32_cleanup_rec_t
{
  ptw32_cleanup_callback_t routine;
  void *arg;
  struct ptw32_cleanup_rec_t *prev;
} ptw32_cleanup_t;
# define thread_cleanup_push(_rout, _arg)			\
  {								\
    ptw32_cleanup_t _cleanup;					\
    _cleanup.routine = (ptw32_cleanup_callback_t)(_rout);	\
    _cleanup.arg = (_arg);					\
    __try {

# define thread_cleanup_pop(_execute)			\
    } __finally {					\
      if ( _execute || AbnormalTermination()) {		\
	(*(_cleanup.routine))(_cleanup.arg);		\
      }							\
    }							\
  }

#elif !defined(_WIN32) && !defined(_WIN64)
/* Assume we are using pthread */
# include <pthread.h>
# define thread_cleanup_push pthread_cleanup_push
# define thread_cleanup_pop  pthread_cleanup_pop
#else
# error FIXME: non VC compiler on Windows are not supported!
#endif

#define SG_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(mutex)	\
  Sg_LockMutex(&(mutex));				\
  thread_cleanup_push(Sg__MutexCleanup, &(mutex))
#define SG_INTERNAL_MUTEX_SAFE_LOCK_END() /* dummy */; thread_cleanup_pop(1)


SG_CDECL_BEGIN

SG_EXTERN void Sg_InitMutex(SgInternalMutex *mutex, int recursive);
SG_EXTERN void Sg_LockMutex(SgInternalMutex *mutex);
SG_EXTERN void Sg__MutexCleanup(void *mutex);
SG_EXTERN void Sg_UnlockMutex(SgInternalMutex *mutex);
SG_EXTERN void Sg_DestroyMutex(SgInternalMutex *mutex);

SG_EXTERN int  Sg_InternalThreadStart(SgInternalThread *thread,
				      SgThreadEntryFunc *entry, void *param);
SG_EXTERN void Sg_InternalThreadYield();
SG_EXTERN void Sg_SetCurrentThread(SgInternalThread *ret);

SG_EXTERN void Sg_InitCond(SgInternalCond *cond);
SG_EXTERN void Sg_DestroyCond(SgInternalCond *cond);
SG_EXTERN int  Sg_Notify(SgInternalCond *cond);
SG_EXTERN int  Sg_NotifyAll(SgInternalCond *cond);
SG_EXTERN int  Sg_Wait(SgInternalCond *cond, SgInternalMutex *mutex);
SG_EXTERN int  Sg_WaitWithTimeout(SgInternalCond *cond, SgInternalMutex *mutex, int msecs);

SG_EXTERN void Sg_ExitThread(SgInternalThread *thread, void *ret);
SG_EXTERN void Sg_TerminateThread(SgInternalThread *thread);

SG_CDECL_END

#endif /* SAGITTARIUS_THREAD_H_ */
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
