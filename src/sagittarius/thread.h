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
#ifdef _MSC_VER
typedef struct SgInternalMutexRec
{
  HANDLE mutex;
} SgInternalMutex;
typedef struct SgInternalThreadRec
{
  HANDLE  thread;
  void   *returnValue;
} SgInternalThread;
typedef unsigned int __stdcall SgThreadEntryFunc(void *);
#define SG_INTERNAL_THREAD_INIT(thr)         ((thr)->thread = (HANDLE)NULL)
#define SG_INTERNAL_THREAD_INITIALIZED_P(thr) ((thr)->thread != (HANDLE)NULL)
struct SgInternalCondRec
{
  int waiters_count;
  CRITICAL_SECTION waiters_count_lock;
  HANDLE semaphore;
  HANDLE watiers_done;
  size_t was_broadcast;
} SgInternalCond;
typedef struct SgInternalCondRec * SgInternalCond;

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

SG_CDECL_BEGIN

SG_EXTERN void Sg_InitMutex(SgInternalMutex *mutex, int recursive);
SG_EXTERN void Sg_LockMutex(SgInternalMutex *mutex);
SG_EXTERN void Sg_UnlockMutex(SgInternalMutex *mutex);
SG_EXTERN void Sg_DestroyMutex(SgInternalMutex *mutex);

SG_EXTERN void Sg_InternalThreadStart(SgInternalThread *thread, SgThreadEntryFunc *entry, void *param);
SG_EXTERN void Sg_InternalThreadYield();
SG_EXTERN SgInternalThread Sg_CurrentThread();

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
