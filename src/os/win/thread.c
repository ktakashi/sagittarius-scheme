/* thread.c                                         -*- mode:c; coding:utf-8 -*-
 *
 *   Copyright (c) 2010-2013  Takashi Kato <ktakashi@ymail.com>
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
#include <windows.h>
#define LIBSAGITTARIUS_BODY
#include <sagittarius/thread.h>
#include <sagittarius/system.h>
#include <sagittarius/error.h>
#include <sagittarius/vm.h>

#include "../../gc-incl.inc"

void Sg_InitMutex(SgInternalMutex *mutex, int recursive)
{
  mutex->mutex = CreateMutex(NULL, FALSE, NULL);
}

void Sg_LockMutex(SgInternalMutex *mutex)
{
  WaitForSingleObject(mutex->mutex, INFINITE);
}

void Sg_UnlockMutex(SgInternalMutex *mutex)
{
  ReleaseMutex(mutex->mutex);
}

void Sg__MutexCleanup(void *mutex_)
{
  SgInternalMutex *mutex = (SgInternalMutex *)mutex_;
  ReleaseMutex(mutex->mutex);
}

void Sg_DestroyMutex(SgInternalMutex *mutex)
{
  if (mutex->mutex != INVALID_HANDLE_VALUE) {
    CloseHandle(mutex->mutex);
    mutex->mutex = INVALID_HANDLE_VALUE;
  }
}

static DWORD ExceptionFilter(EXCEPTION_POINTERS *ep, DWORD *ei)
{
  return EXCEPTION_EXECUTE_HANDLER;
}

typedef struct ThreadParams
{
  SgThreadEntryFunc *start;
  void *arg;
} ThreadParams;

static unsigned int __stdcall win32_thread_entry(void *params)
{
  ThreadParams *threadParams = (ThreadParams *)params;
  SgThreadEntryFunc *start;
  void *arg;
  DWORD ei[] = { 0, 0, 0 };
  unsigned int status;
  
  start = threadParams->start;
  arg = threadParams->arg;
  /* temporary storage is no longer needed. */
  free(threadParams);
  __try {
    status = (*start)(arg);
  } __except (ExceptionFilter(GetExceptionInformation(), ei)) {
    return FALSE;
  }
  return status;
}

int Sg_InternalThreadStart(SgInternalThread *thread, SgThreadEntryFunc *entry,
			   void *param)
{
  /* this heap must be freed in win32_thread_entry */
  ThreadParams *params = (ThreadParams*)malloc(sizeof(ThreadParams));
  params->start = entry;
  params->arg = param;
  thread->thread = (HANDLE)_beginthreadex(NULL, 0, win32_thread_entry,
					  params, 0, NULL);
  return TRUE;
}

void Sg_InternalThreadYield()
{
  Sleep(1);
}

void Sg_SetCurrentThread(SgInternalThread *ret)
{
  ret->thread = GetCurrentThread();
}

void Sg_InitCond(SgInternalCond *cond)
{
  cond->waiters_count = 0;
  cond->was_broadcast = 0;
  cond->mutex = NULL;
  cond->semaphore = CreateSemaphore(NULL, 0, 0x7fffffff, NULL);
  InitializeCriticalSection(&cond->waiters_count_lock);
  cond->waiters_done = CreateEvent(NULL, FALSE, FALSE, NULL);
}

void Sg_DestroyCond(SgInternalCond *cond)
{
  CloseHandle(cond->semaphore);
  cond->semaphore = NULL;
  CloseHandle(cond->waiters_done);
  cond->waiters_done = NULL;
  DeleteCriticalSection(&cond->waiters_count_lock);
}

int Sg_Notify(SgInternalCond *cond)
{
  int have_waiters; BOOL r = TRUE;
  if (!cond->mutex) return 0; /* nobody ever waited on this cond var */
  
  SG_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(*cond->mutex);

  EnterCriticalSection(&cond->waiters_count_lock);
  have_waiters = cond->waiters_count > 0;
  LeaveCriticalSection(&cond->waiters_count_lock);
  if (have_waiters) {
    r = ReleaseSemaphore(cond->semaphore, 1, 0);
  }

  SG_INTERNAL_MUTEX_SAFE_LOCK_END();
  if (!r) return -1;
  return 0;
}

int Sg_NotifyAll(SgInternalCond *cond)
{
  int have_waiters = 0;
  BOOL r0 = TRUE;
  DWORD r1 = WAIT_OBJECT_0, err = 0;

  if (!cond->mutex) return 0; /* nobody ever waited on this cond var */

  SG_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(*cond->mutex);

  EnterCriticalSection(&cond->waiters_count_lock);
  cond->was_broadcast = have_waiters = (cond->waiters_count > 0);

  if (have_waiters) {
    r0 = ReleaseSemaphore(cond->semaphore, cond->waiters_count, 0);
    if (!r0) err = GetLastError();
    LeaveCriticalSection(&cond->waiters_count_lock);
    if (r0) {
      r1 = WaitForSingleObject(cond->waiters_done, INFINITE);
      cond->was_broadcast = FALSE;
    }
  } else {
    LeaveCriticalSection(&cond->waiters_count_lock);
  }
  SG_INTERNAL_MUTEX_SAFE_LOCK_END();

  if (!r0) { SetLastError(err); return -1; }
  if (r1 != WAIT_OBJECT_0) return -1;
  return 0;
}

static int wait_internal(SgInternalCond *cond, SgInternalMutex *mutex,
			 struct timespec *pts)
{
  int last_waiter, bad_mutex = FALSE;
  DWORD msecs, r0, r1;
  if (pts) {
    unsigned long now_sec, target_sec;
    unsigned long target_usec, now_usec;
    Sg_GetTimeOfDay(&now_sec, &now_usec);
    target_sec = (unsigned long)pts->tv_sec;
    target_usec = pts->tv_nsec / 1000;
    if (target_sec < now_sec
	|| (target_sec == now_sec && target_usec <= now_usec)) {
      msecs = 1;
    } else if (target_usec >= now_usec) {
      msecs = (DWORD)ceil((target_sec - now_sec) * 1000
			  + (target_usec - now_usec)/1000.0);
    } else {
      msecs = (DWORD)ceil((target_sec - now_sec - 1) * 1000
			  + (1.0e6 + target_usec - now_usec)/1000.0);
    }
    if (msecs == 0) msecs++;
  } else {
    msecs = INFINITE;
  }
  EnterCriticalSection(&cond->waiters_count_lock);
  if (cond->mutex != NULL && cond->mutex != mutex) {
    bad_mutex = TRUE;
  } else {
    cond->waiters_count++;
    if (cond->mutex == NULL) cond->mutex = mutex;
  }
  LeaveCriticalSection(&cond->waiters_count_lock);

  if (bad_mutex) {
    Sg_Error(UC("Attempt to wait on condition variables %p with different"
		" mutex %p"), cond, mutex);
  }

  r0 = SignalObjectAndWait(mutex->mutex, cond->semaphore, msecs, FALSE);
  /* always unlock it */
  EnterCriticalSection(&cond->waiters_count_lock);
  cond->waiters_count--;
  last_waiter = cond->was_broadcast && cond->waiters_count == 0;
  LeaveCriticalSection(&cond->waiters_count_lock);
  if (last_waiter) {
    r1 = SignalObjectAndWait(cond->waiters_done, mutex->mutex, INFINITE, FALSE);
  } else {
    r1 = WaitForSingleObject(mutex->mutex, INFINITE);
  }
  if (r0 == WAIT_TIMEOUT) return SG_INTERNAL_COND_TIMEDOUT;
  if (r0 != WAIT_OBJECT_0 || r1 != WAIT_OBJECT_0) return -1;
  return 0;
}

int Sg_Wait(SgInternalCond *cond, SgInternalMutex *mutex)
{
  return wait_internal(cond, mutex, NULL);
}

int Sg_WaitWithTimeout(SgInternalCond *cond, SgInternalMutex *mutex,
		       struct timespec *pts)
{
  return wait_internal(cond, mutex, pts);
}

void Sg_ExitThread(SgInternalThread *thread, void *ret)
{
  thread->returnValue = ret;
  _endthreadex((unsigned int)ret);
}

#if defined(_M_IX86) || defined(_X86_)
#define PTW32_PROGCTR(Context)  ((Context).Eip)
#endif

#if defined (_M_IA64)
#define PTW32_PROGCTR(Context)  ((Context).StIIP)
#endif

#if defined(_MIPS_)
#define PTW32_PROGCTR(Context)  ((Context).Fir)
#endif

#if defined(_ALPHA_)
#define PTW32_PROGCTR(Context)  ((Context).Fir)
#endif

#if defined(_PPC_)
#define PTW32_PROGCTR(Context)  ((Context).Iar)
#endif

#if defined(_AMD64_)
#define PTW32_PROGCTR(Context)  ((Context).Rip)
#endif

#if !defined(PTW32_PROGCTR)
#error Module contains CPU-specific code; modify and recompile.
#endif

static void cancel_self(DWORD unused)
{
  ULONG exceptionInformation[3];
  exceptionInformation[0] = (ULONG)(2);
  exceptionInformation[1] = (ULONG)(0);
  exceptionInformation[2] = (ULONG)(0);

  RaiseException (-1, 0, 3, (CONST ULONG_PTR *)exceptionInformation);
}

void Sg_TerminateThread(SgInternalThread *thread)
{
  /* FIXME I'm not sure if it's valid or not */
  /* Get current thread */
  HANDLE selfH = Sg_VM()->thread.thread;
  HANDLE threadH = thread->thread;
  if (SG_EQ(selfH, threadH)) {
    /* self termination.
       we do not support this, so just ignore.
       TODO: should we throw error?
     */
    return;
  }

  SuspendThread(threadH);
  if (WaitForSingleObject(threadH, 0) == WAIT_TIMEOUT) {
    CONTEXT context;
    context.ContextFlags = CONTEXT_CONTROL;
    GetThreadContext(threadH, &context);
    PTW32_PROGCTR(context) = (DWORD_PTR)cancel_self;
    SetThreadContext(threadH, &context);
    ResumeThread(threadH);
  }
  thread->returnValue = SG_UNDEF;
  
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
