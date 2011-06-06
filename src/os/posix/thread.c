/* -*- C -*- */
/*
 * thread.c
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
#include <pthread.h>
#include <sys/time.h>
#include <signal.h>
#include <time.h>
#include <errno.h>
#define LIBSAGITTARIUS_BODY
#include <sagittarius/thread.h>
#include <sagittarius/core.h>

void Sg_InitMutex(SgInternalMutex *mutex, int recursive)
{
  if (recursive) {
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(&mutex->mutex, &attr);
    pthread_mutexattr_destroy(&attr);
  } else {
    pthread_mutex_init(&mutex->mutex, NULL);
  }
}

void Sg_LockMutex(SgInternalMutex *mutex)
{
  pthread_mutex_lock(&mutex->mutex);
}

void Sg_UnlockMutex(SgInternalMutex *mutex)
{
  pthread_mutex_unlock(&mutex->mutex);
}

void Sg_DestroyMutex(SgInternalMutex *mutex)
{
  pthread_mutex_destroy(&mutex->mutex);
}

void Sg_InternalThreadStart(SgInternalThread *thread, SgThreadEntryFunc *entry, void *param)
{
  pthread_create(&thread->thread, NULL, entry, param);
  pthread_detach(thread->thread);
}

void Sg_InternalThreadYield()
{
  sched_yield();
}

SgInternalThread Sg_CurrentThread()
{
  SgInternalThread ret = { pthread_self() };
  return ret;
}

void Sg_InitCond(SgInternalCond *cond)
{
  pthread_cond_init(&cond->cond, NULL);
}

void Sg_DestroyCond(SgInternalCond *cond)
{
  pthread_cond_destroy(&cond->cond);
}

int Sg_Notify(SgInternalCond *cond)
{
  int ret = pthread_cond_signal(&cond->cond);
  return ret == 0;
}

int Sg_NotifyAll(SgInternalCond *cond)
{
  int ret = pthread_cond_broadcast(&cond->cond);
  return ret == 0;
}

int Sg_Wait(SgInternalCond *cond, SgInternalMutex *mutex)
{
  int ret = pthread_cond_wait(&cond->cond, &mutex->mutex);
  return ret == 0;
}

int Sg_WaitWithTimeout(SgInternalCond *cond, SgInternalMutex *mutex, int msecs)
{
  struct timeval  now;
  struct timespec timeout;
  int ret = 0;
  if (gettimeofday(&now, NULL) != 0) {
    Sg_Panic("Fail to get current time");
  }
  now.tv_usec += msecs * 1000;
  while (now.tv_usec >= 1000000) {
    now.tv_sec++;
    now.tv_usec -= 1000000;
  }
  timeout.tv_sec = now.tv_sec;
  timeout.tv_nsec = now.tv_usec * 1000;
  while (timeout.tv_nsec >= 1000000000) {
    timeout.tv_sec++;
    timeout.tv_nsec -= 1000000000;
  }

  do {
    ret = pthread_cond_timedwait(&cond->cond, &mutex->mutex, &timeout);
  } while (ret == EINTR);
  ASSERT(ret != EINVAL);
  return ETIMEDOUT != ret;
}

void Sg_ExitThread(SgInternalThread *thread, void *ret)
{
  pthread_exit(ret);
}

void Sg_TerminateThread(SgInternalThread *thread)
{
  pthread_cancel(thread->thread);
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
