/* thread.c                                         -*- mode:c; coding:utf-8 -*-
 *
 *   Copyright (c) 2010-2021  Takashi Kato <ktakashi@ymail.com>
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

#include <sagittarius/config.h>

#include <pthread.h>
#include <sys/time.h>
#include <signal.h>
#include <string.h>
#include <fcntl.h>
/* to make pthread redirect happen */
#include "../../gc-incl.inc"

#include <sagittarius/private/thread.h>
#include <sagittarius/private/core.h>
#include <sagittarius/private/error.h>
#include <sagittarius/private/pair.h>
#include <sagittarius/private/string.h>
#include <sagittarius/private/symbol.h>
#include <sagittarius/private/vm.h>
#include <sagittarius/private/unicode.h>

/* Thank you Debian, we need this stupid kludge */
#ifndef HAVE_MUTEX_RECURSIVE
#define PTHREAD_MUTEX_RECURSIVE PTHREAD_MUTEX_RECURSIVE_NP
#endif

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

void Sg__MutexCleanup(void *mutex_)
{
  SgInternalMutex *mutex = (SgInternalMutex *)mutex_;
  pthread_mutex_unlock(&mutex->mutex);
}

void Sg_DestroyMutex(SgInternalMutex *mutex)
{
  pthread_mutex_destroy(&mutex->mutex);
}

static void exit_handler(int signum)
{
  pthread_exit(NULL);
}

static void* thread_entry(void *params)
{
  SgThreadEntryFunc *entry = (SgThreadEntryFunc *)((void **)params)[0];
  void *param = ((void **)params)[1];
  struct sigaction        sa;
  /* we use SIGALRM to cancel select */
  memset(&sa, 0, sizeof(sa));
  sa.sa_handler = exit_handler;
  sigemptyset(&sa.sa_mask);
#ifdef SIGABRT
  sigaction(SIGABRT, &sa, NULL );
#endif
#ifdef SIGSEGV
  sigaction(SIGSEGV, &sa, NULL );
#endif
#ifdef SIGBUS
  sigaction(SIGBUS,  &sa, NULL );
#endif
#ifdef SIGILL
  sigaction(SIGILL,  &sa, NULL );
#endif
#ifdef SIGFPE
  sigaction(SIGFPE,  &sa, NULL );
#endif
#if 0
  sigaction(SIGINT,  &sa, NULL );
#endif
  ((void **)params)[1] = NULL;
  return entry(param);
}

int Sg_InternalThreadStart(SgInternalThread *thread, SgThreadEntryFunc *entry, void *param)
{
  int ok = TRUE;
  pthread_attr_t thattr;
  void **params = SG_NEW_ARRAY(void *, 2);
  pthread_attr_init(&thattr);
  pthread_attr_setdetachstate(&thattr, PTHREAD_CREATE_DETACHED);
  params[0] = entry;
  params[1] = param;
  
  if (pthread_create(&thread->thread, &thattr, thread_entry, params) != 0) {
    ok = FALSE;
  }
  pthread_attr_destroy(&thattr);
  return ok;
}

void Sg_SetCurrentThread(SgInternalThread *ret)
{
  ret->thread = pthread_self();
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
  return pthread_cond_signal(&cond->cond);
}

int Sg_NotifyAll(SgInternalCond *cond)
{
  return pthread_cond_broadcast(&cond->cond);
}

int Sg_Wait(SgInternalCond *cond, SgInternalMutex *mutex)
{
  return pthread_cond_wait(&cond->cond, &mutex->mutex);
}

int Sg_WaitWithTimeout(SgInternalCond *cond, SgInternalMutex *mutex,
		       struct timespec *pts)
{
  return pthread_cond_timedwait(&cond->cond, &mutex->mutex, pts);
}

void Sg_ExitThread(SgInternalThread *thread, void *ret)
{
  if (Sg_MainThreadP()) {
    /* at least on macosx, pthread_exit on main thread hangs
       most likely due to the other live threads automatically
       created (by GC?). So, just exit, if the thread is main
       thread.
     */
    Sg_Exit(0);
  } else {
    pthread_exit(ret);
  }
}

void Sg_TerminateThread(SgInternalThread *thread)
{
  pthread_cancel(thread->thread);
}

int  Sg_InterruptThread(SgInternalThread *thread)
{
  return pthread_kill(thread->thread, SIGALRM) == 0;
}

static void ignore_handler(int signum)
{
  SgVM *vm = Sg_VM();
  /* VM may not be initialised yet. */
  if (vm) {
    (&vm->thread)->interrupted = TRUE;
    vm->attentionRequest = TRUE;
  }
}

/* called from Sg__InitSystem */
void Sg__InitThread()
{
  struct sigaction        actions;
  /* we use SIGALRM to cancel select */
  memset(&actions, 0, sizeof(actions));
  sigemptyset(&actions.sa_mask);
  /* actions.sa_flags = SA_RESTART; */
  actions.sa_handler = ignore_handler;
  sigaction(SIGALRM, &actions, NULL);
}

SgInternalSemaphore * Sg_InitSemaphore(SgString *name, int value)
{
  SgInternalSemaphore *semaphore = SG_NEW(SgInternalSemaphore);
  if (value > SEM_VALUE_MAX) {
    Sg_AssertionViolation(SG_INTERN("make-semaphore"),
			  SG_MAKE_STRING("value is too big"),
			  SG_LIST1(SG_MAKE_INT(value)));
  }
  if (name) {
    char *semname = Sg_Utf32sToUtf8s(name);
    int flags = 0;
    if (semname[0] != '/') {
      Sg_AssertionViolation(SG_INTERN("make-semaphore"),
			    SG_MAKE_STRING("name must start with '/'"),
			    SG_LIST1(name));
    }
    if (value >= 0) flags |= O_CREAT;
    else value = 0;		/* ignore it, please */
    /* TODO mode? */
    semaphore->semaphore = sem_open(semname, flags, 0666, value);
    if (semaphore->semaphore == SEM_FAILED) {
      int e = errno;
      char *msg = strerror(e);
      if (e == ENOENT) {
	Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR,
		   SG_INTERN("open-semaphore"),
		   Sg_Utf8sToUtf32s(msg, strlen(msg)),
		   name, SG_UNDEF);
      } else {
	Sg_SystemError(errno, UC("failed to sem_open %A"), 
		       Sg_Utf8sToUtf32s(msg, strlen(msg)));
      }
    }
    semaphore->name = SG_OBJ(name);
  } else {
    /* Some *BSD related don't support sem_init, it'd return EPERM */
    sem_t *sem = SG_NEW(sem_t);
    if (value < 0) {
      Sg_AssertionViolation(SG_INTERN("make-semaphore"),
			    SG_MAKE_STRING("anonymous semaphore must have positive initial value"),
			    SG_LIST1(SG_MAKE_INT(value)));
    }
    if (sem_init(sem, 1, value) == -1) {
      char *msg = strerror(errno);
      Sg_SystemError(errno, UC("failed to sem_init %A"), 
		     Sg_Utf8sToUtf32s(msg, strlen(msg)));
    }
    semaphore->semaphore = sem;
    semaphore->name = SG_FALSE;
  }
  return semaphore;
}

#if !defined(HAVE_SEM_TIMEDWAIT) && defined(HAVE_SEM_TRYWAIT)
/* This is workaround for OSX. We do some approximate timeout */
#include <time.h>

#define MAX_TRY_COUNT 10
static int emulate_sem_timewait(sem_t *sem, const struct timespec *timeout)
{
  /* CAUTION: this is too sloppy!!!

     The idea is that using sem_trywait and nanosleep. We calculate delta
     which is MAX_TRY_COUNT percent of the timeout time, then calling 
     sem_trywait until either it succeeds or hit MAX_TRY_COUNT times 
     failure.

     There's a better solution which uses thread to monitor but this function
     is POSIX, so OSX *MUST* support.

     TODO: probably won't be fix, though.
      This doesn't consider any signal call during waiting/sem_trywait.
      Maybe better to consider those thing, but why should we? This should
      not be a function user program emulates!
  */
  if (sem_trywait(sem) == 0) {
    return 0;
  } else {
    int i;
    struct timespec delta;
    /* well... */
    delta.tv_sec = timeout->tv_sec / 10;
    delta.tv_nsec = timeout->tv_nsec / 10;
    /* check */
    if (timeout->tv_sec < 0 || timeout->tv_nsec > 1000000000) {
      errno = EINVAL;
      return -1;
    } else if (delta.tv_sec == 0 && delta.tv_nsec == 0) {
      /* invalid delta */
      errno = EINVAL;
      return -1;
    }
    /* now try */
    for (i = 0; i < MAX_TRY_COUNT; i++) {
      /* we don't consider non-sleeping time, sorry */
      nanosleep(&delta, NULL);
      if (sem_trywait(sem) == 0) return 0;
    }
    errno = ETIMEDOUT;
    return -1;
  }
  
}
#endif

int Sg_WaitSemaphore(SgInternalSemaphore *semaphore, struct timespec *pts)
{
  int r = 0;
  if (pts) {
#if defined(HAVE_SEM_TIMEDWAIT)
    r = sem_timedwait(semaphore->semaphore, pts);
#elif defined(HAVE_SEM_TRYWAIT)
    r = emulate_sem_timewait(semaphore->semaphore, pts);
#else
    Sg_SystemError(-1, UC("sem_timedwait is not supported on this platform"));
#endif
  } else {
    r = sem_wait(semaphore->semaphore);
  }
  if (r != 0) return errno;
  return r;
}

int Sg_PostSemaphore(SgInternalSemaphore *semaphore)
{
  int r = sem_post(semaphore->semaphore);
  if (r != 0) return errno;
  return r;
}
void Sg_CloseSemaphore(SgInternalSemaphore *semaphore)
{
  if (!SG_FALSEP(semaphore->name)) {
    sem_close(semaphore->semaphore);
  }
}
void Sg_DestroySemaphore(SgInternalSemaphore *semaphore)
{
  if (SG_FALSEP(semaphore->name)) {
    /* it's not supported on OSX */
#ifndef __APPLE__
    sem_destroy(semaphore->semaphore);
#endif
  } else {
    char *name = Sg_Utf32sToUtf8s(semaphore->name);
    sem_unlink(name);
  }
}


/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
