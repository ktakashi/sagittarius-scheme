/* kernal.c                                        -*- mode:c; coding:utf-8; -*-
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
 */
#define LIBSAGITTARIUS_BODY
#include "sagittarius/private/kernel.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/system.h"
#include "sagittarius/private/thread.h"
#include "sagittarius/private/writer.h"
#include "sagittarius/private/vm.h"

#include "gc-incl.inc"

static void kernel_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<kernel 0x%x threads=%d>"), obj,
	    SG_KERNEL(obj)->nThreads);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_KernelClass, kernel_print);

static void append_entry(SgKernel *k, SgObject value)
{
  SgDLinkNode *t = SG_NEW(SgDLinkNode), *n = k->threads;
  t->value = value;
  Sg_LockMutex(&k->lock);

  while (n->next) n = n->next;

  t->prev = n;
  n->next = t;
  k->nThreads++;

  Sg_UnlockMutex(&k->lock);
}

static void remove_entry(SgKernel *k, SgObject value)
{
  SgDLinkNode *n = k->threads;

  Sg_LockMutex(&k->lock);
  
  while (n && n->value != value) n = n->next;
  
  if (n) {
    n->value = NULL;
    /* n->prev must always be there, but just in case */
    if (n->prev) n->prev->next = n->next;
    if (n->next) n->next->prev = n->prev;
    k->nThreads--;
  }
  Sg_UnlockMutex(&k->lock);
}

SgObject Sg_NewKernel(SgVM *rootVM)
{
  SgKernel *k = SG_NEW(SgKernel);
  SG_SET_CLASS(k, SG_CLASS_KERNEL);
  k->nThreads = 1;
  k->threads = SG_NEW(SgDLinkNode);
  k->threads->value = rootVM;
  k->threads->prev =  k->threads->next = NULL;
  Sg_InitMutex(&k->lock, FALSE);

  rootVM->kernel = k;
  return SG_OBJ(k);
}

static void thread_cleanup(void *data)
{
  SgVM *vm = SG_VM(data);
  remove_entry(SG_KERNEL(vm->kernel), vm);
}

typedef struct 
{
  SgVM *vm;
  SgThreadEntryFunc *func;
} thread_param_t;

#ifdef _MSC_VER
typedef unsigned int thread_return_t;
#else
typedef void * thread_return_t;
#endif

static thread_return_t wrap(void *data)
{
  thread_param_t *d = (thread_param_t *)data;
  thread_return_t r;
  SgThreadEntryFunc *func = d->func;
  SgVM *vm = d->vm;
  /* In theory, we should use SG_UNWIND_PROTECT here, but I'm lazy... */
  thread_cleanup_push(thread_cleanup, vm);
  r = (*func)(vm);
  thread_cleanup_pop(TRUE);
  d->vm = NULL;
  d->func = NULL;
  vm = NULL;
  return r;
}

/* For now, all entries are daemon :) */
SgObject Sg_StartManagedThread(SgVM *vm, SgThreadEntryFunc *func, int daemonP)
{
  int err_state = FALSE;
  Sg_LockMutex(&vm->vmlock);
  if (vm->threadState != SG_VM_NEW) {
    err_state = TRUE;
  } else {
    
    thread_param_t *data = SG_NEW(thread_param_t);
    data->func = func;
    data->vm = vm;
    
    ASSERT(vm->thunk);
    vm->threadState = SG_VM_RUNNABLE;
    append_entry(SG_KERNEL(vm->kernel), vm);
    if (!Sg_InternalThreadStart(&vm->thread, (SgThreadEntryFunc *)wrap, data)) {
      remove_entry(SG_KERNEL(vm->kernel), vm);
      vm->threadState = SG_VM_NEW;
      err_state = TRUE;
    }
  }
  Sg_UnlockMutex(&vm->vmlock);
  if (err_state)
    Sg_Error(UC("attempt to start an already-started thread: %S"), vm);
  return SG_OBJ(vm);
}

SgObject Sg_KernelManagedThreads()
{
  SgKernel *k = SG_KERNEL(Sg_VM()->kernel);
  SgObject r = SG_NIL;
  SgDLinkNode *n = k->threads;

  Sg_LockMutex(&k->lock);
  while (n) {
    r = Sg_Cons(n->value, r);
    n = n->next;
  }
  Sg_UnlockMutex(&k->lock);
  return Sg_ReverseX(r);
}

int Sg_KernelManagedCount()
{
  return SG_KERNEL(Sg_VM()->kernel)->nThreads;
}
