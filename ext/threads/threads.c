/* -*- C -*- */
/*
 * threads.c: multi thread extensions.
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

static SgObject thread_error_handler(SgObject *args, int argc, void *data)
{
  return SG_UNDEF;
}

static SG_DEFINE_SUBR(thread_error_handler_STUB, 1, 0, thread_error_handler, SG_FALSE, NULL);

SgObject Sg_MakeThread(SgProcedure *thunk, SgObject name)
{
  SgVM *current = Sg_VM(), *vm;
  if (SG_PROCEDURE_REQUIRED(thunk) != 0) {
    Sg_Error(UC("thunk required, but got %S"), thunk);
  }
  vm = Sg_NewVM(current, name);
  vm->thunk = thunk;
  vm->exceptionHandler = SG_OBJ(&thread_error_handler_STUB);
  return SG_OBJ(vm);
}

static void* thread_entry(void *data)
{
  SgVM *vm = SG_VM(data);
  /* TODO cleanup */
  if (Sg_SetCurrentVM(vm)) {
    SG_UNWIND_PROTECT {
      vm->result = Sg_Apply(SG_OBJ(vm->thunk), SG_NIL);
    } SG_WHEN_ERROR {
      SgObject exc;
      switch (vm->escapeReason) {
      case SG_VM_ESCAPE_CONT:
	vm->resultException = Sg_MakeString(UC("stale continuation throws"), SG_LITERAL_STRING);
	break;
      default:
	Sg_Panic("unknown escape");
      case SG_VM_ESCAPE_ERROR:
	/* TODO thread exception */
	break;
      }
    } SG_END_PROTECT;
  } else {
    /* TODO exception */
  }
  return NULL;
}

SgObject Sg_ThreadStart(SgVM *vm)
{
  int err_state = FALSE;
  Sg_LockMutex(&vm->vmlock);
  if (vm->state != SG_VM_NEW) {
    err_state = TRUE;
  } else {
    ASSERT(vm->thunk);
    vm->state = SG_VM_RUNNABLE;
    Sg_InternalThreadStart(&vm->thread, thread_entry, vm);
  }
  Sg_UnlockMutex(&vm->vmlock);
  if (err_state) Sg_Error(UC("attempt to start an already-started thread: %S"), vm);
  return SG_OBJ(vm);
}

SgObject Sg_ThreadJoin(SgVM *vm, SgObject timeout, SgObject timeoutval)
{
}

SgObject Sg_ThreadStop(SgVM *vm, SgObject timeout, SgObject timeoutval)
{
}

SgObject Sg_ThreadCont(SgVM *vm)
{
}

SgObject Sg_ThreadSleep(SgObject timeout)
{
}

SgObject Sg_ThreadTerminate(SgVM *vm)
{
}

extern void Sg__Init_sagittarius_threads();
extern void Sg__InitMutex();

void Sg__Init_threads()
{
  Sg__InitMutex();
  Sg__Init_sagittarius_threads();
  SG_PROCEDURE_NAME(&thread_error_handler_STUB)
    = Sg_MakeString(UC("thread-exception-handler"), SG_LITERAL_STRING);
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
