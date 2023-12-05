/* selector-iocp.c                                  -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2023  Takashi Kato <ktakashi@ymail.com>
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

#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "socket-selector.h"
#include <windows.h>

typedef struct iocp_context_rec
{
  HANDLE iocp;
  HANDLE thread;		/* waiting thread */
} iocp_context_t;


static void selector_finalizer(SgObject self, void *data)
{
  Sg_CloseSocketSelector(SG_SOCKET_SELECTOR(self));
}

static void system_error(int code)
{
  Sg_SystemError(GetLastError(),
		 UC("Setting up IOCP failed: %A"),
		 Sg_GetLastErrorMessageWithErrorCode(code));
}

SgObject Sg_MakeSocketSelector()
{
  SgSocketSelector *selector = SG_NEW(SgSocketSelector);
  iocp_context_t *ctx = SG_NEW(iocp_context_t);

  SG_SET_CLASS(selector, SG_CLASS_SOCKET_SELECTOR);
  ctx->iocp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
  if (ctx->iocp == NULL) goto err;
  ctx->thread = NULL;
  selector->sockets = SG_NIL;

  Sg_RegisterFinalizer(selector, selector_finalizer, NULL);
  return SG_OBJ(selector);

 err:
  system_error(Sg_GetLastError());
  return SG_UNDEF;		/*  dummy */
}

void Sg_CloseSocketSelector(SgSocketSelector *selector)
{
  iocp_context_t *ctx = (iocp_context_t *)selector->context;
  CloseHandle(ctx->iocp);
  Sg_UnregisterFinalizer(selector);
}

SgObject Sg_SocketSelectorAdd(SgSocketSelector *selector, SgSocket *socket)
{
  iocp_context_t *ctx = (iocp_context_t *)selector->context;
  HANDLE r;
  r = CreateIoCompletionPort(ctx->iocp, (HANDLE)socket->socket,
			     (ULONG_PTR)socket, 0);
  if (r == NULL) {
    system_error(Sg_GetLastError());
  }
  selector->sockets = Sg_Cons(socket, selector->sockets);
  return SG_OBJ(selector);
}

SgObject Sg_SocketSelectorWait(SgSocketSelector *selector, SgObject timeout)
{
  iocp_context_t *ctx = (iocp_context_t *)selector->context;
  int n = Sg_Length(selector->sockets), i, millis = INFINITE;
  ULONG removed;
  LPOVERLAPPED_ENTRY entries;
  BOOL r;
  struct timespec spec, *sp;
  SgObject ret = SG_NIL;

  if (ctx->thread != NULL) {
    Sg_Error(UC("There's a thread already waiting for %A"), selector);
  }
  ctx->thread = GetCurrentThread();

  sp = Sg_GetTimeSpec(timeout, &spec);
  if (sp) {
    millis = sp->tv_sec * 1000;
    millis += sp->tv_nsec / 1000000;
  }

  entries = SG_NEW_ATOMIC2(OVERLAPPED_ENTRY *, n * sizeof(OVERLAPPED_ENTRY));
  r = GetQueuedCompletionStatusEx(ctx->iocp, entries, n, &removed, millis, TRUE);
  if (r) {
    for (i = 0; i < removed; i++) {
      ret = Sg_Cons((SgObject) entries[i].lpCompletionKey, ret);
    }
  } else {
    system_error(Sg_GetLastError());
  }
  ctx->thread = NULL;
  return ret;
}

static void CALLBACK dummy(ULONG_PTR param)
{
  /* Do nothing */
}

SgObject Sg_SocketSelectorInterrupt(SgSocketSelector *selector)
{
  iocp_context_t *ctx = (iocp_context_t *)selector->context;
  if (ctx->thread) {
    QueueUserAPC(dummy, ctx->thread, 0);
  }
  return selector;
}
